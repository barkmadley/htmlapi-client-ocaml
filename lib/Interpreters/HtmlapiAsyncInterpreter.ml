open Core.Std
open Async.Std

open Htmlapi

module Interpreter = struct

  let document_get_by_id documents id : Nethtml.document list =
    let f elem items =
      match elem with
      | Nethtml.Data _s -> [], items
      | Nethtml.Element (_n, attrs, children) ->
      begin
        match List.Assoc.find attrs "id" with
        | Some id_v when id_v = id -> [], (elem :: items)
        | _                        -> children, items
      end
    in
    dfs_fold f documents []

  let download uri : microdata_document Deferred.t =
    printf "GET %s ..." (Uri.to_string uri);
    Cohttp_async.Client.get uri
    >>= fun (_,body) ->
    Pipe.to_list (Cohttp_async.Body.to_pipe body)
    >>= fun lines ->
    let body = String.concat lines in
    (* printf "%s\n%!" body; *)
    printf " OK\n";
    let ch = new Netchannels.input_string body in
    let documents = Nethtml.parse ~dtd:Nethtml.relaxed_html40_dtd ch in
    ch # close_in ();
    let documents =
      match Uri.fragment uri with
      | Some fragment ->
      begin
        let lookup = document_get_by_id documents fragment in
        match lookup with
        | [] -> documents
        | _  -> lookup
      end
      | None -> documents
    in
    return { uri = uri; doc = documents }

  (* TODO: download uri caching *)
  let rec unsafePerform (m : 'a FreeHtmlapi.t) : 'a Deferred.t =
    match m with
    | FreeHtmlapi.Return x -> return x
    | FreeHtmlapi.Wrap fa ->
      match fa with
      | HtmlapiFunctor.Follow (uri, cont) ->
      begin
        download uri
        >>= fun doc ->
        unsafePerform (cont doc)
      end
      | HtmlapiFunctor.Get_field (obj, field_name, cont) ->
      begin
        let v = Htmlapi.microdata_object_get obj field_name in
        match v with
        (* No results, potentially find field after derefencing self link *)
        | [] ->
        begin
          let links = Htmlapi.microdata_object_links obj in
          match List.Assoc.find links "self" with
          | None -> unsafePerform (cont [])
          | Some l ->
          begin
            download l >>= fun doc ->
            match Htmlapi.microdata_document_objects doc with
            | [o] ->
              unsafePerform (cont (Htmlapi.microdata_object_get o field_name))
            | other -> unsafePerform (cont [])
          end
        end
        (* Result is a link to another object, dereference link and return the object *)
        | [Link a] ->
        begin
          download a >>= fun doc ->
          let v =
            List.map ~f:(fun o -> Object o) (Htmlapi.microdata_document_objects doc)
          in
          unsafePerform (cont v)
        end
        | _ -> unsafePerform (cont v)
      end
end