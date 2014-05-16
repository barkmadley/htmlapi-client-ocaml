open Core.Std
open Async.Std

open Htmlapi

module Interpreter = struct

  let document_get_by_id document id : Nethtml.document list =
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
    dfs_fold f document []

  let download uri : microdata_document Deferred.t =
    Cohttp_async.Client.get uri
    >>= fun (_,body) ->
    Pipe.to_list (Cohttp_async.Body.to_pipe body)
    >>= fun lines ->
    let body = String.concat lines in
    printf "GET %s\n%!" (Uri.to_string uri);
    (* printf "%s\n%!" body; *)
    let ch = new Netchannels.input_string body in
    let documents = Nethtml.parse ch in
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

  let rec unsafePerform m : 'a Deferred.t =
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
end