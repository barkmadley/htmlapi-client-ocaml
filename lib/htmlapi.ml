(** 
 
*)

open Core.Std
open Async.Std

open Free

type microdata_document =
  {
    uri: Uri.t;
    doc: Nethtml.document list;
  }

type microdata_object =
  {
    doc: microdata_document;
    root: Nethtml.document;
  }

type microdata_field =
  | Object of microdata_object
  | Data of string
  | Link of Uri.t
  | Multiple of microdata_field list

let microdata_object_type mobj : string list =
  match mobj.root with
  | Nethtml.Data _ -> []
  | Nethtml.Element (_n, attrs, _c) ->
    String.split ~on:' ' (
      Option.value (List.Assoc.find attrs "itemtype") ~default:""
    )

let microdata_object_to_string mobj =
  Printf.sprintf "<object: [%s]>"
    (String.concat ~sep:", " (microdata_object_type mobj))

let rec dfs_fold f to_process items =
  match to_process with
  | [] -> items
  | Nethtml.Data _ as head :: tail ->
  begin
    let add, items_next = f head items in
    dfs_fold f tail items_next
  end
  | Nethtml.Element _ as head :: tail ->
  begin
    let add, items_next = f head items in
    dfs_fold f (add @ tail) items_next
  end

let get_children doc =
  match doc with
  | Nethtml.Data _ -> []
  | Nethtml.Element (_, _, c) -> c

(* TODO: does this have to be a string? *)
let microdata_object_list_properties mobj : string list =
  let f elem items =
    match elem with
    | Nethtml.Data _s -> [], items
    | Nethtml.Element (_n, attrs, children) ->
    begin
      match List.Assoc.find attrs "itemprop" with
      | Some p -> [], (p :: items)
      | _      -> children, items
    end
  in
  List.dedup (List.rev (dfs_fold f (get_children mobj.root) []))

let make_microdata_field doc root domname attrs children =
  match List.Assoc.find attrs "itemscope", List.Assoc.find attrs "itemtype" with
  | _,Some _ | Some _,_ -> Object ({ doc = doc; root = root; })
  | _ ->
  begin
    match domname with
    | "a" when List.Assoc.mem attrs "href" ->
    begin
      Link (Uri.resolve "" doc.uri (Uri.of_string (List.Assoc.find_exn attrs "href")))
    end
    | _ ->
    begin
      let f elem items =
        match elem with
        | Nethtml.Data s -> [], (String.strip s :: items)
        | Nethtml.Element (n, _, c) -> c, items
      in
      let r = String.concat ~sep:" " (List.rev (dfs_fold f children [])) in
      Data r
    end
  end

let join_microdata_fields field1 field2 =
  match field1 with
  | Multiple xs ->
  begin
    match field2 with
    | Multiple ys -> Multiple (xs @ ys)
    | _ -> Multiple (xs @ [field2])
  end
  | _ ->
  begin
    match field2 with
    | Multiple ys -> Multiple (field1 :: ys)
    | _ -> Multiple (field1 :: [field2])
  end

let microdata_object_get mobj str : microdata_field option =
  let f elem items =
    match elem with
    | Nethtml.Data _ -> [], items
    | Nethtml.Element (name, attrs, children) ->
    begin
      let recurse =
        match List.Assoc.find attrs "itemscope" with
        | Some _ -> []
        | None -> children
      in
      let new_items =
        match List.Assoc.find attrs "itemprop" with
        | Some p when p = str ->
        begin
          let new_obj = make_microdata_field mobj.doc elem name attrs children in
          match items with
          | None   -> Some new_obj
          | Some x -> Some (join_microdata_fields x new_obj)
        end
        | _                   -> items
      in
      recurse, new_items
    end
  in
  dfs_fold f (get_children mobj.root) None

let rec html_to_string html =
  match html with
  | Nethtml.Data s -> s
  | Nethtml.Element (n, a, c) ->
  begin
    Printf.sprintf "<%s%s>%s</%s>" n (
      (* Attributes *)
      String.concat ~sep:"" (
        List.map ~f:(fun (k,v) -> " " ^ k ^ "=\"" ^ v ^ "\"") a
      )
    ) (
      (* Children *)
      String.concat ~sep:"" (List.map ~f:html_to_string c)
    ) n
  end

(* let microdata_single_property_to_string obj value =
  match value with
  | Nethtml.Data s -> s
  | Nethtml.Element (n, attrs, children) when String.lowercase n = "a" ->
  begin
    
  end
  | _ ->
  begin
    let f elem items =
      match elem with
      | Nethtml.Data s -> [], (String.strip s :: items)
      | Nethtml.Element (n, _, c) -> c, items
    in
    let r = String.concat ~sep:" " (List.rev (dfs_fold f [value] [])) in
    r
  end *)


let rec microdata_object_to_string_with_prop mobj =
  let props = microdata_object_list_properties mobj in
  let prop_strings =
    List.map ~f:(fun prop ->
      let value = microdata_object_get mobj prop in
      let val_str =
        Option.value_map ~default:"<none>"
          ~f:microdata_property_to_string value
      in
      "\t" ^ prop ^ ":" ^ (String.concat ~sep:"\n" (List.map ~f:(fun x -> "\t" ^ x) (String.split_lines val_str)))
    ) props
  in
  Printf.sprintf "<object: [%s]>\n%s\n"
    (String.concat ~sep:", " (microdata_object_type mobj))
    (String.concat ~sep:"\n" prop_strings)

and microdata_property_to_string value =
  match value with
  | Data s -> s
  | Link a -> "<a href=" ^ Uri.to_string a
  | Object o -> microdata_object_to_string_with_prop o
  | Multiple m -> "[" ^
    String.concat ~sep:", " (
      List.map ~f:microdata_property_to_string m
    ) ^ "]"

module HtmlapiFunctor = struct

  type 'next htmlapi =
    (* document_list = enter uri *)
    | Follow of Uri.t * (microdata_document -> 'next)

  type 'next t = 'next htmlapi (* for the Free functor *)

  let fmap f x = match x with
    | Follow (uri, cont) -> Follow (uri, Fn.compose f cont)

end

module FreeHtmlApi = Free(HtmlapiFunctor)
module FreeHtmlApi_Extra = MonadUtils(FreeHtmlApi)

module IOInterpreter = struct

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
    | FreeHtmlApi.Return x -> return x
    | FreeHtmlApi.Wrap fa ->
      match fa with
      | HtmlapiFunctor.Follow (uri, cont) ->
      begin
        download uri
        >>= fun doc ->
        unsafePerform (cont doc)
      end
end

(* let docs_to_string docs =
  let buffer = Buffer.create 10 in
  let netout = Netchannels.output_buffer () buffer in
  let () = Nethtml.write netout docs in
  netout # close_out ();
  buffer.contents buffer *)

let get_items mdoc : microdata_object list =
  let module LA = List.Assoc in
  let make_item _context node = node in
  let f elem items =
    match elem with
    | Nethtml.Data _s -> [], items
    | Nethtml.Element (_name, attrs, children) ->
    begin
      match LA.find attrs "itemtype", LA.find attrs "itemprop" with
      | Some _, None -> [], ({ doc = mdoc; root = elem} :: items)
      | _ -> children, items
    end
  in
  List.rev (dfs_fold f mdoc.doc [])

let example : microdata_document FreeHtmlApi.t =
  let (>>=) m n = FreeHtmlApi.bind m n in
  let (>>) m n = FreeHtmlApi_Extra.seq m n in
  let download uri = FreeHtmlApi.liftF (HtmlapiFunctor.Follow (uri, Fn.id)) in
  let print_uri uri_s : microdata_document FreeHtmlApi.t =
    download (Uri.of_string uri_s) >>= fun docs ->
    let items = get_items docs in
    let strings =
      List.iter ~f:(fun item ->
        printf "%s\n%!" (microdata_object_to_string_with_prop item);
      ) items
    in
    FreeHtmlApi.return docs
  in

  print_uri "http://localhost:8080/" >>
  print_uri "http://localhost:8080/inline-link" >>
  print_uri "http://localhost:8080/inline-repr" >>
  print_uri "http://localhost:8080/inline-repr#12345"

let main () : unit Deferred.t =
  IOInterpreter.unsafePerform example >>= (fun _ -> return ())

let () =
  upon (main ()) (fun () -> Shutdown.shutdown 0);
  never_returns (Scheduler.go ())


module Uri = struct
  module Inner_Uri = struct
    include Uri
    let compare = compare
    let hash = Hashtbl.hash
  end

  include Inner_Uri
  include Hashable.Make (Inner_Uri)
end

type context =
  {
    cache: (Nethtml.document list) Uri.Table.t;
    mutable current: (Uri.t * Nethtml.document list) option;
  }

let get_uri (uri, _html):context = uri
let get_html (c:context) =
    Option.value ~default:[] (Option.map ~f:snd c.current)

let make_context () =
  return
  {
    cache = Uri.Table.create ();
    current = None;
  }

type item = Nethtml.document

type itemtype = string

type property_key = string * Nethtml.document list

type property_value =
  | Item of item
  | Data of string
  | Multiple of property_value list

let rec dfs_fold f to_process items =
  match to_process with
  | [] -> items
  | Nethtml.Data _s :: tail -> dfs_fold f tail items
  | Nethtml.Element (_name, attrs, children) as head :: tail ->
  begin
    let add, items_next = f head items in
    dfs_fold f (add @ tail) items_next
  end

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

let download_non_fragment context uri : Nethtml.document list Deferred.t =
  match Hashtbl.find context.cache uri with
  | Some docs -> return docs
  | None ->
  begin
    Cohttp_async.Client.get uri
    >>= fun (_,body) ->
    Pipe.to_list (Cohttp_async.Body.to_pipe body)
    >>= fun lines ->
    let body = String.concat lines in
    (* printf "%s\n%!" body; *)
    let ch = new Netchannels.input_string body in
    let documents = Nethtml.parse ch in
    ch # close_in ();
    let () = Hashtbl.replace context.cache uri documents in
    return documents
  end

let enter context uri: context Deferred.t =
  printf "GET %s\n%!" (Uri.to_string uri);
  let default_host = Some "127.0.0.1" in
  let uri =
    Uri.with_port uri (Some 8080)
  in
  let uri =
    match Uri.host uri with
    | None ->
    begin
      match context.current with
      | None -> Uri.with_host uri default_host
      | Some (u,_c) ->
        Uri.with_host uri (Option.first_some (Uri.host u) default_host)
    end
    | Some _ -> uri
  in
  download_non_fragment context (Uri.with_fragment uri None) >>|
  fun content ->
  let found_content =
    Option.value
      ~default:content
      (Option.map (Uri.fragment uri) ~f:(document_get_by_id content))
  in
  let new_current = uri, found_content in
  let () = context.current <- Some (new_current) in
  context


let elem_children = function
  | Nethtml.Data _ -> []
  | Nethtml.Element (_name, _attrs, children) -> children

let get_items context =
  let module LA = List.Assoc in
  let make_item _context node = node in
  let f elem items =
    match elem with
    | Nethtml.Data _s -> [], items
    | Nethtml.Element (_name, attrs, children) ->
    begin
      match LA.find attrs "itemtype", LA.find attrs "itemprop" with
      | Some _, None -> [], (make_item context elem :: items)
      | _ -> children, items
    end
  in
  dfs_fold f (get_html context) []

let itemtype itemtype_str =
  [itemtype_str]

let itemtype_to_string itemtype = itemtype

let get_itemtype _context item =
  match item with
  | Nethtml.Data _ -> itemtype ""
  | Nethtml.Element (_n, attrs, _c) ->
    itemtype (Option.value (List.Assoc.find attrs "itemtype") ~default:"")

let is_type context item itemtype =
  List.mem (get_itemtype context item) itemtype

let make_props text head =
  (* TODO: split text on space and create separate elements *)
  [(text, [head])]

let get_properties _context item =
  let rec dfs_helper to_process props =
    match to_process with
    | [] -> props
    | Nethtml.Data _ :: tail -> dfs_helper tail props
    | Nethtml.Element (_n, attrs, children) as head :: tail ->
    begin
      match List.Assoc.find attrs "itemprop" with
      | None -> dfs_helper (children @ tail) props
      | Some text -> dfs_helper tail (make_props text head @ props)
    end
  in
  dfs_helper (elem_children item) []

let prop_k_to_string p =
  match p with
  | Nethtml.Data s -> s (* Should not happen really *)
  | Nethtml.Element (_n, attrs, _c) ->
    Option.value (List.Assoc.find attrs "itemprop") ~default:"missing!"

let property_key_to_string (key, _nodes) = key

let prop_v_to_string p =
  match p with
  | Nethtml.Data s -> s
  | Nethtml.Element (_n, _attrs, children) ->
  begin
    match children with
    | [Nethtml.Data s] -> s
    | _ -> "missing!"
  end

let item_to_string context item =
  Printf.sprintf "<item: [%s]>"
    (String.concat ~sep:", "
      (List.map ~f:itemtype_to_string (get_itemtype context item)))

let rec prop_value_to_string c _i p =
  match p with
  | Multiple props ->
  begin
     Printf.sprintf "[%s]"
      (String.concat ~sep:", "
        (List.map ~f:(prop_value_to_string c _i) props))
  end
  | Data s -> s
  | Item i -> item_to_string c i

let get_value context _item (_key, nodes) : property_value Deferred.t =
  let get_v node =
    match node with
    | Nethtml.Data s -> return (Data s)
    | Nethtml.Element ("a", attrs, _c) ->
    begin
      match List.Assoc.find attrs "href" with
      | None -> return (Data (prop_v_to_string node))
      | Some href ->
      begin
        let uri = Uri.of_string href in
        enter context uri
        >>= fun new_context ->
        match get_items new_context with
        | [item] -> return (Item item)
        | [] -> return (Data "no items")
        | multiple -> return (Multiple (List.map ~f:(fun i -> Item i) multiple))
      end
    end
    | Nethtml.Element (_n, attrs, _c) ->
    begin
      match List.Assoc.find attrs "itemtype" with
      | Some t -> return (Item node)
      | None -> return (Data (prop_v_to_string node))
    end
  in
  match nodes with
  | []     -> return (Data "")
  | [node] -> get_v node
  | nodes  ->
    Deferred.all (List.map ~f:get_v nodes)
    >>= fun items ->
    return (Multiple items)



let enter_s c s =
  enter c (Uri.of_string s)
