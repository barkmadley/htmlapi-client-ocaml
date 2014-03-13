(** 
 
*)

open Core.Std
open Async.Std

type context = Uri.t * Nethtml.document list

let get_uri (uri, _html) = uri
let get_html (_uri, html) = html
let make_context uri html = (uri, html)

type item = Nethtml.document

type itemtype = string

type property_key = string * Nethtml.document list

type property_value =
  | Item of item
  | Data of string
  | Multiple of property_value list

let elem_children = function
  | Nethtml.Data _ -> []
  | Nethtml.Element (_name, _attrs, children) -> children

let rec dfs_fold f to_process items =
  match to_process with
  | [] -> items
  | Nethtml.Data _s :: tail -> dfs_fold f tail items
  | Nethtml.Element (_name, attrs, children) as head :: tail ->
  begin
    let add, items_next = f head items in
    dfs_fold f (add @ tail) items_next
  end

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

let get_value _context _item (_key, nodes) : property_value =
  let get_v node =
    match node with
    | Nethtml.Data s -> Data s
    | Nethtml.Element (_n, attrs, _c) ->
    begin
      match List.Assoc.find attrs "itemtype" with
      | Some t -> Item node
      | None -> Data (prop_v_to_string node)
    end
  in
  match nodes with
  | []     -> Data ""
  | [node] -> get_v node
  | nodes  -> Multiple (List.map ~f:get_v nodes)

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

let download uri : Nethtml.document list Deferred.t =
  Cohttp_async.Client.get uri
  >>= fun (_,body) ->
  Pipe.to_list body
  >>= fun lines ->
  let body = String.concat lines in
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
  return documents


let enter uri : context Deferred.t =
  (* printf "GET %s\n%!" (Uri.to_string uri); *)
  download uri >>| make_context uri

let enter_s s =
  enter (Uri.of_string s)
