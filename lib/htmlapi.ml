(** 
 
*)

open Core.Std
open Async.Std

type context = Nethtml.document list

type item = Nethtml.document

type itemtype = string

type property_key = Nethtml.document

type property_value =
  | Items of item list
  | Data of string

let prop_value_to_string c i p =
  match p with
  | Data s -> s
  | Items i -> "prop_value_to_string (Items i)"

let elem_children = function
  | Nethtml.Data _ -> []
  | Nethtml.Element (_name, _attrs, children) -> children

let get_items context =
  let module LA = List.Assoc in
  let make_item context node = node in
  let rec dfs_helper to_process items =
    match to_process with
    | [] -> items
    | Nethtml.Data _s :: tail -> dfs_helper tail items
    | Nethtml.Element (name, attributes, children) as head :: tail ->
    begin
      match LA.find attributes "itemtype", LA.find attributes "itemprop" with
      | Some t, None -> dfs_helper tail (make_item context head :: items)
      | _ -> dfs_helper (children @ tail) items
    end
  in
  dfs_helper context []

let itemtype itemtype_str =
  [itemtype_str]

let itemtype_to_string itemtype = itemtype

let get_itemtype context item =
  match item with
  | Nethtml.Data _ -> itemtype ""
  | Nethtml.Element (_n, attrs, _c) ->
    itemtype (Option.value (List.Assoc.find attrs "itemtype") ~default:"")

let is_type context item itemtype =
  List.mem (get_itemtype context item) itemtype

let make_props text head =
  (* TODO: split text on space and create separate elements *)
  [head]

let get_properties context item =
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

let get_value context item property =
  match property with
  | Nethtml.Data s -> Data s
  | Nethtml.Element (_n, attrs, children) ->
  begin
    match children with
    | [Nethtml.Data s] -> Data s
    | _ -> Data "test"
  end

let property_key_to_string p =
  match p with
  | Nethtml.Data s -> s (* Should not happen really *)
  | Nethtml.Element (_n, attrs, _c) ->
    Option.value (List.Assoc.find attrs "itemprop") ~default:"missing!"

let enter uri f =
  (* printf "GET %s\n%!" (Uri.to_string uri); *)
  Cohttp_async.Client.get uri
  >>= fun (_,body) ->
  (* printf "Got body\n%!"; *)
  Pipe.to_list body
  >>= fun lines ->
  let body = String.concat lines in
  (* printf "%s\n%!" body; *)
  let ch = new Netchannels.input_string body in
  let documents = Nethtml.parse ch in
  ch # close_in ();
  return (f documents)

let enter_s s f =
  enter (Uri.of_string s) f
