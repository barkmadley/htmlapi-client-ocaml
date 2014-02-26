(** 
 
*)

open Core.Std
open Async.Std

type context = Nethtml.document list

type item = string

type itemtype = string

type property_key = string

type property_value =
  | Remote of context
  | Items of item list

let get_items context =
  []

let itemtype itemtype_str =
  [itemtype_str]

let itemtype_to_string itemtype = itemtype

let get_itemtype context item =
  itemtype item

let is_type context item itemtype =
  List.mem (get_itemtype context item) itemtype

let get_properties context item =
  []

let get_value context item property =
  Items []

let enter uri f =
  printf "GET %s\n%!" (Uri.to_string uri);
  Cohttp_async.Client.get uri
  >>= fun (_,body) ->
  printf "Got body\n%!";
  Pipe.to_list body
  >>= fun lines ->
  let body = String.concat lines in
  printf "%s\n%!" body;
  let ch = new Netchannels.input_string body in
  let documents = Nethtml.parse ch in
  ch # close_in ();
  return (f documents)

let enter_s s f =
  enter (Uri.of_string s) f
