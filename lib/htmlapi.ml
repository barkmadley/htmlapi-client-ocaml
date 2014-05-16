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
    let _, items_next = f head items in
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
        | Nethtml.Element (_n, _, c) -> c, items
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
    (* microdata_document = follow uri *)
    | Follow of Uri.t * (microdata_document -> 'next)

    (* microdata_object list = object microdata_document *)
    (* (string * uri) list = links microdata_document *)
    (* (string * form) list = forms microdata_document *)

    (* string list = props microdata_object *)
    (* microdata_field option = get_field microdata_object string *)

    (* (string * uri) list = links microdata_object *)
    (* (string * form) list = forms microdata_object *)

  type 'next t = 'next htmlapi (* for the Free functor *)

  let fmap f x = match x with
    | Follow (uri, cont) -> Follow (uri, Fn.compose f cont)

end

module FreeHtmlapi = Free(HtmlapiFunctor)
module FreeHtmlapi_Extra = MonadUtils(FreeHtmlapi)


let microdata_document_objects mdoc : microdata_object list =
  let module LA = List.Assoc in
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
