(** 
 
*)

open Core.Std
open Async.Std

open Free


let indent s = "  " ^ s
let indents s = List.map ~f:indent s

let rec html_to_string_depth html : string list =
  match html with
  | Nethtml.Data s -> [String.strip s]
  | Nethtml.Element (n, a, c) ->
  begin
    let attr_string =
      String.concat ~sep:"" (
        List.map ~f:(fun (k,v) -> " " ^ k ^ "=\"" ^ v ^ "\"") a
      )
    in
    [
      Printf.sprintf "<%s%s>" n attr_string;
    ] @
    indents (List.concat (List.map ~f:html_to_string_depth c)) @
    [
      Printf.sprintf "</%s>" n;
    ]
  end

let html_to_string html = String.concat ~sep:"\n" (html_to_string_depth html)

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

let microdata_object_links mobj =
  let f elem items =
    match elem with
    | Nethtml.Data _s -> [], items
    | Nethtml.Element ("a", attrs, c) ->
    begin
      match List.Assoc.find attrs "rel", List.Assoc.find attrs "href" with
      | Some rel, Some href ->
        c, ((rel, Uri.resolve "" mobj.doc.uri (Uri.of_string href)) :: items)
      | _ -> c, items
    end
    | Nethtml.Element (_n, _, c) -> c, items
  in
  dfs_fold f [mobj.root] []

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

let microdata_object_get mobj str : microdata_field list =
  let f elem items =
    (* Printf.printf "microdata_object_get: %s\n%s\n%!" str (html_to_string elem); *)
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
          (* Printf.printf "\tfound: %s\n%s\n%!" str (html_to_string elem); *)
          let new_obj = make_microdata_field mobj.doc elem name attrs children in
          new_obj :: items
        end
        | _                   -> items
      in
      recurse, new_items
    end
  in
  let r = dfs_fold f (get_children mobj.root) [] in
  (* (match r with
  | None -> Printf.printf "couldn't find: %s\n%!" str
  | Some s -> Printf.printf "did find: %s\n%!" str); *)
  r

let rec microdata_object_to_strings mobj : string list =
  let props = microdata_object_list_properties mobj in
  let props_strings =
    List.concat_map props ~f:(fun prop ->
      let value = microdata_object_get mobj prop in
      let prop_strings = microdata_property_to_strings value in
      let p = prop ^ ":" in
      match prop_strings with
      | [] -> [p ^ "<none>"]
      | [one] -> [p ^ one]
      | many -> p :: indents many
    )
  in
  let links = microdata_object_links mobj in
  let links_strings =
    List.map links ~f:(fun (link,v) ->
      "link - " ^ link ^ ":" ^ (Uri.to_string v)
    )
  in
  Printf.sprintf "<object: [%s]>" (String.concat ~sep:", " (microdata_object_type mobj))
  :: indents (props_strings @ links_strings)

and microdata_property_to_strings (values : microdata_field list) : string list =
  let rs =
    List.map values ~f:(fun value ->
      match value with
      | Data s -> [s]
      | Link a -> ["<a href=" ^ Uri.to_string a]
      | Object o -> microdata_object_to_strings o
    )
  in
  if List.length rs > 1 then
  begin
    let f i acc item_strings =
      let number = Printf.sprintf "[%d] " i in
      acc @ (List.map ~f:(fun s -> number ^ s) item_strings)
    in
    List.foldi ~init:[] ~f:f rs
  end
  else
    List.concat rs

let microdata_object_to_string_with_prop mobj =
  let ss = microdata_object_to_strings mobj in
  String.concat ~sep:"\n" ss

(* let rec microdata_object_to_string_with_prop mobj =
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
    ) ^ "]" *)

module HtmlapiFunctor = struct

  type 'next htmlapi =
    (* microdata_document = follow uri *)
    | Follow of Uri.t * (microdata_document -> 'next)

    (* microdata_object list = object microdata_document *)
    (* (string * uri) list = links microdata_document *)
    (* (string * form) list = forms microdata_document *)

    (* string list = props microdata_object *)
    (* microdata_field option = get_field microdata_object string *)
    | Get_field of microdata_object * string * (microdata_field list -> 'next)


    (* (string * uri) list = links microdata_object *)
    (* (string * form) list = forms microdata_object *)

  type 'next t = 'next htmlapi (* for the Free functor *)

  let fmap f x = match x with
    | Follow (uri, cont) -> Follow (uri, Fn.compose f cont)
    | Get_field (obj, field, cont) -> Get_field (obj, field, Fn.compose f cont)

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
      match LA.find attrs "itemprop" with
      | Some _ ->
      begin
        [], items
      end
      | None ->
      begin
        match LA.find attrs "itemtype" with
        | Some _ -> children, ({ doc = mdoc; root = elem} :: items)
        | None -> children, items
      end
    end
  in
  List.rev (dfs_fold f mdoc.doc [])
