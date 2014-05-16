(**

*)

open Core.Std
open Async.Std

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

val microdata_document_objects : microdata_document -> microdata_object list

val microdata_object_type : microdata_object -> string list

val microdata_object_to_string : microdata_object -> string
val microdata_object_to_string_with_prop : microdata_object -> string

val microdata_object_list_properties : microdata_object -> string list

val microdata_object_get : microdata_object -> string -> microdata_field option

val microdata_property_to_string : microdata_field -> string

val dfs_fold : (Nethtml.document -> 'accum -> (Nethtml.document list * 'accum)) -> Nethtml.document list -> 'accum -> 'accum

module HtmlapiFunctor : sig
  type 'next htmlapi =
    | Follow of Uri.t * (microdata_document -> 'next)

  type 'next t = 'next htmlapi (* for the Free functor *)

  val fmap : ('a -> 'b) -> 'a t -> 'b t

end

module FreeHtmlapi : sig
  type 'a t = Return of 'a
              | Wrap of ('a t) HtmlapiFunctor.t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val liftF : 'a HtmlapiFunctor.t -> 'a t
end


module FreeHtmlapi_Extra : sig

  val join : 'a FreeHtmlapi.t FreeHtmlapi.t -> 'a FreeHtmlapi.t
  val map : ('a -> 'b) -> 'a FreeHtmlapi.t -> 'b FreeHtmlapi.t

  val seq : 'a FreeHtmlapi.t -> 'b FreeHtmlapi.t -> 'b FreeHtmlapi.t
end

(*

(** The context is an abstract type that tracks things like what the current
  document uri is such that object field traversals can load other documents
  and other hypermedia minitia.
*)
type context

(** A Microdata item is defined by the w3c
  @see <http://www.w3.org/TR/microdata/> W3C microdata specification
*)
type item

(** 
*)
val get_items : context -> item list

(**
*)
val item_to_string : context -> item -> string

(** A Microdata itemtype. An item can have multiple types.
*)
type itemtype

(** Create an itemtype from a string (itemtypes are space separated so any
  given itemtype string can result in multiple types)
*)
val itemtype : string -> itemtype list

val itemtype_to_string : itemtype -> string

(** Get the itemtypes for a specific item (there can be 0 or more)
*)
val get_itemtype : context -> item -> itemtype list

(** Check if a given item is of a specific itemtype
*)
val is_type : context -> item -> itemtype -> bool

type property_key

val property_key_to_string : property_key -> string

val get_properties : context -> item -> property_key list

type property_value =
  | Item of item
  | Data of string
  | Multiple of property_value list

val get_value : context -> item -> property_key -> property_value Deferred.t

val prop_value_to_string : context -> item -> property_value -> string

val make_context : unit -> context Deferred.t

val enter : context -> Uri.t -> context Deferred.t
val enter_s : context -> string -> context Deferred.t

*)