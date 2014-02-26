(**

*)

open Core.Std
open Async.Std

open Cow

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

type property_key = string

val get_properties : context -> item -> property_key list

type property_value =
  | Remote of context
  | Items of item list

val get_value : context -> item -> property_key -> property_value

val enter : Uri.t -> ( context -> 'returned ) -> 'returned Deferred.t
val enter_s : string -> ( context -> 'returned ) -> 'returned Deferred.t

