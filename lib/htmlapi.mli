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

val html_to_string : Nethtml.document -> string

type microdata_field =
  | Object of microdata_object
  | Data of string
  | Link of Uri.t

val microdata_document_objects : microdata_document -> microdata_object list

val microdata_object_type : microdata_object -> string list

val microdata_object_to_string : microdata_object -> string
val microdata_object_to_string_with_prop : microdata_object -> string

val microdata_object_list_properties : microdata_object -> string list

val microdata_object_get : microdata_object -> string -> microdata_field list

val microdata_object_links : microdata_object -> (string * Uri.t) list

val microdata_property_to_strings : microdata_field list -> string list

val dfs_fold : (Nethtml.document -> 'accum -> (Nethtml.document list * 'accum)) -> Nethtml.document list -> 'accum -> 'accum

module HtmlapiFunctor : sig
  type 'next htmlapi =
    | Follow of Uri.t * (microdata_document -> 'next)
    | Get_field of microdata_object * string * (microdata_field list -> 'next)

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
