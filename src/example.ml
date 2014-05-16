open Core.Std
open Async.Std


open Htmlapi

let example : microdata_document FreeHtmlapi.t =
  let (>>=) m n = FreeHtmlapi.bind m n in
  let (>>) m n = FreeHtmlapi_Extra.seq m n in
  let download uri = FreeHtmlapi.liftF (HtmlapiFunctor.Follow (uri, Fn.id)) in
  let print_uri uri_s : microdata_document FreeHtmlapi.t =
    download (Uri.of_string uri_s) >>= fun doc ->
    let items = microdata_document_objects doc in
    let strings =
      List.iter ~f:(fun item ->
        printf "%s\n%!" (microdata_object_to_string_with_prop item);
      ) items
    in
    FreeHtmlapi.return doc
  in

  print_uri "http://localhost:8080/" >>
  print_uri "http://localhost:8080/inline-link" >>
  print_uri "http://localhost:8080/inline-repr" >>
  print_uri "http://localhost:8080/inline-repr#12345"

let main () : unit Deferred.t =
  HtmlapiAsyncInterpreter.Interpreter.unsafePerform example >>= (fun _ -> return ())

let () =
  upon (main ()) (fun () -> Shutdown.shutdown 0);
  never_returns (Scheduler.go ())