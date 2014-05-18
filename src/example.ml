open Core.Std
open Async.Std


open Htmlapi

let example : microdata_document FreeHtmlapi.t =
  let (>>=) m n = FreeHtmlapi.bind m n in
  let (>>) m n = FreeHtmlapi_Extra.seq m n in
  let download uri = FreeHtmlapi.liftF (HtmlapiFunctor.Follow (uri, Fn.id)) in
  let print_uri uri_s : microdata_document FreeHtmlapi.t =
    download (Uri.of_string uri_s) >>= fun doc ->
    (* printf "%s\n%!" (String.concat ~sep:"\n" (List.map ~f:html_to_string doc.doc)); *)
    let items = microdata_document_objects doc in
    let strings =
      List.iter ~f:(fun item ->
        printf "TOP LEVEL\n%s\n%!" (microdata_object_to_string_with_prop item);
      ) items
    in
    FreeHtmlapi.return doc
  in

  (* print_uri "http://localhost:8080/" >>
  print_uri "http://localhost:8080/inline-link" >>
  print_uri "http://localhost:8080/inline-repr" >>
  print_uri "http://localhost:8080/inline-repr#12345" (* >> *)
  print_uri "http://holidayplace.co.uk/about/customer-feedback" *)
  print_uri (Sys.argv.(1))


let example2 : unit FreeHtmlapi.t =
  let (>>=) m n = FreeHtmlapi.bind m n in
  let (>>) m n = FreeHtmlapi_Extra.seq m n in
  let download uri = FreeHtmlapi.liftF (HtmlapiFunctor.Follow (uri, Fn.id)) in
  let get obj field = FreeHtmlapi.liftF (HtmlapiFunctor.Get_field (obj, field, Fn.id)) in
  let print_uri uri_s : unit FreeHtmlapi.t =
    download (Uri.of_string uri_s) >>= fun doc ->
    (* printf "%s\n%!" (String.concat ~sep:"\n" (List.map ~f:html_to_string doc.doc)); *)
    let items = microdata_document_objects doc in
    let h = List.nth_exn items 0 in
    let () =
      printf "objects[0]=\n%s\n"
        (microdata_object_to_string_with_prop h)
    in
    get h "testing" >>= fun mfield ->
    let () =
      printf "objects[0].testing=\n%s\n"
        (String.concat ~sep:"\n" (microdata_property_to_strings mfield))
    in
    FreeHtmlapi.return () >>
    get h "user" >>= fun mfield ->
    let () =
      printf "objects[0].user=\n%s\n"
        (String.concat ~sep:"\n" (microdata_property_to_strings mfield))
    in
    match mfield with
    | [Object o] ->
    begin
      get o "other" >>= fun mfield ->
      let () =
        printf "objects[0].user.other=\n%s\n"
          (String.concat ~sep:"\n" (microdata_property_to_strings mfield))
      in
      FreeHtmlapi.return ()
    end
    | _ -> FreeHtmlapi.return ()
  in

  (* print_uri "http://localhost:8080/" >>
  print_uri "http://localhost:8080/inline-link" >>
  print_uri "http://localhost:8080/inline-repr" >>
  print_uri "http://localhost:8080/inline-repr#12345" (* >> *)
  print_uri "http://holidayplace.co.uk/about/customer-feedback" *)
  print_uri (Sys.argv.(1))

let main () : unit Deferred.t =
  HtmlapiAsyncInterpreter.Interpreter.unsafePerform example2 >>= (fun _ -> return ())

let () =
  upon (main ()) (fun () -> Shutdown.shutdown 0);
  never_returns (Scheduler.go ())