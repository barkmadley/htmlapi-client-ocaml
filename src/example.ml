open Core.Std
open Async.Std

let print_items url c : unit Deferred.t =
  (* printf "Reached!\n"; *)
  Deferred.all (
    List.map (Htmlapi.get_items c) ~f:(fun item ->
      Deferred.all (
        List.map (Htmlapi.get_properties c item) ~f:(fun prop ->
          Htmlapi.get_value c item prop
          >>= fun v ->
          begin
            match v with
            | Item i ->
            begin
              Deferred.all (
                List.map (Htmlapi.get_properties c i) ~f:(fun prop ->
                  Htmlapi.get_value c i prop
                  >>= fun inner_v ->
                  begin
                    return (
                      sprintf "\t\t%s = %s\n"
                        (Htmlapi.property_key_to_string prop)
                        (Htmlapi.prop_value_to_string c i inner_v)
                    )
                  end
                )
              )
            end
            | Multiple _ | Data _ -> return ["\t\ttesting"]
          end
          >>= fun values ->
          return (sprintf "\t%s = %s\n%s"
            (Htmlapi.property_key_to_string prop)
            (Htmlapi.prop_value_to_string c item v)
            (String.concat ~sep:"\n" values))
        )
      )
      >>= fun prop_values ->
      begin
        return (
          sprintf "%s\n%s%!"
            (Htmlapi.item_to_string c item)
            (String.concat ~sep:"\n" prop_values);
        )
      end
    )
  )
  >>= fun items ->
  begin
    printf "url: %s\n%s\n" url
      (String.concat ~sep:"\n" items)
      ;
    return ()
  end

let get context url : unit Deferred.t =
  return url
  >>= Htmlapi.enter_s context
  >>= print_items url

let main () : unit Deferred.t =
  Htmlapi.make_context () >>= fun c ->
  Deferred.all_unit
    (List.map ~f:(get c)
      [
        "http://localhost:8080/";
        "http://localhost:8080/inline-link";
        "http://localhost:8080/inline-repr";
        "http://localhost:8080/inline-repr#12345";
      ]
    )

  
let example : Nethtml.document list FreeHtmlApi.t =
  let (>>=) m n = FreeHtmlApi.bind m n in
  let download uri = FreeHtmlApi.liftF (HtmlapiFunctor.Follow (uri, Fn.id)) in

  download (Uri.of_string "http://localhost:8080/") >>= fun docs ->
  let items = get_items docs in
  let strings =
    List.iter (fun item ->
      Printf.printf "%s\n%!" 
    ) items
  in
  download (Uri.of_string "http://localhost:8080/inline-link") >>= fun docs2 ->
  FreeHtmlApi.return docs

let main () : unit Deferred.t =
  IOInterpreter.unsafePerform example

let () =
  upon (main ()) (fun () -> Shutdown.shutdown 0);
  never_returns (Scheduler.go ())
