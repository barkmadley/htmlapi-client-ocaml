open Core.Std
open Async.Std

let print_items url c : unit Deferred.t =
  (* printf "Reached!\n"; *)
  printf "url: %s\n" url;
  List.iter (Htmlapi.get_items c) ~f:(fun item ->
    printf "%s\n%!" (Htmlapi.item_to_string c item);
    List.iter (Htmlapi.get_properties c item) ~f:(fun prop ->
      printf "\t%s = %s\n"
        (Htmlapi.property_key_to_string prop)
        (Htmlapi.prop_value_to_string c item (Htmlapi.get_value c item prop))
    )
  )
  |> return

let get url : unit Deferred.t =
  return url
  >>= Htmlapi.enter_s
  >>= print_items url

let main () : unit Deferred.t =
  Deferred.all_unit
    (List.map ~f:get
      [
        "http://localhost:8080/";
        "http://localhost:8080/inline-link";
        "http://localhost:8080/inline-repr";
        "http://localhost:8080/inline-repr#12345";
      ]
    )

let () =
  upon (main ()) (fun () -> Shutdown.shutdown 0);
  never_returns (Scheduler.go ())
