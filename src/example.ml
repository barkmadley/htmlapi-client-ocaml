open Core.Std
open Async.Std

let main () : unit Deferred.t =
  Htmlapi.enter_s "http://localhost:8080/"
  >>= fun c ->
  (* printf "Reached!\n"; *)
  List.iter (Htmlapi.get_items c) ~f:(fun item ->
    printf "<item: [%s]>\n%!"
      (String.concat ~sep:", " (List.map ~f:Htmlapi.itemtype_to_string (Htmlapi.get_itemtype c item)));
    List.iter (Htmlapi.get_properties c item) ~f:(fun prop ->
      printf "\t%s = %s\n"
        (Htmlapi.property_key_to_string prop)
        (Htmlapi.prop_value_to_string c item (Htmlapi.get_value c item prop))
    )
  )
  |> return

let () =
  upon (main ()) (fun () -> Shutdown.shutdown 0);
  never_returns (Scheduler.go ())
