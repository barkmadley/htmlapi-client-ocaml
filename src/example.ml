open Core.Std
open Async.Std

let main () : unit Deferred.t =
  Htmlapi.enter_s "http://localhost:8080/" (fun c ->
    printf "Reached!\n";
    List.iter (Htmlapi.get_items c) ~f:(fun item ->
      printf "<item: [%s]>\n%!"
        (String.concat ~sep:", " (List.map ~f:Htmlapi.itemtype_to_string (Htmlapi.get_itemtype c item)))
    )
  )

let () =
  upon (main ()) (fun () -> Shutdown.shutdown 0);
  never_returns (Scheduler.go ())
