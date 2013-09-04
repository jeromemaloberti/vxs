let install_mirage host kernel =
  let aux () =
    lwt vms = Xs_ops.install_mirage host "mir" kernel 50 24L in
    Lwt.return vms
  in
  Lwt_main.run (aux ())

let destroy_vms host vms =
  Printf.printf "destroying %d vms\n%!" (List.length vms);
  let aux () =
    Xs_ops.with_rpc_and_session host (fun ~rpc ~session_id ->
      lwt () = Lwt_list.iter_s (fun vm -> Xs_ops.template_uninstall rpc session_id vm) vms in
      Lwt.return ()
    )
  in
  Lwt_main.run (aux ())

(*
    lwt pools = X.Pool.get_all ~rpc ~session_id in
    let pool = List.hd pools in
    lwt master = X.Pool.get_master ~rpc ~session_id  ~self:pool in
    lwt servertime = X.Host.get_servertime ~rpc ~session_id ~host:master in
*)
let _ =
  let usage = Printf.sprintf "Usage: %s -h host -k kernel" Sys.argv.(0) in
  let host = ref None in
  let kernel = ref None in
  Arg.parse [("-h", Arg.String (fun x -> host := Some x), "Host");
	     ("-k", Arg.String (fun x -> kernel := Some x), "Kernel")]
    (fun x -> Printf.eprintf "Warning: ignoring unexpected argument %s\n" x)
    usage;
  match !host,!kernel with
  | Some h,Some k -> 
    let host_config = Host.({host = h; username = "root"; password = "xenroot"}) in
    let vms = install_mirage host_config k in
  (*  let vms_list = Xs_ops.break vms 10 in *)
   (* List.iter (fun vms ->  destroy_vms host_config vms) vms *)
    destroy_vms host_config vms
  | _ -> print_endline usage;
    exit 1
    
