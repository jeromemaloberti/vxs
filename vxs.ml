(*
 * Copyright (C) 2006-2012 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* Test of http put *)
open Lwt

module X = Xen_api_lwt_unix
open Cohttp_lwt_unix
module Cli = Cmdliner
    
type common_opts = { host_: string option; username_: string option; password_: string option}

let opt_str = function None -> "None" |	Some v -> v

let config_host copts file_opts =
  try 
    Host.({host = (Options.default_opt_no_none file_opts.Options.host copts.host_ "host");
           username = opt_str (Options.default_opt file_opts.Options.username copts.username_);
           password = opt_str (Options.default_opt file_opts.Options.password copts.password_)})
  with Options.Option_not_set opt ->
    Printf.printf "Error: you need to give a value to option: %s\n" opt;
    exit 1

let config copts =
  let file_opts = Options.get_file_opts () in
  config_host copts file_opts

let config_pool copts nfs_server nfs_path =
  let file_opts = Options.get_file_opts () in
  let host = config_host copts file_opts in
  let nfs_server = Options.default_opt file_opts.Options.nfs_server nfs_server in
  let nfs_path = Options.default_opt file_opts.Options.nfs_path nfs_path in
  (host, nfs_server, nfs_path)


(* From opam *)
let mk_subdoc ?(names="COMMANDS") commands =
  `S names ::
    List.map (fun (cs,_,d) ->
      let bold s = Printf.sprintf "$(b,%s)" s in
    let cmds = String.concat ", " (List.map bold cs) in
    `I (cmds, d)
   ) commands
    
let mk_subcommands_aux ?(name="COMMAND") my_enum commands default initial_pos =
  let command =
    let doc =
      Cli.Arg.info ~docv:name ~doc:
        (Printf.sprintf
           "Name of the sub-command. See the $(b,%sS) section for more info.%s"
           name
           (match default with
           | None   -> ""
           | Some d -> " " ^ d))
        [] in
    let commands =
      List.fold_left
        (fun acc (cs,f,_) -> List.map (fun c -> c,f) cs @ acc)
        [] commands in
    Cli.Arg.(required & pos initial_pos (some & my_enum commands) None & doc) in
  let params =
    let doc = Cli.Arg.info ~doc:"Optional parameters." [] in
    Cli.Arg.(value & pos_right initial_pos string [] & doc) in
  command, params

let mk_subcommands ?name commands initial_pos =
  mk_subcommands_aux ?name Cli.Arg.enum commands None initial_pos

let get_all_templates copts branch iso name cs_name =
  if branch = None && iso = None && name = None && cs_name = None then
    []
  else begin
    let aux () =
      let host_config = config copts in
      Printf.printf "About to get templates\n%!";
      lwt templates = Xs_ops.get_xenserver_templates_main host_config in
      let open Xs_ops in
      return (List.filter (fun t ->
	(match cs_name with
	| Some n -> 
	  Printf.printf "Checking: %s vs %s\n%!" t.vxs_name n;
	  t.vxs_name = n && t.vxs_ty = Cloudstack
	| None -> true) && 
	  (match name with
	  | Some n -> t.vxs_name = n
	  | None -> true) &&
	  (match branch with
	  | Some b -> (match t.vxs_ty with Pxe b' -> b = b' | _ -> false)
	  | None -> true) &&
	  (match iso with
	  | Some i -> (match t.vxs_ty with Mainiso i' -> i = i' | _ -> false)
	  | None -> true))
		templates)
    in
    Lwt_main.run (aux ())
  end

let get_template_uuid copts branch iso name uuid =
  Printf.printf "Template branch %s iso %s template %s uuid %s\n"
    (opt_str branch) (opt_str iso) (opt_str name) (opt_str uuid);
  match uuid with
  | Some uuid -> uuid
  | None -> 
    let templates = get_all_templates copts branch iso name None in
    if (List.length templates) <> 1 then
      begin
        Printf.printf "I expected exactly one template to match your request.\n";
        if List.length templates = 0 then begin
          Printf.printf "No templates matched the query.\n";
        end else begin
          Printf.printf "I found the following templates:\n";
          List.iter (fun x ->
            Printf.printf "* %s %s installed at %s\n" x.Xs_ops.vxs_uuid x.Xs_ops.vxs_name x.Xs_ops.vxs_install_time;
          ) templates;
        end;
	exit 1
      end;
    let ret = (List.hd templates).Xs_ops.vxs_uuid in
    Printf.printf "Template uuid:%s\n" ret;
    ret

let get_cs_uuid copts cs_name =
  Printf.printf "In get_cs_uuid\n%!";
  let templates = get_all_templates copts None None None cs_name in
  if (List.length templates) <> 1 then
    begin
      Printf.printf "Expecting 1 template: got %d.\n%! (cs_name=%s)" (List.length templates) 
      (match cs_name with Some x -> x | None -> "(none)");
      exit 1
    end
  else (List.hd templates).Xs_ops.vxs_uuid

let pool_create copts nhosts nfs_server nfs_path branch iso template_name uuid pool_name rpms =
  Printf.printf "pool_create nhost %d nfs_server %s nfs_path %s branch %s iso %s template %s uuid %s pool %s\n"
    nhosts (opt_str nfs_server) (opt_str nfs_path) (opt_str branch) (opt_str iso) (opt_str template_name) (opt_str uuid) 
    pool_name;
  Printf.printf "add-rpms %s\n" (String.concat ", " rpms);
  let template_uuid = get_template_uuid copts branch iso template_name uuid in
  let (host,nfs_server,nfs_path) = config_pool copts nfs_server nfs_path in
  let aux () =
    lwt () = if (List.length rpms) > 0 then begin
      lwt (_,uuid) = Xs_ops.template_clone host template_uuid (template_uuid ^ "_temp") in
      Printf.printf "created temporary template %s\n" uuid;
      lwt () = Xs_ops.add_rpms host uuid rpms in
      lwt () = Xs_ops.create_pool host uuid pool_name nhosts nfs_server nfs_path in 
      lwt () = Xs_ops.template_destroy host uuid in
      Printf.printf "destroyed temporary template %s\n" uuid;
      Lwt.return ()
    end else 
	lwt () = Xs_ops.create_pool host template_uuid pool_name nhosts nfs_server nfs_path in 
	Lwt.return ()
    in
    Lwt.return ()
  in
  Lwt_main.run (aux ())

let template_destroy copts branch iso template_name uuid =
  Printf.printf "template_destroy\n";
  let template_uuid = get_template_uuid copts branch iso template_name uuid in
  let host = config copts in
  let aux () =
    lwt () = Xs_ops.template_destroy host template_uuid in 
    return ()
  in
  Lwt_main.run (aux ())

let template_cache copts =
  Printf.printf "template_cache_regenerate\n";
  let host = config copts in
  let aux () =
    lwt () = Xs_ops.regenerate_template_cache host in
    return ()
  in
  Lwt_main.run (aux ())

let template_clone copts branch iso template_name uuid new_name =
  Printf.printf "template_clone\n";
  let template_uuid = get_template_uuid copts branch iso template_name uuid in
  let host = config copts in
  let aux () =
    lwt (_,uuid) = Xs_ops.template_clone host template_uuid new_name in 
    Printf.printf "template-clone %s\n" uuid;
    return ()
  in
  Lwt_main.run (aux ())

let template_install copts source nofakev6d disk mem =
  let host_config = config copts in
  let branch = match source with 
    | Xs_ops.Pxe branch -> branch 
    | Xs_ops.Mainiso _ -> "trunk-ring3" in
  let aux () =
    lwt vm_uuid = Xs_ops.create_xenserver_template host_config source disk mem in
    Printf.printf "%s\n" vm_uuid;
    let nofakev6d = match branch with
      | "trunk-ring3" | "clearwater" | "clearwater-ring3" -> true
      | _ -> nofakev6d in
    if not nofakev6d then begin
      let rpm = Printf.sprintf "/usr/groups/admin/web/www.uk.xensource.com/html/carbon/%s/latest/xe-phase-1/v6-test.rpm" branch in
      lwt () = Xs_ops.add_rpms host_config vm_uuid [rpm] in
      return ()
    end else
      return ()
  in
  Lwt_main.run (aux ())

let template_create_cli copts branch iso nofakev6d disk mem =
  match iso with
  | Some file -> template_install copts (Xs_ops.Mainiso file) nofakev6d disk mem
  | None -> 
    match branch with 
    | Some branch -> template_install copts (Xs_ops.Pxe branch) nofakev6d disk mem
    | None -> template_install copts (Xs_ops.Pxe "trunk-ring3") nofakev6d disk mem

let template_list copts branch iso latest minimal =
  let aux () =
    let host_config = config copts in
    lwt templates = Xs_ops.get_xenserver_templates_main host_config in
    let open Xs_ops in
    let templates = if latest && (List.length templates) > 0 then
	let sorted = List.fast_sort (fun t1 t2 -> - (String.compare t1.vxs_install_time t2.vxs_install_time)) templates in
	[(List.hd sorted)]
      else List.filter (fun t -> 
	(match branch with 
	| Some b -> (match t.vxs_ty with Pxe b' -> b = b' | _ -> false)
	| None -> true) &&
	  (match iso with 
	  | Some i -> (match t.vxs_ty with Mainiso i' -> i = i' | _ -> false)
	  | None -> true))
	templates in
    if not minimal then
      Printf.printf "%-20s | %-36s | %-20s | %-30s |\n" "NAME" "UUID" "INSTALL TIME" "INSTALL TYPE";
    List.iter (fun t ->
      if minimal then
	Printf.printf "%s\n" t.vxs_uuid
      else 
	Printf.printf "%-20s | %-36s | %-20s | %-30s |\n" t.vxs_name t.vxs_uuid t.vxs_install_time (Rpc.to_string (rpc_of_installty t.vxs_ty))) templates;
    Lwt.return ()
  in
  Lwt_main.run (aux ())

let install_debian copts name =
  let aux () =
    let host_config = config copts in
    lwt rc = Xs_ops.install_wheezy host_config name in
    exit rc
  in
  Lwt_main.run (aux ())

let install_centos5 copts name =
  let aux () =
    let host_config = config copts in
    lwt rc = Xs_ops.install_centos57 host_config name in
    exit rc
  in
  Lwt_main.run (aux ())

let install_centos6 copts name =
  let aux () =
    let host_config = config copts in
    lwt _ = Xs_ops.install_centos65 host_config name in
    exit 0
  in
  Lwt_main.run (aux ())

let install_cloudstack_template copts =
  let aux () =
    let host_config = config copts in
    lwt _ = Xs_ops.install_cloudstack_template host_config "cs" in
    exit 0
  in
  Lwt_main.run (aux ())

let install_cloudstack copts branch iso template_name uuid cs_name =
  let aux () =
    let host_config = config copts in
    let vxs_template_uuid = get_template_uuid copts branch iso template_name uuid in
    let cs_template_uuid = get_cs_uuid copts cs_name in
    lwt _ = Xs_ops.install_cloudstack host_config cs_template_uuid vxs_template_uuid in
    exit 0
  in 
  Lwt_main.run (aux ())

let install_mirage copts name kernel n_vms memory =
  match kernel with
  | None ->
    Printf.printf "You need to specify a kernel!\n";
    exit 1
  | Some k ->
    let aux () =
      let host_config = config copts in
      lwt vms = Xs_ops.install_mirage host_config name k n_vms memory in
      exit (if ((List.length vms) = n_vms) then 0 else 1)
    in
    Lwt_main.run (aux ())

let vm_install copts command name kernel n_vms memory =
  Printf.printf "vm-install name:%s\n" name;
  match command with
  | `debian -> install_debian copts name
  | `centos5 -> install_centos5 copts name
  | `centos6 -> install_centos6 copts name
  | `mirage -> install_mirage copts name kernel n_vms memory
  | _ -> Printf.printf "Wrong parameters\n";
    ()

let quicktest copts branch rpms =
  let aux () =
    let host_config = config copts in
    let script = "#!/bin/bash\n/opt/xensource/debug/quicktest\n" in
    lwt rc = Xs_ops.template_exec host_config (opt_str branch) script rpms in
    exit rc
  in
  Lwt_main.run (aux ())

let test copts command branch rpms =
  let command_str = match command with `quicktest -> "quicktest" | _ -> "Bad" in
  Printf.printf "test cmd:%s branch:%s rpms:%s\n" command_str (opt_str branch) (String.concat ", " rpms);
  match command with
  | `quicktest -> quicktest copts branch rpms
  | _ -> Printf.printf "Wrong parameters\n";
    ()

let add_rpms copts uuid rpms =
  Printf.printf "add-rpms %s %s\n" uuid (String.concat ", " rpms);
  let host_config = config copts in
  let aux () =
    lwt () = Xs_ops.add_rpms host_config uuid rpms in
    return ()
  in
  Lwt_main.run (aux ())

let exec copts vm script nowait =
  Printf.printf "exec %s %s\n" vm script;
  let host_config = config copts in
  let aux () =
    lwt rc = Xs_ops.exec_rpc host_config vm script nowait in
    exit rc
  in
  Lwt_main.run (aux ())

let ssh copts vm =
  Printf.printf "ssh %s\n" vm;
  let host_config = config copts in
  let aux () =
    lwt ip = Xs_ops.get_vm_ip host_config vm in
    Printf.printf "ip %s\n" ip;
    let ssh_addr = "root@" ^ ip in
    (* Transfer control directly to another program in the path. *)
    let () = Unix.execvp "ssh" [| "ssh"; ssh_addr |] in
    Lwt.return ()
   in
  Lwt_main.run (aux ())

(*	let uri = Printf.sprintf "http://%s/" !host in
        let rpc = make uri in
        lwt session_id = Session.login_with_password rpc !username !password "1.0" in
        try_lwt
                lwt pools = Pool.get_all rpc session_id in
                let pool = List.hd pools in
				lwt blob = Pool.create_new_blob rpc session_id pool "test7" "text" true in
                let body = match Body.body_of_string "testing" with | Some x -> x | None -> failwith "Error" in
                lwt () = http_put (Printf.sprintf "%sblob?session_id=%s&ref=%s" uri session_id blob) body in
                return ()
        finally
                Session.logout rpc session_id*)

let help common_opts man_format cmds topic = match topic with
  | None -> `Help (`Pager, None) (* help about the program. *)
  | Some topic ->
    let topics = "topics" :: "patterns" :: "environment" :: cmds in
    let conv, _ = Cli.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
    match conv topic with
    | `Error e -> `Error (false, e)
    | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok ()
    | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
    | `Ok t ->
      let page = (topic, 7, "", "", ""), [`S topic; `P "Say something";] in
      `Ok (Cli.Manpage.print man_format Format.std_formatter page)

let common_opts_sect = "COMMON OPTIONS"
let help_secs = [
 `S common_opts_sect;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";`Noblank;]

(* Options common to all commands *)

let common_opts host_ username_ password_ = { host_; username_; password_ }

let common_opts_t =
  let docs = common_opts_sect in
  let host =
    let doc = "Hostname to connect to." in
    Cli.Arg.(value & opt (some string) None & info ["H"; "host"] ~docs ~doc ~docv:"HOST")
  in
  let user =
    let doc = "Username to log in with." in
    Cli.Arg.(value & opt (some string) (Some "root") & info ["U"; "user"] ~docs ~doc)
  in
  let pw =
    let doc = "Password to log in with." in
    Cli.Arg.(value & opt (some string) (Some "xenroot") & info ["P"; "password"] ~docs ~doc)
  in
  Cli.Term.(pure common_opts $ host $ user $ pw)

let branch_opt () =
  let doc = "Branch for the template specifier." in
  Cli.Arg.(value & opt (some string) None & info ["b"; "branch"]
             ~docv:"BRANCH" ~doc)

let iso_opt () =
  let doc = "Iso for the template specifier." in
  Cli.Arg.(value & opt (some string) None & info ["i"; "iso"]
             ~docv:"ISO" ~doc)
  
let template_name_opt () =
  let doc = "Name of the template." in
  Cli.Arg.(value & opt (some string) None & info ["n"]
             ~docv:"TEMPLATE_NAME" ~doc)

let uuid_opt () =
  let doc = "UUID of the template." in
  Cli.Arg.(value & opt (some string) None & info ["U"; "uuid"] ~doc ~docv:"UUID")

let cs_name_opt () =
  let doc = "Name of the CS template." in
  Cli.Arg.(value & opt (some string) None & info ["cs"] ~doc ~docv:"CS_NAME")

(* Commands *)
let vm_install_cmd =
  let commands = [
    ["debian"], `debian, "Install a VM with Debian Wheezy.";
    ["centos5"], `centos5, "Install a 32 bits VM with CentOS 5.7.";
    ["centos6"], `centos6, "Install a 64 bits VM with CentOS 6.4.";
    ["mirage"], `mirage, "Install N Mirage VMs with the specified kernel.";
  ] in
  let man = [
    `S "DESCRIPTION";
    `P "Commands to install VMs"] @ (mk_subdoc commands) @ help_secs in
  let command, _ = mk_subcommands commands 0 in
  let docs = common_opts_sect in
  let vm_name =
    let doc = "Name of the new VM." in
    Cli.Arg.(required & pos 1 (some string) None & info [] ~docs ~doc ~docv:"NAME")
  in
  let n_hosts =
    let doc = "Number of Mirage VMs to install." in
    Cli.Arg.(value & opt int 1 & info ["s"] ~docs ~doc ~docv:"NVMS")
  in
  let memory =
    let doc = "Memory setup of Mirage VMs in Mb." in
    Cli.Arg.(value & opt int64 24L & info ["m"] ~docs ~doc ~docv:"MEMORY")
  in
  let kernel =
    let doc = "Mirage kernel to upload." in
    Cli.Arg.(value & opt (some non_dir_file) None & info ["k"; "kernel"] ~docs ~doc ~docv:"KERNEL")
  in
  let doc = "Install a XenServer 6.x VM from a VXS template" in
  Cli.Term.(pure vm_install $ common_opts_t $ command $ vm_name $ kernel $ n_hosts $ memory),
  Cli.Term.info "vm-install" ~sdocs:common_opts_sect ~doc ~man

let test_cmd =
  let doc = "Commands to run tests" in
  let commands = [
    ["quicktest"], `quicktest, "Provision a VM from the latest template VXS of $(b,branch) and run quicktest.";
  ] in
  let man = [
    `S "DESCRIPTION";
    `P "Commands to run tests"] @ (mk_subdoc commands) @ help_secs in
  let command, _ = mk_subcommands commands 0 in
  let docs = common_opts_sect in
  let branch =
    let doc = "Branch to test." in
    Cli.Arg.(value & opt (some string) (Some "trunk-ring3") & info ["b"; "branch"] ~docs ~doc ~docv:"BRANCH")
  in
  let rpms =
    let doc = "RPM files to copy to the VM." in
    Cli.Arg.(non_empty & pos_right 0 non_dir_file [] & info [] ~docv:"RPMS" ~doc ~docs)
  in
  let doc = "Run quicktest inside a XenServer 6.x VM." in
  Cli.Term.(pure test $ common_opts_t $ command $ branch $ rpms),
  Cli.Term.info "test" ~sdocs:common_opts_sect ~doc ~man

let add_rpms_cmd =
  let docs = common_opts_sect in
  let uuid =
    let doc = "UUID of the VM." in
    Cli.Arg.(required & pos 0 (some string) None & info [] ~docs ~doc ~docv:"UUID")
  in
  let rpms =
    let doc = "RPM files to copy to the VM." in
    Cli.Arg.(non_empty & pos_right 0 non_dir_file [] & info [] ~docv:"RPMS" ~doc ~docs)
  in
  let doc = "Copy and install RPM files in a VM." in
  let man = [
    `S "DESCRIPTION";
    `P "Copy and install RPM files in a VM."] @ help_secs
  in
  Cli.Term.(pure add_rpms $ common_opts_t $ uuid $ rpms),
  Cli.Term.info "template-add-rpm" ~sdocs:common_opts_sect ~doc ~man

let pool_install_cmd =
  let docs = common_opts_sect in
  let n_hosts =
    let doc = "Number of hosts in the pool." in
    Cli.Arg.(value & opt int 1 & info ["s"] ~docs ~doc ~docv:"NHOSTS")
  in
  let nfs_server =
    let doc = "NFS Server address." in
    Cli.Arg.(value & opt (some string) None & info ["N"; "nfs-server"] ~docs ~doc ~docv:"NFS_SERVER")
  in
  let nfs_path =
    let doc = "NFS path." in
    Cli.Arg.(value & opt (some string) None & info ["P"; "nfs-path"] ~docs ~doc ~docv:"NFS_PATH")
  in
  let branch = branch_opt () in
  let iso = iso_opt () in
  let template_name = template_name_opt () in
  let uuid = uuid_opt () in
  let pool_name =
    let doc = "Pool name." in
    Cli.Arg.(required & pos 0 (some string) None & info [] ~docs ~doc ~docv:"POOL_NAME")
  in
  let rpms =
    let doc = "RPMs to copy to the pool." in
    Cli.Arg.(value & pos_right 0 non_dir_file [] & info [] ~docs ~doc ~docv:"RPM")
  in
  let doc = "Install a pool." in
  let man = [
    `S "DESCRIPTION";
    `P "Install a virtual pool on a host."] @ help_secs
  in
  Cli.Term.(pure pool_create $ common_opts_t $ n_hosts $ nfs_server $ nfs_path $ branch $ iso $ 
	      template_name $ uuid $ pool_name $ rpms),
  Cli.Term.info "install" ~sdocs:common_opts_sect ~doc ~man


let cloudstack_install_cmd =
  let docs = common_opts_sect in
  let branch = branch_opt () in
  let iso = iso_opt () in
  let template_name = template_name_opt () in
  let uuid = uuid_opt () in
  let cs_name = cs_name_opt () in
  let doc = "Install a cloudstack (requires both a cloudstack management template and a xenserver template)." in
  let man = [
    `S "DESCRIPTION";
    `P "Install a virtual cloudstack on a host."] @ help_secs
  in
  Cli.Term.(pure install_cloudstack $ common_opts_t $ branch $ iso $ 
	      template_name $ uuid $ cs_name),
  Cli.Term.info "cs-install" ~sdocs:common_opts_sect ~doc ~man

let cloudstack_template_create_cmd =
  let docs = common_opts_sect in
  let doc = "Create a cloudstack template." in
  let man = [
    `S "DESCRIPTION";
    `P "Create a cloudstack management server template on a host.";
  `P "This includes a DHCP server that will listen on eth1, serving 192.168.1.x addresses to 02:00:00:00:00:0x"] @ help_secs
  in
  Cli.Term.(pure install_cloudstack_template $ common_opts_t),
  Cli.Term.info "cs-template-create" ~sdocs:common_opts_sect ~doc ~man

let template_destroy_cmd =
  let docs = common_opts_sect in
  let branch = branch_opt () in
  let iso = iso_opt () in
  let template_name = template_name_opt () in
  let uuid = uuid_opt () in
  let doc = "Destroy a template." in
  let man = [
    `S "DESCRIPTION";
    `P "Destroy a VXS template."] @ help_secs
  in
  Cli.Term.(pure template_destroy $ common_opts_t $ branch $ iso $ template_name $ uuid),
  Cli.Term.info "template-destroy" ~sdocs:common_opts_sect ~doc ~man

let template_clone_cmd =
  let docs = common_opts_sect in
  let branch = branch_opt () in
  let iso = iso_opt () in
  let template_name = template_name_opt () in
  let uuid = uuid_opt () in
  let new_name =
    let doc = "New template name." in
    Cli.Arg.(required & pos 0 (some string) None & info [] ~docs ~doc ~docv:"NEW_NAME")
  in
  let doc = "Clone a template." in
  let man = [
    `S "DESCRIPTION";
    `P "Clone an existing VXS template."] @ help_secs
  in
  Cli.Term.(pure template_clone $ common_opts_t $ branch $ iso $ template_name $ uuid $ new_name),
  Cli.Term.info "template-clone" ~sdocs:common_opts_sect ~doc ~man

let template_cache_cmd =
  let docs = common_opts_sect in
  let doc = "Regenerate the template cache." in
  let man = [
    `S "DESCRIPTION";
    `P "Regenerate the template cache."] @ help_secs
  in
  Cli.Term.(pure template_cache $ common_opts_t),
  Cli.Term.info "template-cache-regen" ~sdocs:common_opts_sect ~doc ~man

let template_create_cmd =
  let branch = branch_opt () in
  let iso = iso_opt () in
  let nov6d =
    let doc = "Do not install the fake v6d." in
    Cli.Arg.(value & flag & info ["n"; "nofakev6d"] ~doc)
  in
  let disk =
    let doc = "Disk size in Gb." in
    Cli.Arg.(value & opt int64 40L & info ["d"] ~doc ~docv:"DISK")
  in
  let memory =
    let doc = "Memory size in Mb." in
    Cli.Arg.(value & opt int64 2048L & info ["m"] ~doc ~docv:"MEM")
  in
  let doc = "Create a Virtual Xen Server Template" in
  let man = [
    `S "DESCRIPTION";
    `P "Install a Virtual Xen Server as a Template"] @ help_secs
  in
  Cli.Term.(pure template_create_cli $ common_opts_t $ branch $ iso $
      nov6d $ disk $ memory),
  Cli.Term.info "template-create" ~sdocs:common_opts_sect ~doc ~man
    
let template_list_cmd =
  let branch = branch_opt () in
  let iso = iso_opt () in
  let latest =
    let doc = "List only the most recent templates." in
    Cli.Arg.(value & flag & info ["l"; "latest"] ~doc)
  in
  let minimal =
    let doc = "Print only the UUIDs of the templates." in
    Cli.Arg.(value & flag & info ["m"; "minimal"] ~doc)
  in
  let doc = "List the Virtual Xen Server templates " in
  let man = [
    `S "DESCRIPTION";
    `P "List the Virtual Xen Server templates"] @ help_secs
  in
  Cli.Term.(pure template_list $ common_opts_t $ branch $ iso $ latest $ minimal),
  Cli.Term.info "template-list" ~sdocs:common_opts_sect ~doc ~man
    
let exec_cmd =
  let docs = common_opts_sect in
  let vm =
    let doc = "UUID or name of the VM." in
    Cli.Arg.(required & pos 0 (some string) None & info [] ~docs ~doc ~docv:"VM")
  in
  let script =
    let doc = "Script to execute on the VM." in
    Cli.Arg.(required & pos 1 (some non_dir_file) None & info [] ~docv:"SCRIPT" ~doc ~docs)
  in
  let nowait =
    let doc = "Do not wait for the execution." in
    Cli.Arg.(value & flag & info ["n"; "nowait"] ~doc)
  in
  let doc = "Execute a script on a XenServer 6.x VM." in
  let man = [
    `S "DESCRIPTION";
    `P "Execute a script on a VM."] @ help_secs
  in
  Cli.Term.(pure exec $ common_opts_t $ vm $ script $ nowait),
  Cli.Term.info "exec" ~sdocs:common_opts_sect ~doc ~man

let ssh_cmd =
  let docs = common_opts_sect in
  let vm =
    let doc = "UUID or name of the VM." in
    Cli.Arg.(required & pos 0 (some string) None & info [] ~docs ~doc ~docv:"VM")
  in
  let doc = "Open an SSH session to a XenServer 6.x VM." in
  let man = [
    `S "DESCRIPTION";
    `P "Execute a script on a VM."] @ help_secs
  in
  Cli.Term.(pure ssh $ common_opts_t $ vm),
  Cli.Term.info "ssh" ~sdocs:common_opts_sect ~doc ~man

let default_cmd =
  let doc = "Virtual XenServer Management Toolkit" in
  let man = help_secs in
  Cli.Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_opts_t)),
  Cli.Term.info "vxs" ~version:"0.2" ~sdocs:common_opts_sect ~doc ~man


let cmds = [ vm_install_cmd; test_cmd; pool_install_cmd; template_clone_cmd; template_create_cmd; 
	     template_destroy_cmd; template_list_cmd; add_rpms_cmd; exec_cmd; ssh_cmd;
	     template_cache_cmd; cloudstack_template_create_cmd; cloudstack_install_cmd ]

let () = 
  Printexc.record_backtrace true;
  try
    match Cli.Term.eval_choice default_cmd cmds with
    | `Error _ -> exit 1 | _ -> exit 0
  with e -> Printf.printf "Error: exception %s\n" (Printexc.to_string e)
