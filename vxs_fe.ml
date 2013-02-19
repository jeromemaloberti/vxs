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

type common_opts = { host_: string; username_: string option; password_: string option}

let opt_str = function None -> "None" |	Some v -> v

let config copts =
	Host.({host = copts.host_;
         username = opt_str copts.username_;
         password = opt_str copts.password_})

let make_rpc copts =
	let uri = Printf.sprintf "http://%s/" copts.host_ in
  let rpc = X.make uri in
  rpc

let install copts branch nofakev6d =
  let host_config = config copts in
  let branch = opt_str branch in
  let aux () =
	  lwt vm_uuid = Xs_ops.create_xenserver_template host_config (Xs_ops.Pxe branch) in
    Printf.printf "%s\n" vm_uuid;
    if not nofakev6d then begin
        let rpc = make_rpc copts in
        let rpm = Printf.sprintf "/usr/groups/admin/web/www.uk.xensource.com/html/carbon/%s/latest/xe-phase-1/v6-test.rpm" branch in
        lwt session_id = X.Session.login_with_password rpc
          (opt_str copts.username_) (opt_str copts.password_) "1.1" in
        lwt () = Vxs.add_rpm host_config session_id vm_uuid rpm in
        return ()
    end else
      return ()
  in
	Lwt_main.run (aux ())

let add_rpms copts uuid rpms =
  Printf.printf "add-rpms %s %s\n" uuid (String.concat ", " rpms);
  let host_config = config copts in
  let rpc = make_rpc copts in
  let aux () =
    lwt session_id = X.Session.login_with_password rpc
      (opt_str copts.username_) (opt_str copts.password_) "1.1" in
    lwt () = Lwt_list.iter_s
      (fun rpm -> Vxs.add_rpm host_config session_id uuid rpm) rpms in
    return ()
  in
  Lwt_main.run (aux ())

let call_rpc copts uuid script =
  Printf.printf "call-rpc %s %s\n" uuid script;
  let host_config = config copts in
  let rpc = make_rpc copts in
  let aux () =
	  lwt session_id = X.Session.login_with_password rpc
      (opt_str copts.username_) (opt_str copts.password_) "1.1" in
 	  lwt n = Vxs.submit_rpc host_config session_id uuid "ls /" in
    lwt result = Vxs.get_response host_config session_id uuid n in
    Printf.printf "%s\n%!" result;
    return ()
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
    Cli.Arg.(required & pos 0 (some string) None & info [] ~docs ~doc ~docv:"HOST")
  in
  let user =
    let doc = "Username to log in with." in
    Cli.Arg.(value & opt (some string) (Some "root") & info ["u"; "user"] ~docs ~doc)
  in
  let pw =
    let doc = "Password to log in with." in
    Cli.Arg.(value & opt (some string) (Some "xenroot") & info ["p"; "password"] ~docs ~doc)
  in
  Cli.Term.(pure common_opts $ host $ user $ pw)

(* Commands *)
let add_rpms_cmd =
  let docs = common_opts_sect in
  let uuid =
    let doc = "UUID of the VM." in
    Cli.Arg.(required & pos 1 (some string) None & info [] ~docs ~doc ~docv:"UUID")
  in
  let rpms =
    let doc = "RPM files to copy to the VM." in
    Cli.Arg.(non_empty & pos_right 1 non_dir_file [] & info [] ~docv:"RPMS" ~doc ~docs)
  in
  let doc = "Copy and install RPM files in a VM." in
  let man = [
    `S "DESCRIPTION";
    `P "Copy and install RPM files in a VM."] @ help_secs
  in
  Cli.Term.(pure add_rpms $ common_opts_t $ uuid $ rpms),
  Cli.Term.info "add-rpms" ~sdocs:common_opts_sect ~doc ~man

let create_template_cmd =
  let branch =
    let doc = "Branch to install as a template." in
    Cli.Arg.(value & opt (some string) (Some "trunk-ring3") & info ["b"; "branch"]
                 ~docv:"BRANCH" ~doc)
  in
  let nov6d =
    let doc = "Do not install the fake v6d." in
    Cli.Arg.(value & flag & info ["n"; "nofakev6d"] ~doc)
  in
  let doc = "install a Virtual Xen Server Template" in
  let man = [
    `S "DESCRIPTION";
    `P "Install a Virtual Xen Server as a Template"] @ help_secs
  in
  Cli.Term.(pure install $ common_opts_t $ branch $ nov6d),
  Cli.Term.info "create-template" ~sdocs:common_opts_sect ~doc ~man

let call_rpc_cmd =
  let docs = common_opts_sect in
  let uuid =
    let doc = "UUID of the VM." in
    Cli.Arg.(required & pos 1 (some string) None & info [] ~docs ~doc ~docv:"UUID")
  in
  let script =
    let doc = "Script to execute on the VM." in
    Cli.Arg.(required & pos 2 (some non_dir_file) None & info [] ~docv:"SCRIPT" ~doc ~docs)
  in
  let doc = "Execute a script on a VM." in
  let man = [
    `S "DESCRIPTION";
    `P "Execute a script on a VM."] @ help_secs
  in
  Cli.Term.(pure call_rpc $ common_opts_t $ uuid $ script),
  Cli.Term.info "call-rpc" ~sdocs:common_opts_sect ~doc ~man

let default_cmd =
  let doc = "Virtual XenServer Management Toolkit" in
  let man = help_secs in
  Cli.Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_opts_t)),
  Cli.Term.info "vxs_fe" ~version:"0.2" ~sdocs:common_opts_sect ~doc ~man

let cmds = [create_template_cmd; add_rpms_cmd; call_rpc_cmd]

let () = match Cli.Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
