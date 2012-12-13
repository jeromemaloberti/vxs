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

let host = ref ""
let uri = ref "http://127.0.0.1/"
let username = ref "root"
let password = ref "password"
let anon = ref []

let consume_arg () =
	let arg = List.hd !anon in
	anon := List.tl (!anon);
	arg

let test host_config rpc uuid = 
	lwt session_id = X.Session.login_with_password rpc !username !password "1.1" in
  lwt n = Vxs.submit_rpc host_config session_id uuid "ls /" in
  lwt result = Vxs.get_response host_config session_id uuid n in
  Printf.printf "%s\n%!" result;
  Lwt.return ()

let main () =
	let op = consume_arg () in
	let host_config = Host.({host = !host; username = !username; password = !password}) in
	let uri = Printf.sprintf "http://%s/" host_config.Host.host in
  let rpc = X.make uri in
	match op with
	| "install" ->
			lwt vm_uuid = Xs_ops.create_xenserver_template host_config "trunk-ring3" in
      Printf.printf "%s\n" vm_uuid;
      return ()				
  | "addrpm" ->
			let uuid = consume_arg () in
			let rpm = consume_arg () in
			lwt session_id = X.Session.login_with_password rpc !username !password "1.1" in
      lwt () = Vxs.add_rpm host_config session_id uuid rpm in
			return ()
	| "rpc" ->
			let uuid = consume_arg () in
			let script = consume_arg () in 
			lwt session_id = X.Session.login_with_password rpc !username !password "1.1" in
 			lwt n = Vxs.submit_rpc host_config session_id uuid "ls /" in
      lwt result = Vxs.get_response host_config session_id uuid n in
      Printf.printf "%s\n%!" result;
      return ()
  | _ -> 
			Printf.printf "Unknown operation";
			return ()
          
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

let _ =
	let op = ref "install" in
  Arg.parse [
      "-host", Arg.Set_string host, (Printf.sprintf "Hostname of server to connect to (default %s)" !host);
      "-u", Arg.Set_string username, (Printf.sprintf "Username to log in with (default %s)" !username);
      "-pw", Arg.Set_string password, (Printf.sprintf "Password to log in with (default %s)" !password);
  ] (fun x -> anon := x :: !anon)
      "VXS toolkit";
	anon := List.rev !anon;
	
  Lwt_main.run (main ())


