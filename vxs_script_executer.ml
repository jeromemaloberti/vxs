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

open Lwt

open Xen_api
open Cohttp_lwt_unix

module X=Xen_api_lwt_unix


let rec connect rpc host username password =
	try_lwt
		  lwt session = X.Session.login_with_password rpc username password "1.0" in
      Printf.printf "Connected\n%!";
      return session
  with _ ->
      Lwt_unix.sleep 5.0 >> connect rpc host username password

let process_rpc host rpc session_id vm k v =
	lwt blob = Blob.of_uuid rpc session_id v in
	lwt contents = Blob.get_blob host session_id blob in
	let filename = Printf.sprintf "/tmp/%s" blob.Blob.u in
	lwt () = Lwt_io.with_file ~mode:Lwt_io.output filename (fun o -> Lwt_io.write o contents) in
  lwt () = Lwt_unix.chmod filename 0o777 in
  lwt status = Lwt_unix.system (Printf.sprintf "%s > %s.log 2> %s.err" filename filename filename) in
  lwt result = Utils.read_file (Printf.sprintf "%s.log" filename) in
  lwt err = Utils.read_file (Printf.sprintf "%s.err" filename) in
  lwt rblob = Blob.add_blob rpc session_id vm (Printf.sprintf "response%d" k) in
  lwt () = Blob.put_blob host session_id rblob result in
  return (status=Lwt_unix.WEXITED 0, rblob)
      
let process_other_config host rpc session_id _ref record =
	let other_config = record.API.vM_other_config in
	let get stem =
		let rpcs = List.filter (fun (k,v) -> Utils.startswith stem k) other_config in
		let len = String.length stem in
		let rpcs = List.map (fun (k,v) -> try [(int_of_string (String.sub k len (String.length k - len)),v)] with _ -> []) rpcs in
		let rpcs = List.concat rpcs in
		rpcs
	in
	let rpcs = get "rpc" in
	let responses = get "response" in
  
	let sorted = List.sort (fun (x,_) (y,_) -> compare x y) rpcs in
	lwt () = Lwt_list.iter_s (fun (k,v) ->
		  if not (List.mem_assoc k responses) then
			  lwt (success,result_blob) = process_rpc host rpc session_id _ref k v in
			  X.VM.add_to_other_config rpc session_id _ref (Printf.sprintf "response%d" k) result_blob.Blob.u
		  else return ()) sorted in
  Printf.printf "other_config: [%s]\n%!" (String.concat "," (List.map (fun (x,y) -> Printf.sprintf "%s=%s" x y) other_config));
	return ()


let run =
	lwt host = Utils.read_file "/etc/vxs_host" in
  lwt username = Utils.read_file "/etc/vxs_username" in
  lwt password = Utils.read_file "/etc/vxs_password" in
  lwt vm = Util_inventory.lookup Util_inventory._installation_uuid in
	let uri = Printf.sprintf "http://%s/" host in
  let rpc = Xen_api_lwt_unix.make uri in
	let host_config = Host.make host username password in

	let rec inner () =
		try_lwt 
			  lwt session_id = connect rpc host username password in
	      let rec process_events token = 
				  lwt vm_ref = X.VM.get_by_uuid ~rpc ~session_id ~uuid:vm in
          
				  lwt events = X.Event.from ~rpc ~session_id ~classes:[Printf.sprintf "vm/%s" vm_ref] ~token ~timeout:60.0 in
			    
			    let ef = Event_types.event_from_of_rpc events in 
				  lwt () = Lwt_list.iter_s (fun ev -> 
					    match Event_helper.record_of_event ev with
						  | Event_helper.VM (_ref,Some record) -> process_other_config host_config rpc session_id _ref record
						  | _ -> return ()) ef.Event_types.events in
				  process_events ef.Event_types.token
        in
        process_events ""
    with _ ->
			  Lwt_unix.sleep 60.0 >> inner ()
	in
  inner ()
      
let _ = 		
	Lwt_main.run run
      
			
