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

let submit_rpc host_config session_id uuid cmd =
	let rpc = Host.get_rpc host_config in
	lwt vm = X.VM.get_by_uuid rpc session_id uuid in
	lwt other_config = X.VM.get_other_config rpc session_id vm in
	let get stem =
		let rpcs = List.filter (fun (k,v) -> Utils.startswith stem k) other_config in
		let len = String.length stem in
		let rpcs = List.map (fun (k,v) -> try [(int_of_string (String.sub k len (String.length k - len)),v)] with _ -> []) rpcs in
		let rpcs = List.concat rpcs in
		rpcs
	in
	let rpcs = get "rpc" in
  let max = List.fold_left (fun x (y,_) -> max x y) 0 rpcs in
	let new_rpc = max + 1 in
	let rpcname = Printf.sprintf "rpc%d" new_rpc in
	lwt blob = Blob.add_blob rpc session_id vm rpcname in
  lwt () = Blob.put_blob host_config session_id blob cmd in
  lwt () = X.VM.add_to_other_config rpc session_id vm rpcname blob.Blob.u in
  return new_rpc

let get_response host_config session_id uuid n =
	let rpc = Host.get_rpc host_config in
	lwt vm_ref = X.VM.get_by_uuid rpc session_id uuid in
	let response = Printf.sprintf "response%d" n in
	let rec process_events token = 
   	lwt events = X.Event.from ~rpc ~session_id ~classes:[Printf.sprintf "vm/%s" vm_ref] ~token ~timeout:60.0 in
    
    let ef = Event_types.event_from_of_xmlrpc events in 
    lwt results = Lwt_list.map_s (fun ev -> 
	      Lwt.return (match Event_helper.record_of_event ev with
		    | Event_helper.VM (_ref,Some record) -> 
				    if List.mem_assoc response record.API.vM_other_config 
				    then Some (List.assoc response record.API.vM_other_config)
				    else None
			  | _ ->
		        None)) ef.Event_types.events in
    let result = List.fold_left (fun acc x ->
			  match acc,x with
				| None,None -> None
				| Some x, _ -> acc
				| None,Some _ -> x) None results in
		match result with
		| None -> process_events ef.Event_types.token
		| Some x -> return x
  in
  lwt result_blob = process_events "" in
  lwt blob = Blob.of_uuid rpc session_id result_blob in
  lwt stdout = Blob.get_blob host_config session_id blob in
  return stdout
      
let add_rpm host_config session_id uuid rpm_filename =
	let rpc = Host.get_rpc host_config in
	let key = "rpm-blobs" in
	lwt vm_ref = X.VM.get_by_uuid rpc session_id uuid in
  lwt value = Utils.read_file rpm_filename in
  let blobname = Filename.basename rpm_filename in
  lwt blob = Blob.add_blob_with_content host_config rpc session_id vm_ref blobname value in
  lwt oc = X.VM.get_other_config rpc session_id vm_ref in
  let rpms = try Utils.split ',' (List.assoc key oc) with _ -> [] in
	let new_rpms = String.concat "," (blob.Blob.u::rpms) in
	lwt () = X.VM.remove_from_other_config rpc session_id vm_ref key in
  X.VM.add_to_other_config rpc session_id vm_ref key new_rpms
      
