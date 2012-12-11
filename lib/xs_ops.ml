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

(* Xenserver VM operations *)

open Lwt

module X = Xen_api_lwt_unix

open Cohttp_lwt_unix
open Host

open CamlTemplate.Model

let pxedir = "/usr/groups/netboot/pxelinux.cfg"



(* API:
 *
 * Vxs.template_create : host_config -> branch:string -> uuid:string
 * Vxs.template_list : host_config -> uuid:string * (name * branch * build_number) list
 * Vxs.create_pool : 
 * 
 *)


type vxs_template_config = {
	branch : string;              (* e.g. trunk-ring3 *)

	vm_uuid : string;                  (* VM uuid *)
	vxs_root_password : string; 

	post_install : Blob.t;   (* Called by the host installer *)
	initscript : Blob.t;     (* Called on firstboot before firstboot
							  scripts! (sets host/dom0 uuid) *)
	veryfirstboot : Blob.t;  (* Installed into firstboot.d *)
	firstboot : Blob.t; 
	id_dsa : Blob.t;
	answerfile : Blob.t;     (* Host installer answerfile *)
}

type vxs_pool_config = {
	n : int;
	name : string;
	networks : int;
}

let create_hash host vxs =
	let h = Hashtbl.create 10 in
	let l = ["branch",vxs.branch;
			 "host",host.host;
			 "username",host.username;
			 "password",host.password;
			 "vm_uuid",vxs.vm_uuid;
			 "vxs_root_password",vxs.vxs_root_password;
			 "post_install_uuid",vxs.post_install.Blob.u;
			 "initscript_uuid",vxs.initscript.Blob.u;
			 "veryfirstboot_uuid",vxs.veryfirstboot.Blob.u;
			 "firstboot_uuid",vxs.firstboot.Blob.u;
			 "id_dsa_uuid",vxs.id_dsa.Blob.u;
			 "answerfile_uuid",vxs.answerfile.Blob.u;] in
	List.iter (fun (x,y) -> Hashtbl.add h x (Tstr y)) l;
	h

let cache = CamlTemplate.Cache.create ()

let get template host vxs =
	let h = create_hash host vxs in
	let tmpl = CamlTemplate.Cache.get_template cache (Printf.sprintf "templates/%s.tmpl" template) in
	let buf = Buffer.create 256 in
	CamlTemplate.merge tmpl h buf;
	Buffer.contents buf

let get_pxe_config = get "install/pxe_config"
let get_firstboot = get "install/firstboot"
let get_initscript = get "install/initscript"
let get_post_install = get "install/post_install"
let get_answerfile = get "install/answerfile"
let get_veryfirstboot = get "install/veryfirstboot"

let exn_to_string = function
        | Api_errors.Server_error(code, params) ->
                Printf.sprintf "%s %s" code (String.concat " " params)
        | e -> Printexc.to_string e



let check_pxe_dir () =
	try_lwt 
		Lwt_unix.stat pxedir >> return ()
    with _ ->
		fail (Failure "No PXE dir")



let create_xenserver_template host branch =
	lwt () = check_pxe_dir () in
	let uri = Printf.sprintf "http://%s/" host.host in
    let rpc = X.make uri in
    lwt session_id = X.Session.login_with_password rpc host.username host.password "1.0" in
    lwt templates = X.VM.get_all_records_where ~rpc ~session_id ~expr:"field \"name__label\" = \"Other install media\"" in
    let (template,_) = List.hd templates in
	Printf.printf "Found template ref: %s\n" template;
	lwt vm = X.VM.clone ~rpc ~session_id ~vm:template ~new_name:(Printf.sprintf "%s-unknown" branch) in
    lwt vm_uuid = X.VM.get_uuid ~rpc ~session_id ~self:vm in
    lwt () = X.VM.provision ~rpc ~session_id ~vm in
    let gig = Int64.mul 1024L (Int64.mul 1024L 1024L) in
	let g2 = Int64.mul gig 2L in
	let g40 = Int64.mul gig 40L in
    lwt () = X.VM.set_memory_limits ~rpc ~session_id ~self:vm ~static_min:g2 ~static_max:g2 ~dynamic_min:g2 ~dynamic_max:g2 in

    lwt () = X.VM.remove_from_HVM_boot_params ~rpc ~session_id ~self:vm ~key:"order" in
    lwt () = X.VM.add_to_HVM_boot_params ~rpc ~session_id ~self:vm ~key:"order" ~value:"ncd" in

    lwt nets = X.Network.get_all_records_where ~rpc ~session_id ~expr:"field \"bridge\" = \"xenbr0\"" in
    let (network,_) = List.hd nets in
	lwt vif = X.VIF.create ~rpc ~session_id ~device:"0" ~network ~vM:vm ~mAC:"" ~mTU:1500L ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[] in
    lwt pools = X.Pool.get_all ~rpc ~session_id in
	let pool = List.hd pools in
	lwt default_sr = X.Pool.get_default_SR ~rpc ~session_id ~self:pool in
    lwt vdi = X.VDI.create ~rpc ~session_id ~sR:default_sr ~name_label:"Root disk" ~name_description:"" ~virtual_size:g40 ~_type:`user ~sharable:false ~read_only:false ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[] in 
    lwt vbd = X.VBD.create ~rpc ~session_id ~vDI:vdi ~vM:vm ~userdevice:"0" ~bootable:true ~mode:`RW ~_type:`Disk ~unpluggable:false ~empty:false ~other_config:["owner",""] ~qos_algorithm_type:"" ~qos_algorithm_params:[] in
    ignore(vbd);
    lwt answerfile = Blob.add_blob rpc session_id vm "answerfile" in
    lwt post_install = Blob.add_blob rpc session_id vm "post_install" in
    lwt initscript = Blob.add_blob rpc session_id vm "initscript" in
    lwt veryfirstboot = Blob.add_blob rpc session_id vm "veryfirstboot" in
    lwt firstboot = Blob.add_blob rpc session_id vm "firstboot" in
    lwt id_dsa = Blob.add_blob rpc session_id vm "id_dsa" in

    let vxs_template_config = {
		branch;
		vm_uuid;
        vxs_root_password = host.password;
		post_install;
		initscript;
		veryfirstboot;
		firstboot;
		id_dsa;
		answerfile;
	} in

	let blobs = [
		answerfile, get_answerfile;
		post_install, get_post_install;
		initscript, get_initscript;
		veryfirstboot, get_veryfirstboot;
		firstboot, get_firstboot;
	] in
	
	lwt () = Lwt_list.iter_s (fun (x,y) -> 
	  Blob.put_blob host session_id x (y host vxs_template_config)) blobs in

    let pub_name = (Filename.concat (Sys.getenv "HOME") ".ssh/id_rsa.pub") in
    lwt exist = try_lwt 
      lwt _ = Lwt_unix.stat pub_name in
      Lwt.return true
      with _ -> Lwt.return false  in 
    lwt () = if exist then begin
      lwt id_dsa_string = Utils.read_file pub_name  in
      Blob.put_blob host session_id id_dsa id_dsa_string
    end else Lwt.return () in   

    let pxe_path = Printf.sprintf "%s/%s" pxedir vm_uuid in
	let pxe_config = get_pxe_config host vxs_template_config in
	
	lwt fd = Lwt_unix.openfile pxe_path [Lwt_unix.O_WRONLY; Lwt_unix.O_CREAT] 0o666 in
    let c = Lwt_io.(of_fd output fd) in

    lwt () = Lwt_io.write c pxe_config in
    lwt () = Lwt_io.close c in

    lwt () = X.VM.start ~rpc ~session_id ~vm ~start_paused:false ~force:false in

    lwt () = X.VM.remove_from_HVM_boot_params ~rpc ~session_id ~self:vm ~key:"order" in
    lwt () = X.VM.add_to_HVM_boot_params ~rpc ~session_id ~self:vm ~key:"order" ~value:"cd" in
 
    let rec wait token =
		lwt events = X.Event.from ~rpc ~session_id ~classes:[Printf.sprintf "vm/%s" vm] ~token ~timeout:1.0 in
	    let ef = Event_types.event_from_of_xmlrpc events in 
		let finished = List.exists (fun ev -> 
			match Event_helper.record_of_event ev with
				| Event_helper.VM (_,Some r) ->
					r.API.vM_power_state = `Halted) ef.Event_types.events in
		if not finished then
			wait ef.Event_types.token
		else return ()
    in 
    lwt () = wait "" in

    lwt () = X.VM.set_is_a_template ~rpc ~session_id ~self:vm ~value:true in

    return vm_uuid


