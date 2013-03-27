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

type installty = 
	| Pxe of string (* branch name *)
	| Mainiso of string (* iso name *)
with rpc

let string_of_installty x = Jsonrpc.to_string (rpc_of_installty x) 

let pxedir = "/usr/groups/netboot/pxelinux.cfg"

(* API:
 *
 * Vxs.template_create : host_config -> branch:string -> uuid:string
 * Vxs.template_list : host_config -> uuid:string * (name * branch * build_number) list
 * Vxs.create_pool : 
 * 
 *)

type vxs_template_config = {
	  ty : installty;              (* e.g. Pxe "trunk-ring3" or Mainiso "main.iso" *)
    
	  vm_uuid : string;                  (* VM uuid *)
	  vxs_root_password : string; 
    
	  post_install : Blob.t;   (* Called by the host installer *)
	  initscript : Blob.t;     (* Called on firstboot before firstboot
							                  scripts! (sets host/dom0 uuid) *)
	  veryfirstboot : Blob.t;  (* Installed into firstboot.d *)
	  firstboot : Blob.t; 
	  id_dsa : Blob.t;
	  answerfile : Blob.t;     (* Host installer answerfile *)
	  vsed : Blob.t;
}

type vxs_pool_config = {
	  n : int;
	  name : string;
	  networks : int;
}

let create_hash host vxs =
	let h = Hashtbl.create 10 in
	let l = [    "host",host.host;
			     "username",host.username;
			     "password",host.password;
			     "vm_uuid",vxs.vm_uuid;
			     "vxs_root_password",vxs.vxs_root_password;
			     "post_install_uuid",vxs.post_install.Blob.u;
			     "initscript_uuid",vxs.initscript.Blob.u;
			     "veryfirstboot_uuid",vxs.veryfirstboot.Blob.u;
			     "firstboot_uuid",vxs.firstboot.Blob.u;
			     "id_dsa_uuid",vxs.id_dsa.Blob.u;
			     "answerfile_uuid",vxs.answerfile.Blob.u;
			     "vsed_uuid",vxs.vsed.Blob.u;
		] in
	let extra = match vxs.ty with
		| Pxe branch -> 
			[ "branch",branch;
			  "sourcetype","url" ]
		| Mainiso isoname ->
			[ "branch","";
			  "sourcetype","local"] 
	in
	List.iter (fun (x,y) -> Hashtbl.add h x (Tstr y)) (l @ extra);
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
let get_linux_cmdline = get "install/linux_cmdline"

let meg = Int64.mul 1024L 1024L
let gig = Int64.mul 1024L meg
let g2 = Int64.mul gig 2L
let g40 = Int64.mul gig 40L
let m4 = Int64.mul meg 4L

let exn_to_string = function
  | Api_errors.Server_error(code, params) ->
      Printf.sprintf "%s %s" code (String.concat " " params)
  | e -> Printexc.to_string e

type vxs_template = {
  vxs_r : string;
  vxs_name : string;
  vxs_uuid : string;
  vxs_ty : installty;
} with rpc

type vxs_templates = vxs_template list with rpc

let with_rpc_and_session host f =
    let uri = Printf.sprintf "http://%s/" host.host in
    let rpc = X.make uri in
    lwt session_id = X.Session.login_with_password rpc host.username host.password "1.0" in
    Lwt.finalize (fun () -> f ~rpc ~session_id) (fun () -> X.Session.logout ~rpc ~session_id)    

let update_vxs_template_cache ~rpc ~session_id =
  lwt vms = X.VM.get_all_records ~rpc ~session_id in
  let vxs_templates = List.filter (fun (ref,_rec) -> List.mem_assoc "vxs_template" _rec.API.vM_other_config) vms in
  let vxs_templates = List.map (fun (ref,_rec) ->
    { vxs_r = ref;
      vxs_name = _rec.API.vM_name_label;
      vxs_uuid = _rec.API.vM_uuid;
      vxs_ty = installty_of_rpc (Jsonrpc.of_string (List.assoc "vxs_ty" _rec.API.vM_other_config)); }
  ) vxs_templates in
  lwt [p] = X.Pool.get_all ~rpc ~session_id in
  lwt () = X.Pool.remove_from_other_config ~rpc ~session_id ~self:p ~key:"vxs_template_cache" in
  lwt () = X.Pool.add_to_other_config ~rpc ~session_id ~self:p ~key:"vxs_template_cache" ~value:(Jsonrpc.to_string (rpc_of_vxs_templates vxs_templates)) in
  Lwt.return ()

let check_pxe_dir () =
	try_lwt 
		Lwt_unix.stat pxedir >> return ()
    with _ ->
		fail (Failure "No PXE dir")

let get_xenserver_templates host =
  with_rpc_and_session host (fun ~rpc ~session_id ->
    lwt p = X.Pool.get_all ~rpc ~session_id >|= List.hd in
    lwt oc = X.Pool.get_other_config ~rpc ~session_id ~self:p in
    let result = 
      try 
	let s = List.assoc "vxs_template_cache" oc in
	vxs_templates_of_rpc (Jsonrpc.of_string s)
      with _ ->
	[] 
    in
    Lwt.return result)

let install_vxs host template new_name =
  with_rpc_and_session host (fun ~rpc ~session_id -> 
    lwt vm = 
      try_lwt 
        X.VM.get_by_uuid ~rpc ~session_id ~uuid:template
      with _ -> 
         lwt vms = X.VM.get_by_name_label ~rpc ~session_id ~label:template in
         Lwt.return (List.hd vms)
    in
    lwt oc = X.VM.get_other_config ~rpc ~session_id ~self:vm in
    lwt () = if not (List.mem_assoc "vxs_template" oc) then Lwt.fail (Failure "not a VXS template") else Lwt.return () in
    lwt is_t = X.VM.get_is_a_template ~rpc ~session_id ~self:vm in
    lwt () = if not is_t then Lwt.fail (Failure "Not a template") else Lwt.return () in
    lwt new_vm = X.VM.clone ~rpc ~session_id ~vm ~new_name in
    lwt () = X.VM.provision ~rpc ~session_id ~vm:new_vm in
    lwt () = X.VM.remove_from_other_config ~rpc ~session_id ~self:new_vm ~key:"vxs_template" in
    lwt () = X.VM.add_to_other_config ~rpc ~session_id ~self:new_vm ~key:"vxs" ~value:"true" in
    lwt uuid = X.VM.get_uuid ~rpc ~session_id ~self:new_vm in
    Lwt.return uuid)

let create_xenserver_template host ty =
  lwt () = match ty with | Pxe _ -> check_pxe_dir () | _ -> Lwt.return () in
  with_rpc_and_session host (fun ~rpc ~session_id -> 
    lwt templates = X.VM.get_all_records_where ~rpc ~session_id ~expr:"field \"name__label\" = \"Other install media\"" in
    let (template,_) = List.hd templates in
	Printf.printf "Found template ref: %s\n" template;
	lwt vm = X.VM.clone ~rpc ~session_id ~vm:template ~new_name:"xenserver-unknown" in
    lwt vm_uuid = X.VM.get_uuid ~rpc ~session_id ~self:vm in
    lwt () = X.VM.provision ~rpc ~session_id ~vm in
    lwt () = X.VM.set_memory_limits ~rpc ~session_id ~self:vm ~static_min:g2 ~static_max:g2 ~dynamic_min:g2 ~dynamic_max:g2 in

    lwt nets = X.Network.get_all_records_where ~rpc ~session_id ~expr:"field \"bridge\" = \"xenbr0\"" in
    let (network,_) = List.hd nets in
	lwt vif = X.VIF.create ~rpc ~session_id ~device:"0" ~network ~vM:vm ~mAC:"" ~mTU:1500L ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[] ~locking_mode:`unlocked ~ipv4_allowed:[] ~ipv6_allowed:[] in
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
    lwt vsed = Blob.add_blob rpc session_id vm "vsed" in

    let vxs_template_config = {
		ty;
		vm_uuid;
        vxs_root_password = host.password;
		post_install;
		initscript;
		veryfirstboot;
		firstboot;
		id_dsa;
		answerfile;
		vsed;
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

    lwt vsed_script = Utils.read_file "scripts/vsed" in
    lwt () = Blob.put_blob host session_id vsed vsed_script in

    let pub_name = (Filename.concat (Sys.getenv "HOME") ".ssh/id_rsa.pub") in
    lwt exist = try_lwt 
      lwt _ = Lwt_unix.stat pub_name in
      Lwt.return true
      with _ -> Lwt.return false  in 
    lwt () = if exist then begin
      lwt id_dsa_string = Utils.read_file pub_name  in
      Blob.put_blob host session_id id_dsa id_dsa_string
    end else Lwt.return () in   

    let linux_cmdline = get_linux_cmdline host vxs_template_config in

    let pxe_path = Printf.sprintf "%s/%s" pxedir vm_uuid in
	let pxe_config = get_pxe_config host vxs_template_config in

	lwt () = begin match vxs_template_config.ty with
		| Pxe branch ->
			lwt fd = Lwt_unix.openfile pxe_path [Lwt_unix.O_WRONLY; Lwt_unix.O_CREAT] 0o666 in
            let c = Lwt_io.(of_fd output fd) in
	
			lwt () = Lwt_io.write c pxe_config in
            lwt () = Lwt_io.close c in

            lwt () = X.VM.remove_from_HVM_boot_params ~rpc ~session_id ~self:vm ~key:"order" in
            lwt () = X.VM.add_to_HVM_boot_params ~rpc ~session_id ~self:vm ~key:"order" ~value:"ncd" in

            Lwt.return () 
        | Mainiso isoname ->
			lwt isos = X.VDI.get_by_name_label ~rpc ~session_id ~label:isoname in
            let iso = List.hd isos in
            lwt vbd = X.VBD.create ~rpc ~session_id ~vDI:iso ~vM:vm ~userdevice:"3" ~bootable:true ~mode:`RO ~_type:`CD ~unpluggable:true ~empty:false ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[] in
            lwt vdi2 = X.VDI.create ~rpc ~session_id ~sR:default_sr ~name_label:"Autoinstall disk" ~name_description:"" ~virtual_size:m4 ~_type:`user ~sharable:false ~read_only:false ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[] in
            lwt vbd = X.VBD.create ~rpc ~session_id ~vDI:vdi2 ~vM:vm ~userdevice:"1" ~bootable:false ~mode:`RW ~_type:`Disk ~unpluggable:false ~empty:false ~other_config:["owner",""] ~qos_algorithm_type:"" ~qos_algorithm_params:[] in
            let fname = "/tmp/autoinstalldisk" in
            lwt () = Utils.create_vfat_disk fname "autoinstall" in
            lwt () = Utils.copy_to_vfat fname "cmdline" linux_cmdline in
            lwt contents = Utils.read_file fname  in
            lwt () = Utils.put_disk host session_id vdi2 contents in
            Lwt.return ()
    end in

    lwt () = X.VM.start ~rpc ~session_id ~vm ~start_paused:false ~force:false in
    lwt () = X.VM.remove_from_HVM_boot_params ~rpc ~session_id ~self:vm ~key:"order" in
    lwt () = X.VM.add_to_HVM_boot_params ~rpc ~session_id ~self:vm ~key:"order" ~value:"cd" in
    lwt () = X.VM.add_to_other_config ~rpc ~session_id ~self:vm ~key:"vxs_ty" ~value:(string_of_installty ty) in 

    let rec wait token =
		lwt events = X.Event.from ~rpc ~session_id ~classes:[Printf.sprintf "vm/%s" vm] ~token ~timeout:1.0 in
	    let ef = Event_types.event_from_of_rpc events in 
		let finished = List.exists (fun ev -> 
			match Event_helper.record_of_event ev with
				| Event_helper.VM (_,Some r) ->
					r.API.vM_power_state = `Halted) ef.Event_types.events in
		if not finished then
			wait ef.Event_types.token
		else return ()
    in 
    lwt () = wait "" in

    lwt oc = X.VM.get_other_config ~rpc ~session_id ~self:vm in
    if List.mem_assoc "vxs_template" oc 
    then begin
      lwt () = X.VM.set_is_a_template ~rpc ~session_id ~self:vm ~value:true in
      lwt () = update_vxs_template_cache ~rpc ~session_id in
      return vm_uuid
    end else begin
      (* Leaking disk if we're using mainiso install type *)
      lwt () = X.VDI.destroy ~rpc ~session_id ~self:vdi in
      lwt () = X.VM.destroy ~rpc ~session_id ~self:vm in
      Lwt.fail (Failure "VM failed to install correctly")
   end)



(*
let create_pool host template pool_name nhosts =
    let uri = Printf.sprintf "http://%s/" host.host in
    let rpc = X.make uri in
    lwt session_id = X.Session.login_with_password rpc host.username host.password "1.0" in
    lwt p = X.Pool.get_all ~rpc ~session_id >|= List.hd in
    lwt oc = X.Pool.get_other_config ~rpc ~session_id ~self:p in
    let s = List.assoc "vxs_template_cache" oc in
    let templates = vxs_templates_of_rpc (Jsonrpc.of_string s) in
    let t = try
*)
