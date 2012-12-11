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


type host_config = {
	host : string;                (* e.g. st11.uk.xensource.com *)
	username : string;            (* For the underlying xenserver *)
	password : string;            
}

let make host username password =
	{ host; username; password }

let get_rpc host =
	let uri = Printf.sprintf "http://%s/" host.host in
    X.make uri
