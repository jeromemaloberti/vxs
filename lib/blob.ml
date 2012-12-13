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
open Cohttp_lwt_unix

module X=Xen_api_lwt_unix

type t = {
	  r : string;
	  u : string;
}

let http_put url content = 
  let headers = Cohttp.Header.of_list ["connection","close"] in
  Client.call ~headers `PUT ~body:content ~chunked:false (Uri.of_string url) >>= function 
  |None -> assert false
  |Some (res, body) ->
      Lwt_stream.iter_s (fun s -> return ()) (Body.stream_of_body body)
          
let http_get url =
	Client.call `GET (Uri.of_string url) >>= function
	| None -> assert false
	| Some (res,body) ->
			lwt list = (Lwt_stream.to_list (Body.stream_of_body body)) in
      return (String.concat "" list)
          
let put_blob host_config session_id blob value =
	match Body.body_of_string value with 
	| Some v -> 
			let uri = Printf.sprintf "http://%s/blob?session_id=%s&ref=%s" host_config.Host.host session_id blob.r in
			http_put uri v
	| None -> 
			Lwt.return ()
          
let add_blob rpc session_id vm name =
	lwt blob = X.VM.create_new_blob ~rpc ~session_id ~vm ~name ~mime_type:"" ~public:true in
  lwt uuid = X.Blob.get_uuid ~rpc ~session_id ~self:blob in
  return { r = blob; u = uuid; }
      
let get_blob host_config session_id blob = 
	let uri = Printf.sprintf "http://%s/blob?session_id=%s&ref=%s" host_config.Host.host session_id blob.r in
  http_get uri
      
let add_blob_with_content host_config rpc session_id vm name value =
	lwt blob = add_blob rpc session_id vm name in
  lwt () = put_blob host_config session_id blob value in
  return blob
      
let of_uuid rpc session_id u =
	lwt r = X.Blob.get_by_uuid rpc session_id u in
	return { r; u }
      
let of_ref rpc session_id r =
	lwt u = X.Blob.get_uuid rpc session_id r in
	return { r; u }
      
