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

let read_file fname =
  Lwt_io.with_file ~mode:Lwt_io.input fname Lwt_io.read 

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

let put_disk host_config session_id vdi value =
  match Body.body_of_string value with
  | Some v ->
    let uri = Printf.sprintf "http://%s/import_raw_vdi?session_id=%s&vdi=%s" host_config.Host.host session_id vdi in
    http_put uri v
  | None ->
    Lwt.return ()

(** True if string 'x' starts with prefix 'prefix' *)
let startswith prefix x =
  let x_l = String.length x and prefix_l = String.length prefix in
  prefix_l <= x_l && String.sub x 0 prefix_l  = prefix

let rec split ?limit:(limit=(-1)) c s =
  let i = try String.index s c with Not_found -> -1 in
  let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
  if i = -1 || nlimit = 0 then
    [ s ]
  else
    let a = String.sub s 0 i
    and b = String.sub s (i + 1) (String.length s - i - 1) in
    a :: (split ~limit: nlimit c b)

let create_zero_file ?megs:(megs=4) path =
  Lwt_process.exec (Lwt_process.shell (Printf.sprintf "dd if=/dev/zero of=\"%s\" bs=1M count=%d" path megs))

let create_vfat_disk ?megs:(megs=4) path label =
  lwt _ = create_zero_file ~megs path in
  lwt _ = Lwt_process.exec (Lwt_process.shell (Printf.sprintf "/sbin/mkfs.vfat \"%s\" -n \"%s\"" path label)) in
  Lwt.return ()

let copy_to_vfat disk filename contents =
  (* mktemp would be better here *)
  let tempname="/tmp/vfat_tmp_file" in
  lwt fd = Lwt_unix.openfile tempname [Lwt_unix.O_WRONLY; Lwt_unix.O_CREAT] 0o666 in
  let c = Lwt_io.(of_fd output fd) in
  lwt () = Lwt_io.write c contents in
  lwt () = Lwt_io.close c in
  lwt _ = Lwt_process.exec (Lwt_process.shell (Printf.sprintf "mcopy -i \"%s\" %s ::%s" disk tempname filename)) in
  Lwt.return ()

let create_extfs_disk ?megs:(megs=5) path =
  lwt _ = create_zero_file ~megs path in
  lwt _ = Lwt_process.exec (Lwt_process.shell (Printf.sprintf "sudo /sbin/mke2fs -F -j \"%s\"" path)) in
  Lwt.return ()

let mount_extfs mnt_path image =
  lwt _ = Lwt_process.exec (Lwt_process.shell (Printf.sprintf "sudo mkdir -p \"%s\"" mnt_path)) in
  lwt _ = Lwt_process.exec (Lwt_process.shell
			      (Printf.sprintf "sudo mount -o loop \"%s\" \"%s\"" image mnt_path)) in
  (* lwt () = Lwt_unix.mkdir mnt 0o755 in -- cannot sudo !! *)
  Lwt.return ()

let copy_to_path filename contents =
  lwt fd = Lwt_unix.openfile filename [Lwt_unix.O_WRONLY; Lwt_unix.O_CREAT] 0o666 in
  let c = Lwt_io.(of_fd output fd) in
  lwt () = Lwt_io.write c contents in
  lwt () = Lwt_io.close c in
  Lwt.return ()
