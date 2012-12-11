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

(* Code to parse the XenSource inventory file *)
open Lwt

let inventory_filename = "/etc/xensource-inventory"

(* Keys which must exist: *)
let _installation_uuid		= "INSTALLATION_UUID"
let _control_domain_uuid	= "CONTROL_DOMAIN_UUID"
let _management_interface	= "MANAGEMENT_INTERFACE"
let _management_address_type	= "MANAGEMENT_ADDRESS_TYPE"
let _build_number           = "BUILD_NUMBER"

(* Optional keys: *)
let _current_interfaces		= "CURRENT_INTERFACES"
let _oem_manufacturer		= "OEM_MANUFACTURER"
let _oem_model				= "OEM_MODEL"
let _oem_build_number		= "OEM_BUILD_NUMBER"
let _machine_serial_number	= "MACHINE_SERIAL_NUMBER"
let _machine_serial_name	= "MACHINE_SERIAL_NAME"

let loaded_inventory = ref false
let inventory = Hashtbl.create 10

let of_char c = String.make 1 c
(* trim any quotes off the ends *)
let strip_quotes v =
	if String.length v >= 2
		&& v.[0] = '\''
			&& v.[String.length v - 1] = '\''
	then String.sub v 1 (String.length v - 2)
	else v

let fold_right f string accu =
	let accu = ref accu in
	for i = String.length string - 1 downto 0 do
		accu := f string.[i] !accu
	done;
	!accu

let explode string =
	fold_right (fun h t -> h :: t) string []

let implode list =
	String.concat "" (List.map of_char list)

let rec split ?limit:(limit=(-1)) c s =
	let i = try String.index s c with Not_found -> -1 in
	let nlimit = if limit = -1 || limit = 0 then limit else limit - 1 in
	if i = -1 || nlimit = 0 then
		[ s ]
	else
		let a = String.sub s 0 i
		and b = String.sub s (i + 1) (String.length s - i - 1) in
		a :: (split ~limit: nlimit c b)

(** Returns true for whitespace characters, false otherwise *)
let isspace = function
	| ' ' | '\n' | '\r' | '\t' -> true
	| _ -> false

(** Removes all the characters from the ends of a string for which the predicate is true *)
let strip predicate string =
	let rec remove = function
	| [] -> []
	| c :: cs -> if predicate c then remove cs else c :: cs in
	implode (List.rev (remove (List.rev (remove (explode string)))))

let parse_inventory_entry line =
	match split ~limit:2 '=' line with
		| [k; v] ->
			(* trim whitespace *)
			Some (k, strip_quotes (strip isspace v))
		| _ -> None

let string_of_table h = 
	let lines = List.fold_left (fun acc (k, v) ->
		Printf.sprintf "%s='%s'\n" k v :: acc) [] h in
	String.concat "" lines

let read_inventory_contents () =
	let stream = Lwt_io.lines_of_file inventory_filename in
    lwt () = Lwt_stream.iter (fun line ->
		match parse_inventory_entry line with
			| Some (k, v) -> Hashtbl.add inventory k v
			| None -> ()) stream in
	loaded_inventory := true;
    Lwt.return ()

exception Missing_inventory_key of string

let lookup ?default key =
	lwt () = if not (!loaded_inventory) then read_inventory_contents () else return () in
	if (Hashtbl.mem inventory key)
	then
		return (Hashtbl.find inventory key)
	else
		match default with
			| None   -> fail (Missing_inventory_key key)
			| Some v -> return v
