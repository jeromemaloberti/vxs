type file_opts = { host: string option; username: string option; password: string option; 
		 nfs_server: string option; nfs_path: string option}
with rpc

let string_of_opts x = Jsonrpc.to_string (rpc_of_file_opts x) 
let opts_of_string x = file_opts_of_rpc (Jsonrpc.of_string x) 

let read_all_channel channel =
  let buffer_size = 4096 in
  let buffer = Buffer.create buffer_size in
  let string = String.create buffer_size in
  let chars_read = ref 1 in
  while !chars_read <> 0 do
    chars_read := input channel string 0 buffer_size;
    Buffer.add_substring buffer string 0 !chars_read
  done;
  Buffer.contents buffer
    
let get_file_opts () =
  let rc_name = (Filename.concat (Sys.getenv "HOME") ".vxs_rc") in
  if not (Sys.file_exists rc_name) then
    begin
      let out = open_out rc_name in
      let defaults_opts = { host = None; username = Some "root"; password = Some "xenroot"; 
			    nfs_server = None; nfs_path = None } in
      output_string out (string_of_opts defaults_opts);
      close_out out;
      defaults_opts
    end
  else
    begin
      let in_file = open_in rc_name in
      let f = read_all_channel in_file in
      opts_of_string f
    end

exception Option_not_set

let default_opt default opt =
  match default with
  | Some o -> 
    (match opt with
    | Some o' -> opt
    | None -> default)
  | None -> opt

let default_opt_no_none default opt opt_name =
  let value = default_opt default opt in
  match value with
    Some o -> o
  | None -> 
    Printf.printf "Option %s is not set\n" opt_name;
    raise Option_not_set 
