open Msg_types

module type WorldState = sig
  val get_account : bytes -> account
  val get_storage_data : bytes -> bytes -> bytes
  val get_code : bytes -> bytes
  val get_blockhash : int -> bytes
end

let of_z z =
  let sign = if (Z.lt z Z.zero) then "\001" else "\000" in
  let str = sign ^ (Z.to_bits z) in
  Bytes.of_string str

let to_z b =
  let sign = if Bytes.get b 0 = '\001' then Z.neg Z.one else Z.one in
  let magnitude = Z.of_bits (Bytes.to_string (Bytes.sub b 1 ((Bytes.length b) - 1))) in
  Z.mul sign magnitude

let zero = of_z Z.zero

module type MockBlockhash = sig
  val hashes : bytes list
end

module InMemoryWorldState ( Hashes : MockBlockhash ) = struct
  let get_account _ = {nonce=zero;balance=zero;code_empty=true}
  let get_storage_data _ _ = zero
  let get_code _ = Bytes.empty
  let get_blockhash i = List.nth Hashes.hashes i
end

module Connections = struct
  let connection_table : (int , (in_channel * out_channel)) Hashtbl.t
      = Hashtbl.create 10
  let connection_mutex : Mutex.t = Mutex.create ()
  let register chans : unit =
    let my_id = Thread.id (Thread.self()) in
    Mutex.lock connection_mutex;
    Hashtbl.add connection_table my_id chans;
    Mutex.unlock connection_mutex
  let get () =
    let my_id = Thread.id (Thread.self()) in
    Mutex.lock connection_mutex;
    let r = Hashtbl.find connection_table my_id in
    Mutex.unlock connection_mutex;
    r
  let unregister () =
    let my_id = Thread.id (Thread.self()) in
    Mutex.lock connection_mutex;
    let (in_chan, out_chan) = Hashtbl.find connection_table my_id in
    Hashtbl.remove connection_table my_id;
    Mutex.unlock connection_mutex;
    close_in in_chan;
    close_out out_chan
end

let input_framed in_chan decoder =
  let len = input_binary_int in_chan in
  let bytes = Bytes.create len in
  decoder (Pbrt.Decoder.of_bytes bytes)
let output_framed out_chan encoder v =
  let enc = Pbrt.Encoder.create() in
  encoder v enc;
  let encoded = Pbrt.Encoder.to_bytes enc in
  output_binary_int out_chan (Bytes.length encoded);
  output_bytes out_chan encoded;

module NetworkWorldState = struct
  let send_query (q: Msg_types.vmquery) (decoder : Pbrt.Decoder.t -> 'a) : 'a =
    let (in_chan,out_chan) = Connections.get () in
    output_framed out_chan Msg_pb.encode_vmquery q;
    input_framed in_chan decoder

  let get_account (addr : bytes) : account =
    send_query (Get_account {address = addr})
               Msg_pb.decode_account
  let get_storage_data addr offset =
    (send_query (Get_storage_data {address = addr; offset=offset})
               Msg_pb.decode_storage_data).data
  let get_code addr =
    (send_query (Get_code {address = addr})
               Msg_pb.decode_code).code
  let get_blockhash i =
    (send_query (Get_blockhash {offset = Int32.of_int i})
               Msg_pb.decode_blockhash).hash
end

let serve addr (run_transaction : Msg_types.call_context -> Msg_types.call_result) =
  (* server side *)
  let process_transaction chans =
    let (in_chan,out_chan) = chans in
    let call_context = input_framed in_chan Msg_pb.decode_call_context in
    let call_result = run_transaction call_context in
    output_framed out_chan Msg_pb.encode_call_result call_result
  in
  let accept_connection conn =
    let fd, _ = conn in
    let chans = (Unix.in_channel_of_descr fd, Unix.out_channel_of_descr fd) in
    Connections.register chans;
    process_transaction chans;
    Connections.unregister ()
  in
  let serve_on socket =
    while true do
      let conn = Unix.accept socket in
      let _ = Thread.create accept_connection conn in
      ()
    done
  in
  let create_socket addr =
    let open Unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    Unix.bind sock addr;
    listen sock 10;
    sock
  in
  serve_on (create_socket addr)
