open Yojson
open Yojson.Basic.Util
open Msg_types
open IeleApi

let file = Sys.argv.(1)

let addr = Unix.ADDR_UNIX file

let emptyTrie = `String "0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"
let dummy     = `String "0x0000000000000000000000000000000000000000000000000000000000000000"

let process_requests (_in,fd) =
  let stream = Yojson.Safe.stream_from_channel _in in
  try
    while true do
      let request_safe = Stream.next stream in
      let request = Yojson.Safe.to_basic request_safe in
      let _method = request |> member "method" |> to_string in
      let params = request |> member "params" |> to_list in
      let response = match _method, params with
      | "eth_getCode", [`String address; `String number] -> eth_getCode address number
      | "eth_getBlockByNumber", [`String number; `Bool _] -> eth_getBlockByNumber number
      | "eth_getTransactionReceipt", [`String hash] -> eth_getTransactionReceipt hash
      | "iele_sendTransaction", [tx] -> iele_sendTransaction tx
      | "iele_call", [tx; `String number] -> iele_call tx number
      | "eth_getBalance", [`String address; `String number] -> eth_getBalance address number
      | "eth_getStorageRoot", [`String address; `String number] -> if eth_isStorageEmpty address number then emptyTrie else dummy
      | "personal_newAccount", _ -> personal_newAccount ()
      | "personal_unlockAccount", _ -> `Bool true
      | "test_rewindToBlock", [`Int number] -> test_rewindToBlock number
      | "test_mineBlocks", [`Int number] -> test_mineBlocks number
      | "test_modifyTimestamp", [`Int timestamp] -> test_modifyTimestamp timestamp
      | "miner_setEtherbase", [`String address] -> miner_setEtherbase address
      | "test_setChainParams", [params] -> test_setChainParams params
      | _ -> failwith "invalid rpc request"
      in
      let real_response = `Assoc [("result", response)] in
      let response_str = Yojson.Basic.to_string real_response in
      let _ = Unix.send_substring fd response_str 0 (String.length response_str) [] in
      ()
    done
  with Stream.Failure -> ()

let accept_connection conn =
  let fd, _ = conn in
  let in_chan = Unix.in_channel_of_descr fd in
  let chans = (in_chan,fd) in
  let finish () = Unix.shutdown fd Unix.SHUTDOWN_SEND;
                  Unix.close fd in
  (try
    process_requests chans
  with
    exn -> finish (); raise exn);
  finish ()

let serve_on socket =
  while true do
    let conn = Unix.accept socket in
    accept_connection conn
  done

let create_socket addr =
  let open Unix in
  let sock = socket PF_UNIX SOCK_STREAM 0 in
  let timeout = ref 1 in
    while (
      try
        Unix.bind sock addr;
        false
      with Unix.Unix_error(Unix.EADDRINUSE, _, _) -> 
        print_endline("Socket in use, retrying in " ^ (string_of_int !timeout) ^ "...");
        true
    ) do
      Unix.sleep !timeout;
      timeout := !timeout + 1
    done;
    listen sock 10;
    sock;;

serve_on (create_socket addr);;
