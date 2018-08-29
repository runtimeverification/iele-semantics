let rev_string str =
  let len = String.length str in
  let buf = Buffer.create len in
  for i = len - 1 downto 0 do
    Buffer.add_char buf (String.get str i)
  done;
  Buffer.contents buf

exception Break of int

let be_int i =
  let le = Z.to_bits i in
  let be = rev_string le in
  let len = String.length be in
  try 
    for i = 0 to len - 1 do
      if be.[i] <> '\000' then raise (Break i)
    done;
    ""
  with Break i -> String.sub be i (len - i)

let be_int_width i width =
  let be = be_int i in
  let byte_width = (width + 7) / 8 in
  let unpadded_byte_width = String.length be in
  let padded = Bytes.make byte_width '\000' in
  Bytes.blit_string be 0 padded (byte_width - unpadded_byte_width) unpadded_byte_width;
  Bytes.to_string padded

let string_of_char ch = String.make 1 ch

let rlp_encode_length len offset =
  if len < 56 then 
    let ch = Char.chr (len + offset) in
    string_of_char ch
  else
    let len_le = Z.to_bits (Z.of_int len) in
    let len_be = rev_string len_le in
    let len_len = String.length len_be in
    let ch = Char.chr (len_len + offset + 55) in
    (string_of_char ch) ^ len_be

let rlp_encode_string str =
  let len = String.length str in
  if len = 1 && Char.code (String.get str 0) < 128 then str
  else (rlp_encode_length len 128) ^ str

module UnionFind =
struct
  type t = { prev: int array; size: int array }

  let create n = { prev=Array.init n (fun i -> i); size = Array.make n 0 }

  let rec find uf i =
    let prev = uf.prev.(i) in
    if prev = i then i else begin
      let _class = find uf prev in
      uf.prev.(i) <- _class;
      _class
    end

  let union uf a b =
    let classa = find uf a in
    let classb = find uf b in
    if classa <> classb then begin
      if uf.size.(classa) > uf.size.(classb) then
        uf.prev.(classb) <- classa
      else if uf.size.(classa) < uf.size.(classb) then
        uf.prev.(classa) <- classb
      else begin
        uf.size.(classa) <- uf.size.(classa) + 1;
        uf.prev.(classb) <- classa
      end
    end
end
