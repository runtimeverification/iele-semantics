let pgm = try read_line () with End_of_file -> ""

let dasm = Evm.dasm_hex_string pgm

let iele = Conversion.evm_to_iele dasm

let new_bytes = Iele.asm_iele iele

let `Hex new_pgm = Hex.of_string new_bytes

let () = print_endline new_pgm
