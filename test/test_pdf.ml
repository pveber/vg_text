let echo font size errbuf =
  Ok ()

let exec = Filename.basename Sys.executable_name

let main () =
  let usage = Printf.sprintf
      "Usage: %s [OPTION]... [STRING]... \n\
       Writes UTF-8 encoded strings to a PDF document on stdout.\n\
       Options:" exec
  in
  let font = ref "" in
  let size = ref 20. in
  let msg = Buffer.create 255 in
  let add_string s = Buffer.add_string msg s; Buffer.add_char msg ' ' in
  let options = [
    "-f", (Arg.Set_string font), " FILE, specify the OpenType font file to use";
    "-s", (Arg.Set_float size), " SIZE, specify the font size (in mm)";
  ]
  in
  Arg.parse (Arg.align options) add_string usage;
  let msg =
    let l = Buffer.length msg in
    Buffer.sub msg 0 (if l > 0 then l - 1 else 0) (* rem. last ' ' *)
  in
  if !font = "" then (
    prerr_endline "Provide a path to an opentype font" ;
    Arg.usage options usage ;
    exit 1
  ) ;
  match echo font !size msg with
  | Error e -> Format.eprintf "%s: %s@." exec e; exit 1
  | Ok () -> exit 0

let () = main ()
