(* code adapted from vecho.ml in vg library *)
(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Vg

let otfm_err_str err =
  Format.fprintf Format.str_formatter "%a" Otfm.pp_error err;
  Format.flush_str_formatter ()

let ( >>= ) x f = match x with
  | Error _ as e -> e
  | Ok v -> f v

let render_pdf font_info renderable =
  let r = Vgr_pdf.otf_font (Vg_text.Font.data font_info) in
  (r : (_, Otfm.error) result :> (_, [> Otfm.error]) result) >>= fun otf_font ->
  let font f =
    if Vg_text.Font.name font_info = f.Font.name then
      otf_font
    else
      Vgr_pdf.font f
  in
  let r = Vgr.create (Vgr_pdf.target ~font ()) (`Channel stdout) in
  ignore (Vgr.render r renderable);
  ignore (Vgr.render r `End) ;
  Ok ()

let echo font size text =
  Vg_text.Font.load font >>= fun font ->
  let i, bbox = Vg_text.cut ~size font text in
  render_pdf font (`Image (Box2.size bbox, bbox, i))

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
  match echo !font !size msg with
  | Error (#Otfm.error as e) ->
    Format.eprintf "%s: %s@." exec (otfm_err_str e); exit 1
  | Error (`Read_error msg) ->
    Format.eprintf "%s: %s@." exec msg; exit 1
  | Ok () -> exit 0

let () = main ()
