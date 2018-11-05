open Gg
open Vg

let otfm_err_str err =
  Format.fprintf Format.str_formatter "%a" Otfm.pp_error err;
  Format.flush_str_formatter ()

let ( >>= ) x f = match x with
  | Error _ as e -> e
  | Ok v -> f v

let render_pdf font_info renderable =
  let r = Vgr_pdf.otf_font (Vg_text.font_raw font_info) in
  (r : (_, Otfm.error) result :> (_, [> Otfm.error]) result) >>= fun otf_font ->
  let font f =
    if Vg_text.font_name font_info = f.Font.name then
      otf_font
    else
      Vgr_pdf.font f
  in
  let r = Vgr.create (Vgr_pdf.target ~font ()) (`Channel stdout) in
  ignore (Vgr.render r renderable);
  ignore (Vgr.render r `End) ;
  Ok ()

let echo font size text =
  Vg_text.load_otf font >>= fun font_info ->
  let font = { Font.name = Vg_text.font_name font_info;
               slant = `Normal;
               weight = `W400;
               size } in
  (* let glyphs = Vg_text.glyphs_of_string font_info text in *)
  let glyphs, advances, len = Vg_text.layout font_info ~font_size:size text in
  let i =
    I.const (Color.black) |>
    I.cut_glyphs ~text ~advances font glyphs |>
    I.move V2.(0.5 * (v 0. size))
  in
  let size = Size2.v len (2. *. size) in
  let view = Box2.v P2.o size in
  render_pdf font_info (`Image (size, view, i))

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
