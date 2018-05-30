(* code adapted from vecho.ml in vg library *)
(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Result

let str = Printf.sprintf
let otfm_err_str err =
  Format.fprintf Format.str_formatter "%a" Otfm.pp_error err;
  Format.flush_str_formatter ()
    (* let ( >>= ) x f = match x with
     *   | Error e -> Error (str "%s: %s" fn (otfm_err_str e))
     *   | Ok v -> f v
     * in *)

let ( >>= ) x f = match x with
  | Error _ as e -> e
  | Ok v -> f v

(* Font information *)

module Int = struct type t = Vg.glyph let compare = compare end
module Gmap = Map.Make (Int) (* glyph maps *)
module Cmap = Gmap           (* uchar maps *)

type font_info = {
  font_name : string ;
  raw : string;                                      (* The font bytes. *)
  cmap : int Cmap.t;           (* Maps unicode scalar values to glyphs. *)
  advs : int Gmap.t;             (* Maps glyph to advances in em space. *)
  kern : int Gmap.t Gmap.t;    (* Maps glyph pairs to kern adjustement. *)
  units_per_em : int;                        (* Number of units per em. *)
}

let font_name fi = fi.font_name
let font_raw fi = fi.raw

let add_adv acc g adv _ = Gmap.add g adv acc
let add_cmap acc kind (u0, u1) g =
  let acc = ref acc in
  begin match kind with
  | `Glyph_range ->
      for i = 0 to (u1 - u0) do acc := Cmap.add (u0 + i) (g + i) !acc done;
  | `Glyph ->
      for u = u0 to u1 do acc := Cmap.add u g !acc done
  end;
  !acc

let add_ktable acc i =
  (if i.Otfm.kern_dir = `H && i.Otfm.kern_kind = `Kern then `Fold else `Skip),
  acc

let add_kpair acc g0 g1 kv =
  let m = try Gmap.find g0 acc with Not_found -> Gmap.empty in
  Gmap.add g0 (Gmap.add g1 kv m) acc

let get_glyph fi g = try Gmap.find g fi.cmap with Not_found -> 0
let get_adv fi g = try Gmap.find g fi.advs with Not_found -> 0
let get_kern fi g g' =
  try Gmap.find g' (Gmap.find g fi.kern) with Not_found -> 0

(* Text layout *)

let fixed_layout size text =
  let units_per_em = 1000. in
  let add_adv acc _ = function
  | `Malformed _ -> acc | `Uchar _ -> acc + 600 (* Courier's advance *)
  in
  let len = size *. (float (Uutf.String.fold_utf_8 add_adv 0 text)) in
  [], [], (len /. units_per_em)

let otf_layout fi size text =
  let u_to_em = float fi.units_per_em in
  let rec add_glyph (gs, advs, len as acc) i = function
  | `Malformed _ -> add_glyph acc i (`Uchar Uutf.u_rep)
  | `Uchar u ->
      let g = get_glyph fi (Uchar.to_int u) in
      let adv = get_adv fi g in
      let sadv = V2.v ((size *. (float adv)) /. u_to_em) 0. in
      (g :: gs, sadv :: advs, len + adv)
  in
  let gs, advs, len = Uutf.String.fold_utf_8 add_glyph ([], [], 0) text in
  gs, advs, ((size *. (float len)) /. u_to_em)

let otf_kern_layout fi size text =
  let u_to_em = float fi.units_per_em in
  let rec add (prev, gs, advs, kerns as acc) i = function
  | `Malformed _ -> add acc i (`Uchar Uutf.u_rep)
  | `Uchar u ->
      let g = get_glyph fi (Uchar.to_int u) in
      let advs = get_adv fi g :: advs in
      let kerns = if prev = -1 then kerns else (get_kern fi prev g) :: kerns in
      (g, g :: gs, advs, kerns)
  in
  let rec advances acc len advs kerns = match advs, kerns with
  | adv :: advs, k :: kerns ->
      let adv = adv + k in
      let sadv = V2.v ((size *. (float adv)) /. u_to_em) 0. in
      advances (sadv :: acc) (len + adv) advs kerns
  | adv :: [], [] -> acc, len + adv
  | _ -> assert false
  in
  let _, gs, advs, kerns = Uutf.String.fold_utf_8 add (-1, [], [], []) text in
  let advs, len = advances [] 0 (List.rev advs) (List.rev kerns) in
  gs, advs, ((size *. float len) /. u_to_em)

(* Text rendering *)

let renderable (fname, info) size kern text =
  let glyphs_rev, advances_rev, len = match info with
  | None -> fixed_layout size text
  | Some info when kern -> otf_kern_layout info size text
  | Some info -> otf_layout info size text
  in
  let glyphs, advances = List.rev glyphs_rev, List.rev advances_rev in
  let font = { Font.name = fname; slant = `Normal; weight = `W400; size } in
  let i =
    I.const (Color.black) >>
    I.cut_glyphs ~text ~advances font glyphs >>
    I.move V2.(0.5 * (v size size))
  in
  let size = Size2.v (len +. size) (2. *. size) in
  let view = Box2.v P2.o size in
  `Image (size, view, i)

let string_of_file inf =
  try
    let ic = if inf = "-" then stdin else open_in_bin inf in
    let close ic = if inf <> "-" then close_in ic else () in
    let buf_size = 65536 in
    let b = Buffer.create buf_size in
    let s = Bytes.create buf_size in
    try
      while true do
        let c = input ic s 0 buf_size in
        if c = 0 then raise Exit else
        Buffer.add_subbytes b s 0 c
      done;
      assert false
    with
    | Exit -> close ic; Ok (Buffer.contents b)
    | Failure _ -> close ic; Error (`Read_error (str "%s: input file too large" inf))
    | Sys_error e -> close ic; Error (`Read_error (str "%s: %s" inf e))
  with
  | Sys_error e -> Error (`Read_error (str "%s: %s" inf e))

let load_otf fn =
  match string_of_file fn with
  | Error _ as e -> e
  | Ok raw ->
    let d = Otfm.decoder (`String raw) in
    let r =
      Otfm.postscript_name d                      >>= fun font_name ->
      Otfm.head d                                 >>= fun head ->
      Otfm.cmap d add_cmap Cmap.empty             >>= fun (_, cmap) ->
      Otfm.hmtx d add_adv Gmap.empty              >>= fun advs ->
      Otfm.kern d add_ktable add_kpair Gmap.empty >>= fun kern ->
      let font_name = match font_name with None -> "Unknown" | Some n -> n in
      let units_per_em = head.Otfm.head_units_per_em in
      Ok { font_name ; raw; cmap; advs; kern; units_per_em }
    in
    (r : (_, Otfm.error) result :> (_, [> Otfm.error]) result)

let to_glyphs fi text =
  let f acc _ = function
    | `Malformed _ -> get_glyph fi (Uchar.to_int Uutf.u_rep) :: acc
    | `Uchar u ->
      let g = get_glyph fi (Uchar.to_int u) in
      g :: acc
  in
  Uutf.String.fold_utf_8 f [] text
  |> List.rev

let text_size glyphs = assert false
