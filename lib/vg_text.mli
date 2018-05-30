open Gg
open Vg

type font_info

val font_name : font_info -> string
val font_raw : font_info -> string

val load_otf : string -> (font_info,
                          [> Otfm.error | `Read_error of string]) result

val to_glyphs : font_info -> string -> glyph list
val text_size : string -> box2

