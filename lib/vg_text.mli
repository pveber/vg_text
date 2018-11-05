open Gg
open Vg

type font_info

val font_name : font_info -> string
val font_raw : font_info -> string
val font_ascender : font_info -> float
val font_descender : font_info -> float

val load_otf : string -> (font_info,
                          [> Otfm.error | `Read_error of string]) result

val glyphs_of_string : font_info -> string -> glyph list
val layout : font_info -> font_size:float -> string -> int list * v2 list * float
val text_length : font_info -> font_size:float -> string -> float

