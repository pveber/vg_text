open Vg

type font_info

val font_name : font_info -> string
val font_raw : font_info -> string

val load_otf : string -> (font_info,
                          [> Otfm.error | `Read_error of string]) result

val glyphs_of_string : font_info -> string -> glyph list
val text_length : font_info -> font_size:float -> string -> float

