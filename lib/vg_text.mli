open Gg
open Vg

type font_info

val load_otf : string -> (font_info, string) result

val text_size : glyph list -> box2
