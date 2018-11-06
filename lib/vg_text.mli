open Gg
open Vg

module Font : sig
  type t

  val name : t -> string
  val data : t -> string
  val ascender : t -> float
  val descender : t -> float

  val load : string -> (t,
                        [> Otfm.error | `Read_error of string]) result
end

val glyphs_of_string : Font.t -> string -> glyph list
val layout : Font.t -> font_size:float -> string -> int list * v2 list * float
val text_length : Font.t -> font_size:float -> string -> float
val cut :
  ?col:Color.t ->
  ?size:float ->
  Font.t ->
  string ->
  image * box2
