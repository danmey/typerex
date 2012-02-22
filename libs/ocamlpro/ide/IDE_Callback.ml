(**************************************************************************)
(*                                                                        *)
(*                        TypeRex OCaml Studio                            *)
(*                                                                        *)
(*                           Tiphaine Turpin                              *)
(*                                                                        *)
(*  Copyright 2011-2012 INRIA Saclay - Ile-de-France / OCamlPro           *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

exception Quit

type property = [
  `face of Face.face | `fontified | `help_echo of string option
| `font_lock_multiline
]

module type Callback = sig
(*
  val goto_char : ?unit:[ `byte | `char ] -> int -> unit
*)
  val buffer_file_name : unit -> string
  val point : ?unit:[ `byte | `char ] -> unit -> int
(*
  val line_number : unit -> int
  val column_number : unit -> int
*)
  val do_auto_save : unit -> unit
  val message : ('a, unit, string, unit) format4 -> 'a
  val display_temp : string -> ('a, unit, string, unit) format4 -> 'a
  val read_from_minibuffer : ('a, unit, string, string) format4 -> 'a
  val y_or_n_p : ('a, unit, string, bool) format4 -> 'a
(*
  val buffer_name : unit -> string
  val cd : ('a, unit, string, unit) format4 -> 'a
  val insert : ('a, unit, string, unit) format4 -> 'a
  val find_file : string -> unit
  val set_cleared_buffer : ('a, unit, string, unit) format4 -> 'a
*)
  val goto : ?unit:[ `byte | `char ] -> string -> int -> unit
  val revert_with_history : unit -> unit
  val revert_buffer_visiting : string -> unit
  val save_buffer_visiting : string -> unit
  val rename_file : string -> string -> unit
  val highlight : ?unit:[ `byte | `char ] -> Face.face -> int -> int -> unit
(*
  val highlight_regions :
    ?unit:[ `byte | `char ] -> ?forever:bool -> (Face.face * int * int) list -> unit
  val propertize_regions :
    ?unit:[ `byte | `char ] -> (int * int * property list) list -> unit
  val propertize_region_lists :
    ?unit:[ `byte | `char ] -> (property list * (int * int) list) list -> unit
  val propertize_region_lists_command :
    ?unit:[ `byte | `char ] -> (property list * (int * int) list) list -> string
*)
  val return_fontifying_data :
    int -> int ->
    (Face.face * (int * int) list) list ->
    ((int * int) * string) list ->
    string
  val return_completion_data :
    int ->
    prefix:string ->
    (char * string) list ->
    string
  val present_grep_results :
    root:string ->
    contents:string ->
    (Face.face * int * int) list ->
    local_overlays:(Face.face * int * int) list ->
    errors:string option -> unit
(*
  val show_completions : int -> string list -> unit
*)
end

module type SocketCallback =
  functor (Arg : sig
    val connection : Ocp_rpc.tagged_connection
  end) ->
    Callback
