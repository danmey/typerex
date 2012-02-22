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

(** Callbacks to an IDE *)

(** Raised when the user aborts the operation during a callback (for
    example, with C-g under Emacs). *)
exception Quit

type property = [
  `face of Face.face | `fontified | `help_echo of string option
| `font_lock_multiline
]

(** Editor API *)
module type Callback = sig

  (** {4 Functions from Emacs API} *)

  (** In the following, positions are converted to start from 0 instead
      of 1 in Emacs, and the unit is byte by default, instead of char.
      Filenames are always absolute.

      We have commented all functions that are not currently used, to
      minimize the cost of porting TypeRex to another editor. Furthermore,
      only the default [`byte] unit is currently used. *)

  (*
    val goto_char : ?unit:[ `byte | `char ] -> int -> unit
  *)

  (** Filename under focus (to which the querry applies). *)
  val buffer_file_name : unit -> string

  (** Current cursor position. *)
  val point : ?unit:[ `byte | `char ] -> unit -> int
(*
  val line_number : unit -> int
  val column_number : unit -> int
*)
  (** Write each modified buffer in #<file>#. *)
  val do_auto_save : unit -> unit

  (** Display a short message. *)
  val message : ('a, unit, string, unit) format4 -> 'a

  (** Display a possibly long message in the most appropriate way,
      until next user event. *)
  val display_temp : string -> ('a, unit, string, unit) format4 -> 'a

  (** Prompt the user for a string, with provided question. *)
  val read_from_minibuffer : ('a, unit, string, string) format4 -> 'a

  (** Prompt the user for a yes/no answer. *)
  val y_or_n_p : ('a, unit, string, bool) format4 -> 'a
(*
  val buffer_name : unit -> string
  val cd : ('a, unit, string, unit) format4 -> 'a
  val insert : ('a, unit, string, unit) format4 -> 'a
  val find_file : string -> unit

  (** Set buffer with the specified name, clearing it if it exists. *)
  val set_cleared_buffer : ('a, unit, string, unit) format4 -> 'a

*)

  (** {4 Custom functions defined in util.el} *)

  (** Goto the given (filename, position), in the current
      frame (openning the file as needed). The cursor motion should be
      made (buffer-locally) undoable. *)
  val goto : ?unit:[ `byte | `char ] -> string -> int -> unit

  (** Reread the current file. This should keep the modifications
      history for undoing, and push a new undo item on the local undo
      list. *)
  val revert_with_history : unit -> unit

  (** Reload the specified filename if it is currently opened, and
      flush its undo-list. A new undo item may be pushed which just
      calls ocp-undo, for convenience. *)
  val revert_buffer_visiting : string -> unit

  (** Save the specified file if it is opened. *)
  val save_buffer_visiting : string -> unit

  (** If the filename is opened, record that its name has changed to
      the provided new name, and flush its undo list. *)
  val rename_file : string -> string -> unit

  (** Highlight the specified range in the current buffer, until the
      next user action. *)
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

  (** This function should computes the answer that the ide expects to
      a [modify-buffer] command, currently fontifying information and
      inline help. arguments are the modified range (which should
      first be cleared from existing faces and helps), a list of
      faces, each with a list of ranges to apply to, and a list of
      inline helps to put. *)
  val return_fontifying_data :
    int -> int ->
    (Face.face * (int * int) list) list ->
    ((int * int) * string) list ->
    string

  (** This function should computes the answer that the ide expects to
      a [complete] command. arguments are the position, the prefix
      string to complete (with the relevant definition of "word"), and
      then list of candidates (with a one-char description of their
      kind). *)
  val return_completion_data :
    int -> 
    prefix:string ->
    (char * string) list ->
    string

  (** Show the result of a grep, given the project's absolute root,
      the contents (currently in grep-minor-mode format), the overlays
      to highlight occurrences in the result list, and the overlays to
      highlight the local occurences in the current buffer. *)
  val present_grep_results :
    root:string ->
    contents:string ->
    (Face.face * int * int) list ->
    local_overlays:(Face.face * int * int) list ->
    errors:string option -> unit

end

module type SocketCallback =
  functor
    (Arg : sig
      val connection : Ocp_rpc.tagged_connection
    end) ->
      Callback
