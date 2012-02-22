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

open OcpLang
open Unix
open Face
include Debug.Tag(struct let tag = "eclipseCallback" end)

let callback_read_error = "CALLBACK_READ_ERROR"
let error_in_callback = "ERROR_IN_CALLBACK"
let quit = "Quit"

exception CallbackReadError of string
exception ErrorInCallback of string

module Make(Arg : sig
  val connection : Ocp_rpc.tagged_connection
end) = struct

  let command_k k fmt =
    Printf.ksprintf
      (function command ->
        match
          Arg.connection#send_request
            ~write_command:(function oc -> output_string oc command)
        with
          | [res] when res = callback_read_error ->
              raise (CallbackReadError command)
          | error_mark :: err when error_mark = error_in_callback ->
	      if err = [quit] then
		raise IDE_Callback.Quit
	      else
		let e = String.concat " " err in
		raise (ErrorInCallback e)
          | res -> k (String.concat "\n" res))
      fmt

  let parse_string s =
    let len = String.length s in
    if len >= 2 && s.[0] = '"' && s.[len-1] = '"' then
      String.sub s 1 (len - 2)
    else
      invalid_arg "parse_string"

  let quote s = String.replace_chars (String.escaped s) [' ', "\\32"]

  let command_int fmt = command_k int_of_string fmt
  let command_bool fmt =
    command_k
      (function "t" -> true | "nil" -> false | _ -> invalid_arg "bool")
      fmt
  let command_string fmt = command_k parse_string fmt
  let command_unit fmt = command_k ignore fmt

  let pos ?(unit=`byte) n =
    match unit with
      | `char -> assert false
      | `byte -> Printf.sprintf "%d" n

  let goto_char ?unit n = command_unit "goto-char %s" (pos ?unit n)

  let point ?(unit=`byte) () =
    match unit with
      | `char -> assert false
      | `byte -> command_int "point-bytes"
(*
  let line_number : unit -> int
  let column_number : unit -> int
*)
  let do_auto_save () = command_unit "do-auto-save"
  let message fmt =
    Printf.ksprintf (function s -> command_unit "message %s" (quote s)) fmt
  let display_temp buffer_name fmt =
    Printf.ksprintf
      (function s ->
        command_unit "display-message-or-buffer %S %s" buffer_name (quote s)) fmt
  let read_from_minibuffer _ = assert false
  let y_or_n_p _ = assert false
  let buffer_file_name () = command_string "buffer-file-name"
(*
  let buffer_name : unit -> string
  let cd : ('a, unit, string, unit) format4 -> 'a
  let insert : ('a, unit, string, unit) format4 -> 'a
  let set_cleared_buffer : ('a, unit, string, unit) format4 -> 'a
*)
  let find_file fname = command_unit "find-file %s" fname

  let goto ?unit file pos = find_file file ; goto_char ?unit pos
  let revert_with_history () = assert false
  let revert_buffer_visiting _ = assert false
  let save_buffer_visiting _ = assert false
  let rename_file _ _ = assert false
  let highlight ?unit face b e = () (* Stub *)
(*
  let highlight_regions :
    ?unit:[ `byte | `char ] -> ?forever:bool -> (Face.face * int * int) list -> unit
  let propertize_regions :
    ?unit:[ `byte | `char ] -> (int * int * property list) list -> unit
  let propertize_region_lists :
    ?unit:[ `byte | `char ] -> (property list * (int * int) list) list -> unit
  let propertize_region_lists_command :
    ?unit:[ `byte | `char ] -> (property list * (int * int) list) list -> string
*)
  let return_fontifying_data b e faces helps =
    let regions =
      List.fold_left
        (fun acc (face, rs) ->
          List.fold_left
            (fun acc (b, e) -> ((b, e), face) :: acc)
            acc
            rs)
        []
        faces
    in
    let regions =
      List.sort (fun (r, _) (r', _) -> compare r r') regions
    in
    let result =
      Printf.sprintf "%d %d\n" b e ^
        String.concat "\n"
        (List.map
           (function (b, e), face ->
             Printf.sprintf "%s %d %d" (Face.face_eclipse_name face) b e)
           regions)
    in
    debugln "result:\n%s\n" result;
    result

  let return_completion_data pos ~prefix candidates = assert false

  (* unused *)
  let eclipse_face = function
    | `typerex `governing -> "governing"
    | `font_lock `keyword -> "keyword"
    | `font_lock `ftype -> "type"
    | `font_lock `variable_name -> "variableName"
    | `desc [`weight `ultra_bold]
    | _ -> "normal"

  let present_grep_results ~root ~contents overlays ~local_overlays ~errors =
    command_unit "present-grep-results"

  let show_completions _ _ = assert false

end
