(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Format

(** Context in which idents are interpreted uniquely. *)
type ctx = {
  modname : string;
    (** (capitalized) module name *)
  kind : [ `interface | `implementation | `pack ];
    (** source file kind *)
  source_digest : Digest.t
    (** digest of the source file. For pre-processed files, this is
        the original source file ; for other generated source
        (ocamllex/ocamlyacc), this is the OCaml (.ml/.mli) file. *)
}

type t = { stamp: int; name: string; mutable flags: int ; ctx : ctx }

let global_flag = 1
let predef_exn_flag = 2

(** Context for idents defined in {!Predef} *)
let predef_ctx = {
  modname = "*predef*";
  kind = `implementation;
  source_digest = ""
}

(** Context for persistent idents *)
let persistent_ctx = {
  modname = "*persistent*";
  kind = `implementation;
  source_digest = ""
}

(** Empty context *)
let no_ctx = {
  modname = "*none*";
  kind = `implementation;
  source_digest = ""
}

let current_ctx = ref no_ctx

let set_context modname kind sourcefile =
  current_ctx := {
    modname;
    kind;
    source_digest = Digest.file sourcefile
  }

let set_pack_context modname =
  current_ctx := {
    modname;
    kind = `pack;
    source_digest = ""
  }

(* A stamp of 0 denotes a persistent identifier *)

let currentstamp = ref 0

let create_with_ctx ctx s =
  incr currentstamp;
  { name = s; stamp = !currentstamp; flags = 0; ctx }

let create s =
  incr currentstamp;
  create_with_ctx !current_ctx s

let create_predef_exn s =
  incr currentstamp;
  { name = s; stamp = !currentstamp; flags = predef_exn_flag; ctx = predef_ctx }

let create_persistent s =
  { name = s; stamp = 0; flags = global_flag; ctx = persistent_ctx }

let renaming_disabled = ref false

let disabling_renaming f x =
  let disabled = !renaming_disabled in
  renaming_disabled := true;
  let restore () = renaming_disabled := disabled in
  try let r = f x in restore () ; r
  with e -> restore () ; raise e

let rename i =
  if !renaming_disabled then
    i
  else (
    incr currentstamp;
    { i with stamp = !currentstamp; ctx = !current_ctx }
  )

let name i = i.name

let stamp i = i.stamp

let unique_name i = i.name ^ "_" ^ string_of_int i.stamp

let unique_toplevel_name i = i.name ^ "/" ^ string_of_int i.stamp

let name_with_ctx i =
  let kind =
    match
      i.ctx.kind with
        | `interface -> "(MLI)"
        | `implementation -> "(ML)"
        | `pack -> "(PACK)"
  in
  i.ctx.modname ^ kind ^ "/" ^ unique_toplevel_name i

let persistent i = (i.stamp = 0)

let equal i1 i2 = i1.name = i2.name

let same i1 i2 = i1 = i2
  (* Possibly more efficient version (with a real compiler, at least):
       if i1.stamp <> 0
       then i1.stamp = i2.stamp
       else i2.stamp = 0 && i1.name = i2.name *)

let binding_time i = i.stamp

let current_time() = !currentstamp
let set_current_time t = currentstamp := max !currentstamp t

let reinit_level = ref (-1)

let reinit () =
  if !reinit_level < 0
  then reinit_level := !currentstamp
  else currentstamp := !reinit_level

let hide i =
  { i with stamp = -1; ctx = !current_ctx }

let make_global i =
  i.flags <- i.flags lor global_flag

let global i =
  (i.flags land global_flag) <> 0

let is_predef_exn i =
  (i.flags land predef_exn_flag) <> 0

let print ppf i =
  match i.stamp with
  | 0 -> fprintf ppf "%s!" i.name
  | -1 -> fprintf ppf "%s#" i.name
  | n -> fprintf ppf "%s/%i%s" i.name n (if global i then "g" else "")

type 'a tbl =
    Empty
  | Node of 'a tbl * 'a data * 'a tbl * int

and 'a data =
  { ident: t;
    data: 'a;
    previous: 'a data option }

let empty = Empty

(* Inline expansion of height for better speed
 * let height = function
 *     Empty -> 0
 *   | Node(_,_,_,h) -> h
 *)

let mknode l d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h
  and hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  Node(l, d, r, (if hl >= hr then hl + 1 else hr + 1))

let balance l d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h
  and hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  if hl > hr + 1 then
    match l with
    | Node (ll, ld, lr, _)
      when (match ll with Empty -> 0 | Node(_,_,_,h) -> h) >=
           (match lr with Empty -> 0 | Node(_,_,_,h) -> h) ->
        mknode ll ld (mknode lr d r)
    | Node (ll, ld, Node(lrl, lrd, lrr, _), _) ->
        mknode (mknode ll ld lrl) lrd (mknode lrr d r)
    | _ -> assert false
  else if hr > hl + 1 then
    match r with
    | Node (rl, rd, rr, _)
      when (match rr with Empty -> 0 | Node(_,_,_,h) -> h) >=
           (match rl with Empty -> 0 | Node(_,_,_,h) -> h) ->
        mknode (mknode l d rl) rd rr
    | Node (Node (rll, rld, rlr, _), rd, rr, _) ->
        mknode (mknode l d rll) rld (mknode rlr rd rr)
    | _ -> assert false
  else
    mknode l d r

let rec add id data = function
    Empty ->
      Node(Empty, {ident = id; data = data; previous = None}, Empty, 1)
  | Node(l, k, r, h) ->
      let c = compare id.name k.ident.name in
      if c = 0 then
        Node(l, {ident = id; data = data; previous = Some k}, r, h)
      else if c < 0 then
        balance (add id data l) k r
      else
        balance l k (add id data r)

let rec find_stamp s = function
    None ->
      raise Not_found
  | Some k ->
      if k.ident.stamp = s then k.data else find_stamp s k.previous

let rec find_same id = function
    Empty ->
      raise Not_found
  | Node(l, k, r, _) ->
      let c = compare id.name k.ident.name in
      if c = 0 then
        if id.stamp = k.ident.stamp
        then k.data
        else find_stamp id.stamp k.previous
      else
        find_same id (if c < 0 then l else r)

let rec find_name name = function
    Empty ->
      raise Not_found
  | Node(l, k, r, _) ->
      let c = compare name k.ident.name in
      if c = 0 then
        k.data
      else
        find_name name (if c < 0 then l else r)

let rec keys_aux stack accu = function
    Empty ->
      begin match stack with
        [] -> accu
      | a :: l -> keys_aux l accu a
      end
  | Node(l, k, r, _) ->
      keys_aux (l :: stack) (k.ident :: accu) r

let keys tbl = keys_aux [] [] tbl
