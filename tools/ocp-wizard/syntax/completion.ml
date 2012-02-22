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
open OcamlTokenize
open Util
include Debug.Tag(struct let tag = "completion" end)
open OCamlTokenBuffer

type non_ident =
  | Method
  | Variant (* polymorphic variant *)
  | TypeVariable
  | Label (* argument label *)

type env = {
  idents :
    ([ `ident of Env_untyped.path_sort | `nonIdent of non_ident ] *
     int StringMap.t
    ) list;
  opens : string list
}

type local_envs = {
  mutable local_env : env array;
  mutable local_env_bound : int
}

let non_ident_kinds = [Method ; Variant ; TypeVariable ; Label]

let initial_env = {
  idents =
    List.map
      (function kind -> kind, StringMap.empty)
      (List.map (function k -> `ident k) Env_untyped.all_kinds @
         List.map (function k -> `nonIdent k) non_ident_kinds);
  opens = []
}

let initial_env () = {
  local_env = [| initial_env |];
  local_env_bound = 0
}

let invalidate_env_after envs pos =
  envs.local_env_bound <- min envs.local_env_bound pos

(*
type completion = {
  buffer : string; (** buffer in which a word is begin completed *)
(*
  word_start : int;
  word_end : int; (** end of word, i.e., last cursor position *)
  completions : string list
*)
}

let ongoing = ref None
*)

let non_ident2string = function
  | Method -> "method"
  | Variant -> "polymorphic variant"
  | TypeVariable -> "type variable"
  | Label -> "argument label"

(* Represents where this ident will fit in the syntax tree.  *)
type ident_completion_context =
  | Member of Longident.t * Env_untyped.path_sort list
  | Short of Env_untyped.path_sort list
  | NonIdent of non_ident
  | Blank

let kind2string = function
  | Member (lid, kinds) ->
    Printf.sprintf "%s.[%s]"
    (lid2string lid) (String.concat "," (List.map Env_untyped.kind2string kinds))
  | Short kinds -> String.concat "," (List.map Env_untyped.kind2string kinds)
  | NonIdent i -> non_ident2string i
  | Blank -> "blank"

let can_complete t =
  String.for_all
    (function
      | 'a' .. 'z' | 'A' .. 'Z' | '_' | '~' | '?'  -> true
      | _ -> false)
    t.string

let kinds ~qualified = function
  | _ -> Env_untyped.all_kinds

let rec member_or_field = function
  | Parser.UIDENT m :: Parser.DOT :: before ->
    (match member_or_field before with
      | Member (lid, kinds) -> Member (Longident.Ldot (lid, m), kinds)
      | Short kinds -> Member (Longident.Lident m, kinds)
      | _ -> assert false)
  | Parser.UIDENT m :: before ->
    Member (Longident.Lident m, kinds ~qualified:true before)
  | before -> Short [Env_untyped.Label]

let ident_completion_context preceeding =
  let open Parser in
  (match preceeding with
    | [] -> debugln "no preceeding token"
    | t :: _ ->
      debugln "preceeding token: %s"
        (match t with
          | LIDENT id | UIDENT id -> id
          | DOT -> "."
          | _ -> "other"));
  match preceeding with
    | BACKQUOTE :: _ -> NonIdent Variant
    | QUOTE :: _ -> NonIdent TypeVariable
    | SHARP :: _
    | METHOD :: _
    | (VIRTUAL | PRIVATE) :: METHOD :: _
    | (VIRTUAL | PRIVATE) :: (VIRTUAL | PRIVATE) :: METHOD :: _
      -> NonIdent Method
    | DOT :: before -> member_or_field before
    | _ -> Short (kinds ~qualified:false preceeding)

let any_ident_completion_context before = function
  | Some (_, (Parser.LABEL l | Parser.OPTLABEL l)) -> l, NonIdent Label
  | Some (prefix, t) -> prefix, ident_completion_context before
  | None -> (* Don't complete an ident from scratch *)
    "",
    match ident_completion_context before with
      | Short _ -> Blank
      | ctx -> ctx

(* offset is the position relative to the beginning of the token that
   we are completing *)
let any_completion_context buffer pos offset =
  let before = rev_tokens_before ~max:10 buffer pos in
  let current = buffer.t_buf.(pos2pointer buffer pos) in
  match current.token with
    | _ when offset = 0 -> any_ident_completion_context before None
    | Token t ->
      let prefix = String.before current.string offset in
      any_ident_completion_context before (Some (prefix, t))
    | _ -> "", Blank

let classify_ident ~before = function
  | Parser.LIDENT id | Parser.UIDENT id ->
    Some
      (id,
       (match before with
         | Parser.DOT :: _ -> `member
         | Parser.SHARP :: _ -> `method_name
         | Parser.QUOTE :: _ -> `typevar
         | Parser.BACKQUOTE :: _ -> `variant
         | _ -> `ident))
  | _ -> None

let insert_token env bound before token =
  match token with
    | Parser.LIDENT i | Parser.UIDENT i | Parser.LABEL i | Parser.OPTLABEL i ->
      let id, kind = any_ident_completion_context before (Some (i, token)) in
      if (match kind with Member _ -> false | _ -> true) then
        let kinds = match kind with
          | Short kinds -> List.map (function k -> `ident k) kinds
          | NonIdent k -> [`nonIdent k]
          | Blank | Member _ -> assert false
        in
        let idents =
          List.map
            (function kind, map as entry ->
              if List.mem kind kinds then
                kind, StringMap.add id bound map
              else 
                entry)
            env.idents
        and opens =
          (match before with
            | (Parser.OPEN | Parser.INCLUDE) :: _ ->
            (* This is wrong with open M.N *)
              id :: env.opens
            | _ -> env.opens)
        in {idents ; opens}
      else env
    | _ -> env

let local_env contents local_env pos =
  if Array.length local_env.local_env <= pos then (
    let local_envs =
      Array.create
        (max 1 (Array.length contents.t_buf)) local_env.local_env.(0) in
    Array.blit local_env.local_env 0 local_envs 0 (local_env.local_env_bound + 1);
    local_env.local_env <- local_envs
  );
  if local_env.local_env_bound < pos then (
    let tokens = sub contents local_env.local_env_bound pos
    and before = rev_tokens_before contents local_env.local_env_bound in
    ignore
      (Array.fold_left
         (fun before t ->
           let env = local_env.local_env.(local_env.local_env_bound)
           and bound = local_env.local_env_bound in
           local_env.local_env_bound <- bound + 1;
           let env, before =
             match t.token with
               | Token t -> insert_token env bound before t, t :: before
               | _ -> env, before
           in
           local_env.local_env.(bound + 1) <- env;
           before)
         before
         tokens)
  );
  local_env.local_env.(pos)

let collect_in_buffer ~prefix contents local_envs pos =
  let {idents ; opens} = local_env contents local_envs pos in
  List.map
    (function kind, map ->
      let l =
      StringMap.fold
        (fun id pos l ->
          if String.starts_with ~prefix id then
            (id, pos) :: l
          else
            l)
        map
        []
      in
      let l = List.sort (fun (_, p) (_, p') -> p - p') l in
      kind, List.map fst l)
    idents,
  opens

let collect_in_env program kinds prefix lid env =
  let idents kind =
    Env_untyped.fold
      (fun name path desc l ->
        if String.starts_with name ~prefix then
          let doc = lazy (
            let desc = Env_untyped.decl2string path desc
            and comments =
              try
                let id = Resolve.resolve_path kind path env in
                OcpWizard.id2comments program id
              with _ -> None
            in
            match comments with
              | Some comments -> desc ^ "\n\n" ^ comments
              | None -> desc
          ) in
          (kind, name, doc) :: l
        else
          l)
      kind lid env
  in
  List.fold_right
    idents kinds []

let collect_persistent load_path =
  let set = Hashtbl.create 100
  and list = ref [] in
  List.iter
    (function p ->
      if Sys.file_exists p && Sys.is_directory p then
        Array.iter
          (function f ->
            if List.exists
              (Filename.check_suffix f)
              [".ml" ; ".mli" ; ".mll" ; ".mly" ; ".cmi"]
            then
              let f = String.capitalize (Filename.chop_extension f) in
              if not (Hashtbl.mem set f) then (
                Hashtbl.add set f ();
                list := f :: !list
              ))
          (Sys.readdir p))
    load_path;
  !list

let collect_persistent =
  ProgramCache.make_program_source_file_cache
    ~value_up_to_date:(fun _ _ -> ())
    (fun program source_id ->
       let load_path =
         Program.source_load_path ~prefix:`absolute program
           (Program.find_source program source_id) in
       collect_persistent load_path)

let collect_in_persistent prefix program source_id =
  let persistent = collect_persistent program source_id in
  List.fold_left
    (fun l name ->
      if String.starts_with name ~prefix then
        name :: l
      else
        l)
    []
    persistent

(** @raise Not_found *)
let completions program source_id contents local_envs pos =
  goto contents pos;
  debugln "buffer state: %t" (t_snapshot contents);
  let token_to_complete, offset =
    if contents.offset = 0 && contents.t_pre > 0 &&
      can_complete contents.t_buf.(contents.t_pre - 1)
    then
      let t = contents.t_pre - 1 in
      t,
      contents.t_buf.(t).length
    else
      contents.t_pre, contents.offset
  in
  debugln "complete token %S at its %dth char"
    contents.t_buf.(pos2pointer contents token_to_complete).string offset;
  let prefix, kind =
    any_completion_context contents token_to_complete offset in
(*
  let prefix = String.before prefix offset in
*)
  debugln "complete ident prefix %S of kind %s" prefix (kind2string kind);
  let idents, opens =
    collect_in_buffer ~prefix contents local_envs (contents.t_pre - 1) in
  (* may not be in the program ! *)
  debugln "looking for source";
  let env =
    try
      debugln "trying source env";
      ProgramCache.source_env program (Program.find_source program source_id)
    with _ ->
(*
      try
        debugln "trying program env";
        ProgramCache.program_env program with _ ->
*)
        debugln "using initial environment";
        Env.initial in
  debugln "found source";
  let env =
    List.fold_left
      (fun env id ->
        try
          let p, sg =
            Env.lookup_module (Longident.Lident id) env in
          (*
            let sg = module_lid2sig  -> Env.t -> Longident.t -> ident_context * Types.signature
          *)
          match sg with
            | Types.Mty_signature sg ->
              Env.open_signature p sg env
            | _ -> env
        with _ -> env)
      env
      opens
  in
  (*
    let load_path = Program.source_load_path ~prefix:`absolute program source in
  *)
  let lid, locals =
    match kind with
      | Member (lid, _) ->
        debugln "completing a member of %s" (lid2string lid);
        Some lid, []
      | Blank -> None, []
      | Short kinds ->
        None,
        List.map (function s -> Env_untyped.Value, s, lazy "")
          (List.concat
             (List.map (function k -> (List.assoc (`ident k) idents)) kinds))
      | NonIdent kind ->
        None,
        List.map (function s -> Env_untyped.Value, s, lazy "")
          (List.assoc (`nonIdent kind) idents)
  and kinds = match kind with
      | Member (_, kinds) | Short kinds -> kinds
      | NonIdent _
      | Blank -> []
  in
  let persistent =
    if lid = None && List.mem Env_untyped.Module kinds then
      List.map (function s -> Env_untyped.Module, s, lazy "")
        (collect_in_persistent prefix program source_id)
    else
      []
  in
  let globals = collect_in_env program kinds prefix lid env in
  let candidates = locals @ globals @ persistent in
  prefix, candidates
