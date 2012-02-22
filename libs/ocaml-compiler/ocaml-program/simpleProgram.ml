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
open Program
open Cmt_format
include Debug.Tag(struct let tag = "simpleProgram" end)

let project_file_names = [".typerex" ; ".ocp-wizard" ; ".ocamlwizard"]

(* Try to locate a project file in the directory containing d.  d
   must be an absolute directory name ! *)
let rec find_file_backwards d filenames =
  try
    List.find_map
      (function pf ->
	let pf = Filename.concat d pf in
	if Sys.file_exists pf then
	  Some (d, pf, [], [])
	else
	  None)
      filenames
  with Not_found when d <> "/" ->
    let d', pf, suffix, rel = find_file_backwards (Filename.dirname d) filenames in
    d', pf, suffix @ [Filename.basename d],
    Filename.parent_dir_name :: rel

let find_file_backwards d filenames =
  if List.exists (fun f -> Filename.basename f <> f) filenames
  then
    invalid_arg "find_file_backwards: filenames must not be paths"
  else
    find_file_backwards d filenames

let find_project_file d =
  if Filename.is_relative d then
    invalid_arg "find_project_file: dirname must be absolute"
  else
    find_file_backwards d project_file_names

let words s = String.split_chars s [' '; '\t']

type project_file = {
  source_dirs : string list;
  include_dirs : string list;
  excluded_names : string list;
  nostdlib : bool;
  cmt : string option
}

exception ProjectFileError of (string * string)

let parse_line ~permissive ~dirs ~incs ~exclude ~cmt_dir ~nostdlib file l =
  let l = String.strip l in
  if l = "" then
    ()
  else if l.[0] = 'I' then
    let l = String.sub l 1 (String.length l - 1) in
    incs := !incs @ words l
  else if l.[0] = '-' then
    let l = String.sub l 1 (String.length l - 1) in
    exclude := !exclude @ words l
  else if String.starts_with l ~prefix:"CMT" then
    if not permissive && !cmt_dir <> None then
      raise (ProjectFileError (file, "duplicate 'CMT' directive"))
    else
      let l = String.sub l 3 (String.length l - 3) in
      if not permissive && l = "" then
        raise (ProjectFileError (file, "invalid 'CMT' directive"))
      else
        cmt_dir := Some (String.strip l)
  else if l = "NOSTDLIB" then
    nostdlib := true
  else if l.[0] = '#' then
    ()
  else
    dirs := !dirs @ words l

(* Return the dirs, include dirs, and exclude files of a project. *)
let parse_project_file ~permissive pf =
  let dirs = ref []
  and incs = ref []
  and exclude = ref []
  and cmt_dir = ref None
  and nostdlib = ref false in
  List.iter (parse_line ~permissive ~dirs ~incs ~exclude ~cmt_dir ~nostdlib pf)
    (File.lines_of_file pf);
  {
    source_dirs = List.rev !dirs;
    include_dirs = List.rev !incs;
    excluded_names = List.rev !exclude;
    nostdlib = !nostdlib;
    cmt = !cmt_dir
  }

let make_absolute file =
  if Filename.is_relative file then
    if file = "." then Sys.getcwd () else
      Filename.concat (Sys.getcwd ()) file
  else
    file

let concat_if_relative d name =
  if name = Filename.current_dir_name then
    d
  else if d = Filename.current_dir_name then
    name
  else if Filename.is_relative name then
    Filename.concat d name
  else
    name

let prefix_by l name =
  if Filename.is_relative name (*&& (name = "" || name.[0] <> '~')*) then
    List.fold_right Filename.concat l name
  else
    name

let classify_source f =
  let open Filename in
    if check_suffix f ".ml" then `ml
    else if check_suffix f ".mli" then `mli
    else if check_suffix f ".mll" || check_suffix f ".mly" then
      `ml
    else
      `other

let is_generated f =
  let kind = classify_source f in
  let prefix = Filename.chop_extension f in
  match kind with
    | `mli -> Sys.file_exists (prefix ^ ".mly")
    | `ml ->
      Sys.file_exists (prefix ^ ".mly") ||
      Sys.file_exists (prefix ^ ".mll")
    | `other -> false

let directory_of file =
  if Sys.file_exists file && Sys.is_directory file then
    file
  else
    Filename.dirname file

type project_info = {
  absolute_root : string;
    (* absolute path to the root of the project *)
  project_file : string option;
  here2root : string list;
    (* relative path from here to the root, e.g. [".."; ".."] *)
  root2here : string list;
    (* relative path from root to here *)
  dirs : string list;
    (* project directories (relative to the root) *)
  incs : string list;
    (* include directories (relative to the root) *)
  exclude : string list;
    (* excluded source files or units *)
  cmt_dir : string option
}

let project_dirs ~permissive ~ignore_project_file ~default_cwd ~stdlib file =
  let absolute_dir = make_absolute (directory_of file) in
  try
    if ignore_project_file then raise Not_found;
    let absolute_root, pf, root2here, here2root =
      find_project_file absolute_dir in
    let project_file = parse_project_file ~permissive pf in
    { absolute_root;
      here2root;
      project_file = Some pf;
      root2here;
      dirs = project_file.source_dirs;
      incs =
        List.map (Misc.expand_directory stdlib) project_file.include_dirs
      @ (if project_file.nostdlib then [] else [stdlib]);
      exclude = project_file.excluded_names;
      cmt_dir = project_file.cmt }
  with Not_found ->
    { absolute_root = absolute_dir;
      project_file = None;
      here2root = [];
      root2here = [];
      dirs = if default_cwd then ["."] else [];
      incs = [stdlib];
      exclude = [];
      cmt_dir = None }

let check_project p =
  let fail fmt =
    Printf.ksprintf
    (function e ->
      let file = match p.project_file with
        | Some f -> f
        | None -> Printf.sprintf "'implicit project at %s'" p.absolute_root
      in
      raise (ProjectFileError (file, e)))
      fmt
  in
  let check_duplicate fail dirs =
  ignore
    (List.fold_right
       (fun d dirs ->
         if List.mem d dirs then
           fail d
         else
           d :: dirs)
       (List.map (concat_if_relative p.absolute_root) dirs)
       [])
  in
  check_duplicate (fail "duplicate include directory %s") p.incs;
  check_duplicate (fail "duplicate source directory %s") p.dirs;
  check_duplicate (fail "%s is both a source and include directory")
    (p.dirs @ p.incs);
  let check_absent =
    List.iter
      (function d ->
        let d = concat_if_relative p.absolute_root d in
        if not (Sys.file_exists d) then
          fail "directory %s does not exist" d
       else if not (Sys.is_directory d) then
          fail "%s is not a directory" d)
  in
  check_absent p.incs;
  check_absent p.dirs;
  check_absent (List.map Filename.dirname p.exclude);
  match p.cmt_dir with
    | Some d -> check_absent [d]
    | None -> ()

let cmt2packed_units ~root file =
  let cmt_info, _ = ProgramCache.read_cmt (concat_if_relative root file) in
  match cmt_info.Cmt_format.cmt_annots with
    | Cmt_format.Packed (sg, units) ->
      let units =
        List.map
          (function u ->
            let basename = Misc.chop_extension_if_any (Filename.basename u) in
            concat_if_relative (Filename.dirname file) basename)
          units
      in
      Some units
    | _ -> None

(* Given a project description, return the set of compilation units
   contained in those directories. *)
let project_units ~inc ~root dirs =
  let open Filename in
  let units =
    List.fold_left
      (fun files d ->
        let d_abs = concat_if_relative root d
        and d = if d = "." then "" else d in
        if Sys.file_exists d_abs && Sys.is_directory d_abs then
          let fs = Array.to_list (Sys.readdir d_abs) in
          let mls =
	    List.filter_map
	      (function f ->
	        if not inc &&
	          (List.exists (check_suffix f)
                     [".ml" ; ".mli" ; ".mly" ; ".mll" ; ".mlpack"] ||
                     check_suffix f ".cmt" &&
                     cmt2packed_units ~root (concat_if_relative d f) <> None)
	          || inc &&
	          List.exists (check_suffix f) [".cmt" ; ".cmti" ; ".cmi"]
	        then
	          Some (concat d (chop_extension f))
	        else
	          None)
	      fs
          in
          files @ mls
        else
          files)
      []
      dirs
  in List.setify units

let rec collect_cmts abs rel t =
  Array.iter
    (function f ->
      let abs = Filename.concat abs f
      and rel = Filename.concat rel f in
      if Sys.file_exists abs && Sys.is_directory abs then
        collect_cmts abs rel t
      else if Filename.check_suffix f ".cmt" || Filename.check_suffix f ".cmti" then
        Hashtbl.add t f rel)
    (Sys.readdir abs)

let collect_cmts ~root dir =
  let t = Hashtbl.create 100 in
  let abs = concat_if_relative root dir in
  if Sys.file_exists abs && Sys.is_directory abs then
    collect_cmts abs dir t;
  t

(*
let rec find_in_rec ?(root="") acc files f =
  if List.mem f files then
    Filename.concat acc f
  else
    List.find_map
      (function d ->
        let acc = Filename.concat acc d in
        let abs = Filename.concat root acc in
        if Sys.file_exists abs && Sys.is_directory abs then
          try
            Some (find_in_rec ~root acc (Array.to_list (Sys.readdir abs)) f)
          with
              Not_found -> None
        else
          None)
      files

let find_in_rec ?(root="") ~dir f = find_in_rec ~root "" [dir] f
*)

let exact_matches root cmts sourcefile =
  List.find_all
    (function cmt ->
      try
        (fst (ProgramCache.read_cmt (concat_if_relative root cmt)))
          .cmt_source_digest =
        ProgramCache.cached_digest (concat_if_relative root sourcefile)
      with _ -> false)
    cmts

let source_kind2cmt_suffix = function
  | `ml -> ".cmt"
  | `mli -> ".cmti"

let mlpack2units ~root file =
  let paths =
    List.concat
      (List.map
         (function l ->
           let l = String.strip l in
           words l)
         (File.lines_of_file (concat_if_relative root file)))
  in
  List.map
    (function p ->
      let dir = concat_if_relative (Filename.dirname file) (Filename.dirname p)
      and modname = Filename.basename p in
      try
        List.find_map
          (function suffix ->
            let prefix = concat_if_relative dir modname in
            if Sys.file_exists (concat_if_relative root prefix ^ suffix) then
              Some prefix
            else
            let prefix = concat_if_relative dir (String.uncapitalize modname) in
            if Sys.file_exists (concat_if_relative root prefix ^ suffix) then
              Some prefix
            else
              None)
          [".ml"; ".mli"; ".mll"; ".mly"; ".mlpack"; ".cmti" ; ".cmt"]
      with Not_found ->
        (* There should be a warning *)
        concat_if_relative dir (String.uncapitalize modname))
    paths  

let packed_units ~root file =
  if Filename.check_suffix file ".mlpack" then
    Some (mlpack2units ~root file)
  else if Filename.check_suffix file ".cmt" then
    cmt2packed_units ~root file
  else
    invalid_arg "packed_units"

let read_one_unit ~root ?assume ~load_path ~exclude prefix ~inc =
  (match assume with
    | Some f -> debugln "assuming %s" f
    | None -> ());
  let rec first_existing = function
    | [] -> None
    | t :: q ->
      let f = prefix ^ t in
      let abs_f = concat_if_relative root f in
      let excluded = List.mem f exclude || List.mem prefix exclude in
      if assume = Some f ||
        not excluded && Sys.file_exists abs_f && not (Sys.is_directory abs_f)
      then
	Some (f, t)
      else
	first_existing q
  in
  let file kind sort =
    let source = first_existing
      (match kind with
	| `ml -> [".mll" ; ".mly" ; ".ml" ; "" (* stub for *caml-toplevel* *)]
	| `mli -> [".mly" ; ".mli"])
    in
    (* We should make a concrete unit even when no sources are
       available (to allow getting documentation from embedded
       sources, for example). *)
    match source with
      | Some (source, extension) -> Some (
        debugln "found %s" source;
	let preprocessor =
	  match extension with
	    | ".mll" -> Some `ocamllex
	    | ".mly" -> Some `ocamlyacc
	    | _ -> None (* stub *)
	in
	{
	  source = source;
	  preprocessor;
	  nopervasives = false; (* stub *)
	  load_path;
	  typedtree = prefix ^ source_kind2cmt_suffix kind;
	}
      )
      | None -> None
  in
  let interface = file `mli (function `signature s -> s | _ -> assert false) in
  let implementation =
    match file `ml (function `structure s -> s | _ -> assert false) with
      | Some impl -> `impl impl
      | None ->
        match first_existing [".mlpack" ; ".cmt"] with
          | Some (file, _) ->
            (match packed_units root file with
              | Some p_units ->
                `pack {
                  p_interface = interface;
                  p_load_path = load_path;
                  p_units;
                  p_typedtree = prefix ^ ".cmt"
                }
              | None -> `none)
          | None -> `none
  in
  if implementation = `none && interface = None || inc then
    match first_existing [".cmti" ; ".cmt" ; ".cmi"] with
      | Some (a_signature, _) ->
        Some (prefix, Abstract { a_load_path = load_path ; a_signature })
      | None -> None
  else
    let unit =
      match implementation with
        | `impl impl -> Concrete { implementation = Some impl ; interface }
        | `none -> Concrete { implementation = None ; interface }
        | `pack p -> Pack p
    in
    Some (prefix, unit)

let assign_cmt first root cmts assigned_cmts prefix kind typedtree =
  let sourcefile_suffix =
    match kind with
      | `ml -> ".ml"
      | `mli -> ".mli"
  in
  let together = prefix ^ source_kind2cmt_suffix kind in
  let cmts = Hashtbl.find_all cmts (Filename.basename together) in
  match cmts, lazy (exact_matches root cmts (prefix ^ sourcefile_suffix)) with
    | _ when typedtree <> together -> typedtree
    | [typedtree], _ -> typedtree
    | _, lazy [typedtree] ->
      assert first;
      Hashtbl.add assigned_cmts typedtree ();
      typedtree
    | _ -> typedtree

let assign_cmts_to_unit first root cmts assigned_cmts prefix =
  let assign kind = function
    | Some file ->
      Some {file with typedtree =
          assign_cmt first root cmts assigned_cmts prefix kind file.typedtree}
    | None -> None
  in
  function
    | Concrete { implementation ; interface } ->
      Concrete {
        implementation = assign `ml implementation;
        interface = assign `mli interface
      }
    | Pack unit ->
      Pack { unit with
        p_interface = assign `mli unit.p_interface;
        p_typedtree =
          assign_cmt first root cmts assigned_cmts prefix `ml unit.p_typedtree
      }
    | unit -> unit

let assign_cmts root cmts units =
  let assigned_cmts = Hashtbl.create 100 in
  (* assignement for sources with only one match or only one exact match *)
  let units =
    List.map
      (function prefix, unit ->
        prefix, assign_cmts_to_unit true root cmts assigned_cmts prefix unit)
      units
  in
  (* Second assignement for sources with saeveral matches, none exact,
     but all but one already assigned to another source. *)
  let unassigned_cmts = Hashtbl.create 100 in
  Hashtbl.iter
    (fun basename cmt ->
      if not (Hashtbl.mem assigned_cmts cmt) then
        Hashtbl.add unassigned_cmts basename cmt
      else
        debugln "already assigned %s" cmt)
    cmts;
  List.map
    (function prefix, unit ->
      prefix,
      assign_cmts_to_unit false
        root unassigned_cmts (Hashtbl.create 100) prefix unit)
    units

exception FileNotInProgram of string * string list
exception FileExcluded of string * string

let program
    ?(ignore_absent=false) ?(ignore_extension=false) ?(ignore_project_file=false)
    ?(default_cwd=true) ?(cwd=Sys.getcwd ()) file =

  (* We may lift this restriction later. *)
  if Filename.basename file <> file then
    invalid_arg "file must be in the current directory";

  (* Quick hack: *)
  let old_cwd = Sys.getcwd () in
  Sys.chdir cwd;

  if Sys.file_exists file && Sys.is_directory file then
    invalid_arg "file is a directory";

  let source_kind =
    match classify_source file with
      | `other ->
        if ignore_extension then `ml
        else invalid_arg  (file ^ " is not an OCaml source file")
      | `ml | `mli as k -> k
  in

  let project =
    project_dirs
      ~permissive:ignore_absent ~ignore_project_file ~default_cwd
      ~stdlib:Config.standard_library
      file
  in
  check_project project;
  let units_proj =
    project_units ~inc:false ~root:project.absolute_root project.dirs
  and units_libs =
    project_units ~inc:true ~root:project.absolute_root project.incs
  and load_path = project.dirs @ project.incs
  and exclude = project.exclude in
  let file = prefix_by project.root2here file in
  let absolute_file = Filename.concat project.absolute_root file in
  let file =
    if List.mem (Filename.dirname absolute_file) (project.dirs @ project.incs) then
      absolute_file
    else
      file
  in
  let prefix =
    try Filename.chop_extension file
    with Invalid_argument _ when ignore_extension -> file in
  let source = prefix, source_kind in
  let missing = not (List.mem prefix units_proj) in
  let excluded_by =
    try
      Some (List.find (function x -> x = file || x = prefix) exclude)
    with
        Not_found -> None
  in
  debugln "source = %s, prefix = %s, excluded = %s"
    file prefix (String.concat "," exclude);
  if not ignore_absent then (
    match project.project_file with
      | Some pf ->
        (match excluded_by with
          | Some x -> raise (FileExcluded (pf, x))
          | None -> ());
        if missing && project.dirs <> [] then
          raise (FileNotInProgram (pf, project.root2here))
      | None -> ()
  );
  let units_proj =
    if missing then (
      (*
        assert (units = [] || ignore_absent);
      *)
      prefix :: units_proj
    ) else
      units_proj
  in
  let files inc =
    List.filter_map
      (function pref ->
        let assume =
          if prefix = pref && ignore_absent then
            Some file
          else
            None
        in
        read_one_unit
          ?assume ~root:project.absolute_root ~load_path ~exclude pref ~inc)
  in
  let files = files false units_proj @ files true units_libs in
  let files =
    match project.cmt_dir with
      | Some dir ->
        assign_cmts project.absolute_root
          (collect_cmts ~root:project.absolute_root dir)
          files
      | None -> files
  in
  let project = {
    root = project.absolute_root;
    units = Hashtbl.of_list files
  } in
  Sys.chdir old_cwd;
  source, project






