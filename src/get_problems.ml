open Yojson.Basic
open Yojson.Basic.Util

let extract_problems (json: Yojson.Basic.json) =
  let problems = [json]
                 |> filter_member "problems"
                 |> flatten
  in
  let ids = problems |> filter_member "problem_id" |> filter_int in
  let problem_hashes = problems |> filter_member "problem_spec_hash" |> filter_string in
  List.combine ids problem_hashes

let extract_problems_from_snapshot_blob (f: string) =
  let l = extract_problems (from_file f) in
  let oc = open_out "problem_hashes" in
  List.iter (fun (id, hash) -> Printf.fprintf oc "%d, %s\n" id hash) l;
  close_out oc
