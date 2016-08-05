open Spec
open Num

let parse_vertex ic =
  let line = input_line ic in
  try Scanf.sscanf line "%s@,%s" (fun s1 s2 -> (num_of_string s1, num_of_string s2)) with
    | _ -> failwith ("couldn't parse vertex " ^ line)

let parse_silhouette ic =
  let num_polygon = input_line ic |> int_of_string in
  let polygons = Array.make num_polygon { num_vertices = 0; vertices = [| |] } in
  let i = ref 0 in
  while (!i < num_polygon) do
    polygons.(!i).num_vertices <- (input_line ic |> int_of_string);
    polygons.(!i).vertices <- Array.make polygons.(!i).num_vertices (Int 0, Int 0);
    let j = ref 0 in
    while (!j < polygons.(!i).num_vertices) do
      polygons.(!i).vertices.(!j) <- parse_vertex ic;
      incr j
    done;
    incr i
  done;
  { num_polygon = num_polygon; polygons = polygons }

let parse_line ic =
  let line = input_line ic in
  try Scanf.sscanf line "%s@,%s %s@,%s" (fun x1 y1 x2 y2 -> (num_of_string x1, num_of_string y1), (num_of_string x2, num_of_string y2)) with
  | _ -> failwith ("couldn't parse vertex " ^ line)

let parse_skeleton ic =
  let num_lines = input_line ic |> int_of_string in
  let lines = Array.make num_lines ((Int 0, Int 0), (Int 0, Int 0)) in
  let i = ref 0 in
  while (!i < num_lines) do
    lines.(!i) <- parse_line ic;
    incr i
  done;
  { num_lines = num_lines; lines = lines }

let parse_problem file =
  let ic = open_in file in
  let silhouette = parse_silhouette ic in
  let skeleton = parse_skeleton ic in
  close_in ic;
  (silhouette, skeleton)
