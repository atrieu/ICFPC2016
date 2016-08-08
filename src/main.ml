open Spec
open Parse_problem
open Print_solution
open Num

let empty_solution: solution = { source = []; facets = []; destination = [] }

let lower_left (p1: vertex) (p2: vertex): vertex =
  (min_num (fst p1) (fst p2), min_num (snd p1) (snd p2))

let lower_left_of_polygon (p: polygon): vertex =
  let i = ref 1 in
  let ans = ref (p.vertices.(0)) in
  while (!i < p.num_vertices) do
    ans := lower_left !ans p.vertices.(!i);
    incr i;
  done;
  !ans

let lower_left_of_silhouette (s: silhouette): vertex =
  let i = ref 1 in
  let ans = ref (s.polygons.(0) |> lower_left_of_polygon) in
  while (!i < s.num_polygon) do
    ans := lower_left !ans (s.polygons.(!i) |> lower_left_of_polygon);
    incr i;
  done;
  !ans

let make_square (ll: vertex): vertex list =
  [ll; (fst ll +/ Int 1, snd ll); (fst ll +/ Int 1, snd ll +/ Int 1); (fst ll, snd ll +/ Int 1)]

let find_center_of_polygon (p: polygon): vertex =
  let sum = Array.fold_left (fun acc x -> (fst acc +/ fst x, snd acc +/ snd x)) (Int 0, Int 0) p.vertices
  in (fst sum // (Int p.num_vertices), snd sum // (Int p.num_vertices))

let find_center_of_silhouette (s: silhouette): vertex =
  let sum = Array.fold_left (fun acc x -> let y = find_center_of_polygon x in (fst acc +/ fst y, snd acc +/ snd y)) (Int 0, Int 0) s.polygons
  in (fst sum // (Int s.num_polygon), snd sum // (Int s.num_polygon))

let make_square' ((x, y): vertex): vertex list =
  let half = (Int 1) // (Int 2) in
  [(x -/ half, y -/ half); (x +/ half, y -/ half); (x +/ half, y +/ half); (x -/ half, y +/ half)]

let solve (p: problem): solution =
   let ll = lower_left_of_silhouette (fst p) in
     { source = [(Int 0, Int 0); (Int 1, Int 0); (Int 1, Int 1); (Int 0, Int 1)]; facets = [[0; 1; 2; 3]]; destination = make_square ll }
(*  let center = find_center_of_silhouette (fst p) in
    { source = [(Int 0, Int 0); (Int 1, Int 0); (Int 1, Int 1); (Int 0, Int 1)]; facets = [[0; 1; 2; 3]]; destination = make_square' center }*)

let solve_problem (f: string) =
  let p = parse_problem f in
  solve p |> print_solution_to_file "toto"

let _ =
  solve_problem (Sys.argv.(1))
