open Spec
open Parse_problem
open Print_solution
open Big_int

let empty_solution: solution = { source = [(Int zero_big_int, Int zero_big_int); (Int unit_big_int, Int zero_big_int); (Int unit_big_int, Int unit_big_int); (Int zero_big_int, Int unit_big_int)]; facets = [[0; 1; 2; 3]]; destination = [] }

let solve (p: problem): solution =
  { source = [(Int zero_big_int, Int zero_big_int); (Int unit_big_int, Int zero_big_int); (Int unit_big_int, Int unit_big_int); (Int zero_big_int, Int unit_big_int)]; facets = [[0; 1; 2; 3]]; destination = Array.to_list (fst p).polygons.(0).vertices }

let solve_problem (f: string) =
  let p = parse_problem f in
  solve p |> print_solution_to_file "toto"

let _ =
  solve_problem (Sys.argv.(1))
