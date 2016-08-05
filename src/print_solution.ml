open Spec
open Big_int

let print_point p =
  match p with
  | Frac (a, b) -> Printf.sprintf "%s/%s" (string_of_big_int a) (string_of_big_int b)
  | Int a -> Printf.sprintf "%s" (string_of_big_int a)

let print_vertex (p1, p2) =
  (print_point p1) ^ "," ^ (print_point p2)

let print_solution_to_file (f: string) (s: solution) =
  let oc = open_out f in
  Printf.fprintf oc "%d\n" (List.length s.source);
  List.iter (fun p -> Printf.fprintf oc "%s\n" (print_vertex p)) s.source;
  Printf.fprintf oc "%d\n" (List.length s.facets);
  List.iter (fun l1 -> Printf.fprintf oc "%d " (List.length l1); List.iter (fun p -> Printf.fprintf oc "%d " p) l1; Printf.fprintf oc "\n") s.facets;
  List.iter (fun p -> Printf.fprintf oc "%s\n" (print_vertex p)) s.destination;
  close_out oc
