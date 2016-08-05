open Spec
open Num

let print_point p = Printf.sprintf "%s" (string_of_num p)

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
