open Num

type point = num

type vertex = (point * point)

type polygon = { mutable num_vertices: int; mutable vertices: vertex array }

type silhouette = { mutable num_polygon: int; mutable polygons: polygon array }

type skeleton = { mutable num_lines: int; mutable lines: (vertex * vertex) array }

type problem = (silhouette * skeleton)

type solution = { mutable source: vertex list; mutable facets: (int list) list; mutable destination: vertex list }
