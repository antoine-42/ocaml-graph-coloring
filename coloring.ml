(* 1 *)
module StringSet = Set.Make(String)
module StringMap = Map.Make(String)
type graph = StringSet.t StringMap.t

(* 2 *)
let add_edge vertex_1 vertex_2 graph =
  let add_oriented_edge vertex_out vertex_in graph =
    let set =
      try
        StringMap.find vertex_out graph (* Si le sommet de depart de l'arrete est deja le sommet de depart d'autres arretes, reutiliser l'ensemble des sommets d'arrivees de ses arretes. *)
      with
        Not_found -> StringSet.empty (* Sinon, creer un nouvel ensemble. *)
    in
    let set = StringSet.add vertex_in set (* Ajouter le sommet d'arrivee de la nouvelle arrete a cet ensemble. *)
    in StringMap.add vertex_out set graph (* Remplacer l'ensemble des sommets d'arrivees des arretes du sommet de depart par l'ensemble qui viens d'etre cree. *)
  in
  let graph = add_oriented_edge vertex_1 vertex_2 graph in add_oriented_edge vertex_2 vertex_1 graph (* Ajouter une arrete orientee de vertex_1 a vertex_2 et de vertex_2 a vertex_1. *)

(* 3 *)
let remove_vertex vertex graph =
  let graph = StringMap.remove vertex graph in (* Enlever les arretes partant du sommet. *)
  StringMap.map (fun set -> StringSet.remove vertex set) graph (* Enlever les arretes allant vers le sommet. *)

(* 4 *)
module Int = struct
  type t = int
  let compare = fun x y -> x - y (* Les ensembles ne peuvent utiliser que les types comparables. *)
end

module IntSet = Set.Make(Int)

(* 5 *)
let rec color_set n =
  if n <= 1 then IntSet.singleton 1 (* Si n = 1 renvoyer un ensemble avec 1. *)
  else IntSet.add n (color_set (n - 1)) (* Sinon faire un appel recursif avec n-1 et ajouter n a l'ensemble resultat. *)

(* 6 *)
type disp_color = IntSet.t StringMap.t

(* 7 *)
let init_colors graph n =
  StringMap.map (fun set -> color_set n) graph (* Pour tous les sommets du graphe, creer un set contenant les entiers de 1 a n. *)

(* 8 *)
let remove_color color vertex color_map =
  let set = StringMap.find vertex color_map in (* Trouver l'ensemble qui contient les couleurs disponibles du sommet. *)
  StringMap.add vertex (IntSet.remove color set) color_map (* Enlever la couleur des couleurs disponibles du sommet. *)

(* 9 *)
exception Failed

(* 10 *)
let rec try_first f set =
  if IntSet.is_empty set then raise (Failed) else (* Si l'ensemble est vide, renvoyer une erreur. *)
  let i = IntSet.choose set in (* Choisir le premier element de l'ensemble. *)
  try
    f i (* Si pas d'erreur, applique f a i. *)
  with
    Failed -> try_first f (IntSet.remove i set) (* Sinon, reessayer en enlevant i de l'ensemble. *)

(* 11 *)
type coloring = int StringMap.t

(* 12 *)
let rec color graph colors =
  if StringMap.is_empty graph then StringMap.empty else (* Si g est vide retourner le coloriage vide. *)
  let (curr_vertex, vertex_neighboors) = StringMap.choose graph in (* Sinon choisir un sommet (curr_vertex) et prendre ses voisins. *)
  let rec color_vertex vertex_available_colors_set =
    if IntSet.is_empty vertex_available_colors_set then raise (Failed) else (* Si plus de couleur disponible alors echec. *)
    let curr_color = IntSet.choose vertex_available_colors_set in (* Choisir une couleur parmis les couleurs disponibles. *)
    try
      let c' = StringMap.mapi ( (* Pour toutes les entrees de couleurs disponibles. *)
        fun curr_vertex' vertex_available_colors_set' ->
        if StringSet.mem curr_vertex' vertex_neighboors (* Si le sommet actuel est voisin du sommet curr_vertex. *)
        then IntSet.remove curr_color vertex_available_colors_set' (* Alors enlever la couleur choisie de ses couleurs possibles. *)
        else vertex_available_colors_set' ) colors in (* Sinon conserver ses couleurs. *)
      let g' = remove_vertex curr_vertex graph in (* Prendre le graphe sans le sommet. *)
      StringMap.add curr_vertex curr_color (color g' c') (* Appliquer color sur le nouveau graphe et entrees de couleurs disponibles, et ajouter au resultat la couleur choisie du sommet curr_vertex. *)
    with
      Failed -> color_vertex (IntSet.remove curr_color vertex_available_colors_set) (* Si echec, enlever la couleur choisie des couleurs disponibles de curr_vertex. *)
  in
  let vertex_color_set = StringMap.find curr_vertex colors in (* Prendre son ensemble de couleurs disponibles. *)
  color_vertex vertex_color_set
