# ouv

type decision_tree =
  | Leaf of bool
  | Node of int * decision_tree * decision_tree

let tree = Node (1, 
                 Node (2, Leaf true, Leaf false), 
                 Node (2, Leaf false, Leaf true))


let rec cons_arbre depth = function
  | [] -> failwith "Empty truth table"
  | [x] -> Leaf x
  | xs -> 
      let half = List.length xs / 2 in
      let left, right = List.split_at half xs in
      Node(depth, cons_arbre (depth+1) left, cons_arbre (depth+1) right)

La fonction List.split_at est une fonction hypothétique qui divise une liste en deux à un certain index. OCaml n'a pas cette fonction dans sa bibliothèque standard, donc vous devrez l'implémenter vous-même ou utiliser une approche différente pour diviser la liste en deux.


let split_at n xs =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h::t as l -> if i = 0 then List.rev acc, l
                   else aux (i-1) (h::acc) t  in
  aux n [] xs


let truth_table = [true; false; true; false]
let decision_tree = cons_arbre 1 truth_table

let rec liste_feuilles = function
  | Leaf b -> [b]
  | Node (_, left, right) -> liste_feuilles left @ liste_feuilles right