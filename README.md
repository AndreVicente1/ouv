# ouv


## Arbre de décision

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
      let left, right = split_at half xs in
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



let rec print_tree = function
  | Leaf b -> print_endline (string_of_bool b)
  | Node (depth, left, right) -> 
      print_endline ("Node depth: " ^ string_of_int depth);
      print_tree left; 
      print_tree right


## Compression de l’arbre de décision et ZDD


Chaque élément de la liste est un couple d'une liste d'entiers et d'un pointeur vers un noeud. 

type node = int
type liste_deja_vus = (int list * node) list



Tout d'abord, nous devons définir une fonction find_replace qui cherche un grand entier dans ListeDejaVus et renvoie un pointeur vers le nœud correspondant si trouvé.


let rec find_replace n = function
  | [] -> None
  | (m, node) :: xs -> if m = n then Some node else find_replace n xs



Ensuite, nous définissons la fonction CompressionParListe qui  renvoie un couple contenant l'arbre de décision compressé et la liste ListeDejaVus mise à jour.



let rec CompressionParListe g listeDejaVus =
  match g with
  | Leaf _ -> g, listeDejaVus
  | Node(depth, left, right) ->
    let left, listeDejaVus = CompressionParListe left listeDejaVus in
    let right, listeDejaVus = CompressionParListe right listeDejaVus in
    let n = liste_feuilles g in
    match find_replace n listeDejaVus with
    | Some node -> node, listeDejaVus
    | None -> let g = Node(depth, left, right) in g, (n, g) :: listeDejaVus



let rec dot oc = function
  | Leaf b -> 
      let id = string_of_bool b in
      Printf.fprintf oc "%s [label=\"%s\"];\n" id id
  | Node (depth, left, right) -> 
      let id = string_of_int depth in
      Printf.fprintf oc "%s [label=\"%s\"];\n" id id;
      dot_edge oc id "dashed" left;
      dot_edge oc id "solid" right

and dot_edge oc parent style = function
  | Leaf b -> 
      let id = string_of_bool b in
      Printf.fprintf oc "%s -- %s [style=%s];\n" parent id style
  | Node (depth, _, _) -> 
      let id = string_of_int depth in
      Printf.fprintf oc "%s -- %s [style=%s];\n" parent id style


let oc = open_out "graph.dot" in
dot oc decision_tree;
close_out oc



## Compression avec historique stocké dans une structure arborescente


type node = int

type arbreDejaVus =
  | Node of node option * arbreDejaVus * arbreDejaVus
  | Leaf



La fonction search_tree parcourt l'arbre en fonction de la table de vérité et renvoie le pointeur vers le nœud du graphe s'il est trouvé :


let rec search_tree = function
  | [], Node (Some node, _, _) -> Some node
  | [], Node (None, _, _) -> None
  | [], Leaf -> None
  | false :: xs, Node (_, left, _) -> search_tree (xs, left)
  | true :: xs, Node (_, _, right) -> search_tree (xs, right)
  | _, Leaf -> None



La fonction insert_tree insère un pointeur vers un nœud du graphe dans l'arbre en fonction de la table de vérité :


let rec insert_tree node = function
  | [], Node (_, left, right) -> Node (Some node, left, right)
  | [], Leaf -> Node (Some node, Leaf, Leaf)
  | false :: xs, Node (n, left, right) -> Node (n, insert_tree node (xs, left), right)
  | true :: xs, Node (n, left, right) -> Node (n, left, insert_tree node (xs, right))
  | xs, Leaf -> insert_tree node (xs, Node (None, Leaf, Leaf))



let rec CompressionParArbre g arbreDejaVus tableVerite =
  match g with
  | Leaf _ -> g, arbreDejaVus
  | Node(depth, left, right) ->
    let left, arbreDejaVus = CompressionParListe left arbreDejaVus (false :: tableVerite) in
    let right, arbreDejaVus = CompressionParListe right arbreDejaVus (true :: tableVerite) in
    let n = liste_feuilles g in
    match search_tree (tableVerite, arbreDejaVus) with
    | Some node -> node, arbreDejaVus
    | None -> 
      let g = Node(depth, left, right) in 
      g, insert_tree g (tableVerite, arbreDejaVus)