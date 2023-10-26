(* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
bouton "Évaluer le code" ci-dessous ou [Ctrl-e]. *)

type decision_tree =
  | Leaf of bool
  | Node of int * decision_tree * decision_tree

let split_at n xs =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h::t as l -> if i = 0 then List.rev acc, l
        else aux (i-1) (h::acc) t  in
  aux n [] xs


let rec cons_arbre depth = function
  | [] -> failwith "table vérité vide"
  | [x] -> Leaf x
  | xs ->
      let half = List.length xs / 2 in
      let left, right = split_at half xs in
      Node(depth, cons_arbre (depth+1) left, cons_arbre (depth+1) right)


let rec liste_feuilles = function
  | Leaf b -> [b]
  | Node (_, left, right) -> liste_feuilles left @ liste_feuilles right

let rec print_tree = function
  | Leaf b -> print_endline (string_of_bool b)
  | Node (depth, left, right) ->
      print_endline ("profondeur: " ^ string_of_int depth);
      print_tree left;
      print_tree right

let tree = Node (1, 
                 Node (2, Leaf true, Leaf false), 
                 Node (2, Leaf false, Leaf true));;
print_tree tree
type node = int
type liste_deja_vus = (int list * node) list

let rec find_replace n = function
  | [] -> None
  | (m, node) :: xs -> if m = n then Some node else find_replace n xs

          

let id = ref 0

let rec to_dot = function
  | Leaf b ->
      let i = !id in
      incr id;
      Printf.sprintf "%d [label=\"%b\"];\n" i b, i
  | Node (v, left, right) ->
      let i = !id in
      incr id;
      let left_dot, left_id = to_dot left in
      let right_dot, right_id = to_dot right in
      Printf.sprintf "%d [label=\"%d\"];\n%d -- %d [style=dotted];\n%d -- %d;\n%s%s"
        i v i left_id i right_id left_dot right_dot, i

let tree_to_dot tree =
  let dot, _ = to_dot tree in
  Printf.sprintf "graph {\n%s\n}" dot
        
let rec compressionParListe g listeDejaVus =
  match g with
  | Leaf _ -> g, listeDejaVus
  | Node(depth, left, right) ->
      let left, newDejaVus = compressionParListe left listeDejaVus in
      let right, newDejaVus = compressionParListe right newDejaVus in
      let n = liste_feuilles g in
      match find_replace n listeDejaVus with
      | Some node -> node, listeDejaVus
      | None -> let g = Node(depth, left, right) in g, (n, g) :: listeDejaVus


type arbreDejaVus =
  | Node of node option * arbreDejaVus * arbreDejaVus
  | Leaf

let rec search_tree = function
  | [], Node (Some node, _, _) -> Some node
  | [], Node (None, _, _) -> None
  | [], Leaf -> None
  | false :: xs, Node (_, left, _) -> search_tree (xs, left)
  | true :: xs, Node (_, _, right) -> search_tree (xs, right)
  | _, Leaf -> None

let rec insert_tree node = function
  | [], Node (_, left, right) -> Node (Some node, left, right)
  | [], Leaf -> Node (Some node, Leaf, Leaf)
  | false :: xs, Node (n, left, right) -> Node (n, insert_tree node (xs, left), right)
  | true :: xs, Node (n, left, right) -> Node (n, left, insert_tree node (xs, right))
  | xs, Leaf -> insert_tree node (xs, Node (None, Leaf, Leaf))


let rec compressionParArbre g arbreDejaVus tableVerite =
  match g with
  | Leaf  -> g, arbreDejaVus
  | Node(depth, left, right) ->
      let left, arbreDejaVus = compressionParArbre left arbreDejaVus (false :: tableVerite) in
      let right, arbreDejaVus = compressionParArbre right arbreDejaVus (true :: tableVerite) in
      match search_tree (tableVerite, arbreDejaVus) with
      | Some node -> Node(Some node, Leaf, Leaf), arbreDejaVus
      | None ->
          let g = Node(depth, left, right) in
          (match g with
           | Node(Some node, _, _) -> g, insert_tree node (tableVerite, arbreDejaVus)
           | _ -> failwith "Unexpected case")


type big_int_list = int64 list

let insert (lst: big_int_list) (x: int64): big_int_list =
  List.append lst [x]

let head (lst: big_int_list): int64 option =
  match lst with
  | [] -> None
  | x :: _ -> Some x


let tail (lst: big_int_list): big_int_list =
  match lst with
  | [] -> []
  | _ :: xs -> xs


let decomposition (lst: big_int_list): bool list =
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | x :: xs ->
        let bits = ref [] in
        for i = 0 to 63 do
          bits := (Int64.logand x (Int64.shift_left 1L i) <> 0L) :: !bits
        done;
        aux xs (List.rev_append !bits acc)
  in
  aux lst []
  
    
let truth_table = [true; true;false; true; false; true; false; false; true; false; true;false;false;true;true;false]
let decision_tree = cons_arbre 1 truth_table
let truth_table_25899 = decomposition [25899L]
let decision_tree_25899 = cons_arbre 1 truth_table_25899

let compression,_ = compressionParListe decision_tree_25899 []

let dot_representation = tree_to_dot decision_tree_25899;;


let oc = open_out "graph.dot";;
output_string oc dot_representation;
close_out oc;;

let dot_representation_compresse = tree_to_dot compression;;

let oc2 = open_out "graph_compresse.dot";;
output_string oc2 dot_representation_compresse;
close_out oc2;