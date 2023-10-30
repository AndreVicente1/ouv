(* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
bouton "Évaluer le code" ci-dessous ou [Ctrl-e]. *)

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

(*Question 1.2 : décomposer un entier en liste de bits*)
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

(*Question 1.3 : coupe la liste si n est inférieure à la taille de la liste 
                sinon complète la liste de false*)
let rec completion lst n = 
  match lst with
  (* n>0 et lst = [] -> false *)
  | [] when n > 0 -> false::(completion lst (n-1))
  (* liste vide *)
  | [] -> []
  (* n>0 et lst != [] -> ajouter l'élément *)
  | h::t when n > 0 -> h::(completion t (n-1))
  | _ -> [] 
  
(*Question 1.4 : recompose la liste de bits en un int*)
let composition bits =
  let rec aux bits acc =
    match bits with
    | [] -> acc
    | h::t -> aux t ((acc * 2) + (if h then 1 else 0))
  in
  aux (List.rev bits) 0

(*Question 1.5 : construire la table de verite de taille n*)
let table x n =
  let bits = decomposition x in
  let complete = completion bits n in
  complete

(*Question 1.6 : entier aléatoire de n bits maximum*)
let genAlea n =
  let l = n / 64 in  (* Nombre d'entiers de 64 bits nécessaires *)
  let random_int = Random.int (1 lsl (n - l * 64)) in  (* Entier aléatoire inférieur à n - l * 64 *)
  let random_int64_list = List.init l (fun _ -> Int64.of_int (Random.bits ())) in  (* Liste d'entiers de taille l random 32 bits converties en 64 bits *)
  let random_int64 = Int64.of_int random_int in  (* Conversion de l'entier aléatoire de moins de 64 bits en int64 *)
  let result = List.rev_append random_int64_list [random_int64] in  (* Deuxieme entier < 2^36 en fin de liste *)
  result


(*Question 2.7*)
type decision_tree =
  | Leaf of bool
  | Node of int * decision_tree ref * decision_tree ref

(*Question 2.8*)
let split_at n xs =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h::t as l -> if i = 0 then List.rev acc, l
        else aux (i-1) (h::acc) t  in
  aux n [] xs


let rec cons_arbre depth = function
  | [] -> failwith "table vérité vide"
  | [x] -> Leaf (x)
  | xs ->
      let half = List.length xs / 2 in
      let left, right = split_at half xs in
      Node(depth, ref (cons_arbre (depth+1) left), ref(cons_arbre (depth+1) right))

(*Question 2.9*)
let rec liste_feuilles = function
  | Leaf b -> [b]
  | Node (_, left, right) -> liste_feuilles !left @ liste_feuilles !right

let rec print_tree = function
  | Leaf b -> print_endline (string_of_bool b)
  | Node (depth, left, right) ->
      print_endline ("profondeur: " ^ string_of_int depth);
      print_tree !left;
      print_tree !right

(*let tree = Node (1, 
                 Node (2, Leaf true, Leaf false), 
                 Node (2, Leaf false, Leaf true));;
print_tree tree*)
type node = int
(*type liste_deja_vus = (int list * node) list*)

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
      let left_dot, left_id = to_dot !left in
      let right_dot, right_id = to_dot !right in
      Printf.sprintf "%d [label=\"%d\"];\n%d -- %d [style=dotted];\n%d -- %d;\n%s%s"
        i v i left_id i right_id left_dot right_dot, i

let tree_to_dot tree =
  let dot, _ = to_dot tree in
  Printf.sprintf "graph {\n%s\n}" dot


type liste_deja_vus = (big_int_list * decision_tree ref) list ref

(*Retourne la liste privée de ses n premiers éléments*)
let rec drop n lst =
  if n <= 0 then lst
  else match lst with
    | [] -> []
    | _::t -> drop (n-1) t
        
let rec compressionParListe g listeDejaVus =
  match !g with
  | Leaf _ -> ()
  | Node (_, left, right) -> 
      (* Parcours suffixe *)
      compressionParListe left listeDejaVus;
      compressionParListe right listeDejaVus;

      let leaf_list = liste_feuilles !g in
      let second_half = drop ((List.length leaf_list) / 2) leaf_list in

      (* Appliquer la règle-Z *)
      if List.for_all ((=) false) second_half then
        g := !left
      else
        let n = Int64.of_int (composition leaf_list) in
        (* Appliquer la règle-M *)
        try
          let _, tree_ref = List.find (fun (n', _) -> n' = n) !listeDejaVus in
          g := !tree_ref
        with Not_found ->
          listeDejaVus := (n, g) :: !listeDejaVus
    
(*let rec compressionParListe g listeDejaVus =
  match g with
  | Leaf _ -> g, listeDejaVus
  | Node(depth, left, right) ->
      let left, newDejaVus = compressionParListe !left listeDejaVus in
      let right, newDejaVus = compressionParListe !right newDejaVus in
      let n = liste_feuilles g in
      match find_replace n listeDejaVus with
      | Some node -> node, listeDejaVus
      | None -> let g = Node(depth, ref left, ref right) in g, (n, g) :: listeDejaVus*)
    
(*let rec compress_tree_with_liste_deja_vus tree liste_deja_vus =
  match tree with
  | Leaf b -> Leaf b
  | Node(depth, left, right) ->
    let left_compressed = compress_tree_with_liste_deja_vus left liste_deja_vus in
    let right_compressed = compress_tree_with_liste_deja_vus right liste_deja_vus in

    (* Calculate the leaves associated with the current node *)
    let leaves = match (left_compressed, right_compressed) with
      | (Leaf lb, Leaf rb) -> [lb; rb]
      | _ -> [] (* Handle non-leaf nodes *)

    (* Check if the second half of the leaves contains only false values *)
    in let is_second_half_false = List.for_all (fun x -> x = false) (List.tl (List.rev leaves)) in

    if is_second_half_false then
      (* Rule-Z: Replace the pointer from the parent to the left child *)
      left_compressed
    else
      (* Calculate the integer value n corresponding to the leaves *)
      let n = composition leaves in

      (* Check if n is in ListeDejaVus and if so, replace the pointer *)
      match List.assoc_opt n !liste_deja_vus with
      | Some compressed_node -> compressed_node
      | None ->
        (* Rule-M: Add n-compressed_node pair to ListeDejaVus *)
        let compressed_node = Node(depth, left_compressed, right_compressed) in
        liste_deja_vus := (n, compressed_node) :: !liste_deja_vus;
        compressed_node

let compressionParListe tree =
  let liste_deja_vus = ref [] in
  let compressed_tree = compress_tree_with_liste_deja_vus tree liste_deja_vus in
  compressed_tree*)


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



    
let truth_table = [true; true; false; true; false; true; false; false; true; false; true; false; false; true; true; false]
let decision_tree = cons_arbre 1 truth_table
let truth_table_25899 = ref(decomposition [25899L])
let decision_tree_25899 = cons_arbre 1 !truth_table_25899;;


let dot_representation = tree_to_dot decision_tree_25899;;


let oc = open_out "graph.dot";;
output_string oc dot_representation;
close_out oc;;

let com = compressionParListe (ref decision_tree_25899 ) (ref []) ;;
let dot_representation_compresse = tree_to_dot decision_tree_25899

let oc2 = open_out "graph_compresse.dot";;
output_string oc2 dot_representation_compresse;
close_out oc2;