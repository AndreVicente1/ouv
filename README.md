# ouv

```
https://try.ocamlpro.com/#code/(*'Ceci'est'un'%c3%a9diteur'pour'OCaml!'''Entrez'votre'programme'ici,'et'envoyez-le'au'toplevel'en'utilisant'le!bouton'$(%c3%89valuer'le'code$('ci-dessous'ou'$/Ctrl-e$1.'*)!!type'decision_tree'=!$5'Leaf'of'bool!$5'Node'of'int'*'decision_tree'*'decision_tree!!let'split_at'n'xs'=!let'rec'aux'i'acc'='function!$5'$/$1'-$.'List.rev'acc,'$/$1!$5'h::t'as'l'-$.'if'i'='0'then'List.rev'acc,'l!else'aux'(i-1)'(h::acc)'t''in!aux'n'$/$1'xs!!!let'rec'cons_arbre'depth'='function!$5'$/$1'-$.'failwith'$(Empty'truth'table$(!$5'$/x$1'-$.'Leaf'x!$5'xs'-$.!let'half'='List.length'xs'/'2'in!let'left,'right'='split_at'half'xs'in!Node(depth,'cons_arbre'(depth+1)'left,'cons_arbre'(depth+1)'right)!!!let'rec'liste_feuilles'='function!$5'Leaf'b'-$.'$/b$1!$5'Node'(_,'left,'right)'-$.'liste_feuilles'left'@'liste_feuilles'right!!let'rec'print_tree'='function!$5'Leaf'b'-$.'print_endline'(string_of_bool'b)!$5'Node'(depth,'left,'right)'-$.!print_endline'($(Node'depth:'$('$2'string_of_int'depth);!print_tree'left;!print_tree'right!!type'node'='int!type'liste_deja_vus'='(int'list'*'node)'list!!let'rec'find_replace'n'='function!$5'$/$1'-$.'None!$5'(m,'node)'::'xs'-$.'if'm'='n'then'Some'node'else'find_replace'n'xs!!!let'rec'compressionParListe'g'listeDejaVus'=!match'g'with!$5'Leaf'_'-$.'g,'listeDejaVus!$5'Node(depth,'left,'right)'-$.!let'left,'newDejaVus'='compressionParListe'left'listeDejaVus'in!let'right,'newDejaVus'='compressionParListe'right'newDejaVus'in!let'n'='liste_feuilles'g'in!match'find_replace'n'listeDejaVus'with!$5'Some'node'-$.'node,'listeDejaVus!$5'None'-$.'let'g'='Node(depth,'left,'right)'in'g,'(n,'g)'::'listeDejaVus!!!type'node'='int!!type'arbreDejaVus'=!$5'Node'of'node'option'*'arbreDejaVus'*'arbreDejaVus!$5'Leaf!!let'rec'search_tree'='function!$5'$/$1,'Node'(Some'node,'_,'_)'-$.'Some'node!$5'$/$1,'Node'(None,'_,'_)'-$.'None!$5'$/$1,'Leaf'-$.'None!$5'false'::'xs,'Node'(_,'left,'_)'-$.'search_tree'(xs,'left)!$5'true'::'xs,'Node'(_,'_,'right)'-$.'search_tree'(xs,'right)!$5'_,'Leaf'-$.'None!!let'rec'insert_tree'node'='function!$5'$/$1,'Node'(_,'left,'right)'-$.'Node'(Some'node,'left,'right)!$5'$/$1,'Leaf'-$.'Node'(Some'node,'Leaf,'Leaf)!$5'false'::'xs,'Node'(n,'left,'right)'-$.'Node'(n,'insert_tree'node'(xs,'left),'right)!$5'true'::'xs,'Node'(n,'left,'right)'-$.'Node'(n,'left,'insert_tree'node'(xs,'right))!$5'xs,'Leaf'-$.'insert_tree'node'(xs,'Node'(None,'Leaf,'Leaf))!!!let'rec'compressionParArbre'g'arbreDejaVus'tableVerite'=!match'g'with!$5'Leaf''-$.'g,'arbreDejaVus!$5'Node(depth,'left,'right)'-$.!let'left,'arbreDejaVus'='compressionParArbre'left'arbreDejaVus'(false'::'tableVerite)'in!let'right,'arbreDejaVus'='compressionParArbre'right'arbreDejaVus'(true'::'tableVerite)'in!match'search_tree'(tableVerite,'arbreDejaVus)'with!$5'Some'node'-$.'Node(Some'node,'Leaf,'Leaf),'arbreDejaVus!$5'None'-$.!let'g'='Node(depth,'left,'right)'in!(match'g'with!$5'Node(Some'node,'_,'_)'-$.'g,'insert_tree'node'(tableVerite,'arbreDejaVus)!$5'_'-$.'failwith'$(Unexpected'case$()!!!type'big_int_list'='int64'list!!let'insert'(lst:'big_int_list)'(x:'int64):'big_int_list'=!List.append'lst'$/x$1!!let'head'(lst:'big_int_list):'int64'option'=!match'lst'with!$5'$/$1'-$.'None!$5'x'::'_'-$.'Some'x!!!let'tail'(lst:'big_int_list):'big_int_list'=!match'lst'with!$5'$/$1'-$.'$/$1!$5'_'::'xs'-$.'xs!!!let'decomposition'(lst:'big_int_list):'bool'list'=!let'rec'aux'lst'acc'=!match'lst'with!$5'$/$1'-$.'acc!$5'x'::'xs'-$.!let'bits'='ref'$/$1'in!for'i'='0'to'63'do!bits':='(Int64.logand'x'(Int64.shift_left'1L'i)'$-$.'0L)'::'$&bits!done;!aux'xs'(List.rev_append'$&bits'acc)!in!aux'lst'$/$1
```


## Échauffement


```
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
      let rec loop i bits =
        if i = 64 then 
          bits
        else
          let bit = (Int64.logand x (Int64.shift_left 1L i) <> 0L) in
          loop (i + 1) (bit :: bits)
      in
      let bits = loop 0 [] in
      aux xs (List.rev_append bits acc)
  in
  aux lst []

```


## Arbre de décision

```
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


let split_at n xs =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h::t as l -> if i = 0 then List.rev acc, l
                   else aux (i-1) (h::acc) t  in
  aux n [] xs



let truth_table = [true; true;false; true; false; true; false; false; true; false; true;false;false;true;true;false]
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
```

## Compression de l’arbre de décision et ZDD


Chaque élément de la liste est un couple d'une liste d'entiers et d'un pointeur vers un noeud. 

```
type node = int
type liste_deja_vus = (int list * node) list
```


Tout d'abord, nous devons définir une fonction find_replace qui cherche un grand entier dans ListeDejaVus et renvoie un pointeur vers le nœud correspondant si trouvé.

```
let rec find_replace n = function
  | [] -> None
  | (m, node) :: xs -> if m = n then Some node else find_replace n xs
```


Ensuite, nous définissons la fonction CompressionParListe qui  renvoie un couple contenant l'arbre de décision compressé et la liste ListeDejaVus mise à jour.


```
let rec compressionParListe g listeDejaVus =
  match g with
  | Leaf _ -> g, listeDejaVus
  | Node(depth, left, right) ->
    let left, listeDejaVus = compressionParListe left listeDejaVus in
    let right, listeDejaVus = compressionParListe right listeDejaVus in
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

let write_graph oc tree =
  Printf.fprintf oc "graph {\n";
  dot oc tree;
  Printf.fprintf oc "}\n"

let oc = open_out "graph.dot";;
write_graph oc decision_tree;
close_out oc
```


## Compression avec historique stocké dans une structure arborescente

```
type node = int

type arbreDejaVus =
  | Node of node option * arbreDejaVus * arbreDejaVus
  | Leaf
```


La fonction search_tree parcourt l'arbre en fonction de la table de vérité et renvoie le pointeur vers le nœud du graphe s'il est trouvé :

```
let rec search_tree = function
  | [], Node (Some node, _, _) -> Some node
  | [], Node (None, _, _) -> None
  | [], Leaf -> None
  | false :: xs, Node (_, left, _) -> search_tree (xs, left)
  | true :: xs, Node (_, _, right) -> search_tree (xs, right)
  | _, Leaf -> None
```


La fonction insert_tree insère un pointeur vers un nœud du graphe dans l'arbre en fonction de la table de vérité :

```
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

```