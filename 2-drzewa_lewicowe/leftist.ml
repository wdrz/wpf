type 'a queue = Leaf | Node of 'a * 'a queue * 'a queue * int

(* Wyjątek podnoszony przez delete_min gdy kolejka jest pusta *)
exception Empty

(* Pusta kolejka priorytetowa *)
let empty = Leaf

(* Sprawdza czy kolejka jest pusta*)
let is_empty q =
    match q with
    | Leaf -> true
    | _ -> false

(* jesli sw = true funkcja ustawia drzewa rosnaco po liczbie w korzeniu
   jesli sw = false to ustawia malejaco po dlugosci prawej sciezki *)
let order q1 q2 sw =
    match q1, q2 with
    | Node (a1, _, _, z1), Node (a2, _, _, z2) ->
        if sw then if a1 < a2 then (q1, q2) else (q2, q1)
        else if z1 < z2 then (q2, q1) else (q1, q2)
    | _, Leaf -> (q1, q2)
    | Leaf, _ -> (q2, q1)

(* Laczy dwie kolejki *)
let rec join q1 q2 =
    let (q1, q2) = order q1 q2 true in
    match q1, q2 with
    | Node (a1, q11, q12, _), Node (a2, _, _, _) ->
        let (q11, q3) = order q11 (join q12 q2) false in
        begin match q11, q3 with
        | _, Node (_, _, _, z3) ->
            Node (a1, q11, q3, z3 + 1)
        | _, _ ->
            Node (a1, q11, q3, 0)
        end
    | a, _ -> a

(* Dodaje element do kolejki *)
let add el q = join (Node (el, Leaf, Leaf, 0)) q

(* Kasuje najmniejszy element z kolejki *)
let delete_min q =
    match q with
    | Leaf -> raise Empty
    | Node (el, q1, q2, _) -> (el, join q1 q2)
