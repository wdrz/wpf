(* Autor: Witold Drzewakowski
 *
 * ISet - Interval sets
 * Copyright (C) 1996-2019 Xavier Leroy, Nicolas Cannasse, Markus Mottl,
   Jacek Chrzaszcz, Witold Drzewakowski
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(* Interval Set.
   This is an interval set, i.e. a set of integers, where large
   intervals can be stored as single elements. Intervals stored in the
   set are disjoint. *)

(* Typ domknietego przedzialu liczb calkowitych [[a, b]], gdzie [a]
   oznacza poczatek przedzialu, a [b] koniec *)

type range = int * int


(* Typ zbioru przedzialow, tj. zbior liczb calkowitych, w ktorym duze
   przedzialy sa przechowywane jako pojedyncze elementy. Przechowywane
   przedzialy sa rozlaczne. Typ zaimplementowany za pomoca zrownowazonego
   drzewa binarneych wyszukiwan. Typ [t] moze byc [Empty], badz zawierac
   kolejno: drzewo lewe, przechowywany przedzial wartosci, drzewo prawe,
   najwieksza glebokosc wierzcholka w drzewie, # liczb trzymanych w zbiorze*)

type t =
  | Empty
  | Node of t * range * t * int * int


(* operator dodawawania, ktory dla liczb ktorych suma jest wieksza od
   max_int, zwraca max_int; w innych przypadkach zachowuje sie tak jak +
   Wymaga aby co najmniej jedna liczba byla dodatnia *)

let ( +! ) a b = if a > 0 && b > 0 && a + b <= 1 then max_int else a + b


(* operator brania roznicy, ktory dla liczb ktorych roznica jest wieksza od
   max_int, zwraca max_int; w innych przypadkach zachowuje sie tak jak - *)

let ( -! ) a b = if a >= 0 && b < 0 && a - b < 0 then max_int else a - b


(* [height d] zwraca wysokosc drzewa [d]. Dziala w czasie O(1) *)

let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0


(* [liczba d] zwraca liczbe liczb zawartych w przedzialach
   przechowywanych w drzewie [d]. Dziala w czasie O(1) *)

let liczba = function
  | Node (_, _, _, _, x) -> x
  | Empty -> 0


(* [make l v r] zwraca drzewo [d] o korzeniu [v] prawym poddrzewie [r]
   oraz lewym poddrzewie [l]. Drzewo [d] niekoniecznie jest zbalansowane.
   Dziala w czasie O(1). Nie wymaga aby drzewa [l] i [r] byly zbalansowane. *)

let make l ((a, b) as k) r =
  Node (l, k, r, max (height l) (height r) +! 1,
    b -! a +! 1 +! liczba l +! liczba r)


(* [bal l k r], o ile to konieczne wykonuje pojedyncza rotacje
   na drzewie [make l k r] i efekt swojej pracy zwraca jako wynik
   Dziala w czasie O(1) *)

let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r

(* [empty] zwraca pusty zbior *)
let empty = Empty

(* [is_empty] sprawdza czy dany zbior jest pusty *)
let is_empty x =
  x = Empty


(* [add_one x d] zwraca zbalansowane drzewo zawierajace wszystkie
   przedzialy zawarte w drzewie [d] oraz przedzial [x]. Dziala w
   czasie O(log n), gdzie n jest # wierzcholkow w drzewie, o ile
   drzewo jest zbalansowane. Wymaga aby [d] nie zawieralo przedzialow
   sasiadujacych ani przecinajacych przedzial [x] *)

let rec add_one (a, b) = function
  | Node (l, ((_, kb) as k), r, _, _) ->
      if kb < a then
        bal l k (add_one (a, b) r)
      else (* c = 0 zgodnie z zalozeniem nie zajdzie *)
        bal (add_one (a, b) l) k r
  | Empty -> Node (Empty, (a, b), Empty, 1, b -! a +! 1)


(* [join l v r] zwraca zrownowazone drzewo zlozone z przedzialow z drzew
   [l] i [r] oraz przedzialu [v]. Zaklada, ze wszystkie przedzialy z [l] i
   [r] nie przecinaja ani nie sasiaduja z [v], wszystkie przedzialy z [l]
   leza na lewo od przedzialu [v] oraz wszystkie przedzialy z [r] leza
   na prawo od [v]. Wymaga takze aby drzewa [l] i [r] byly zrownowazone.
   Dziala w czasie rzedu roznicy wysokosci drzew [l] i [r]. *)

let rec join l v r =
  match l, r with
  | Empty, _ -> add_one v r
  | _, Empty -> add_one v l
  | Node (ll, lv, lr, lh, _), Node (rl, rv, rr, rh, _) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r


(* [remove_min_elt d] zwraca [(d2, k)], gdzie [k] jest najmniejszym
   elementem w [d] natomiast [d2] to [d] z usunietym [k]. Jesli
   nie istnieje najmniejszy element w [d], zwraca [Not_found]. Dziala
   w zlozonosci O(log n), o ile drzewo [d] jest zbalansowane. *)

let rec remove_min_elt = function
  | Node (Empty, k, r, _, _) -> (k, r)
  | Node (l, k, r, _, _) ->
    let (e, d) = remove_min_elt l in
    (e, bal d k r)
  | Empty -> raise Not_found


(* [split_range x (a, b)] zwraca pare typu wariantowego
   [(a, x-1), (x+1, b)] o ile przedzialy te stanowia poprawna definicje
   przedzialu domknietego. Jesli ktorykolwiek nie stanowi, tj. gdy poczatek
   jest wiekszy od konca, w miejsce tego przedzialu poojawia sie None.
   Funkcja zaklada, ze [x \in <a, b>] *)

let split_range x (a, b) =
  let r1 = if x > a then Some (a, x - 1) else None in
  let r2 = if x < b then Some (x + 1, b) else None in
  (r1, r2)


(* [add_one_war x d] zwraca [add_one x d] jesli [x] jest [Some (_)] lub [d]
   gdy [x] jest [None]. Zalozenia i czas dzialania ma te same co [add_one] *)

let add_one_war x d =
  match x with
  | Some (r) -> add_one r d
  | None -> d


(* [split x s] zwraca krotke [(l, p, r)], gdzie
   [l] to zbalansowane drzewo zlozone z przedzialow nalezacych do
   drzewa [s], ktore leza na lewo od [x]; [r] to zbalansowane drzewo
   zlozone z przedzialow nalezacych do drzewa [s] ktore leza na
   prawo od [x]; [p] to [false] jesli [s] nie zawiera elementu
   rownego [x] lub [true] jesli [s] zawiera element rowny [x].
   Funkcja dziala w czasie O(log n), o ile [s] jest zbalansowane. *)

let split x d =
  let rec loop = function
    | Empty ->
        (Empty, false, Empty)
    | Node (l, (a, b), r, _, _) ->
        if x > b then
          let (lr, pres, rr) = loop r in
          (join l (a, b) lr, pres, rr)
        else if x < a then
          let (ll, pres, rl) = loop l in
          (ll, pres, join rl (a, b) r)
        else
          let (r1, r2) = split_range x (a, b) in
          ((add_one_war r1 l), true, (add_one_war r2 r))
  in
  loop d


(** [mem x s] zwraca [true] gdy [s] zawiera [x] lub [false] wpp.
    Dziala w czasie O(log n), o ile drzewo [d] jest zbalansowane *)

let mem x d =
  let rec loop = function
    | Empty -> false
    | Node (l, (a, b), r, _, _) ->
      if x > b then loop r
      else if x < a then loop l
      else true in
  loop d


(* [extended_range d r] zwraca sume teoriomnogosciowa
   przedzialu [r] oraz wszystkich przedzialow w drzewie [d]
   ktore przecinaja badz sasiaduja z [r]
   Funkcja dziala w czasie O(log n), gdzie [n] to liczba wierzcholkow
   w drzewie, o ile drzewo jest zbalansowane. *)

let extended_range (ka, kb) d =
  let rec loop1 = function
    | Node (l, (a, b), r, _, _) ->
      if b < ka - 1 then loop1 r
      else if a >= ka then loop1 l
      else a
    | Empty -> ka in
  let rec loop2 = function
    | Node (l, (a, b), r, _, _) ->
      if a > kb +! 1 then loop2 l
      else if b <= kb then loop2 r
      else b
    | Empty -> kb in
  (loop1 d, loop2 d)


(* [remove (x, y) s] zwraca zbior zawierajacy te same elementy co [s],
   poza tymi ktore naleza do przedzialu domknietego [[x, y]].
   Funkcja zaklada, ze [x <= y]. Dziala w czasie O(log n) o ile drzewo
   na wejsciu jest zbalansowane. W tym wypadku takze zwraca zbalansowane
   drzewo. *)

let remove (a, b) d =
  let (d1, _, pom) = split a d in
  let (_, _, d2) = split b pom in
  try
    let (m, dm) = remove_min_elt d2 in
    join d1 m dm
  with Not_found -> d1


(* [add (x, y) d] zwraca zbior zawierajacy te same elementy co [d],
   oraz liczby z przedzialu domknietego [[x,y]]. Zaklada ze [x <= y].
   Jesli [d] jest drzewem zbalansowanym to [add (x, y) d] tez jest.
   Dziala w zlozonosci O(log n), gdzie [n] jest liczba wierzcholkow
   drzewa, o ile drzewo jest zbalansowane. *)

let add x d =
  let e = extended_range x d in
  add_one e (remove e d)


(* [iter f s] wykonuje funkcje [f] na wszystkich przedzialach w [s].
   Przedzialy sa przekazywane do [f] w rosnacym porzadku.
   Dziala w czasie O(n) *)

let iter f d =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop d


(* [fold f s a] oblicza [(f xN ... (f x2 (f x1 a))...)], gdzie x1
   ... xN to wszystkie przedzialy w [s], w rosnacej kolejnosci.
   Dziala w czasie O(n) *)

let fold f d acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc d


(* [elements d] zwraca uporzadkowana rosnaco liste przedzialow z [d].
   Dziala w czasie O(n) *)

let elements d =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] d


(* [below n s] zwraca liczbe elementow [s] ktore sa mniejsze badz
   rowne [n]. Jesli takich elementow jest wiecej niz max_int, wynik
   to max_int. Dziala w czasie log(n). *)

let below x d =
  let (l, v, _) = split x d in
  liczba l +! if v then 1 else 0
