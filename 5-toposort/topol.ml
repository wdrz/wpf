(* Autor: Witold Drzewakowski
 * Sortowanie topologiczne
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl,
 *   Witold Drzewakowski
 *
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


type ('k, 'v) map =
  | Empty
  | Node of ('k, 'v) map * 'k * 'v * ('k, 'v) map * int

type ('k, 'v) t = ('k, 'v) map

let height = function
  | Node (_, _, _, _, h) -> h
  | Empty -> 0

let make l k v r = Node (l, k, v, r, max (height l) (height r) + 1)

let bal l k v r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lv, lr, _) ->
        if height ll >= height lr then make ll lk lv (make lr k v r)
        else
          (match lr with
          | Node (lrl, lrk, lrv, lrr, _) ->
              make (make ll lk lv lrl) lrk lrv (make lrr k v r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rv, rr, _) ->
        if height rr >= height rl then make (make l k v rl) rk rv rr
        else
          (match rl with
          | Node (rll, rlk, rlv, rlr, _) ->
              make (make l k v rll) rlk rlv (make rlr rk rv rr)
          | Empty -> assert false)
    | Empty -> assert false
  else Node (l, k, v, r, max hl hr + 1)

let empty = Empty

let add x d map =
  let rec loop = function
    | Node (l, k, v, r, h) ->
        let c = compare x k in
        if c = 0 then Node (l, x, d, r, h)
        else if c < 0 then
          let nl = loop l in
          bal nl k v r
        else
          let nr = loop r in
          bal l k v nr
    | Empty -> Node (Empty, x, d, Empty, 1) in
  loop map

let find x map =
  let rec loop = function
    | Node (l, k, v, r, _) ->
        let c = compare x k in
        if c < 0 then loop l
        else if c > 0 then loop r
        else v
    | Empty -> raise Not_found in
  loop map

let mem x map =
  let rec loop = function
    | Node (l, k, v, r, _) ->
        let c = compare x k in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false in
  loop map

let fold f map acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, v, r, _) ->
	  loop (f k (loop acc l)) r in
  loop acc map

(* wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)
exception Cykliczne

(* Etykieta przypisywana wierzcholkom grafu, kolejno:
   wierzcholek rozpatrywany, odwiedzony, nieodwiedzony *)
type stan = Rozp | Odw | NOdw

(* [nazwij d] ( gdzie [d] jest typu [('a * 'a list) list] ) zwraca
   pare [(l, m)], gdzie [m] to mapa zawierajaca przypisania wartosciom
   typu 'a wystepujacym w [d] kolejnych intow 0, 1, 2, ..., l - 1 w taki
   sposob, ze dwie rozne wartosci maja przypisane dwa rozne inty.
   [l] jest najmniejsza liczba taka, ze jest to mozliwe. *)
let nazwij y =
  let f =
    List.fold_left (fun (n, mapa) el ->
      if mem el mapa then n, mapa
      else n + 1, add el n mapa) in
  List.fold_left (fun acc (x, l) -> f acc (x :: l)) (0, empty) y

(* [wypelnij mapa ls lista] zaklada ze lista sasiedztwa [ls] jest
   poczatkowo pusta i modyfikuje ja tak, aby dla kazdego wierzcholka [v] grafu
   jesli [id] jest liczba przypisana [v] w [mapa] to [ls.(id)] zawiera
   liste wierzcholkow grafu do ktorych prowadzi krawedz z [v] zgodnie z
   reprezentacja grafu w [lista]. Innymi slowy buduje uczciwa liste sasiedztwa
   dla grafu. *)
let wypelnij mapa ls =
  List.iter (fun (p, l) ->
    let tp = find p mapa in
    ls.(tp) <- List.rev_append l ls.(tp))

(* [ topol [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] ]
   zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
   dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
   element [a_i] jest przed kazdym z elementow [a_i1] ... [a_il] *)
let topol lista =
  let n, mapa = nazwij lista in
  let ls = Array.make n [] in
  wypelnij mapa ls lista;
  let st = Array.make n NOdw in
  let rec przejdz acc v =
    let tv = find v mapa in
    if st.(tv) = Rozp then raise Cykliczne
    else if st.(tv) = Odw then acc
    else
      (st.(tv) <- Rozp;
      let wyn = List.fold_left przejdz acc ls.(tv) in
      st.(tv) <- Odw;
      v :: wyn) in
  fold (fun x y -> przejdz y x) mapa []
