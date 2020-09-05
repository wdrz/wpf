(* [nwd a b] zwraca najwiekszy wspolny dzielnik liczb [a] i [b] *)
let nwd a b =
  let rec wyk x y =
    if x = 0 then y
    else wyk (y mod x) x in
  if a <= b then wyk a b else wyk b a

(* [usun_zera [|(x_1, y_1); ..; (x_n, y_n)|]] zwraca [([|(x_k_1, y_k_1), ...
   ..., (x_k_r, y_k_r)|], r) ], gdzie k_i to rosnacy ciag indeksow tych
   wyrazow ciagu (x_i), ktore nie sa zerami *)
let usun_zera l =
  let n = Array.fold_left (fun a (x, _) ->
    if x <> 0 then a + 1 else a) 0 l in
  let nowa = Array.make n (0, 0) in
  let _ = Array.fold_left (fun a x ->
    if x <> (0, 0) then
      (nowa.(a) <- x;
      a + 1)
    else a) 0 l in
  (nowa, n)

(* [test1 [|(x_1, y_1); ...; (x_n, y_n)|]] zwraca [true] gdy istnieje
   [ i \in {1, 2, ..., n} ] taki ze [x_i = y_i] lub [y_i = 0] i
   [false] przeciwnym wypadku. *)
let test1 l =
  Array.fold_left (fun a (f, s) ->
    a || f = s || s = 0 ) false l

(* [test2 [|(x_1, y_1); ...; (x_n, y_n)|]] zwraca [true] gdy
   [nwd (x_1, x_2, ..., x_n)] dzieli [nwd (y_1, y_2, ..., y_n)] i
   [false] przeciwnym wypadku. *)
let test2 l =
  let (g1, g2) =
    Array.fold_left (fun (a, b) (f, s) ->
      nwd a f, nwd b s) (0, 0) l in
  g2 mod g1 = 0

(* implementacja rozwiazania brutalnego przelewanki *)
let przeszukaj l n =
  let q = Queue.create () in
  let final = Array.init n (fun i -> snd l.(i)) in
  let archiwum = Hashtbl.create n in

  (* [rzut stan g] dodaje [stan] do kolejki i zapamietuje odwiedzenie go
     wraz z liczba [g + 1], jesli nie byl wczesniej rozpatrzony, a
     w przeciwnym wypadku nie robi nic *)
  let rzut stan g =
    if not (Hashtbl.mem archiwum stan) then
      (Queue.add stan q;
      Hashtbl.add archiwum stan (g + 1)) in

  let napelnij stan i =
    let nowy = Array.copy stan in
    nowy.(i) <- fst l.(i);
    rzut nowy in

  let wylej stan i =
    let nowy = Array.copy stan in
    nowy.(i) <- 0;
    rzut nowy in

  (* przelewa wode z naczynia [i] do naczynia [j] *)
  let przelej stan i j g =
    if i <> j then
      (let nowy = Array.copy stan in
      let v = min (fst l.(j) - stan.(j)) stan.(i) in
      nowy.(j) <- stan.(j) + v;
      nowy.(i) <- stan.(i) - v;
      rzut nowy g) in

  let pusta = Array.make n 0 in
  Hashtbl.add archiwum pusta 0;
  Queue.add pusta q;

  let wynik = ref (-1) in
  while !wynik = -1 && not (Queue.is_empty q) do
    let stan = Queue.pop q in
    let g = Hashtbl.find archiwum stan in
    for i = 0 to n - 1 do
      napelnij stan i g;
      wylej stan i g;
      for j = 0 to n - 1 do przelej stan i j g done
    done;
    if Hashtbl.mem archiwum final then
      wynik := Hashtbl.find archiwum final
  done;
  !wynik

let przelewanka lin =
  let (l, n) = usun_zera lin in
  if n = 0 then 0
  else if test1 l && test2 l then przeszukaj l n
  else -1

