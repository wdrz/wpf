(* Punkt na plaszczyznie *)
type point = float * float

(* Poskladana kartka: ile razy kartke przebije szpilka wbita w danym punkcie *)
type kartka = point -> int

(* [side a b c] zwraca liczbe dodatnia typu float jesli punkt [c]
   znajduje sie po lewej stronie wektora [[a, b]], liczbe ujemna
   typu float jesli punkt [c] znajduje sie po prawej stronie
   wektora [[a, b]] i zero jesli [c] lezy na prostej [ab] *)
let side (x1, y1) (x2, y2) (x3, y3) =
  (x2 -. x1) *. (y3 -. y1) -. (y2 -. y1) *. (x3 -. x1)

(* [prostokat p1 p2] zwraca kartke - domkniety prostokat o bokach rownoleglych
   do osi ukladu wspolrzednych i lewym dolnym rogu [p1] a prawym górnym [p2].
   Zaklada ze [p1] jest nieostro na lewo i w dol od [p2]. *)
let prostokat (x1, y1) (x2, y2) = function (x3, y3) ->
  if x3 >= x1 && x3 <= x2 && y3 >= y1 && y3 <= y2 then 1 else 0

(* [kolko p r] zwraca kartkę, reprezentującą kółko domknięte o środku
   w punkcie [p] i promieniu [r] *)
let kolko (x1, y1) r = function (x2, y2) ->
  if ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.) <= r *. r then 1 else 0

(* [odbij a b c] zwraca punkt [c'] ktory jest symetryczny do [c] wzgledem
   prostej przechodzacej przez punkty [a] i [b] *)
let odbij (x1, y1) (x2, y2) (x3, y3) =
  if x1 = x2 then (2. *. x1 -. x3, y3)
  else if y1 = y2 then (x3, 2. *. y1 -. y3)
  else
    (* [y = ax + b] to prosta przechodzaca przez [(x1, y1)] i [(x2, y2)] *)
    let a = (y1 -. y2) /. (x1 -. x2) in
    let b = y1 -. a *. x1 in

    (* [y = a2x + b] to prosta przechodzaca przez punkt [(x3, y3)],
       ktora jest prostopadla do prostej [y = ax + b] *)
    let a2 = 0. -. 1. /. a in
    let b2 = y3 -. a2 *. x3 in

    (* [(x4, y4)] to przeciecie prostych [y = ax + b] i [y = a2x + b] *)
    let x4 = (b2 -. b) /. (a -. a2) in
    let y4 = a *. x4 +. b in

    (* Zwraca punkt [(x4, y4)] przesuniety o wektor [(x3, y3)] -> [(x4, y4)] *)
    (2. *. x4 -. x3, 2. *. y4 -. y3)

(* [zloz p1 p2 k] sklada kartke [k] wzdluz prostej przechodzacej
   przez punkty [p1] i [p2] (musza to byc rozne punkty). Papier jest
   skladany w ten sposob, ze z prawej strony prostej (patrzac w kierunku
   od [p1] do [p2]) jest przekladany na lewa. *)
let zloz a b kar = function c ->
  if side a b c < 0. then 0
  else if side a b c = 0. then kar c
  else kar c + kar (odbij a b c)

(* [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = zloz p1_n p2_n (zloz ...
   (zloz p1_1 p2_1 k)...)] czyli wynikiem funkcji jest zlozenie kartki [k]
   kolejno wzdluz wszystkich prostych z listy *)
let skladaj lista kar =
  List.fold_left (fun x (y, z) -> zloz y z x) kar lista

