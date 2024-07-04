
(** description d'une position *)
type vec2 = {
  y : int; (** la coordonnée y *)
  x : int; (** la coordonnée x *)
  };;

type vec2f = {
  yf : float; (** la coordonnée y *)
  xf : float; (** la coordonnée x *)
};;

let modulof m f =
  m *. fst (Float.modf ((f +. m) /. m))
;;

let almost_eq f1 f2=
  let epsilon = 1E-9 in
  Float.abs (f1 -. f2) < epsilon
;;

let angle pivot p1 p2 =
  (-.)
  (Float.atan2 (p2.yf -. pivot.yf) (p2.xf -. pivot.xf))
  (Float.atan2 (p1.yf -. pivot.yf) (p1.xf -. pivot.xf))
  |> modulof (2. *. Float.pi)
;;

module Vec2 = struct
  type t = vec2

  let to_float a = {xf= float_of_int a.x; yf= float_of_int a.y}

  (*
     a ---- b
             \
              \
               c
  *)
  let cross_compare a b c =
    let a = (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x) in
    compare a 0
  let cross_compare_sort pivot u v =
    let cmp = cross_compare u pivot v in
    if cmp = 0 then
      if u.x - pivot.x = 0 then
        compare v.y u.y
      else
        compare u.x v.x
    else
      cmp
end;;

let float_to_nearest_int f =
  if f > 0. then
    truncate (f +. 0.5)
  else
    truncate (f -. 0.5);;

module Vec2f = struct
  type t =vec2f
  let zero = {xf = 0.; yf = 0.}

  (*
     a ---- b
             \
              \
               c
  *)
  let cross_compare a b c =
    let a = (b.xf -. a.xf) *. (c.yf -. a.yf) -. (b.yf -. a.yf) *. (c.xf -. a.xf) in
    compare a 0.

  let fuzzy_cross_compare a b c =
    let determinant = (b.xf -. a.xf) *. (c.yf -. a.yf)
                   -. (b.yf -. a.yf) *. (c.xf -. a.xf) in
    let sine = determinant /. (   (Float.hypot (b.xf -. a.xf)  (b.yf -. a.yf))
                               *. (Float.hypot (c.xf -. b.xf)  (c.yf -. b.yf)) )in
    let epsilon =  5e-10 in
    (* Printf.printf "є \t\t= %.60f\n" epsilon; *)
    (* Printf.printf "sine \t\t= %.60f\n" (Float.abs sine);  *)
    (* Printf.printf "determinant \t= %.60f\n\n" (Float.abs determinant);  *)
    if (Float.abs sine) < epsilon then
      0
    else
      compare determinant 0.

  let cross pivot (u:t) (v:t) =
    let a = (u.xf -. pivot.xf) *. (v.yf -. pivot.yf) -. (u.yf -. pivot.yf) *. (v.xf -. pivot.xf) in
    a

  let to_int u = {
    (* converts to nearest int *)
    x = float_to_nearest_int u.xf;
    y = float_to_nearest_int u.yf
  }

  let angle_compare pivot0 pivot1 p1 p2 =
    compare (angle pivot0 pivot1 p1) (angle pivot0 pivot1 p2)
  ;;

  let apply_to_cords op pnt =
    {xf= op pnt.xf; yf = op pnt.yf}

  let to_tuple v =
    (v.xf, v.yf)
  ;;
  let to_tuple (x, y) =
    {xf= x; yf= y}
  ;;
  let almost_eq u v =
    almost_eq u.xf v.xf &&
    almost_eq u.yf v.yf
end;;



let swap ((a:'a) , (b:'a)) =
  (b, a)

exception OnLine
let is_on_same_side_of_line pline1 pline2 p1 p2 =
  (* what hapens when equal/almost equal? TODO *)
  let p1side = compare ((p1.yf -. pline1.yf) *. (pline2.xf -. pline1.xf)) ((p1.xf -. pline1.xf) *. (pline2.yf -. pline1.yf)) in
  let p2side = compare ((p2.yf -. pline1.yf) *. (pline2.xf -. pline1.xf)) ((p2.xf -. pline1.xf) *. (pline2.yf -. pline1.yf)) in
  if (p1side * p2side = 0) then
    raise OnLine
  else
    p1side * p2side > 0 (* different sign <=> different sides of the line *)

let square_int a = a*a

let rec stack_to_list stack =
  if Stack.is_empty stack then
    []
  else
    let head = Stack.pop stack in
    head :: stack_to_list stack

let rec list_minimum cmp l =
  match l with
  | [] -> failwith "Empty list: no minimum"
  | head :: [] -> head
  | head :: tail -> let tail_min = list_minimum cmp tail in
                    if 0 > cmp head tail_min then head else tail_min;;

(** Grahm scan algorithm *)
let convex_hull_of (points : vec2 list) =
  let pivot = list_minimum compare points in
  let points_ar =
    points
    |> List.filter (fun x -> x <> pivot)
    |> List.sort (Vec2.cross_compare_sort pivot)
    |> fun points_sorted ->  points_sorted @ [pivot]
    |> Array.of_list
  in
  let result = Stack.create () in
  Stack.push pivot result;
  Stack.push points_ar.(0) result;
  for i = 1 to Array.length points_ar -1 do
    let point0 = points_ar.(i) in
    let point1 = ref (Stack.pop result) in
    let point2 = ref (Stack.top result) in
    while Vec2.cross_compare !point2 !point1 point0 < 0 do
      point1 := Stack.pop result;
      point2 := Stack.top result;
    done;
    Stack.push !point1 result;
    Stack.push point0 result
  done;
  let last = Stack.top result in
  (* Printf.printf "%d, %d\n" last.x last.y; *)
  assert (pivot = last);
  let rec rm_colinear points =
    match points with
    | [] -> []
    | [head]-> [head]
    | [h1;h2] -> [h1;h2]
    | h1 :: h2 :: h3 :: tail ->
      if 0 = Vec2.cross_compare h1 h2 h3 then
        rm_colinear (h1 :: h3 :: tail)
      else
        h1 :: rm_colinear (h2 :: h3 :: tail)
  in
  result
  |> stack_to_list
  |> rm_colinear
  |> List.tl

(**
   @param d le rayon de l'espace-temps connu
   @param n le nombre de points de contrôle existants (sans compter celui sur lequel vous vous situez actuellement)

   @param pointsDeControles la liste des coordonnées points de contrôle existants
*)
let aretesMinimales (d:int) (n:int) points_de_controles =
  (** TODO Afficher, sur une ligne, le nombre d'arêtes minimal de la zone de
  sécurité après la création d'un nouveau point de contrôle dans l'espace-temps
  connu.  *)
  let df = float_of_int d in
  let convex_hull_l = (convex_hull_of points_de_controles) in
  if (List.length convex_hull_l) <= 3 then
    max 2 (List.length convex_hull_l)
    (* Vec2f.zero *)
  else
  let convex_hull = Array.of_list convex_hull_l in
  (* let convex_hull_f = Array.map Vec2.to_float convex_hull in *)
  let plus_m i j =
    let m = (i+j) mod (Array.length convex_hull) in
    if m < 0 then
      Array.length convex_hull + m
    else
      m
  in
  for i = 0 to Array.length convex_hull -1 do
    let ii =  plus_m i 1 in
    let iii = plus_m ii 1 in
    assert (Vec2.cross_compare convex_hull.(i) convex_hull.(ii) convex_hull.(iii) < 0)
  done;

    (* p1 p2 characterise a line, d is the radius of the circle cetered at 0. assumes p1 p2 are both inside the circle(or at least that the intersection has 2 points) *)
  let intersection_with_circle p1 p2 df =
    let a_x = float_of_int p1.x in
    let a_y = float_of_int p1.y in
    let b_x = float_of_int p2.x in
    let b_y = float_of_int p2.y in
    if p1.x = p2.x then
      let plus_minus_y = Float.sqrt ((df *. df) -. (a_x *. a_x)) in
      ({xf = a_x; yf = plus_minus_y}, {xf = a_x; yf = -1. *.plus_minus_y})
    else
      let m = (b_y -. a_y) /. (b_x -. a_x) in
      let c = a_y -. m *. a_x in
      let reduced_delta = sqrt ((m*.m +. 1.)*.df*.df -. c*.c) in
      let x_1 = (reduced_delta -. m *. c) /. (m*.m +. 1.) in
      let x_2 = (-1. *. reduced_delta -. m *. c) /. (m*.m +. 1.) in
      let y_of x = m *. (x -. a_x) +. a_y in
      ({xf= x_1; yf= y_of x_1}, {xf= x_2; yf= y_of x_2})
  in
  let center_of_mass =
    let sum = List.fold_left (fun pnt1 -> fun pnt2 -> {x = pnt1.x + pnt2.x; y = pnt1.y + pnt2.y}) {x=0; y=0} convex_hull_l in
    sum
    |> Vec2.to_float
    |> (Vec2f.apply_to_cords (fun f -> Float.div f (float_of_int (Array.length convex_hull)) ))
  in
  let find_intersections convex_hull =
    let rec loopr i result =
      if i >= Array.length convex_hull then result
      else
        let ii =  plus_m i 1 in
        let (intersec_1, intersec_2) = intersection_with_circle convex_hull.(i) convex_hull.(ii) df in
        let angle = angle center_of_mass intersec_2 intersec_1 in
        assert (not (almost_eq angle Float.pi));
        let (intersec_in, intersec_out) = if angle < Float.pi (*+. 5.E-10 *)
          then swap (intersec_1, intersec_2)
          else      (intersec_1, intersec_2)
        in
        let (res_in, res_out) = result in
        let new_res_in  = intersec_in  :: res_in in
        let new_res_out = intersec_out :: res_out in
        loopr (i+1) (new_res_in, new_res_out)
    in loopr 0 ([],[])
  in
  let (in_points, out_points) =
    find_intersections convex_hull
    |> (fun (x,y) ->  List.sort (Vec2f.angle_compare {xf = 0.; yf = 0.} {xf = 0.;  yf= -.df}) x,
                      List.sort (Vec2f.angle_compare {xf = 0.; yf = 0.} {xf = 0.;  yf= -.df}) y)
  in
  assert (List.length in_points = List.length out_points);
  let begining =
    if List.exists (Vec2f.almost_eq {xf = 0.;  yf= -.df}) in_points ||
       List.exists (Vec2f.almost_eq {xf = 0.;  yf= -.df}) out_points
      then (
      (* Printf.printf "almost!"; *)
      {yf = (-1.) *. df *. (Float.cos (-1E-9)); xf= df *. (Float.sin (-1E-9))} )
    else (
      (* Printf.printf "no intersect!"; *)
      {xf = 0.;  yf= -.df})
  in
  let convex_hull_f = Array.map Vec2.to_float convex_hull in
  let visible_sides_from_begining =
    let rec loop i res_visible_sides =
      if i < Array.length convex_hull then
        let ii = plus_m i 1 in
        loop (i+1) begin
          (* try  *)
          assert (convex_hull_f.(i) <> convex_hull_f.(ii));
          if is_on_same_side_of_line convex_hull_f.(i) convex_hull_f.(ii) center_of_mass begining then
            res_visible_sides
          else
            res_visible_sides + 1
          (* with OnLine -> failwith (Printf.sprintf "%d" i) *)
        end
      else
        res_visible_sides
    in loop 0 0
  in
  let max_reduction =
    let rec loop reduction res_maxred in_points_stack out_points_stack : int=
      (* Printf.printf "reduction : %d |" reduction; *)
      match in_points_stack, out_points_stack with
      | in_point :: in_rest, out_point :: out_rest ->
        if Vec2f.almost_eq in_point out_point then
          let new_reduction = reduction +1 in
          (* Printf.printf "border pnt : %.1f, %.1f |\n" in_point.xf in_point.yf; *)
          loop reduction (max new_reduction res_maxred) in_rest out_rest
        else if 0 > Vec2f.angle_compare {xf = 0.; yf = 0.} begining in_point out_point then begin
          (* in_point commes before outpoint anglewise ccw *)
          (* Printf.printf "in pnt : %.1f, %.1f |\n" in_point.xf in_point.yf; *)
          let new_reduction = reduction +1 in
          loop new_reduction (max new_reduction res_maxred) in_rest out_points_stack
        end else begin
          assert (0 < Vec2f.angle_compare {xf = 0.; yf = 0.} begining in_point out_point);
          (* in_point commes after outpoint anglewise ccw *)
          (* Printf.printf "out pnt : %.1f, %.1f |\n" out_point.xf out_point.yf; *)
          let new_reduction = reduction - 1 in
          loop new_reduction (max new_reduction res_maxred) in_points_stack out_rest
        end
      | in_point :: in_rest, [] -> 
        (* Printf.printf "in pnt : %.1f, %.1f |\n" in_point.xf in_point.yf; *)
        let new_reduction = reduction +1 in
        loop new_reduction (max new_reduction res_maxred) in_rest []
      | [], out_point :: out_rest ->
        (* Printf.printf "out pnt : %.1f, %.1f |\n" out_point.xf out_point.yf; *)
        let new_reduction = reduction - 1 in
        loop new_reduction (max new_reduction res_maxred) [] out_rest
      | [],[] -> res_maxred
    in
    loop (visible_sides_from_begining -2 ) 0 in_points out_points
  in
  let ans = (List.length convex_hull_l) - max_reduction in
  assert (ans >= 3);
  ans
;;

let d = Scanf.sscanf (input_line stdin) "%d" (fun (x) -> x)
let n = Scanf.sscanf (input_line stdin) "%d" (fun (x) -> x)
let points_de_controles = 
  List.init n (fun _ -> Scanf.sscanf (input_line stdin) "%d %d" (fun x y -> {y; x}))
  |> List.cons {x= 0; y= 0}
let res = aretesMinimales d n (points_de_controles)


let () = 
  print_int res;
  print_newline ();
  flush stdout