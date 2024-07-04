
(** description d'une position *)
type vec2 = {
  y : int; (** la coordonnée y *)
  x : int; (** la coordonnée x *)
  };;
  
type vec2f = {
  yf : float; (** la coordonnée y *)
  xf : float; (** la coordonnée x *)
};;


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
end;;

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
  if (List.length convex_hull_l) < 3 then
    max 2 (List.length convex_hull_l)
  else
  let convex_hull = Array.of_list convex_hull_l in
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
  
  let convex_hull_f = Array.map Vec2.to_float convex_hull in
  (* For every edge of teh polygon, the line that exteds from the edge intersects the big circle twice.
  P1 and P2 are the vertecies of edge, and H1, H2 are the interseded points on the circle
  they apear on the the line int this order : H1 -- P1 -- P2 -- H2
     Oparate on H1: construct a new polygon by replacing P1 with H1. remove point next to H1 (that is mirror of P2) until 
  the new polygon is convex. keep track on the number of points removed. Repeat the operation for H2 symetricly/.
     By repeating this for all edges, we can find the maximum number of points removed. Number of points in the 
    initial convex hull - this minimum = answer *)
  
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
  let is_intersection_of_circle_and_line p1 p2 (d :int) h :bool =
      (* p1, p2 is a line, d is a radius of a*)
    let on_circle = ((square_int h.x) + ((square_int h.y)) = square_int d) in
    let on_line = (0 = Vec2.cross_compare p1 p2 h) in
    on_circle && on_line
  in
  let max_reduction = ref 0 in
  (try
    for i = 0 to Array.length convex_hull -1 do
      let ii = plus_m i 1 in
      let (h, h') = 
        intersection_with_circle convex_hull.(i) convex_hull.(ii) df 
        |> fun (h1, h2) -> if compare h1 h2 < 0 then (h1, h2) else (h2, h1) in
      let (h1,p1,p2,h2) = 
      if convex_hull.(i) < convex_hull.(ii) then
        h, convex_hull.(i), convex_hull.(ii), h'
      else
        h', convex_hull.(ii), convex_hull.(i), h
      in
      (* Printf.printf "(%1.3f, %1.3f) - (%1.3f, %1.3f)\n" h1.xf h1.yf h2.xf h2.yf ; *)
      let (h1_int, h2_int) = (Vec2f.to_int h1, Vec2f.to_int h2) in
      let (p1_int, p2_int) = convex_hull.(i), convex_hull.(ii) in
      let red1 = ref 0 in
      let j = ref (plus_m ii 1) in
      if is_intersection_of_circle_and_line p1_int p2_int d h2_int then 
        (* h2 is an interger; do interger subroutine *)
        while (Vec2.cross_compare h2_int convex_hull.(!j) 
                                         convex_hull.(plus_m !j 1)) >= 0 do
          j := plus_m !j 1;
          red1 := !red1 + 1
        done
      else 
        (* h2 is NOT an interger; do float subroutine *)
        while (Vec2f.fuzzy_cross_compare h2 convex_hull_f.(!j) 
                                      convex_hull_f.(plus_m !j 1)) >= 0 do
          j := plus_m !j 1;
          red1 := !red1 + 1
        done;
      
      let red2 = ref 0 in
      j := plus_m i (-1);
      if is_intersection_of_circle_and_line p1_int p2_int d h1_int then
        (* h1 is an interger; do interger subroutine *)
        while (Vec2.cross_compare convex_hull.(plus_m !j (-1)) 
                                   convex_hull.(!j)  h1_int) >= 0 do
          j := plus_m !j (-1);
          red2 := !red2 + 1
        done
      else
        (* h1 is NOT an interger; do float subroutine *)
        (* (Printf.printf "%.100f\n" (Vec2f.cross convex_hull_f.(plus_m !j (-1)) convex_hull_f.(!j)  h1); *)
        while (Vec2f.fuzzy_cross_compare convex_hull_f.(plus_m !j (-1)) 
                                   convex_hull_f.(!j)  h1) >= 0 do
          j := plus_m !j (-1);
          red2 := !red2 + 1
        done;
      let red = max !red1 !red2 in
      max_reduction := max !max_reduction red;
      if (!max_reduction = 3) then
        raise (Failure("early loop exit"))
    (* Printf.printf "(%d, %d) - (%d, %d) -> (%f, %f), (%f, %f)\n" 
                 p1.x p1.y  p2.x p2.y    h1.xf h1.yf  h2.xf h2.yf; *)
    done
  with (Failure(_)) -> ());
 
  Array.length convex_hull - !max_reduction
;;

let d = Scanf.sscanf (input_line stdin) "%d" (fun (x) -> x)
let n = Scanf.sscanf (input_line stdin) "%d" (fun (x) -> x)
let points_de_controles = {x= 0; y= 0} :: (List.init n (fun _ -> Scanf.sscanf (input_line stdin) "%d %d" (fun x y -> {y; x})))

let () = 
  let ans = aretesMinimales d n (points_de_controles) in
  print_int ans;
  print_newline ();

 