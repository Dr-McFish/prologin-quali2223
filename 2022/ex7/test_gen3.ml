type cord = {x :int; y :int}

let radius = 3

let is_in_circle r pnt = 
  pnt.x*pnt.x + pnt.y*pnt.y <= r*r

let all_points r =
  let rec all_points pnt r =
    if pnt.x = r && pnt.y = r then
      []
    else if pnt.x > r then
      pnt :: (all_points {x= -r; y= pnt.y +1} r)
    else
      pnt :: (all_points {x= pnt.x +1; y= pnt.y} r)
  in
  all_points {x= -r; y = -r} r

let points_in_radius d =
  all_points d
  |> List.filter (is_in_circle d)
  |> List.filter (fun x -> x <> {x=0 ; y=0});;