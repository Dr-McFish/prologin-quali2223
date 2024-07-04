(**
   @param d distance entre les deux planètes
   @param m distance à laquelle Joseph s'est téléporté
   @param n nombre de trous noirs
   @param positions liste des positions des trous noirs
*)
let state d m n positions =
  (** TODO Retourne l'état de Joseph Marchand. -1 s'il a été happé par un trou
  noir, 0 s'il s'est perdu en chemin, 1 s'il est arrivé à destination. *)
  if d = m then
    1
  else if List.exists ((=) m) positions then (* le position de d est un trou noir *)
    -1
  else
    0

let () =
  let d = read_int () in
  let m = read_int () in
  let n = read_int () in
  let positions = read_line () 
                  |> fun x -> if x = "" then 
                                []
                              else String.split_on_char ' ' x 
                                   |> List.rev_map int_of_string 
                                   |> List.rev 
    in
  print_int (state d m n positions);
  flush stdout