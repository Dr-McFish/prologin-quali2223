
(**
   @param s l'action de l'être céleste 1
   @param t l'action de l'être céleste 2
*)
let meteoriteVaguesVent s t =
  (** TODO Afficher l'entier correspondant au joueur gagnant (1 ou 2) ou 0 en
  cas d'égalité *)
  (s - t + 3) mod 3

let () =
  let s = read_int () in
  let t = read_int () in
  print_int (meteoriteVaguesVent s t);
  flush stdout
