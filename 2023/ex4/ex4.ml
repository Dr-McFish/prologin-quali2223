
let compare_with_errors (str_a : string) (str_b : string) : bool =
  let rec aux_compare l1 l2 i errors_allowed =
    if i < String.length l1 then
      if l1.[i] = l2.[i] then
        aux_compare l1 l2 (i + 1) errors_allowed
      else
        (errors_allowed > 0) && aux_compare l1 l2 (i + 1) (errors_allowed - 1)
    else
      true
  in
  String.length str_a = String.length str_b &&
 aux_compare str_a str_b 0 3

(**
   @param nom nom erroné noté sur le colis
   @param n nombre de boîtes aux lettres nom erroné noté sur le colis
   @param noms N noms de familles
*)
let boiteAuxLettres nom n noms =
  (** TODO affichez le nom corrigé tel qu'il est noté sur la boîte aux lettres.
  *)
  List.find (compare_with_errors nom) noms

let () =
  let nom = read_line () in
  let n = read_int () in
  let noms = List.init n (fun _ -> read_line ()) in
  print_endline (boiteAuxLettres nom n noms);
  flush stdout
