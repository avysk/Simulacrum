open Definitions
open Idiot
open Friendly_idiot
open Cheater
open Teacher
open Die_hard
open Smartie
open Content
open Greedy
open Chicken
open Robinhood
open Envier
;;

Random.self_init ()

(* Customize here *)
let participants = [ (8,  new cheater) ;
                     (8,  new teacher) ;
                     (56, new idiot) ;
                     (8,  new friendly_idiot) ;
                     (8,  new die_hard) ;
                     (8,  new smartie) ;
                     (8,  new content) ;
                     (8,  new greedy) ;
                     (8,  new chicken) ;
                     (8,  new robinhood) ;
                     (8,  new envier) ] ;;

let total = List.fold_left (fun sum el -> sum + fst el) 0 participants

let players =
  List.fold_left (fun arr el -> Array.append arr
                                  (Array.init (fst el)
                                     (fun _ -> (0, Oo.copy (snd el)))))
    [||] participants

let rec run_match_rec p1 p2 rounds score m1 m2 =
  (*
   * p1, p2 -- strategies to play
   * rounds -- rounds left to play
   * score = (p1's score, p2's score) -- current score
   * m1, m2 -- last moves of p1 and p2 (can be None, None at first round)
   *)
  let upd_score s r = fst s + fst r, snd s + snd r in
    if rounds = 0 then score else
      let move1 = p1#play m2 and
          move2 = p2#play m1 in
      let result = calculate_score move1 move2 in
        run_match_rec p1 p2 (rounds - 1) (upd_score score result) move1 move2

let run_match p1 p2 rounds =
  run_match_rec p1 p2 rounds (0, 0) None None

let game i j =
  let p_i = players.(i) and
      p_j = players.(j) in
  let p1 = snd p_i and
      p2 = snd p_j in
  let s1 = fst p_i and
      s2 = fst p_j in
  let result = run_match p1 p2 rounds_total in
    (* result = (score of p1 after rounds_total rounds, same for p2) *)
    players.(i) <- s1 + fst result, p1 ;
    players.(j) <- s2 + snd result, p2

let play_round _ =
  let () = Array.iteri (fun i p -> players.(i) <- 0, snd p) players in
    Array.iteri (fun i _ -> if i = total - 1 then () else
                   let opps = Array.sub players (i + 1) (total - i - 1) in
                     Array.iteri (fun j _ -> (game i (j + i + 1))) opps) players

let cmp a1 a2 =
  match a1, a2 with
  | (n1, _), (n2, _) when n1 = n2 -> 0
  | (n1, _), (n2, _) -> if (n1 > n2) then 1 else -1

let filter_players _ =
  (*
   * This function kills random player with the lowest score, and clones random
   * player with the highest score.
   *)
  let () = Array.sort cmp players in
  let rec find_loser k v =
    match k, v with
    | _ when k = total -> Random.int total
    | _ when (fst players.(k) > v) -> Random.int k
    | _ -> find_loser (k + 1) v in
  let rec find_winner k v =
    match k, v with
    | -1, _ -> Random.int total
    | _ when (fst players.(k) < v) -> Random.int (total - k - 1) + k + 1
    | _ -> find_winner (k - 1) v in
  let loser' = find_loser 1 (fst players.(0)) in
  let winner = find_winner (total - 2) (fst players.(total - 1)) in
  let loser = if loser' = winner then (total + loser' - 1) mod total else loser' in
    players.(loser) <- (fst players.(winner), Oo.copy (snd players.(winner)))

let do_display _ =
  let hash = Hashtbl.create 5 in
  let count p =
    begin
      let name = (snd p)#get_name in
      let sofar = if Hashtbl.mem hash name then Hashtbl.find hash name else 0 in
        Hashtbl.replace hash name (sofar + 1)
    end
  in
  let () = Array.sort cmp players in
  let () = Array.iter (fun p -> count p) players in
  let () = Hashtbl.iter (fun k v -> print_string k ;
                                    print_string "\t" ;
                                    print_int v ;
                                    print_newline () ) hash in
    print_string "Life level: " ;
    print_int (fst players.(0)) ;
    print_string " - " ;
    print_int (fst players.(total - 1)) ;
    print_newline ()

let rec do_the_job n =
  if (n = 0) then do_display () else
    let () = play_round () in
    let () = filter_players () in
      do_the_job (n - 1) ;;

do_the_job generations
