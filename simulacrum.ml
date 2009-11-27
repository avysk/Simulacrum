Random.self_init ()

(* the number of rounds players play against each other during a yer *)
let rounds_total = 10
(* the number of years to simulate *)
let generations = 200 

type move = None | Cooperate | Cheat

class virtual strategy =
(* all playing strategies are inherited from this class *)
object
  val virtual name : string (* name of the strategy to print in results *)
  method get_name = name
  method virtual play : move -> move
  (* Input: the move played by the opponent during the previous round.
   * Output: the move to play this round. *)
end

class idiot =
object
  inherit strategy
  val name = "Idiot"
  method play _ = if Random.bool () then Cooperate else Cheat
end

class friendly_idiot =
object
  inherit strategy
  val name = "Friendly idiot"
  method play _ = Cooperate
end

class cheater =
object
  inherit strategy
  val name = "Cheater"
  method play _ = Cheat
end

class teacher =
object
  inherit strategy
  val name = "Teacher"
  method play m = match m with
      None -> Cooperate (* Cooperate during the first round *)
    |    _ -> m         (* Copy opponent's behavior during the next rounds *)
end

class die_hard =
object
  inherit strategy
  val name = "Die hard"
  val mutable cheated = false (* To remember if opponent cheated *)
  method play m = match m with
      None      -> cheated <- false; Cooperate (* Never cheat first *)
      (* and cheat if opponent cheated at least once before *)
    | Cheat     -> cheated <- true; Cheat
    | Cooperate -> if (cheated) then Cheat else Cooperate
end

class virtual count_money =
(* The base class for the strategies which count average earnings per round *)
object (self)
  inherit strategy
  val mutable my_last = None (* last play of this strategy *)
  val mutable income = 0.0 (* average income per round *)
  val mutable moves = 0.0  (* number of rounds played *)
  val virtual coco : float (* Cooperate-Cooperate profit *)
  val virtual chco : float (* Cheat-Cooperate profit *)
  val virtual coch : float (* Cooperate-Cheat profit *)
  val virtual chch : float (* Cheat-Cheat profit *)
  (* Constants above should be defined in final strategy classes *)
  method private virtual cooperate_if : float -> bool
  (* Cooperate_if takes **some** parameter as input and should return true if
   * the strategy is going to Cooperate; false otherwise.
   * MUST be implemented. *) 
  method play m = match m with
      None      -> income <- 0.0; moves <- 0.0; my_last <- Cooperate; Cooperate
    | _         -> income <- (income +. match my_last, m with
                                  Cooperate, Cooperate ->  coco
                                | Cheat,     Cooperate ->  chco
                                | Cooperate, Cheat     ->  coch
                                | _                    ->  chch ) ;
                              moves <- (moves +. 1.0) ; 
                              if self#cooperate_if (income /. moves) then
                                my_last <- Cooperate
                              else
                                my_last <- Cheat;
                              my_last
end

class virtual my_money =
(* The base class for strategies which track own average earnings per round *)
object
  inherit count_money
  val coco =  3.0
  val chco =  5.0
  val coch =  0.0
  val chch = -1.0
end

class virtual opponent_money =
(* The base class for strategies which track opponent's average earnings per
 * round *)
object
  inherit count_money
  val coco =  3.0
  val chco =  0.0
  val coch =  5.0
  val chch = -1.0
end

class smartie =
object
  inherit my_money
  val name = "Smartie"
  method private cooperate_if = (<=) 2.0
end

class content =
object
  inherit my_money
  val name = "Content"
  method private cooperate_if = (<=) 1.75
end

class greedy =
object
  inherit my_money
  val name = "Greedy"
  method private cooperate_if = (<=) 3.0
end

class chicken =
object
  inherit my_money
  val name = "Chicken"
  method private cooperate_if = (>) 1.75
end

class robinhood =
object
  inherit opponent_money
  val name = "Robin Hood"
  method private cooperate_if = (>) 0.0
end

class envier =
object
  inherit opponent_money
  val name = "Envier"
  method private cooperate_if = (>) 1.75
end 

(* Customize here *)
let participants = [ (8,  new cheater) ;
                     (0,  new teacher) ;
                     (56, new idiot) ;
                     (0,  new friendly_idiot) ;
                     (0,  new die_hard) ;
                     (0,  new smartie) ;
                     (0,  new content) ;
                     (0,  new greedy) ;
                     (0,  new chicken) ;
                     (0,  new robinhood) ;
                     (0,  new envier) ] ;;

let total = List.fold_left (fun sum el -> sum + fst el) 0 participants

let players =
  List.fold_left (fun arr el -> Array.append arr
                                  (Array.init (fst el)
                                     (fun i -> (0, Oo.copy (snd el)))))
    (Array.make 0 (0, new cheater))
    participants

(* 
 * Those below are different from coco, coch, chco and chch defined above. The
 * numbers here are The Rules of The Game; the coco, coch, chco and chch are
 * merely understandings of the given strategy about outcomes.
 *)
let calculate_score move1 move2 = 
  match move1, move2 with
      Cooperate, Cooperate ->  3,  3
    | Cooperate, Cheat     ->  0,  5
    | Cheat,     Cooperate ->  5,  0
    | _                    -> -1, -1

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
    Array.iteri (fun i pl -> if i = total - 1 then () else
                   let opps = Array.sub players (i + 1) (total - i - 1) in
                     Array.iteri (fun j opp -> (game i (j + i + 1))) opps) players

let cmp a1 a2 = match a1, a2 with
    (n1, _), (n2, _) when n1 = n2 -> 0
  | (n1, _), (n2, _) -> if (n1 > n2) then 1 else -1

let filter_players _ =
  (* 
   * This function kills random player with the lowest score, and clones random
   * player with the highest score.
   *)
  let () = Array.sort cmp players in
  let rec find_loser k v = match k, v with
      _ when k = total -> Random.int total
    | _ when (fst players.(k) > v) -> Random.int k
    | _ -> find_loser (k + 1) v in
  let rec find_winner k v = match k, v with
      -1, _ -> Random.int total
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
