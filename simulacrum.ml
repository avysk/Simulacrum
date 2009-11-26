open Random ;;
Random.self_init () ;;

let rounds_total = 10
let generations = 200 

type move = None | Cooperate | Cheat

class virtual strategy =
object
  val virtual name : string 
  method get_name = name
  method virtual play : move -> move
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
      None -> Cooperate
    |    _ -> m
end

class die_hard =
object
  inherit strategy
  val name = "Die hard"
  val mutable cheated = false
  method play m = match m with
      None      -> cheated <- false; Cooperate
    | Cheat     -> cheated <- true; Cheat
    | Cooperate -> if (cheated) then Cheat else Cooperate
end

class virtual count_money =
object (self)
  inherit strategy
  val mutable my_last = None
  val mutable income = 0.0
  val mutable moves = 0.0
  val virtual coco : float
  val virtual chco : float
  val virtual coch : float
  val virtual chch : float
  method private virtual cooperate_if : float -> bool
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
                                my_last <- Cheat ;
                              my_last
end

class virtual my_money =
object
  inherit count_money
  val coco =  3.0
  val chco =  5.0
  val coch =  0.0
  val chch = -1.0
end

class virtual opponent_money =
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

let calculate_score move1 move2 = 
  match move1, move2 with
      Cooperate, Cooperate ->  3,  3
    | Cooperate, Cheat     ->  0,  5
    | Cheat,     Cooperate ->  5,  0
    | _                    -> -1, -1

let rec run_match_rec p1 p2 rounds score m1 m2 =
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
