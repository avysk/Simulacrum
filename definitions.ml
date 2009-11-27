(* the number of rounds players play against each other during a yer *)
let rounds_total = 10
(* the number of years to simulate *)
let generations = 200 

type move = None | Cooperate | Cheat

let calculate_score move1 move2 = 
  match move1, move2 with
      Cooperate, Cooperate ->  3,  3
    | Cooperate, Cheat     ->  0,  5
    | Cheat,     Cooperate ->  5,  0
    | _                    -> -1, -1
