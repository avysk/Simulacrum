open Definitions
open Strategy

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
      None              -> income <- 0.0; moves <- 0.0; my_last <- Cooperate; Cooperate
    | (Cheat|Cooperate) -> income <- (income +. match my_last, m with
                                          Cooperate, Cooperate ->  coco
                                        | Cheat,     Cooperate ->  chco
                                        | Cooperate, Cheat     ->  coch
                                        | _                    ->  chch) ;
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
