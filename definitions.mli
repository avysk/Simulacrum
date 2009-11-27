val rounds_total : int
val generations : int
type move = None | Cooperate | Cheat
(* Hickey pg 114-115: "For the type to be transparent, the interface simply provides
 * the definition. The implementation must contain the same definition." *)
val calculate_score : move -> move -> int * int 

