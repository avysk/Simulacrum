open Definitions ;;

class virtual strategy =
(* all playing strategies are inherited from this class *)
object
  val virtual name : string (* name of the strategy to print in results *)
  method get_name = name
  method virtual play : move -> move
  (* Input: the move played by the opponent during the previous round.
   * Output: the move to play this round. *)
end

class type strategy_t =
object
  method get_name : string
  method play : Definitions.move -> Definitions.move
end
