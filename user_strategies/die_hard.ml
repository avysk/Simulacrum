open Definitions
open Strategy

class die_hard =
object
  inherit strategy
  val name = "Die hard"
  val mutable cheated = false (* To remember if opponent cheated *)
  method play m =
    match m with
    | None      -> cheated <- false; Cooperate (* Never cheat first *)
      (* and cheat if opponent cheated at least once before *)
    | Cheat     -> cheated <- true; Cheat
    | Cooperate -> if (cheated) then Cheat else Cooperate
end

