open Definitions
open Strategy

class idiot =
object
  inherit strategy
  val name = "Idiot"
  method play _ = if Random.bool () then Cooperate else Cheat
end
