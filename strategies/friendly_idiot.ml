open Definitions
open Strategy

class friendly_idiot =
object
  inherit strategy
  val name = "Friendly idiot"
  method play _ = Cooperate
end
