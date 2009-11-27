open Definitions
open Strategy

class cheater =
object
  inherit strategy
  val name = "Cheater"
  method play _ = Cheat
end

