open Definitions
open Strategy

class teacher =
object
  inherit strategy
  val name = "Teacher"
  method play m = match m with
      None -> Cooperate (* Cooperate during the first round *)
    |    _ -> m         (* Copy opponent's behavior during the next rounds *)
end
