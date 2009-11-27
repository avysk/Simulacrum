open Money

class envier =
object
  inherit opponent_money
  val name = "Envier"
  method private cooperate_if = (>) 1.75
end 

