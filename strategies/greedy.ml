open Money

class greedy =
object
  inherit my_money
  val name = "Greedy"
  method private cooperate_if = (<=) 3.0
end
