open Money

class robinhood =
object
  inherit opponent_money
  val name = "Robin Hood"
  method private cooperate_if = (>) 0.0
end
