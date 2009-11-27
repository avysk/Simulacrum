open Money

class chicken =
object
  inherit my_money
  val name = "Chicken"
  method private cooperate_if = (>) 1.75
end
