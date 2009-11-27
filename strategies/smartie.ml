open Money

class smartie =
object
  inherit my_money
  val name = "Smartie"
  method private cooperate_if = (<=) 2.0
end
