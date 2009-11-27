open Money

class content =
object
  inherit my_money
  val name = "Content"
  method private cooperate_if = (<=) 1.75
end
