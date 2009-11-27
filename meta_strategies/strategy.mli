class virtual strategy :
object
  val virtual name : string
  method get_name : string
  method virtual play: Definitions.move -> Definitions.move
end

class type strategy_t =
object
  method get_name : string
  method play : Definitions.move -> Definitions.move
end
