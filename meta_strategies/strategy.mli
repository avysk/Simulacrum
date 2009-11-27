class virtual strategy :
object
  val virtual name : string
  method get_name : string
  method virtual play: Definitions.move -> Definitions.move
end
