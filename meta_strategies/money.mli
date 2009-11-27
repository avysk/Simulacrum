class virtual my_money :
object
  inherit Strategy.strategy
  method private virtual cooperate_if : float -> bool
  method play : Definitions.move -> Definitions.move
end

class virtual opponent_money :
object
  inherit Strategy.strategy
  method private virtual cooperate_if : float -> bool
  method play : Definitions.move -> Definitions.move
end
