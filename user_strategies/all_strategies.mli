type strategy_t =
    < get_name : unit -> string ; play : Definitions.move -> Definitions.move >

class idiot          : strategy_t
class friendly_idiot : strategy_t
class cheater        : strategy_t
class teacher        : strategy_t
class die_hard       : strategy_t
class smartie        : strategy_t
class content        : strategy_t
class greedy         : strategy_t
class chicken        : strategy_t
class robinhood      : strategy_t
class envier         : strategy_t
