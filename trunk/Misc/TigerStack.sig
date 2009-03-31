signature TigerStack =
sig
	type 'a stack
	
	val new : unit -> 'a stack
	val push : 'a stack -> 'a -> unit
	val pop : 'a stack -> 'a option
	val top : 'a stack -> 'a option
	val isEmpty : 'a stack -> bool
	val toList : 'a stack -> 'a list
end