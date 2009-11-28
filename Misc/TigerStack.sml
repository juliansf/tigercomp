structure TigerStack :> TigerStack =
struct
	type 'a stack = 'a list ref
	
	fun new () = ref []
	fun push s n = s := n::(!s)
	fun pop s = case !s of [] => NONE | n::s' => (s := s'; SOME n)
	fun top s = case !s of [] => NONE | n::s' => SOME n
	fun isEmpty s = case !s of [] => true | _ => false
	fun toList s = !s
end