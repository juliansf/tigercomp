structure TigerSet :> TigerSet =
struct
	structure Set = Splayset
	
	type 'item set = 'item Set.set ref

	exception NotFound = Set.NotFound
	exception Empty
	
	fun empty cmp = ref (Set.empty cmp)
	fun singleton cmp x = ref (Set.singleton cmp x)
	fun add (s,x) = s := Set.add (!s, x)
	fun addList (s,xs) = s:= Set.addList (!s, xs)
	fun retrieve (s,x) = Set.retrieve (!s,x)
	fun peek (s,x) = Set.peek (!s,x)
	fun getElement s = 
		case Set.find (fn x => true) (!s) of 
			SOME x => x 
		| NONE => raise Empty
	fun getMaxElement cmp s =
		let val m = getElement s in
			Set.foldr (fn (n, max) => 
				case cmp (n,max) of GREATER => n | _ => max) m (!s)
		end
	fun isEmpty s = Set.isEmpty (!s)
	fun equal (s,s') = Set.equal (!s, !s')
	fun isSubset (s,s') = Set.isSubset (!s, !s')
	fun member (s,x) = Set.member (!s,x)
	fun delete (s,x) = s := Set.delete (!s, x)
	fun numItems s = Set.numItems (!s)
	fun union (s,s') = ref (Set.union (!s, !s'))
	fun intersection (s,s') = ref (Set.intersection (!s, !s'))
	fun difference (s,s') = ref (Set.difference (!s, !s'))
	fun listItems s = Set.listItems (!s)
	fun app f s = Set.app f (!s)
	fun revapp f s = Set.revapp f (!s)
	fun foldr f e s = Set.foldr f e (!s)
	fun foldl f e s = Set.foldl f e (!s)
	fun find f s = Set.find f (!s)
	fun print pp s = 
		let
			fun print' [] = ""
			 |	print' [x] = pp x
			 |	print' (x::(xs as y::xs')) = pp x ^ ", " ^ print' xs
		in 
			BasicIO.print ("{" ^ print' (Set.listItems (!s)) ^ "}")
		end
end