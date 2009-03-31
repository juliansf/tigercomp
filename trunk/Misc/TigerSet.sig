signature TigerSet =
sig
	type 'item set

	exception NotFound
	exception Empty
	
	val empty        : ('_item * '_item -> order) -> '_item set
	val singleton    : ('_item * '_item -> order) -> '_item -> '_item set
	val add          : '_item set * '_item -> unit
	val addList      : '_item set * '_item list -> unit
	val retrieve     : 'item set * 'item -> 'item
	val peek         : 'item set * 'item -> 'item option
	val getElement	 : 'item set -> 'item
	val isEmpty      : 'item set -> bool
	val equal        : 'item set * 'item set -> bool
	val isSubset     : 'item set * 'item set -> bool
	val member       : 'item set * 'item -> bool
	val delete       : '_item set * '_item -> unit
	val numItems     : 'item set ->  int
	val union        : '_item set * '_item set -> '_item set
	val intersection : '_item set * '_item set -> '_item set
	val difference   : '_item set * '_item set -> '_item set
	val listItems    : 'item set -> 'item list
	val app          : ('item -> unit) -> 'item set -> unit
	val revapp       : ('item -> unit) -> 'item set -> unit
	val foldr        : ('item * 'b -> 'b) -> 'b -> 'item set -> 'b
	val foldl        : ('item * 'b -> 'b) -> 'b -> 'item set -> 'b
	val find         : ('item -> bool) -> 'item set -> 'item option
	val print				 : ('item -> string) -> 'item set -> unit
end