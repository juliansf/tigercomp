signature TigerGraph =
sig
    type graph
    type node
    
    val nodes: graph -> node list
    val succ: node -> node list
    val pred: node -> node list
    val adj: node -> node list   (* succ+pred *)
    val eq: node * node -> bool
    val comp: node * node -> order
    val is_adj : node * node -> bool

    val newGraph: unit -> graph
    val newNode : graph -> node
    exception GraphEdge
    val mk_edge: {from: node, to: node} -> unit
    val rm_edge: {from: node, to: node} -> unit
    
    type 'a table = (node, 'a) tigertab.Tabla
    val newTable : unit -> 'a table

    val nodename: node->string  (* for debugging only *)
    val printGraph : bool -> graph -> unit (* for debugging only *)
end
