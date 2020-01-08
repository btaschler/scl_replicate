
#' Get transitive closure from adjacency matrix
#' @param G Adjacency matrix
#' @return Transitive closure matrix
#' @export
get_transitive_closure = function(G) {
    
    TC = nem::transitive.closure(G, mat=TRUE, loops=FALSE)
    return( TC )
    
}

