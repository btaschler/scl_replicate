
#' @export
get_index_mapping = function(n, p) {
    
    K = cbind(
        "ij" = 1:(n*p),
        "i"  = rep(1:n, each = p), 
        "j"  = rep(1:p, times = n)
    )
    
    return(K)
    
}


#' @export
get_linear_index = function(K, i, j) {
    return(
        which( K[ ,"i"] == i & K[ ,"j"] == j )
    )
}


#' @export
get_array_index = function(K, ij) {
    return(
        K[ij, c("i", "j")]
    )
}

