
#' Check that entries are binary in {0,1}
#' @export
check_binary <- function(M) {
    Condition <- all( unique_vals(M) %in% c(0, 1) )
    return( stopifnot(Condition) )
}


#' Binarize entries
#' @export
binarize <- function(M) {
    
    M <- M*1
    M[M > 1] <- 1
    
    check_binary(M)
    
    return(M)
}