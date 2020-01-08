method_gies = function(X, G0) {
    
    X = scale(X)
    stopifnot( !anyNA(X) )
    
    res = run_gies(X, G0)
    
    return(res)
    
}