method_shrinkage = function(X, G0) {
    
    X = scale(X)
    stopifnot( !anyNA(X) )
    
    G_hat = pcor.shrink(X)
    G_hat = abs(G_hat)
    
    return( list(g_hat = G_hat) )
    
}
