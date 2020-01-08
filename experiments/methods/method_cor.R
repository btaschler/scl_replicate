method_cor = function(X, G0, method = "pearson") {

    X = scale(X)
    stopifnot( !anyNA(X) )
    
    corX  = cor(X, method = method)
    G_hat = abs(corX)
    
    return( list(g_hat = G_hat) )

}
