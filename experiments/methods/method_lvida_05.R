method_lvida_05 <- function(X, G0, sig_level = 0.5, method = 'local', skel.method = 'stable') {
    
    X = scale(X)
    stopifnot( !anyNA(X) )
    
    res = run_lvida(X, G0, sig_level = sig_level, method = method, skel.method = skel.method)
    
    return(res)
}