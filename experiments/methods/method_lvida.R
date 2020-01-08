method_lvida <- function(X, G0, sig_level = 0.01, method = 'local', skel.method = 'stable.fast') {
    
    X = scale(X)
    stopifnot( !anyNA(X) )
    
    res = run_lvida(X, G0, sig_level = sig_level, method = method, skel.method = skel.method)
    
    return(res)
}