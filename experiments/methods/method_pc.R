method_pc = function(X, G0, sig_level = 0.01, skel.method = 'stable') {
    
    X = scale(X)
    stopifnot( !anyNA(X) )
    
    res = run_pc(X, sig_level = sig_level, skel.method = skel.method)
    
    return(res)
    
}