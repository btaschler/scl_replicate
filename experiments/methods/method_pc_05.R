method_pc_05 = function(X, G0, sig_level = 0.5, skel.method = 'stable.fast') {
    
    X = scale(X)
    stopifnot( !anyNA(X) )
    
    res = run_pc(X, sig_level = sig_level, skel.method = skel.method)
    
    return(res)
    
}