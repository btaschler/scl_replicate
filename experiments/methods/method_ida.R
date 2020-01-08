method_ida = function(X, G0, skel.method = 'stable.fast') {
    
    X = scale(X)
    stopifnot( !anyNA(X) )
    
    res = run_ida(X, sig_level = 0.01, skel.method = skel.method)

    return(res)

}
