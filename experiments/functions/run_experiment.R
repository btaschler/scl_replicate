run_experiment = function(X, G0, G_star, method, db, db_name, ...) {

    # Method name -> callable function
    method_fcn = get(method)

    # Run
    elapsed_time = system.time({
        res = method_fcn(X, G0)
    })
    
    for (i in seq_along(res)) {
        res[[i]] = res[[i]][1:nrow(G0), ]
    }

    # Get elapsed time
    names(elapsed_time) = NULL
    time = elapsed_time[3]

    # Problem variables
    res$X      = X
    res$G0     = G0
    res$G_star = G_star
    
    # Save
    res$method = method
    res$time = time
    res = c(res, list(...))
    
    # Add results to database
    filehash::dbInsert(db, db_name, res)

    #closeAllConnections()                                                       ## INTERFERES WITH KNITTING R-MARKDOWN
    
    return(res)

}
