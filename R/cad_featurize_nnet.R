
#' Create pairwise histograms
#'
#' @export
#'
cad_featurize_nnet = function(X, G0,
                              bin_count = 100,
                              parallel = TRUE) {
    
    stopifnot( ncol(X) == ncol(G0) )
    
    X = scale(X)
    stopifnot( !anyNA(X) )
    
    n = nrow(X)
    p = ncol(X)
    
    # int_count is number of interventions, including those entirely unseen (if any)
    int_count = nrow(G0)
    
    # Number of pairs for linear indexing
    pair_count = int_count * p
    
    # Index mapping
    K = get_index_mapping(int_count, p)
    
    # Label vector
    Y = get_labels(G0)
    
    
    par_fcn = function(ij, K, X, bin_count) {
        
        i = K[ij, "i"]
        j = K[ij, "j"]
        
        Xij = X[ ,c(i, j)]
        
        C = gplots::hist2d(Xij, nbins = bin_count, show = FALSE)$counts
        
        return(C)
    }
    
    
    if (parallel) {
        
        cl = makeCluster( detectCores() - 1 )
        registerDoParallel(cl)
        
        F_raw = foreach(
            chunk = isplitVector(K[,'ij'], chunks = getDoParWorkers()),
            .packages = c('foreach', 'gplots')
        ) %dopar% {
            foreach(ij = chunk) %do% {
                par_fcn(ij, K, X, bin_count)
            }
        }
        
        stopCluster(cl)
        closeAllConnections()
        
        F_raw = unlist(F_raw, recursive = FALSE)
        
    } else {
        
        F_raw = lapply(K[ ,"ij"], function(ij) par_fcn(ij, K, X, bin_count))
        
    }
    
    
    F = abind::abind(F_raw, along = 3)
    F = aperm(F, c(3,1,2))
    
    stopifnot(
        all.equal(dim(F), c(pair_count, bin_count, bin_count))
    )
    
    
    return(
        list(G0 = G0, F = F, Y = Y, K = K)
    )
    
}

