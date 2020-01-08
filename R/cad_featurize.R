
#' Featurize pairwise scatterplots
#'
#' @param X Data
#' @param G0 Graph with entries 0: unknown, 1: non-causal: 2: causal
#' @param q Number of PCA components to keep (default: 100)
#' @param feature_extractor Feature extractor function to summarize the joint distribution of X[,i] and X[,j] (default: flattened bivariate histogram)
#' @param parallel Run in parallel (default: TRUE)
#' @param pca_package Package to use for PCA (default: "rsvd")
#'
#' @return Feature matrix (F) and corresponding labels (Y)
#' @export
#'
cad_featurize = function(X, G0,
                         q = 100,
                         feature_extractor = feature_extractor_hist2d,
                         parallel = TRUE,
                         pca_package = "rsvd") {
    
    stopifnot( pca_package %in% c("rsvd", "irlba") )
    
    stopifnot( ncol(X) == ncol(G0) )
    
    X = scale(X)
    stopifnot( !anyNA(X) )
    
    # int_count is number of interventions, including those entirely unseen (if any)
    int_count = nrow(G0)
    
    p = ncol(X)
    
    # Index mapping
    K = get_index_mapping(int_count, p)
    
    # Label vector
    Y = get_labels(G0)
    
    
    par_fcn = function(ij, K, X, feature_extractor) {
        
        i = K[ij, "i"]
        j = K[ij, "j"]
        
        Xij = X[ ,c(i, j)]
        
        bin_counts = feature_extractor(Xij)
        
        return(bin_counts)
    }
    
    
    if (parallel) {
        
        cl = makeCluster( detectCores() - 1 )
        registerDoParallel(cl)
        
        F_wide = foreach(
            chunk = isplitVector(K[,'ij'], chunks = getDoParWorkers()),
            .packages = c('foreach', 'gplots'),
            .combine = rbind
        ) %dopar% {
            foreach(
                ij = chunk,
                .combine = rbind
            ) %do% {
                par_fcn(ij, K, X, feature_extractor)
            }
        }
        
        stopCluster(cl)
        closeAllConnections()
        
    } else {
        
        F_raw = sapply(K[ ,"ij"], function(ij) par_fcn(ij, K, X, feature_extractor))
        F_wide = t(F_raw)
        
    }
    
    stopifnot( nrow(F_wide) == nrow(K) )
    
    
    max_num_entries = 100000 * 900
    if ( prod(dim(F_wide)) > max_num_entries ) {
        use_randomized = TRUE
    } else {
        use_randomized = FALSE
    }
    
    
    pca_res = switch(pca_package,
                     rsvd  = rsvd::rpca(F_wide, k = q, center = TRUE, scale = FALSE, rand = use_randomized),
                     irlba = irlba::prcomp_irlba(F_wide, n = q, center = TRUE, scale. = FALSE),
                     stop("PCA package unknown.")
    )
    
    
    F = pca_res$x
    
    
    return(
        list(G0 = G0, F = F, Y = Y, K = K, F_wide = F_wide)
    )
    
}
