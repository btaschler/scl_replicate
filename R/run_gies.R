#' Wrapper for GIES
#' 
#' @param X Data
#' @param G0 Partial knowledge
#' @export
#' 
run_gies <- function(X, G0) {
    
    if (is.null(G0)) {
        # obs data only
        targets      <- list( integer(0) )
        target.index <- rep(1, nrow(X))
    } else {
        # obs and int data
        obs_sample_count <- nrow(X) - nrow(G0)
        
        idx12     <- which(matrixStats::rowSums2(G0) > 0)
        idx_int_X <- obs_sample_count + idx12
        X         <- X[c(1:obs_sample_count, idx_int_X), ]
        
        targets <- array_branch(idx12)
        targets <- prepend( targets, list(integer(0)) )
        
        target.index <- c( rep(1, obs_sample_count), 1 + seq_along(idx12) )
        
        stopifnot( max(target.index) == length(targets) )
        stopifnot( length(target.index) == nrow(X) )
    }
    
    #### call to GIES #########################################################
    
    score <- new("GaussL0penIntScore",
                 data = X, 
                 targets = targets,
                 target.index = target.index,
                 nodes = colnames(X))
    
    # gies.fit <- pcalg::gies(score, maxDegree = 5L)
    gies.fit <- pcalg::gies(score) #CHD
    
    ###########################################################################
    
    
    CPDAG <- as(gies.fit$essgraph, "matrix") * 1
    
    G_hat_strict <- cpdag2adj(CPDAG, "strict")
    G_hat_loose  <- cpdag2adj(CPDAG, "loose")
    
    TC_strict <- get_transitive_closure(G_hat_strict)
    TC_loose  <- get_transitive_closure(G_hat_loose)

    Output <- list(cpdag        = CPDAG,
                   g_hat_strict = G_hat_strict, 
                   g_hat_loose  = G_hat_loose, 
                   tc_strict    = TC_strict,
                   tc_loose     = TC_loose)
    return( Output )
    
}

