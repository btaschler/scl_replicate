#' Wrapper for PC: Peter-Clarke
#' 
#' @param X data
#' @param sig_level Significance level for independence testing
#' @param skel.method Skeleton method
#' @export
#' 
run_pc <- function(X, sig_level, skel.method) {
    
    suffStat  <- list(C=cor(X), n=nrow(X))
    indepTest <- gaussCItest
    
    
    #### call to PC ###########################################################
    
    res <- pcalg::pc(suffStat, indepTest, 
                     alpha = sig_level, 
                     p = ncol(X), 
                     verbose = FALSE,
                     skel.method = skel.method, 
                     numCores = parallel::detectCores() - 1)
    
    ###########################################################################
    
    
    CPDAG <- t( as(res, "matrix") )
    
    G_hat_strict <- cpdag2adj(CPDAG, "strict")
    G_hat_loose  <- cpdag2adj(CPDAG, "loose")
    
    TC_strict    <- get_transitive_closure(G_hat_strict)
    TC_loose     <- get_transitive_closure(G_hat_loose)
    
    Output <- list( cpdag        = CPDAG,
                    g_hat_strict = G_hat_strict, 
                    g_hat_loose  = G_hat_loose, 
                    tc_strict    = TC_strict,
                    tc_loose     = TC_loose )
    return(Output)
    
}