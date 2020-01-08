#' Wrapper for RFCI: really fast causal inference
#' 
#' @param X data
#' @param sig_level Significance level for independence testing
#' @param skel.method Skeleton method
#' @export
#' 
run_rfci <- function(X, sig_level, skel.method) {
    
    suffStat  <- list(C=cor(X), n=nrow(X))
    indepTest <- gaussCItest
    
    res <- pcalg::rfci(suffStat, indepTest, 
                       alpha = sig_level, 
                       p = ncol(X), 
                       verbose = FALSE,
                       skel.method = skel.method, 
                       numCores = parallel::detectCores() - 1)
    
    PAG <- as(res, "matrix")
    
    G_hat_strict <- pag2adj(PAG, "strict")
    G_hat_loose  <- pag2adj(PAG, "loose")
    
    Output <- list( pag = PAG,
                    g_hat_strict = G_hat_strict, 
                    g_hat_loose  = G_hat_loose )
    return(Output)
    
}