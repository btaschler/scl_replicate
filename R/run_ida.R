#' Wrapper for IDA
#'
#' @param X Data matrix of size n-by-p
#' @param sig_level Significance level
#' @param skel.method Skeleton method
#' @export
#'
run_ida <- function(X, sig_level, skel.method) {
    
    G_hat <- matrix(nrow=ncol(X),ncol=ncol(X))
    dimnames(G_hat) <- list(colnames(X),colnames(X))
    
    suffStat <- list(C=cor(X), n=nrow(X))
    
    
    #### call to IDA ##########################################################

    pc.fit <- pcalg::pc(suffStat, 
                        indepTest = gaussCItest, 
                        alpha = sig_level, 
                        p = ncol(suffStat$C),
                        skel.method = skel.method, 
                        numCores = parallel::detectCores() - 1)
    
    ###########################################################################
    
    covX = cov(X)
    for (j in 1:ncol(X)){
        eff <- pcalg::idaFast(j, 1:ncol(X), covX, pc.fit@graph)
        G_hat[j,] <- matrixStats::rowMins( abs(eff) )    # or: apply(abs(eff), 1, min)
    }
    
    Output <- list( g_hat = G_hat )
    return(Output)
}
