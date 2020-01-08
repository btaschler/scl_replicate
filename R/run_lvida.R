
source('lv-ida-master/iscyclic.R')
source('lv-ida-master/lvida.R')


#' Wrapper for IDA
#'
#' @param X Data matrix of size n-by-p
#' @param G0 Masked graph
#' @param sig_level Significance level
#' @param method 'local' / 'global'
#' @param skel.method Skeleton method
#' @export
#'
run_lvida <- function(X, G0, sig_level = 0.01, method = 'local', skel.method = 'stable.fast') {
    
    G_hat <- matrix(nrow = nrow(G0), ncol = ncol(X))
    
    gene_names <- colnames(X)
    dimnames(G_hat) <- list(gene_names[1:nrow(G0)], gene_names)
    
    suffStat  <- list(C = cor(X), n = nrow(X))
    indepTest <- gaussCItest
    
    
    #### call to RFCI #########################################################
    
    rfci.fit <- pcalg::rfci(suffStat, indepTest, 
                            alpha = sig_level, 
                            p = ncol(X), 
                            verbose = FALSE,
                            skel.method = skel.method, 
                            numCores = parallel::detectCores() - 1)
    
    ###########################################################################
    
    
    covX = cov(X)
    
    for (j in 1:nrow(G0)) {
        for (k in 1:ncol(G0)) {
            
            
            #### call to LV-IDA ###############################################
            
            eff <- lv.ida(j, k, covX, rfci.fit@amat, method = method, localcap = NULL)
            
            ###################################################################

                        
            if (all(is.na(eff))) {
                warning('All effects are NAs: setting it to zero!')
                eff = 0
            }
            
            G_hat[j,k] <- min(abs(eff), na.rm = TRUE)
        }
    }
    
    Output <- list( g_hat = G_hat )
    return(Output)
}
