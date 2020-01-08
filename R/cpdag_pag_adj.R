# --------------------------------------------------------------------
# Convert PAG or CPDAG to adjacecy matrices in a strict or loose sense
# --------------------------------------------------------------------

### PAG #####

#' Convert PAG to adjacency matrix
#' @param G graph representation
#' @param method Strict or loose
#' @return Adjacency matrix
#' 
#' @export
#' 
pag2adj = function(G, method = "strict") {
    
    method = match.arg(method, c("strict", "loose"))
    
    p = ncol(G)
    Gtilde = matrix(0, nrow(G), ncol(G))
    
    if (method == "strict") {
        #
        # only keep -->
        #
        for (i in 1:p) {
            for (j in 1:p) {
                arrow <- G[i,j] == 2 & G[j,i] == 3
                if (arrow) { 
                    Gtilde[i,j] <- 1 
                }
            }
        }
    } else {
        #
        # --> or o-> or o-o or ---
        #
        for (i in 1:p) {
            for (j in 1:p) {
                arrow         <- G[i,j] == 2 & G[j,i] == 3
                circle_arrow  <- G[i,j] == 2 & G[j,i] == 1
                circle_circle <- G[i,j] == 1 & G[j,i] == 1
                tail_tail     <- G[i,j] == 3 & G[j,i] == 3
                if (arrow | circle_arrow | circle_circle | tail_tail) { 
                    Gtilde[i,j] <- 1
                }
            }
        }
    }
    
    return(Gtilde)
}



### CPDAG #####

#' Convert CPDAG to adjacency matrix
#' @param G graph representation
#' @param method Strict or loose
#' @return Adjacency matrix
#' 
#' @export
#' 
cpdag2adj = function(G, method = "strict") {
    
    method = match.arg(method, c("strict", "loose"))
    
    p      = ncol(G)
    Gtilde = G * 0
    
    if (method == "strict") {
        #
        # only keep -->
        #
        for (i in 1:p) {
            for (j in 1:p) {
                arrow <- G[i,j] == 1 & G[j,i] == 0
                if (arrow) { 
                    Gtilde[i,j] <- 1 
                }
            }
        }
    } else {
        #
        # --> or ---
        #
        for (i in 1:p) {
            for (j in 1:p) {
                arrow <- G[i,j] == 1 & G[j,i] == 0
                undir <- G[i,j] == 1 & G[j,i] == 1
                if (arrow | undir) { 
                    Gtilde[i,j] <- 1 
                }
            }
        }
    }
    
    return(Gtilde)
}

