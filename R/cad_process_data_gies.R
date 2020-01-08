#' Prepare data for GIES
#'
#' @param obs_data Observational data (n_obs x p)
#' @param int_data Interventional data (n_int x p)
#' @param int_indices Which column (gene) is targeted by each row (intervention)
#' @param G_star True graph
#' @param int_graph_count Number of interventional data used for the graph
#' @param int_sample_count Number of interventional data to sample and add to X
#' @param obs_sample_count Number of observational data to sample and add to X
#' @param remove_near_constant_columns If true, removes columns (genes) which are almost constant
#' @return List of data for CAD
#' @export
#'
cad_process_data_gies = function(obs_data, int_data, int_indices, G_star,
                                 int_graph_count,
                                 obs_sample_count,
                                 remove_near_constant_columns = TRUE) {
    
    ### Total nr of observations and interventions
    
    int_total_count = nrow(int_data)
    obs_total_count = nrow(obs_data)
    
    ### Select subset of interventions to create the graph
    
    int_perm_ids   = sample.int(int_total_count)
    int_graph_ids  = int_perm_ids[1:int_graph_count]

    obs_sample_ids = sample.int(obs_total_count, size = obs_sample_count)
    
    ### Subset true graph
    
    G_star = G_star[int_graph_ids, , drop=FALSE]
    
    ### Create matrix X of training data
    
    X = rbind(obs_data[obs_sample_ids, , drop=FALSE],
              int_data[int_graph_ids, , drop=FALSE])
    
    ### Rearrange genes (columns) with int_indices first to allow for diagonal checking
    
    int_not_graph_ids = setdiff(1:ncol(G_star), int_indices[int_graph_ids])
    jj = c(int_indices[int_graph_ids], int_not_graph_ids)
    X  = X[ , jj, drop=FALSE]
    G_star  = G_star[ , jj, drop=FALSE]
    int_graph_ids = 1:int_graph_count
    
    
    ### Remove near-constant columns
    
    if (remove_near_constant_columns) {
        jj = union(1:int_graph_count,
                   which(colSums(G_star) < (int_graph_count / 2)))
        G_star  = G_star[ ,jj,drop=FALSE]
        X  = X[ , jj, drop=FALSE]
    }
    
    ### Standardize
    
    X = scale(X)
    stopifnot( !anyNA(X) )
    
    ### Return
    
    data = list(
        X = X,
        G_star = G_star,
        obs_sample_ids = obs_sample_ids,
        int_graph_ids  = int_graph_ids,
        int_indices_graph = match(int_indices[int_graph_ids], jj)
    )
    
    return(data)
}

