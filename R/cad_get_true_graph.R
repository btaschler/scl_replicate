#' Get true graph
#'
#' @param obs_data Observational data
#' @param int_data Interventional data
#' @param int_indices Indices of the genes targeted by the interventions
#' @param thresholding Thresholding
#' @param int_thresh_count Number of interventions used to obtain the thresholds
#' @param sparsity_factor Sparsity factor
#' @param obs_thresh_count Number of observational data used to obtain the thresholds
#' @return The true graph G_star, int_data, obs_data, int_indices
#' @export
#'
cad_get_true_graph = function(obs_data, int_data, int_indices,
                              thresholding     = "max_min",
                              int_thresh_count = NULL, 
                              sparsity_factor  = 1,
                              obs_thresh_count = NULL) {
    
    stopifnot( !(is.null(int_thresh_count) && is.null(obs_thresh_count)) )

    int_total_count = nrow(int_data)
    obs_total_count = nrow(obs_data)
    
    int_perm_ids    = sample.int(int_total_count)
    obs_perm_ids    = sample.int(obs_total_count)
    
    ### Split data into thresholding / rest chunks #####
    
    if (thresholding == "max_min") {
        int_thresh_ids = int_perm_ids[1:int_thresh_count]
        int_rest_ids   = setdiff(int_perm_ids, int_thresh_ids)
        
        int_thresh     = int_data[int_thresh_ids, , drop = FALSE]
        int_rest       = int_data[int_rest_ids, , drop = FALSE]
        int_indices    = int_indices[int_rest_ids]
        obs_rest       = obs_data
    } else {
        obs_thresh_ids = obs_perm_ids[1:obs_thresh_count]
        obs_rest_ids   = setdiff(obs_perm_ids, obs_thresh_ids)

        obs_thresh     = obs_data[obs_thresh_ids, , drop = FALSE]
        obs_rest       = obs_data[obs_rest_ids, , drop = FALSE]
        int_rest       = int_data
    }
    
    ### Threshold data to obtain binary matrix #####
    
    if (thresholding == "max_min") {
        int_max = matrixStats::colMaxs(int_thresh) * sparsity_factor
        int_min = matrixStats::colMins(int_thresh) / sparsity_factor
    } else {
        obs_median = matrixStats::colMedians(obs_thresh)
        obs_iqr    = matrixStats::colIQRs(obs_thresh)
        int_zscore = scale(int_rest, center = obs_median, scale = obs_iqr)
    }
    
    G_star = switch(thresholding,
        max_min         = sweep(int_rest, 2, int_max, ">=") | sweep(int_rest, 2, int_min, "<="),
        robust_zscore_7 = abs(int_zscore) >= 7,
        robust_zscore_5 = abs(int_zscore) >= 5,
        robust_zscore_3 = abs(int_zscore) >= 3
    )
    
    G_star = G_star * 1   # convert to 0/1 matrix
    
    Output <- list(
        G_star      = G_star,
        int_data    = int_rest,
        obs_data    = obs_rest,
        int_indices = int_indices
    )
    return(Output)
}


