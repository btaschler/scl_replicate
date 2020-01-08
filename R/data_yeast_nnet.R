#' @export
data_yeast_nnet = function(int_graph_count = 100, p_sample = 100) {
  
  
  library(tidyverse)
  library(cad)

  
  # Get data ------------------------------------------------------------
  
  data_list   = get_yeast_data()
  int_data    = data_list$int_data %>% as.matrix
  int_indices = data_list$int_indices
  obs_data    = data_list$obs_data %>% as.matrix
  
  
  # Get graph from data --------------------------------------------------
  
  
  graph = cad_get_true_graph(
    obs_data, int_data, int_indices,
    thresholding     = "robust_zscore_5", 
    int_thresh_count = NA,
    sparsity_factor  = NA, 
    obs_thresh_count = 80
  )
  
  
  data = cad_process_data(
    graph$obs_data, graph$int_data, graph$int_indices, graph$G_star,
    int_graph_count  = int_graph_count, 
    int_sample_count = nrow(int_data) - int_graph_count,
    obs_sample_count = 80, 
    remove_near_constant_columns = TRUE
  )
  
  G_star = data$G_star
  X      = data$X
  G0     = mask_graph(G_star, mask = "entries", percentage_visible = 50)
  
  
  id_sample = cad_sample_genes(X, G_star, p_sample)
  G_star    = G_star[ , id_sample]
  X         = X[ , id_sample]
  G0        = G0[ , id_sample]
  
  return(list(G_star = G_star, G0 = G0, X = X))
}