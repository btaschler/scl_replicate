#' Sampling p_sample genes
#'
#' @param X Data matrix
#' @param G_star True graph
#' @param p_sample How many genes to sample
#' @return Indices of sampled genes
#'
#' @export
#'
cad_sample_genes = function(X, G_star, p_sample) {

    # Subset
    p = ncol(X)
    p_sample = min(p_sample, p)

    int_graph_count = nrow(G_star)
    q_sample = p_sample - int_graph_count

    gene_ids = sample( (int_graph_count+1):p, q_sample )
    gene_ids = union(1:int_graph_count, gene_ids)

    stopifnot( length(gene_ids) == p_sample )

    return( gene_ids )

}
