
#' From gene names and interventional names, get indices
#'
#' @param int_names Names of the genes that each intervention targeted
#' @param gene_names Names of all genes
#' @return Which column (gene) does each row (intervention) correspond to
#' @export
#'
get_int_indices = function(int_names, gene_names) {
    int_count  = length(int_names)
    int_ids    = rep(NA, int_count)
    gene_count = length(gene_names)
    gene_ids   = 1:gene_count
    for (i in 1:int_count) {
        int_ids[i] = gene_ids[gene_names == int_names[i]]
    }
    return( int_ids )
}
