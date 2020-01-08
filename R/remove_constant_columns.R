#' Remove constant columns
#'
#' @param obs_data Observational data
#' @param int_data Interventional data
#' @param int_names Names or indices of the genes targeted by the interventions
#' @param gene_names All gene names or indices
#' @return Interventional data without columns having constant variance or a fraction of zeros > 0.8
#' @export
#'
remove_constant_columns = function(obs_data, int_data, int_names, gene_names = NULL) {

    if (is.null(gene_names)) {
        if (is.character(int_names)) stop("Please provide all gene names")
        if (is.numeric(int_names)) gene_names = 1:ncol(int_data)
    }

    obs_var = apply(obs_data, 2, var)
    int_var = apply(int_data, 2, var)
    frac_zeros = colMeans(int_data == 0)
    keep_ids = which(obs_var != 0 & int_var != 0 & frac_zeros < 0.8)
    obs_data = obs_data[ , keep_ids, drop=FALSE]
    int_data = int_data[ , keep_ids, drop=FALSE]
    gene_names = gene_names[keep_ids]

    for (i in seq_along(int_names)) {
        if (!(int_names[i] %in% gene_names)) {
            int_names[i] = NA
        }
    }

    int_to_keep = !is.na(int_names)
    int_names = int_names[int_to_keep]
    int_data  = int_data[int_to_keep, ,drop=FALSE]

    int_indices = get_int_indices(int_names, gene_names)

    return( list(obs_data = obs_data,
                 int_data = int_data,
                 int_indices = int_indices,
                 int_names = int_names,
                 gene_names = gene_names,
                 keep_ids = keep_ids) )
}
