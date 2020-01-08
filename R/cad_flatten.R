
#' @export
cad_flatten = function(feat, q = 100) {

    F = t( apply(feat$F, 3, c) )

    max_num_entries = 100000 * 900
    if ( prod(dim(F)) > max_num_entries ) {
        use_randomized = TRUE
    } else {
        use_randomized = FALSE
    }

    pca_res = rsvd::rpca(F, k = q, center = TRUE, scale = FALSE, rand = use_randomized)

    feat$F = pca_res$x

    return(feat)

}

