
#' Unique values in a matrix
#'
#' @param A matrix
#' @return Unique elements of matrix `A`
#' @export
unique_vals <- function(A) {
    return( sort(unique(c(A))) )
}



#' Heatmap
#'
#' @param z Matrix
#' @param x Row indices
#' @param y Column indices
#' @param title Figure title
#' @param ... Additional arguments to be passed to `plot_ly()`
#' @return Plots a heatmap of matrix z
#' @export
#'
heatmap = function(z, x = 1:ncol(z), y = 1:nrow(z), title = NULL, ...) {
    plt = plotly::plot_ly(x = x, y = y, z = z, type = "heatmap",
                          colors = colorRamp(c("white", "black")), ...)
    plotly::layout(plt, yaxis = list(autorange="reversed"), title = title)
}



#' Safe Scaling and Centering of Matrix-like Objects
#'
#' Avoid dividing by scale equal to zero.
#'
#' @param X A numeric matrix-like object
#' @param center Location
#' @param scale Scale
#' @export
#'
zscore = function(X, center = TRUE, scale = TRUE) {
    if (is.logical(center) && center == TRUE) center = matrixStats::colMeans2(X)
    if (is.logical(scale) && scale == TRUE) scale = matrixStats::colSds(X)
    if (is.logical(scale) && scale == FALSE) scale0 = scale
    if (!is.logical(scale)) {
        scale0 = scale
        scale0[scale0 == 0] = 1
    }
    X = base::scale(X, center = center, scale = scale0)

    stopifnot( !any(is.nan(X)) )

    return( X )
}



#' Random sampling
#'
#' Perform random sampling
#'
#' @param X scalar, vector or matrix
#' @param n scalar or vector indicating how many samples
#' @param margin what object to sample (1 = rows, 2 = columns, 1:2 = both)
#' @param ... Additional arguments to be passed to `sample.int()` or `sample()`
#' @return A sample from X
#' @export
#'
random_sample = function(X, n = NULL, margin = NULL, ...) {

    stopifnot( margin %in% c(NULL, 1, 2, 1:2) )

    is_n_vector = length(n) > 1
    if (is.null(n) && is.null(margin)) margin = 1
    if (!is.null(n) && !is_n_vector && is.null(margin)) margin = 1
    if (!is.null(n) && !is_n_vector && !is.null(margin)) n = rep(n, 2)
    if (!is.null(n) && is_n_vector && is.null(margin)) margin = 1:2

    is_margin_vector = length(margin) > 1
    if (is_n_vector && !is_margin_vector) stop("Maybe you want margin = 1:2")

    if ( is.vector(X) ) {
        # Scalar or vector
        if ( length(X) == 1 ) {
            # Scalar
            if (is.null(n)) n = X
            Y = sample.int(X, n, ...)
        } else {
            # Vector
            if (is.null(n)) n = length(X)
            Y = sample(X, n, ...)
        }
    } else {
        # Matrix or data frame
        if (length(margin) == 1) {
            # Sample either rows or columns
            if (margin == 1) {
                # Rows
                if (is.null(n)) n = nrow(X)
                N = nrow(X)
                Y = X[sample.int(N, n, ...), ]
            } else if (margin == 2) {
                # Columns
                if (is.null(n)) n = ncol(X)
                N = ncol(X)
                Y = X[ ,sample.int(N, n, ...)]
            }
        } else {
            # Both
            if (is.null(n)) n = c(nrow(X), ncol(X))
            N = c(nrow(X), ncol(X))
            Y = X[sample.int(N[1], n[1], ...), sample.int(N[2], n[2], ...)]
        }
    }
    return( Y )
}



#' Create string from key, value pairs
#'
#' @param ... Arguments to be passed to `list()`
#' @param outer_sep Separator of pairs
#' @param inner_sep Key, value separator
#' @param key_sep Separator of the words of keys
#' @param case The desired key case
#' @param append String to append at the end
#' @param prepend String to prepend
#' @return String of key, value pairs
#' @export
#'
keyvalue2string = function(..., outer_sep = "___", inner_sep = "__", key_sep = NULL, case = "snake", 
                         append = "", prepend = "") {
    l = list(...)
    k = names(l)
    # k = str_replace_all(k, "_", key_sep) # in case you prefer data_name -> data.name, key_sep = "."
    k = snakecase::to_any_case(k, case = case, sep_out = key_sep)
    k = paste(k, inner_sep, sep = "")
    v = unlist(l)
    v = paste(v, outer_sep, sep = "")
    v[length(v)] = str_remove(v[length(v)], outer_sep)
    n = length(k)
    pairs = rep(NA, n*2)
    pairs[seq(1, n*2, by = 2)] = k
    pairs[seq(2, n*2, by = 2)] = v
    out = str_c(pairs, collapse = "")
    out = str_c(out, append)
    out = str_c(prepend, out)
    return(out)
}
