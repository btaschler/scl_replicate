
#' Predict unknown (0) labels in Y
#'
#' @param feat Output of `cad_featurize` function
#' @param predictor Function used to predict the unknown entries of G0
#' @param debug If TRUE returns intermediate variables too.
#' @param ... Additional arguments to be passed to `predictor`
#'
#' @return Feature matrix (F) and corresponding labels (Y)
#' @export
#'
cad_predict = function(feat,
                       predictor = predictor_glmnet_lasso, 
                       debug = FALSE, 
                       ...) {
    
    int_count = nrow(feat$G0)
    p         = ncol(feat$G0)

    f_hat = predictor(feat$F, feat$Y, ...)

    G_hat = matrix(f_hat, int_count, p, byrow = TRUE)

    stopifnot( !anyNA(G_hat) )

    if (debug == FALSE) {
        return( G_hat )
    } else {
        return( list(G_hat = G_hat, F = feat$F, Y = feat$Y, K = feat$K) )
    }

}

