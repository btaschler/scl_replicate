
#' Evaluate performance using ROC
#'
#' @param G_star True graph
#' @param G0 Masked graph
#' @param G_hat Estimated graph
#' @return Area under the ROC curve
#' @export
#'
DEPREC_get_auc = function(G_star, G0, G_hat) {

    row_count = nrow(G_hat)

    diag(G_hat[1:row_count, 1:row_count])  = NA
    diag(G_star[1:row_count, 1:row_count]) = NA

    G_star = c(G_star)
    G_hat  = c(G_hat)
    G0     = c(G0)

    G_star = G_star[ (G0 == 0) & !is.na(G_star) ]
    G_hat  = G_hat[ (G0 == 0) & !is.na(G_hat) ]

    stopifnot(!any(is.na(G_star)))
    stopifnot(!any(is.na(G_hat)))
    
    AUC = glmnet::auc(as.logical(G_star), G_hat)

    return( AUC )
}
