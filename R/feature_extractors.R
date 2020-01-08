# Feature extractors
#
#   All methods must return a vector and have signature
#   feat = feature_extractor_*(Xij, ...)

#' @export
feature_extractor_hist2d = function(Xij, bin_count = 30) {
    feat = gplots::hist2d(Xij, nbins = bin_count, show = FALSE)$counts
    return( c(feat) )
}

#' @export
feature_extractor_kde2d = function(Xij, bin_count = 30) {
    bw   = c(MASS::bandwidth.nrd( Xij[,1] ), MASS::bandwidth.nrd( Xij[,2] ))
    bw   = pmax(bw, 0.01)
    feat = MASS::kde2d(Xij[,1], Xij[,2], bw, n = bin_count)$z
    return( c(feat) )
}
