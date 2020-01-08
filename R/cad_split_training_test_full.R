#' Process X and Y data
#'
#' @param X Features
#' @param Y Labels in {0, 1, 2} with 0 = unknown and {1, 2} corresponding to {0, 1} in G0
#' @export
#'
cad_split_training_test_full = function(X, Y) {
    m = nrow(X)
    p = ncol(X)

    idx2 = which(Y == 2)
    n2   = length(idx2)

    idx1 = which(Y == 1)
    n1   = length(idx1)

    idx0 = which(Y == 0)

    # stratified sample
    idx2_sample = sample(idx2, n2)
    idx1_sample = sample(idx1, n1)
    
    Xtrain = rbind( X[idx2_sample, , drop = FALSE],
                    X[idx1_sample, , drop = FALSE] )
    
    Ytrain = rbind( matrix(1, n2, 1),
                    matrix(0, n1, 1) )
    
    Xtest = X[idx0, , drop = FALSE]

    f = matrix(0, m, 1)
    f[idx2] = 1;
    f[idx1] = 0;

    return(list(f = f, idx0 = idx0, idx1 = idx1, idx2 = idx2,
                Xtest = Xtest, Xtrain = Xtrain, Ytrain = Ytrain))
}
