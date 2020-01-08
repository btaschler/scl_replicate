#' @export
predictor_biglasso_lasso = function(X, Y, split = cad_split_training_test) {
    
    data = split(X, Y)
    f    = data$f
    idx0 = data$idx0
    
    data$Xtrain = bigmemory::as.big.matrix(data$Xtrain)
    data$Xtest  = bigmemory::as.big.matrix(data$Xtest)
    
    fit     = biglasso::cv.biglasso(data$Xtrain, data$Ytrain, family = "binomial", nfolds = 10)
    f0      = predict(fit, X = data$Xtest, type = "response")
    f[idx0] = f0
    
    return(f)
}


#' @export
predictor_biglasso_relabelling_lasso = function(X, Y, split = cad_split_training_test) {
    
    data = split(X, Y)
    
    data$Xtrain = bigmemory::as.big.matrix(data$Xtrain)
    X           = bigmemory::as.big.matrix(X)
    
    fit = biglasso::cv.biglasso(data$Xtrain, data$Ytrain, family = "binomial", nfolds = 10)
    f   = predict(fit, X = X, type = "response")
    
    return(f)
}

