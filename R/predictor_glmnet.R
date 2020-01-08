#' @export
predictor_glmnet_lasso = function(X, Y, split = cad_split_training_test) {
    
    data = split(X, Y)
    f    = data$f
    idx0 = data$idx0

    fit     = glmnet::cv.glmnet(data$Xtrain, data$Ytrain, family = "binomial", intercept = TRUE)
    f0      = predict(fit, newx = data$Xtest, s = "lambda.min", type = "response")
    f[idx0] = f0

    return(f)
}


#' @export
predictor_glmnet_relabelling_lasso = function(X, Y, split = cad_split_training_test) {
    
    data = split(X, Y)
    
    fit = glmnet::cv.glmnet(data$Xtrain, data$Ytrain, family = "binomial", intercept = TRUE)
    f   = predict(fit, newx = X, s = "lambda.min", type = "response")
    
    return(f)
}

