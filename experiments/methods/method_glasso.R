
library(huge)


# Convert precision matrix to partial correlation ---------------------

precision2partial = function(Omega) {
    L = diag( diag(Omega)^(-0.5) )
    R = L %*% (-Omega) %*% L
    return(R)
}



# Get grid of lambdas for cross validation ----------------------------

get_lambda_grid = function(x, nlambda = NULL, lambda.min.ratio = NULL) {
    n = nrow(x)
    d = ncol(x)

    cov.input = isSymmetric(x)
    if(cov.input)
    {
        S = x
    }
    if(!cov.input)
    {
        x = scale(x)
        S = cor(x)
    }


    if(is.null(nlambda))
        nlambda = 10
    if(is.null(lambda.min.ratio))
        lambda.min.ratio = 0.1

    lambda.max = max(max(S-diag(d)),-min(S-diag(d))) / 2
    lambda.min = lambda.min.ratio*lambda.max
    lambda     = exp(seq(log(lambda.max), log(lambda.min), length = nlambda))

}


method_glasso = function(X, G0) {
    
    X = scale(X)
    stopifnot( !anyNA(X) )
    
    out    = huge(X, method = "glasso")
    best   = huge.select(out)

    Omega  = as.matrix(best$opt.icov)

    # Precision matrix -> Partial Corr matrix
    G_hat = precision2partial(Omega)
    G_hat = abs(G_hat)
    
    return( list(g_hat = G_hat) )
}
