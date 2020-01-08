method_cad_lasso = function(X, G0) {
    
    # Featurize
    feat = cad_featurize(X, G0,
                         q = 100,
                         feature_extractor = feature_extractor_hist2d,
                         parallel = TRUE,
                         pca_package = "rsvd")
    
    # Predict
    G_hat = cad_predict(feat, 
                        predictor = predictor_glmnet_lasso)
    
    return( list(g_hat = G_hat) )
    
}
