predictor_keras = function(X, Y, split = cad_split_training_test, ...) {
    #' Training a deep net for prediction (using a reduced set of training data).
    #'
    #' @seealso \code{\link{predictor_glmnet_lasso}}
    #'          \code{\link{wrapper_keras}}
    #' 
    #' @param X data matrix
    #' @param Y labels
    #' @param split function to split data into training and test sets (default:
    #'              \code{\link{cad_split_training_test}})
    #'
    #' @return \item{f}{ predictions on test set)}
    #'
    #' @export
    
    data = split(X, Y)
    f    = data$f
    idx0 = data$idx0
    
    fit = wrapper_keras(data$Xtrain, data$Ytrain, data$Xtest, ...)
    f[idx0] = fit$y_pred
    
    return(f)
}


predictor_relabelling_keras = function(X, Y, split = cad_split_training_test, ...) {
    #' Training a deep net for prediction (predicting the full data matrix).
    #'
    #' @seealso \code{\link{predictor_glmnet_lasso}}
    #'          \code{\link{wrapper_keras}}
    #'          \code{\link{predictor_keras}}
    #' 
    #' @param X data matrix
    #' @param Y labels
    #' @param split function to split data into training and test sets (default:
    #'              \code{\link{cad_split_training_test}})
    #'
    #' @return \item{f}{ full predictions (incl. previously known entries)}
    #'
    #' @export
    
    data = split(X, Y)
    
    fit = wrapper_keras(data$Xtrain, data$Ytrain, xx_test = X, ...)
    f = fit$y_pred
    
    return(f)
}


wrapper_keras <- function(xx_train, y_train, xx_test,
                          num_layers = 3, 
                          num_nodes = c(256, 128, 64), 
                          rate_drop = c(0.4, rep(0.3, 2)),
                          penalty_l1 = 0.001, 
                          penalty_l2 = 0.001,
                          epochs = 100, 
                          batch_size = min(4096, 2^floor(log2(nrow(xx_train)/5))),
                          verbose = FALSE, 
                          do_regression = TRUE){
    #' Internal function for predictor_keras, calling keras with tensorflow backend.
    #'
    #' Wrapper for keras to do predictions. Automatically initialises model,
    #' layers, nodes and trains a dense deep net. Activation used: relu (for input and hidden layers);
    #' sigmoid (for final layer in case of binary classification) or
    #' softmax (multi-class classification).
    #' Default mode is to produce a regression output (loss: MSE, optimizer: Adam).
    #' In case of classification, binary cross-entropy or categorical cross-entropy
    #' is used for the loss function. 
    #' 
    #' @seealso \code{\link[keras]{keras_model_sequential}}
    #' 
    #' @param  xx_train training data of size [n x p]
    #' @param  y_train  training labels of size [n x 1]
    #' @param  xx_test  test data of size [n_test x p]
    #' @param  num_layers number of hidden layers (default: num_layers = 8)
    #' @param  num_nodes number of nodes per hidden layer, i.e. a vector of size num_layers
    #'          (default: descending (1024 ... 64))
    #' @param  rate_drop dropout rate, i.e. a vector of size num_layers - 1
    #'          (default: (0.4, rep(0.3, 7)))
    #' @param  penalty_l1 regularisation parameter for L1 penalty (default: 0)
    #' @param  penalty_l2 regularisation parameter for L2 penalty (default: 0.005)
    #' @param  epochs number of full passes of the data through the DNN
    #'          (default: 100)
    #' @param  batch_size number of samples used to compute the gradients
    #'          (default: 2048)
    #' @param  verbose logical indicator to toggle visual output
    #' @param  do_regression logical indicator to perform regression (for continuous output)
    #'
    #' @return \item{model}{ fitted DNN}
    #'         \item{y_pred}{ predicted labels for test data}
    #' @export
    
    ### preliminaries ---------------------------------------------------------
    stopifnot( is.numeric(num_layers) & (num_layers >= 2) )
    stopifnot( is.numeric(num_nodes))
    stopifnot( is.numeric(rate_drop))
    stopifnot( length(num_nodes) == (num_layers) )
    stopifnot( length(rate_drop) == (num_layers) )
    
    n <- nrow(xx_train)
    p <- ncol(xx_train)
    n_test <- nrow(xx_test)
    k <- length(unique(y_train))
    
    
    ### preprocessing ---------------------------------------------------------
    ## standardise data
    xx_train <- scale(xx_train)
    xx_test <- scale(xx_test)
    
    if(!do_regression){
        ## relabel group labels to integers starting from 0
        u_y <- sort(unique(y_train))
        if(!all(u_y == 0:(k-1))){
            tmp <- numeric(n)
            for(i in seq_along(u_y)){ tmp[y_train==u_y[i]] <- i-1 }
            y_train <- tmp
        }
        ## in case of multi-class classification: create indicator matrix from group labels
        if(k > 2){ y_train <- to_categorical(y_train, num_classes = k) }
    }
    
    ### construct model -------------------------------------------------------
    model <- keras_model_sequential()
    
    ## specify layers and nodes
    model %>%
        layer_dense(units = num_nodes[1], activation = 'relu', 
                    kernel_regularizer = regularizer_l1_l2(l1 = penalty_l1, 
                                                           l2 = penalty_l2),
                    input_shape = p) %>%
        layer_dropout(rate = rate_drop[1])        #input layer
    for(j in seq(num_layers)){
        model %>%
            layer_dense(units = num_nodes[j], activation = 'relu',
                        kernel_regularizer = regularizer_l1_l2(l1 = penalty_l1, 
                                                               l2 = penalty_l2)) %>%
            layer_dropout(rate = rate_drop[j])    #hidden layers
    }
    
    ### compile model ---------------------------------------------------------
    if(do_regression){  #regression (i.e. continuous output)
        model %>% layer_dense(units = 1)          #output layer
        if(verbose){ summary(model) }             #display model summary
        
        model %>% compile(loss = 'mse',
                          optimizer = 'adam',
                          metrics = c('mae'))
        
    }else if(k==2){  #binary classification
        model %>% layer_dense(units = 1, activation = 'tanh')      #output layer
        if(verbose){ summary(model) }             #display model summary
        
        model %>% compile(loss = 'binary_crossentropy',
                          optimizer = 'adam',
                          metrics = c('accuracy'))
        
    }else{     #multi-class classification (i.e. categorical output)
        model %>% layer_dense(units = k, activation = 'softmax')   #output layer
        if(verbose){ summary(model) }             #display model summary
        
        model %>% compile(loss = 'categorical_crossentropy',
                          optimizer = optimizer_adam(),
                          metrics = c('accuracy'))
    }
    
    ### train deep net --------------------------------------------------------
    history <- model %>% fit(xx_train, y_train,
                             epochs = epochs, 
                             batch_size = batch_size,
                             validation_split = 0.2,
                             verbose = verbose)
    
    ### predict test data -----------------------------------------------------
    if(do_regression){
        y_pred <- model %>% predict(xx_test)
    }else{
        y_pred <- model %>% predict_classes(xx_test)
        if(!all(u_y == seq(k))){   #map classes back to original labels
            tmp <- numeric(n_test)
            for(i in seq(k)){ tmp[y_pred==(i-1)] <- u_y[i] }
            y_pred <- tmp
        }
    }
    
    ### output ----------------------------------------------------------------
    return(list('model' = model, 'y_pred' = y_pred))
}
