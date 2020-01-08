
library(tidyverse)
library(ROCR)
library(MLmetrics)


#' @export
set_diag_na = function(X) {
    m = nrow(X)
    diag(X[1:m, 1:m]) = NA
    return(X)
}


#' @export
get_binary_methods = function() {
    c('gies', 'rfci', 'pc', 'pc_05')
}


#' @export
is_method_binary = function(method) {
    method = str_remove(method, 'method_')
    ifelse(method %in% get_binary_methods(), TRUE, FALSE)
}


#' @export
get_auc = function(G_star, G0, G_hat, method, id_swap = NULL) {
    
    stopifnot( nrow(G_hat) == nrow(G0) )
    
    G_star = set_diag_na(G_star) %>% c
    G0     = set_diag_na(G0) %>% c
    G_hat  = set_diag_na(G_hat) %>% c
    
    if (is.null(id_swap)) {
        id_test = (G0 == 0) & (!is.na(G_star))
    } else {
        G0[id_swap] = 99
        id_test = (G0 == 99) & (!is.na(G_star))
    }
    
    y_true = G_star[id_test]
    y_pred = G_hat[id_test]
    
    if (is_method_binary(method)) {
        y_true = factor(y_true, levels = c('0', '1'))
        y_pred = factor(y_pred, levels = c('0', '1'))
        
        tpr = Sensitivity(y_true = y_true, y_pred = y_pred, positive = '1')
        fpr = 1 - Specificity(y_true = y_true, y_pred = y_pred, positive = '1')
        
        y_true = y_true %>% as.character %>% as.numeric
        y_pred = y_pred %>% as.character %>% as.numeric
        
    } else {
        tpr = NA
        fpr = NA
    }
    
    auc_score = glmnet::auc(y_true, y_pred)
    
    return(
        list(auc = auc_score, tpr = tpr, fpr = fpr, 
             y_true = y_true, y_pred = y_pred)
    )
}

