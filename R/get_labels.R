
#' Convert graph to vector of labels
#' @export
#' 
get_labels = function(G0) {

    Y = as.matrix( c(t(G0)) )
    
    return(Y)
}

