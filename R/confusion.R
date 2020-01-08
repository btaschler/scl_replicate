#' @export
confusion = function(trueVals, predVals, idx = NULL) {
    
    CF = matrix( NA, 2, 2 )
    colnames(CF) = c("0", "1")
    rownames(CF) = c("0", "1")
    
    if (is.null(idx)) {
        print("confusion: Replacing idx=NULL with TRUEs")
        idx = rep_len(TRUE, length(trueVals))
    }
    
    CF['0','0'] = sum( (trueVals[idx] == 0) & (predVals[idx] == 0) )
    CF['0','1'] = sum( (trueVals[idx] == 0) & (predVals[idx] == 1) )
    CF['1','0'] = sum( (trueVals[idx] == 1) & (predVals[idx] == 0) )
    CF['1','1'] = sum( (trueVals[idx] == 1) & (predVals[idx] == 1) )
    
    return(CF)
    
}