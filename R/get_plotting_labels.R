#' @export
get_plotting_labels = function(mask, dict) {
    
    stopifnot(mask %in% c("entries", "rows"))
    
    library(filehash)
    library(tidyverse)
    
    file_path = "C:/Users/user-adm/Documents/cad_datasets/vary_p"
    db = dbInit(file_path, type = "RDS")
    
    res_100  = db$p_100__rep_5
    res_1000 = db$p_1000__rep_5
    res_5000 = db$p_5000__rep_5
    res_Inf  = db$p_Inf__rep_5
    
    G0 = switch(mask,
                entries = 'G0_entries',
                rows    = 'G0_rows')
    
    true_causal_100   = table(res_100[[G0]])[["2"]]
    true_causal_1000  = table(res_1000[[G0]])[["2"]]
    true_causal_5000  = table(res_5000[[G0]])[["2"]]
    true_causal_19000 = table(res_Inf[[G0]])[["2"]]
    
    
    grid = list(
        p100   = pmax(3, ceiling(dict[["100"]]  / 100 * true_causal_100)),
        p1000  = pmax(3, ceiling(dict[["1000"]] / 100 * true_causal_1000)),
        p5000  = pmax(3, ceiling(dict[["5000"]] / 100 * true_causal_5000)),
        p19000 = pmax(3, ceiling(dict[["Inf"]]  / 100 * true_causal_19000))
    )
    
    return(grid)
}