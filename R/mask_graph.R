#' Set graph entries or full rows to zero at random.
#'
#' @param G_star The {0, 1} graph obtained by thresholding the interventional data
#' @param percentage_visible Percentage of visible entries or rows
#' @param mask Flag to mask entries or rows
#' @return Masked graph with entries 0 (= unknown) and {1, 2} corresponding to {0, 1} of G_star
#' @export
#'
mask_graph = function(G_star, percentage_visible = 50, mask = "entries") {
    
    stopifnot( percentage_visible >= 0 && percentage_visible <= 100 )
    stopifnot( mask %in% c("rows", "entries") )
    
    if (mask == "rows") {
        G0 = G_star * 0
        row_count = nrow(G0)
        visible_row_count = ceiling(row_count * percentage_visible / 100)
        idx12 = sample.int(row_count, visible_row_count)
        G0[idx12, ] = 1 + G_star[idx12, ]
        
    } else if (mask == "entries") {
        G0 = c(G_star) * 0
        entry_count = length(G0)
        visible_entry_count = ceiling(entry_count * percentage_visible / 100)
        idx12 = sample.int(entry_count, visible_entry_count)
        G0[idx12] = 1 + G_star[idx12]
        G0 = matrix(G0, nrow(G_star))
    }
    
    attr(G0, "mask") = mask
    attr(G0, "percentage_visible") = percentage_visible
    
    return(G0)
}
