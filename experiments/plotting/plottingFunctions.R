
# library(ROCR)
# library(colorspace)


plot_average_roc = function(data, p, mask) {
    
    par(cex.lab = 2, cex.axis = 2, cex.main = 2)
    par(mar = c(5, 5, 4, 2))
    
    data$method = str_remove(data$method, "method_")
    
    downsampling = 0
    if (p == 19000) {
        data$p[ data$p > 15000 ] = 19000
        downsampling = 0.5
    }
    
    
    bin_methods = get_binary_methods()
    
    bin_data = 
        data %>%
        filter(method %in% bin_methods, p == !!p, mask == !!mask) %>%
        arrange(Method) %>%
        group_by(method, Method) %>%
        nest()
    
    
    real_data = 
        data %>%
        filter(!(method %in% bin_methods), p == !!p, mask == !!mask) %>%
        arrange(Method) %>%
        group_by(method, Method) %>%
        nest()
    
    
    n_real = length(unique(real_data$method))
    n_bin  = length(unique(bin_data$method))
    
    COL = qualitative_hcl(n_real + n_bin, palette = "Dark 3")
    
    COL_REAL = COL[1:n_real]
    COL_BIN  = COL[-(1:n_real)]
    
    LTY = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4)
    PCH = c(1, 2, 3, 5, 6)

    
    for (i in seq_along(real_data$method)) {
        
        method = real_data$method[i]
        y_pred = real_data$data[[i]]$y_pred
        y_true = real_data$data[[i]]$y_true
        
        if (max(unlist(y_pred)) >= 2) stop('>=2')
        if (min(unlist(y_pred)) < 0) stop('<0')
        
        pred_obj = prediction(y_pred, y_true)
        perf_obj = performance(pred_obj, "tpr", "fpr")
        
        if (i == 1) {
            
            tryCatch(
                plot(perf_obj, avg = "threshold", 
                     colorize = FALSE, col = COL_REAL[i], lwd = 3, lty = LTY[i], downsampling = downsampling,
                     xlab = "(Average) false positive rate",
                     ylab = "(Average) true positive rate"),
                error = function(err) {
                    plot(perf_obj, avg = "vertical", 
                         colorize = FALSE, col = COL_REAL[i], lwd = 3, lty = LTY[i], downsampling = downsampling, 
                         xlab = "(Average) false positive rate",
                         ylab = "(Average) true positive rate")
                }
            )
            
            # plot(perf_obj, avg = "threshold", 
            #      colorize = FALSE, col = COL_REAL[i], lwd = 3, lty = LTY[i])
            abline(0, 1, col = "grey", lwd = 2)
        } else {
            
            tryCatch(plot(perf_obj, avg = "threshold", downsampling = downsampling, 
                          colorize = FALSE, col = COL_REAL[i], lwd = 3, lty = LTY[i], add = TRUE),
                     error = function(err) {
                         plot(perf_obj, avg = "vertical", downsampling = downsampling, 
                              colorize = FALSE, col = COL_REAL[i], lwd = 3, lty = LTY[i], add = TRUE)
                     })
            
            # plot(perf_obj, avg = "threshold", 
            #      colorize = FALSE, col = COL_REAL[i], lwd = 3, lty = LTY[i], add = TRUE)
        }
        
    }
    
    
    if (p != 19000) {
        for (j in 1:nrow(bin_data)) {
            points(bin_data$data[[j]]$fpr, bin_data$data[[j]]$tpr, 
                   pch = PCH[j], cex = 2, col = COL_BIN[j], lwd = 3)
        }
    }
    
    
    legend("bottomright", 
           legend = unlist(list(real_data$Method, bin_data$Method)),
           lty = c(LTY[1:n_real], rep(0, n_bin)),
           lwd = c(rep(3, n_real), rep(3, n_bin)),
           col = c(COL_REAL, COL_BIN),
           pch = c(rep(NA, n_real), PCH),
           cex = 2
    )
    
    
    # title(main = sprintf("p: %s    mask: %s", p, mask))
    par(cex.lab = 1, cex.axis = 1, cex.main = 1)
    
}




# PERCENTAGE

plot_average_roc_percentage = function(data, p, mask, percentage_visible) {
    
    par(cex.lab = 2, cex.axis = 2, cex.main = 2)
    par(mar = c(5, 5, 4, 2))
    
    data$method = str_remove(data$method, "method_")
    
    downsampling = 0
    if (p == 19000) {
        data$p[ data$p > 15000 ] = 19000
        downsampling = 0.5
    }
    
    
    bin_methods = get_binary_methods()
    
    bin_data = 
        data %>%
        filter(method %in% bin_methods, p == !!p, mask == !!mask, percentage_visible == !!percentage_visible) %>%
        arrange(Method) %>%
        group_by(method, Method) %>%
        nest()
    
    
    real_data = 
        data %>%
        filter(!(method %in% bin_methods), p == !!p, mask == !!mask, percentage_visible == !!percentage_visible) %>%
        arrange(Method) %>%
        group_by(method, Method) %>%
        nest()
    
    
    n_real = length(unique(real_data$method))
    n_bin  = length(unique(bin_data$method))
    
    COL = qualitative_hcl(n_real + n_bin, palette = "Dark 3")
    
    COL_REAL = COL[1:n_real]
    COL_BIN  = COL[-(1:n_real)]
    
    LTY = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4)
    PCH = c(1, 2, 3, 5, 6)
    
    
    for (i in seq_along(real_data$method)) {
        
        method = real_data$method[i]
        y_pred = real_data$data[[i]]$y_pred
        y_true = real_data$data[[i]]$y_true
        
        if (max(unlist(y_pred)) >= 2) stop('>=2')
        if (min(unlist(y_pred)) < 0) stop('<0')
        
        pred_obj = prediction(y_pred, y_true)
        perf_obj = performance(pred_obj, "tpr", "fpr")
        
        if (i == 1) {
            
            tryCatch(plot(perf_obj, avg = "threshold", 
                          colorize = FALSE, col = COL_REAL[i], lwd = 3, lty = LTY[i], downsampling = downsampling, 
                          xlab = "(Average) false positive rate",
                          ylab = "(Average) true positive rate"),
                     error = function(err) {
                         plot(perf_obj, avg = "vertical", 
                              colorize = FALSE, col = COL_REAL[i], lwd = 3, lty = LTY[i], downsampling = downsampling, 
                              xlab = "(Average) false positive rate",
                              ylab = "(Average) true positive rate")
                     })
            
            # plot(perf_obj, avg = "threshold", 
            #      colorize = FALSE, col = COL_REAL[i], lwd = 3, lty = LTY[i])
            abline(0, 1, col = "grey", lwd = 2)
        } else {
            
            tryCatch(plot(perf_obj, avg = "threshold", downsampling = downsampling, 
                          colorize = FALSE, col = COL_REAL[i], lwd = 3, lty = LTY[i], add = TRUE),
                     error = function(err) {
                         plot(perf_obj, avg = "vertical", downsampling = downsampling, 
                              colorize = FALSE, col = COL_REAL[i], lwd = 3, lty = LTY[i], add = TRUE)
                     })
            
            # plot(perf_obj, avg = "threshold", 
            #      colorize = FALSE, col = COL_REAL[i], lwd = 3, lty = LTY[i], add = TRUE)
        }
        
    }
    
    
    if (p != 19000) {
        for (j in 1:nrow(bin_data)) {
            points(bin_data$data[[j]]$fpr, bin_data$data[[j]]$tpr, 
                   pch = PCH[j], cex = 2, col = COL_BIN[j], lwd = 3)
        }
    }
    
    
    legend("bottomright", 
           legend = unlist(list(real_data$Method, bin_data$Method)),
           lty = c(LTY[1:n_real], rep(0, n_bin)),
           lwd = c(rep(3, n_real), rep(3, n_bin)),
           col = c(COL_REAL, COL_BIN),
           pch = c(rep(NA, n_real), PCH),
           cex = 2
    )
    
    
    # title(main = sprintf("p: %s    mask: %s    perc. vis.: %s", p, mask, percentage_visible))
    par(cex.lab = 1, cex.axis = 1, cex.main = 1)
    
}







######


cad_name_mapping = function(tbl) {
    
    dict = list(
        "cad_lasso" = "SCL-L1", 
        "cad_keras" = "SCL-NN",
        "cad_relabelling_lasso" = "SCL-L1", 
        "cad_relabelling_keras" = "SCL-NN",
        "cor_kendall" = "Kendall",
        "cor_pearson" = "Pearson",
        "cor_spearman" = "Spearman",
        "pc" = "PC (0.01)", 
        "pc_05" = "PC (0.5)", 
        "gies" = "GIES",
        "ida" = "IDA",
        "rfci" = "RFCI",
        "shrinkage" = "GGM",
        "glasso" = "GLASSO",
        "lvida" = "RFCI (0.01)",
        "lvida_05" = "RFCI (0.5)")
    
    tbl$Method = rep(NA, nrow(tbl))
    
    for (i in 1:nrow(tbl)) {
        tbl$Method[i] = dict[[ tbl$method[i] ]]
    }
    
    mylevels = unique(tbl$Method)
    
    tbl$Method = factor(tbl$Method, 
                        levels = mylevels[order(mylevels, decreasing = TRUE)],
                        labels = mylevels[order(mylevels, decreasing = TRUE)])
    
    return(tbl)
}


set_causal_abs = function(tbl) {
    
    causal_perc_dict = list( 
        "100"  = c(1, 5, 10, 15, 25, 50, 75, 100),
        "1000" = c(0.1, 0.5, 1, 5, 10, 15, 25, 50, 75, 100),
        "5000" = c(0.05, 0.1, 0.5, 1, 5, 10, 25, 50, 75, 100),
        "Inf"  = c(0.01, 0.05, 0.1, 0.5, 5, 15, 50, 100)
    )
    
    entries_labels = get_plotting_labels('entries', causal_perc_dict)
    rows_labels = get_plotting_labels('rows', causal_perc_dict)
    
    
    tbl$causal_abs = NA
    tbl$causal_abs[ tbl$p == 100 & tbl$mask == 'entries' & tbl$method == 'cad_relabelling_keras' ] = 
        entries_labels$p100
    tbl$causal_abs[ tbl$p == 100 & tbl$mask == 'entries' & tbl$method == 'cad_relabelling_lasso' ] = 
        entries_labels$p100
    tbl$causal_abs[ tbl$p == 1000 & tbl$mask == 'entries' & tbl$method == 'cad_relabelling_keras' ] = 
        entries_labels$p1000
    tbl$causal_abs[ tbl$p == 1000 & tbl$mask == 'entries' & tbl$method == 'cad_relabelling_lasso' ] = 
        entries_labels$p1000
    tbl$causal_abs[ tbl$p == 5000 & tbl$mask == 'entries' & tbl$method == 'cad_relabelling_keras' ] = 
        entries_labels$p5000
    tbl$causal_abs[ tbl$p == 5000 & tbl$mask == 'entries' & tbl$method == 'cad_relabelling_lasso' ] = 
        entries_labels$p5000
    tbl$causal_abs[ tbl$p == 19000 & tbl$mask == 'entries' & tbl$method == 'cad_relabelling_keras' ] = 
        entries_labels$p19000
    tbl$causal_abs[ tbl$p == 19000 & tbl$mask == 'entries' & tbl$method == 'cad_relabelling_lasso' ] = 
        entries_labels$p19000
    
    tbl$causal_abs[ tbl$p == 100 & tbl$mask == 'rows' & tbl$method == 'cad_relabelling_keras' ] = 
        rows_labels$p100
    tbl$causal_abs[ tbl$p == 100 & tbl$mask == 'rows' & tbl$method == 'cad_relabelling_lasso' ] = 
        rows_labels$p100
    tbl$causal_abs[ tbl$p == 1000 & tbl$mask == 'rows' & tbl$method == 'cad_relabelling_keras' ] = 
        rows_labels$p1000
    tbl$causal_abs[ tbl$p == 1000 & tbl$mask == 'rows' & tbl$method == 'cad_relabelling_lasso' ] = 
        rows_labels$p1000
    tbl$causal_abs[ tbl$p == 5000 & tbl$mask == 'rows' & tbl$method == 'cad_relabelling_keras' ] = 
        rows_labels$p5000
    tbl$causal_abs[ tbl$p == 5000 & tbl$mask == 'rows' & tbl$method == 'cad_relabelling_lasso' ] = 
        rows_labels$p5000
    tbl$causal_abs[ tbl$p == 19000 & tbl$mask == 'rows' & tbl$method == 'cad_relabelling_keras' ] = 
        rows_labels$p19000
    tbl$causal_abs[ tbl$p == 19000 & tbl$mask == 'rows' & tbl$method == 'cad_relabelling_lasso' ] = 
        rows_labels$p19000
    
    return(tbl)
    
}