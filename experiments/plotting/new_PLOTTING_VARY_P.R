# PLOTS
# Experiment: vary_p

# Workspace ---------------------------------------------------------------

# rm(list = ls())
# 
# library(cad)
library(tidyverse)
library(filehash)
library(colorspace)
library(MLmetrics)
library(ROCR)
library(R.devices)

source('experiments/plotting/plottingFunctions.R')


# CHD:
for(f in list.files(file.path(getwd(), "R") )){
    source(file.path(getwd(), "R", f))
}
for(f in list.files(file.path(getwd(), "experiments", "methods") )){
    source(file.path(getwd(), "experiments", "methods", f))
}
source( file.path(getwd(), "experiments", "functions", "run_experiment.R") )
require(R.utils)
fid_data <- filePath(getwd(), 'data', 'shrna_processed_data.rds') 
data_list <- readRDS(fid_data)

res_dir <- getwd() 
# 



dict_auc_obj = list(method_cad_lasso = "g_hat",
                    method_cor_kendall = "g_hat", 
                    method_cor_pearson = "g_hat",
                    method_cor_spearman = "g_hat",
                    method_gies = "tc_strict",
                    method_glasso = "g_hat",
                    method_ida = "g_hat",
                    method_pc = "tc_strict",
                    method_pc_05 = "tc_strict",
                    method_rfci = "g_hat_strict",
                    method_shrinkage = "g_hat",
                    method_cad_keras = "g_hat",
                    method_lvida = "g_hat",
                    method_lvida_05 = "g_hat")


#db_res_path = file.path("C:/Users/user-adm/Documents/cad_results/vary_p")
db_res_path = file.path(res_dir, "results", "vary_p")
db_res = dbInit(db_res_path, type = "RDS")

names = dbList(db_res)
n = length(names)

tbl = tibble(
    p = NA, 
    rep = NA, 
    mask = NA, 
    method = NA, 
    time = NA
)

shared_variables = names(tbl)

for (i in 1:n) {
    res = db_res[[ names[i] ]]
    
    for (n in shared_variables) {
        tbl[i, n] = res[[n]]
    }

    G_hat_type = dict_auc_obj[[ res$method ]]
    
    auc_obj = get_auc(res$G_star, 
                      res$G0, 
                      res[[ G_hat_type ]],
                      res$method)
    
    tbl[i, 'auc'] = auc_obj$auc
    tbl[i, 'tpr'] = auc_obj$tpr
    tbl[i, 'fpr'] = auc_obj$fpr
    tbl[i, 'y_true'] = list( list(auc_obj$y_true) )
    tbl[i, 'y_pred'] = list( list(auc_obj$y_pred) )
}


tbl =
    tbl %>%
    mutate( method = str_remove(method, "method_") ) %>%
    mutate( p = ifelse(p > 15000, 19000, p) ) %>%
    filter( method != 'glasso', 
            method != "cor_spearman",
            method != "rfci") %>%
    cad_name_mapping()


n_rep = tbl$rep %>% unique %>% sort %>% length

tbl_auc =
    tbl %>%
    filter(!(method %in% c('gies', 'rfci', 'pc'))) %>%
    group_by(p, mask, method) %>%
    summarise(avg_auc = mean(auc, na.rm = TRUE),
              stderr_auc = sd(auc, na.rm = TRUE) / sqrt(n_rep),
              avg_time = mean(time, na.rm = TRUE),
              stderr_time = sd(time, na.rm = TRUE) / sqrt(n_rep)) %>%
    cad_name_mapping()


# Plots entries

width = 14
height = 10


#eps('Z:/PAPERS/CAD/Figures/vary_p__entries__panel_1.eps', width, height)
ggplot(filter(tbl_auc, !(method %in% get_binary_methods()), mask == 'entries'), 
       aes(x = p,
           y = avg_auc,
           ymin = avg_auc - stderr_auc,
           ymax = avg_auc + stderr_auc,
           color = Method,
           shape = Method)) +
    geom_line() +
    geom_point(size = 3) +
    geom_errorbar(width = 0.03) +
    scale_color_manual(values = colorspace::qualitative_hcl(length(unique(tbl_auc$Method)), palette = "Dark 3")) +
    scale_shape_manual(values = seq_along(tbl_auc$Method)) +
    theme_light(base_size = 25) +
    # facet_grid(. ~ mask, labeller = label_both) +
    theme(legend.position = "right", legend.direction = "vertical") +
    labs(y = "AUC", x = "p") +
    ylim(c(0.2,1)) +
    scale_x_log10(breaks = c(25, 50, 100, 500, 1000, 5000, 10000, 19000))
#dev.off()


width = 10
height = 10

#ps('Z:/PAPERS/CAD/Figures/vary_p__entries__panel_2.eps', width, height)
plot_average_roc(tbl, 100, 'entries')
#dev.off()

#eps('Z:/PAPERS/CAD/Figures/vary_p__entries__panel_3.eps', width, height)
plot_average_roc(tbl, 1000, 'entries')
#dev.off()

#eps('Z:/PAPERS/CAD/Figures/vary_p__entries__panel_4.eps', width, height)
#plot_average_roc(tbl, 19000, 'entries')
#dev.off()


# Plots rows

width = 14
height = 10

#eps('Z:/PAPERS/CAD/Figures/vary_p__rows__panel_1.eps', width, height)
ggplot(filter(tbl_auc, !(method %in% get_binary_methods()), mask == 'rows'), 
       aes(x = p,
           y = avg_auc,
           ymin = avg_auc - stderr_auc,
           ymax = avg_auc + stderr_auc,
           color = Method,
           shape = Method)) +
    geom_line() +
    geom_point(size = 3) +
    geom_errorbar(width = 0.03) +
    scale_color_manual(values = colorspace::qualitative_hcl(length(unique(tbl_auc$Method)), palette = "Dark 3")) +
    scale_shape_manual(values = seq_along(tbl_auc$Method)) +
    theme_light(base_size = 25) +
    # facet_grid(. ~ mask, labeller = label_both) +
    theme(legend.position = "right", legend.direction = "vertical") +
    labs(y = "AUC", x = "p") +
    ylim(c(0.2,1)) +
    scale_x_log10(breaks = c(25, 50, 100, 500, 1000, 5000, 10000, 19000))
#dev.off()


width = 10
height = 10

#eps('Z:/PAPERS/CAD/Figures/vary_p__rows__panel_2.eps', width, height)
plot_average_roc(tbl, 100, 'rows')
#dev.off()

#eps('Z:/PAPERS/CAD/Figures/vary_p__rows__panel_3.eps', width, height)
plot_average_roc(tbl, 1000, 'rows')
#dev.off()

#eps('Z:/PAPERS/CAD/Figures/vary_p__rows__panel_4.eps', width, height)
#plot_average_roc(tbl, 19000, 'rows')
#dev.off()


# Plots time


width = 7
height = 10


#eps('Z:/PAPERS/CAD/Figures/vary_p__time.eps', width * 2, height)

tbl_time =
    tbl %>%
    filter(method != 'glasso', mask == 'rows') %>%
    group_by(p, method) %>%
    summarise(avg_auc = mean(auc, na.rm = TRUE),
              stderr_auc = sd(auc, na.rm = TRUE) / sqrt(n_rep),
              avg_time = mean(time, na.rm = TRUE),
              stderr_time = sd(time, na.rm = TRUE) / sqrt(n_rep)) %>%
    cad_name_mapping()

ggplot(tbl_time, 
       aes(x = p,
           y = 1 + avg_time,
           ymin = 1 + (avg_time - stderr_time),
           ymax = 1 + (avg_time + stderr_time),
           color = Method,
           shape = Method)) +
    geom_line() +
    geom_point(size = 3) +
    geom_errorbar(width = 0.03) +
    scale_color_manual(values = 
                           colorspace::qualitative_hcl(length(unique(tbl_time$Method)), palette = "Dark 3")) +
    scale_shape_manual(values = seq_along(tbl_time$Method)) +
    theme_light(base_size = 25) +
    # facet_grid(. ~ mask, labeller = as_labeller(c('entries' = 'entry-wise', 'rows' = 'row-wise'))) +
    theme(legend.position = "right", legend.direction = "vertical") +
    labs(y = "Wall clock time (s)", x = "p") +
    scale_x_log10(breaks = c(25, 50, 100, 500, 1000, 5000, 10000, 19000)) +
    scale_y_log10()

#dev.off()


