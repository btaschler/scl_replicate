### 
### LV-IDA - Vary p - shrna data
### 

# rm(list = ls())
# 
# library(tidyverse)
# library(biglasso)
# library(cad)
# library(pcalg)
# library(huge)
# library(corpcor)
# library(filehash)

# Load functions

#source( file.path(getwd(), "experiments", "experiments_add_path.R") )

# Reproducibility

set.seed(0)

# Data: shrna

#data_list   = get_shrna_data()
int_data    = data_list$int_data
int_indices = data_list$int_indices
int_names   = data_list$int_names
obs_data    = data_list$obs_data


# Experimental settings ---------------------------------------------------

# Data
int_thresh_count = 35
int_graph_count  = 35
int_sample_count = 35

obs_thresh_count = NULL
obs_sample_count = 19

thresholding     = "max_min"
sparsity_factor  = 2
data_name        = "shrna"
remove_near_constant_columns = TRUE

# Masking
percentage_visible = 50
mask_seq = c("rows", "entries")

# Repetitions
rep_seq = 1:3 #10                                                                ## TESTING

# Methods

p_seq = c(50,100) #c(25, 50, 100, 200, 500, 1000, 2000, 5000, 10000, Inf)        ## TESTING

method_p_list = list("method_lvida_05" = p_seq[p_seq <= 1000])

method_seq = names(method_p_list)


# Generate datasets -------------------------------------------------------

db_vary_p_path = file.path(getwd(), "data", "vary_p")
db_vary_p = dbInit(db_vary_p_path, type = "RDS")


# Experiment --------------------------------------------------------------


set.seed(0)

db_res_path = file.path(getwd(), "results", "vary_p")
#dbCreate(db_res_path, type = "RDS")
db_res = dbInit(db_res_path, type = "RDS")

for (p in p_seq) {
    
    # repetition
    for (k in rep_seq) {
        
        data = db_vary_p[[ sprintf('p_%s__rep_%s', p, k) ]]
        
        X = data$X
        
        X = scale(X)
        stopifnot( !anyNA(X) )
        
        G_star = data$G_star
        
        # mask
        for (mask in mask_seq) {
            
            G0 = switch(mask,
                        entries = data$G0_entries,
                        rows    = data$G0_rows)
            
            # method
            for (method in method_seq) {
                
                method_p_seq = method_p_list[[method]]
                
                if ( p > max(method_p_seq) ) {
                    next
                } else {
                    
                    db_res_name = sprintf('p_%s__rep_%s__mask_%s__%s',
                                          p,    k,      mask,    method)
                    
                    run_experiment(X, G0, G_star, method, db_res, db_res_name, 
                                   # Data
                                   data_name = data_name,
                                   thresholding = thresholding,
                                   sparsity_factor = sparsity_factor,
                                   remove_near_constant_columns = remove_near_constant_columns,
                                   # Masking
                                   mask = mask,
                                   percentage_visible = percentage_visible,
                                   # Experiment
                                   p = ncol(X),
                                   rep = k)
                    
                }
            }
        }
    }
}


# filesstrings::file.move(
#     list.files(file.path("~", "cad_results", "vary_p"), full.names = TRUE),
#     file.path(get_lab_folder(), "cad", "results", "vary_p", "vary_p")
# )

# filesstrings::file.move(
#     list.files(file.path("~", "cad_datasets", "vary_p"), full.names = TRUE),
#     file.path(get_lab_folder(), "cad", "datasets", "vary_p", "vary_p")
# )