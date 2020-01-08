### 
### Vary p - shrna data ###
###

#rm(list = ls())

# library(tidyverse)
# library(biglasso)
# library(cad)
# library(pcalg)
# library(huge)
# library(corpcor)
# library(filehash)

### Load functions

#source( file.path(getwd(), "experiments", "experiments_add_path.R") )

### Reproducibility

set.seed(0)

### Data: shrna

#data_list   = get_shrna_data()
int_data    = data_list$int_data
int_indices = data_list$int_indices
int_names   = data_list$int_names
obs_data    = data_list$obs_data


### Experiment settings ---------------------------------------------------

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

p_seq = c(50, 100) #c(25, 50, 100, 200, 500, 1000)                               ## TESTING

# method_p_list = list("method_cad_lasso" = p_seq,
#                      "method_cad_keras" = p_seq)
method_p_list = list("method_cad_lasso" = p_seq)

method_seq = names(method_p_list)


### Generate datasets -------------------------------------------------------

db_vary_p_path = file.path(getwd(), "data", "vary_p")
if(!dir.exists(db_vary_p_path)){ dbCreate(db_vary_p_path, type = "RDS") }

db_vary_p = filehash::dbInit(db_vary_p_path, type = "RDS")

# Get true graph
graph = cad_get_true_graph(obs_data, int_data, int_indices,
                           thresholding     = thresholding,
                           int_thresh_count = int_thresh_count,
                           sparsity_factor  = sparsity_factor,
                           obs_thresh_count = obs_thresh_count)

for (p in p_seq) {

    for (k in rep_seq) {

        # Get data (X, G_star)
        data = cad_process_data(graph$obs_data, graph$int_data, graph$int_indices, graph$G_star,
                                int_graph_count  = int_graph_count,
                                int_sample_count = int_sample_count,
                                obs_sample_count = obs_sample_count,
                                remove_near_constant_columns = remove_near_constant_columns)

        # Mask entries
        G0_entries = mask_graph(data$G_star, percentage_visible = percentage_visible, mask = "entries")
        G0_rows    = mask_graph(data$G_star, percentage_visible = percentage_visible, mask = "rows")

        stopifnot( all.equal(sort(unique(c(G0_entries))), c(0,1,2)) )
        stopifnot( all.equal(sort(unique(c(G0_rows))), c(0,1,2)) )

        # In case p = Inf, reduce to max p = ncol(X)
        p_sample = min(p, ncol(data$X))

        # Sample genes
        if (p_sample > nrow(data$G_star)) {
            gene_ids = cad_sample_genes(data$X, data$G_star, p_sample)
        } else {
            gene_ids = 1:p_sample
        }

        row_ids = seq_len(min( nrow(data$G_star), p_sample ))

        # Assign
        db_vary_p[[ sprintf('p_%s__rep_%s', p, k) ]] =
            list(X          = data$X[ , gene_ids],
                 G_star     = data$G_star[row_ids, gene_ids],
                 G0_entries = G0_entries[row_ids, gene_ids],
                 G0_rows    = G0_rows[row_ids, gene_ids])

    }
}



### Experiment --------------------------------------------------------------

set.seed(0)

db_res_path = file.path(getwd(), "results", "vary_p")
if(!dir.exists(db_res_path)){ dbCreate(db_res_path, type = "RDS") }
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
                    
                    res_tmp = run_experiment(X, G0, G_star, method, db_res, db_res_name, 
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
