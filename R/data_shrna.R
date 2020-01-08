# Data paths --------------------------------------------------------------

#' @export
get_shrna_data_path = function() {
    lab_folder = get_lab_folder()
    data_folder = file.path(lab_folder, "tuebingen_shRNA")

    path = list()
    path$processed_data = file.path(data_folder, "shrna_processed_data.rds")
    path$raw_data       = file.path(data_folder, "Count_matrix_PDGWAS_featurecounts_cpm.xlsx")
    return(path)
}


# Excel to rds ------------------------------------------------------------

#' @export
process_shrna_data = function() {

    set.seed(0)

    path = get_shrna_data_path()
    excel_file = path$raw_data

    genes_0 = read_excel(excel_file, col_names = TRUE, range = "A1:A58234")
    interventions_0 = read_excel(excel_file, col_names = FALSE, range = "C1:IN1")
    dataset_0 = read_excel(excel_file, col_names = TRUE, range = "C1:IN58234")

    # Process gene and interventions names, and data
    genes = as_vector(genes_0)
    names(genes) = NULL

    interventions = as_vector(interventions_0)
    names(interventions) = NULL

    dataset = t(dataset_0) # rownames created by default using column names
    colnames(dataset) = genes

    # Divide observational (Scrb) and interventional data
    id_obs = str_which(interventions, "Scrb.*")
    obs_data = dataset[id_obs, ]

    int_names = interventions[-id_obs]
    int_data = dataset[-id_obs, ]
    stopifnot(all.equal(rownames(int_data), int_names))

    # Remove interventions on same gene (_*)
    int_names_clean = str_remove(int_names, "_.*")
    id_dupl_int_names_clean = duplicated(int_names_clean)
    int_names = int_names_clean[!id_dupl_int_names_clean]
    int_data = int_data[!id_dupl_int_names_clean, ]
    rownames(int_data) = int_names
    stopifnot(all.equal(rownames(int_data), int_names))

    # Remove interventions not in genes
    id_int_not_in_genes = which(!(int_names %in% genes))
    int_names = int_names[-id_int_not_in_genes]
    int_data = int_data[-id_int_not_in_genes, ]
    rownames(int_data) = int_names
    stopifnot(all.equal(rownames(int_data), int_names))

    # Remove constant columns
    data_list = remove_constant_columns(obs_data, int_data, int_names, genes)

    # Save to rds
    saveRDS(data_list, file = path$processed_data)

    return(data_list)

}


# Obtain the data ---------------------------------------------------------

#' @export
get_shrna_data = function() {

    path = get_shrna_data_path()

    if ( !file.exists(path$processed_data) ) {
        # Create files
        print("Creating processed file")
        data_list = process_shrna_data()
    } else {
        # Read in files
        print("Reading processed file")
        data_list = readRDS(path$processed_data)
    }

    return(data_list)

}

