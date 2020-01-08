# Data paths --------------------------------------------------------------

#' @export
get_yeast_data_path = function() {
    lab_folder  = get_lab_folder()
    data_folder = file.path(lab_folder, "YeastData")

    path = list()
    path$processed_data = file.path(data_folder, "yeast_processed_data.rds")
    path$int_data       = file.path(data_folder, "int_data.csv")
    path$int_indices    = file.path(data_folder, "int_indices.csv")
    path$obs_data       = file.path(data_folder, "obs_data.csv")
    return(path)
}


# Excel to rds ------------------------------------------------------------

#' @export
process_yeast_data = function() {

    set.seed(0)

    path = get_yeast_data_path()

    int_data    = read.csv(path$int_data,    header = FALSE)
    int_indices = read.csv(path$int_indices, header = FALSE)
    obs_data    = read.csv(path$obs_data,    header = FALSE)

    # Make int_indices a vector
    int_indices = as_vector(int_indices)
    names(int_indices) = NULL

    # Remove constant columns
    data_list = remove_constant_columns(obs_data, int_data, int_indices, 1:ncol(int_data))

    saveRDS(data_list, file = path$processed_data)

    return(data_list)

}


# Obtain the data ---------------------------------------------------------

#' @export
get_yeast_data = function() {

    path = get_yeast_data_path()

    if ( !file.exists(path$processed_data) ) {
        # Create files
        print("Creating processed file")
        data_list = process_yeast_data()
    } else {
        # Read in files
        print("Reading processed file")
        data_list = readRDS(path$processed_data)
    }

    return(data_list)

}

