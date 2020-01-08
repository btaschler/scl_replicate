
#' Reads and stacks tidy data files
#' @param data_path Path where the files are
#' @param pattern Pattern used to select the files
#' @return Tibble with tidy data with file rows stacked
#' @export
#' 
read_stack_files = function(data_path, pattern) {

    files <- dir(data_path, pattern = pattern) # get file names

    data <- files %>%
        # read in all the files, appending the path before the filename
        map(function(x) read_csv(file.path(data_path, x))) %>%
        reduce(rbind)
    return(data)

}
