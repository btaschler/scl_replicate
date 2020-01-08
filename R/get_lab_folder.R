#' Mukherjee lab folder
#'
#' @export
#'
get_lab_folder = function() {
    if (.Platform$OS.type == "unix") {
        folder = file.path('/Volumes', 'ag-mukherjee')
    } else {
        folder = file.path('//fileserver.dzne.de', 'ag-mukherjee')
    }
}
