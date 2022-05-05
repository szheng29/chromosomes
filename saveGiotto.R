#' Export Giotto object to a file
#' 
#' @description saveGiotto() wraps the `spatVector` slot from the feat_info list, and exports the Giotto object as .rds file.
#'
#' @param gobject A Giotto object
#' @param file 	A \link{connection} or the name of the file where the Giotto object is saved to.
#'
#' @return Same as \link{saveRDS}, NULL invisibly.
#' @export
#'
#' @examples
#' my_giotto_object <- addGiottoPoints3D(gobject = my_giotto_object,
#'                                       coords = loc.dna,
#'                                       feat_type = 'dna')
#' saveGiotto(my_giotto_object, file = "my_giotto_file.rds")

saveGiotto <- function(gobject, file = "file.rds") {
    
    # verify gobject class
    if (!inherits(gobject, "giotto")) {
        stop("gobject needs to be a giotto object")
    }
    
    # create a temporal gobject to modifiy
    tmp_gobject <- gobject
    
    # list available features 
    features <- names(tmp_gobject@feat_info)
    
    # wrap all feature's spatVector
    for (i in features) {
        print(paste("Wrapping", i, "spatVector"))
        tmp_gobject@feat_info[[i]]@spatVector <- terra::wrap(tmp_gobject@feat_info[[i]]@spatVector)
    }
    
    print("Writting .rds file")
    saveRDS(tmp_gobject, file)
}
