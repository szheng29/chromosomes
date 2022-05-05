#' Import Giotto object from a file
#' 
#' @description readGiotto() imports the Giotto object from an .rds file, and unpacks the `spatVector` slot from the feat_info list.
#'
#' @param file A \link{connection} or the name of the file where the Giotto object is read from.
#'
#' @return A Giotto object
#' @export
#'
#' @examples
#' my_giotto_object <- readGiotto("my_giotto_file.rds")
#' 
#' ## see the feat_info containing a regular spatVector
#' my_giotto_object@feat_info
 
readGiotto <- function(file = "file.rds") {
    
    # read object
    print("Reading .rds file")
    gobject <- readRDS(file)
    
    # list available features 
    features <- names(gobject@feat_info)
    
    # unpack all feature's spatVector
    for (i in features) {
        print(paste("Unpacking", i, "spatVector"))
        gobject@feat_info[[i]]@spatVector = terra::vect(gobject@feat_info[[i]]@spatVector)
    }
    
    return(gobject)
}
