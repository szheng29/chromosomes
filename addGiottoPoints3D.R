#' Add sub cellular 3D coordinates to Giotto object
#'
#' @param gobject  A Giotto object.
#' @param coords A \link{data.frame} or `spatVector` with at least xyz coordinates and feature ids.
#' @param feat_type a character. The feat_type must previously exist in the Giotto object. Default = "rna".
#'
#' @return A Giotto object with a `spatVector` object in the feat_info slot
#' @export
#'
#' @examples
#' # create Giotto object
#' my_giotto_object <- createGiottoObject(expression = "rna_gene_by_cell.csv",
#'                                        expression_feat = "rna",
#'                                        spatial_locs = "centroids_loc.csv")
#' 
#' # read sub cellular coordinates 
#' sub_locs <- read.csv("sub_locs.csv")
#' 
#' my_giotto_object <- addGiottoPoints3D(my_giotto_object, 
#'                                       coords = sub_locs, 
#'                                       feat_type = "rna")
#' 
#' # verify feat info added
#' my_giotto_object@feat_info$rna 

addGiottoPoints3D <- function (gobject, coords, feat_type = "rna") 
{
    # verify gobject class
    if (!inherits(gobject, "giotto")) {
        stop("gobject needs to be a giotto object")
    }
    # available features types
    feat_types = gobject@expression_feat
    if(!feat_type %in% feat_types) {
        stop(feat_type, ' is not a feature type in the giotto object \n')
    }
    
    if (inherits(coords, "data.frame")) {
        spatvec = terra::vect(as.matrix(coords[,1:2]), type = "points", atts = coords)
        names(spatvec)[4] = 'feat_ID'

        g_points = Giotto:::create_giotto_points_object(feat_type = feat_type, 
                                               spatVector = spatvec)
    }
    else if (inherits(coords, "spatVector")) {
        g_points = Giotto:::create_giotto_points_object(feat_type = feat_type, 
                                               spatVector = coords)
    }
    else {
        stop("Class ", class(coords), " is not supported")
    }
    
    gobject@feat_info[[g_points@feat_type]] = g_points
    
    return(gobject)
}
