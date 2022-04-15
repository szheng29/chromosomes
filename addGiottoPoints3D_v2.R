#' Add point coordinates in xyz as SpatialPointsDataFrame
#'
#' @param gobject A Giotto object.
#' @param coords A `data.frame` or `SpatialPointsDataFrame` with xyz coordinates and feature ids.
#' @param feat_type character. The feat_type must previously exist in the Giotto object when adding the expression. Default = "rna". 
#'
#' @return
#' @export
#'
#' @examples
#' my_working_dir <- "seqFISH_brain/"
#' 
#' #  set Giotto instructions
#' instrs <- createGiottoInstructions(save_plot = FALSE, 
#'                                    show_plot = TRUE,
#'                                    save_dir = my_working_dir, 
#'                                    python_path = NULL)
#' 
#' # create Giotto object
#' my_giotto_object <- createGiottoObject(expression = "seqFISH_brain/rna_gene_by_cell.csv",
#'                                        expression_feat = "rna",
#'                                        spatial_locs = "seqFISH_brain/centroids_loc.csv",
#'                                        instructions = instrs)
#' 
#' # read dna locs 
#' dna_locs <- read.csv("seqFISH_brain/dna_locs.csv")
#' 
#' my_giotto_object <- addGiottoPoints3D_v2(my_giotto_object, dna_locs, feat_type = "rna")
#' 
#' # verify dna points
#' my_giotto_object@feat_info$rna 

addGiottoPoints3D_v2 <- function (gobject, coords, feat_type = "rna") 
{
    if (!inherits(gobject, "giotto")) {
        stop("gobject needs to be a giotto object")
    }
    
    if (inherits(coords, "data.frame")) {
        
        # data.frame like object needs to have 3 coordinate columns and
        # at least one other column as the feat_ID
        
        locs_points <- sp::SpatialPointsDataFrame(coords[,1:3], 
                                                  data = coords, 
                                                  coords.nrs = 1:3)
        
        gobject@feat_info[[feat_type]] <- locs_points
        
    }
    else if (inherits(coords, "SpatialPointsDataFrame")) {
        gobject@feat_info[[feat_type]] <- coords
    }
    else {
        stop("Class ", class(coords), " is not supported")
    }
    return(gobject)
}
