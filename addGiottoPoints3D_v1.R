#' Add point coordinates in xyz as spatVector
#'
#' @param gobject  A Giotto object.
#' @param coords A `data.frame` or `spatVector` with xyz coordinates and feature ids.
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
#' my_giotto_object <- addGiottoPoints3D_v1(my_giotto_object, dna_locs, feat_type = "rna")
#' 
#' # verify dna points
#' my_giotto_object@feat_info$rna 



addGiottoPoints3D_v1 <- function (gobject, coords, feat_type = "rna") 
{
    # available features types
    feat_types = gobject@expression_feat
    if(!feat_type %in% feat_types) {
        stop(feat_type, ' is not a feature type in the giotto object \n')
    }
    
    if (inherits(coords, "data.frame")) {
        spatvec = terra::vect(as.matrix(coords[,1:2]), type = "points", atts = coords)
        names(spatvec)[4] = 'feat_ID'

        g_points = create_giotto_points_object(feat_type = feat_type, 
                                               spatVector = spatvec)
    }
    else if (inherits(coords, "spatVector")) {
        g_points = create_giotto_points_object(feat_type = feat_type, 
                                               spatVector = coords)
    }
    else {
        stop("Class ", class(coords), " is not supported")
    }
    
    gobject@feat_info[[g_points@feat_type]] = g_points
    
    return(gobject)
}

## internal function in Giotto
create_giotto_points_object = function(feat_type = 'rna',
                                       spatVector = NULL,
                                       networks = NULL) {
    
    
    # create minimum giotto points object
    g_points = giottoPoints(feat_type = feat_type,
                            spatVector = NULL,
                            networks = NULL)
    
    ## 1. check terra spatVector object
    if(!methods::is(spatVector, 'SpatVector')) {
        stop("spatVector needs to be a spatVector object from the terra package")
    }
    
    g_points@spatVector = spatVector
    
    ## 2. provide feature id
    g_points@feat_type = feat_type
    
    ## 3. feature_network object
    g_points@networks = networks
    
    # giotoPoints object
    return(g_points)
    
}

giottoPoints <- setClass(
    Class = "giottoPoints",
    
    slots = c(
        feat_type = "ANY",
        spatVector = "ANY",
        networks = "ANY"
    ),
    
    prototype = list(
        feat_type = NULL,
        spatVector = NULL,
        networks = NULL
    )
)



