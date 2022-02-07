#' Plot three-dimensional chromosomes
#'
#' @param coords 
#'
#' @return A `rgl` interactive three-dimensional plot
#' @export
#'
#' @examples
#' library(shiny)
#' library(rgl)
#' teta = seq(0, 3*pi, len = 1000)
#' df1 <- data.frame(x = seq(1,2,len = 1000),
#'                   y = cos(teta),
#'                   z = cos(teta)*sin(teta),
#'                   chr = rep(1,1000))
#' df2 <- data.frame(x = cos(teta)*sin(teta),
#'                   y = seq(0,2,leng = 1000),
#'                   z = cos(teta),
#'                   chr = rep(2,1000))
#' df3 <- rbind(df1,df2)
#' plot3dChromosomes(df3)

plot3dChromosomes <- function(coords, type = 's', col = 'grey95',...) {
    app = shinyApp(
        ui <- bootstrapPage(
            rglwidgetOutput("rglPlot")
        ),
        
        server <- function(input, output) {
            output$rglPlot <- renderRglwidget({
                try(close3d(), silent = TRUE)
                bg3d(col = "white")
                with(coords, plot3d(x, y, z, 
                                    type = type,
                                    col = col, ...))
                axes3d()
                rglwidget()
            })
            
        }
        
    )
    runApp(app)
}
