#' @import ggplot2
#' @importFrom grid unit
#' @importFrom rasterVis gplot
plot_year <- function(data_raster, aoi_df, variable, title_string='', 
                      size_scale=1) {
    theme_set(theme_bw(base_size=8*size_scale))
    long=lat=value=NULL # For R CMD CHECK
    gplot(data_raster) +
        geom_tile(aes(fill=ordered(value, levels=c(0, 1, 2, 3, 4, 5)))) +
        coord_fixed() + 
        scale_fill_manual("Cover",
                         breaks=c("0", "1", "2", "3", "4", "5"),
                         labels=c('No data',
                                  'Forest',
                                  'Non-forest',
                                  'Forest loss',
                                  'Forest gain',
                                  'Water'),
                          values=c('#000000',
                                   '#1ed72f',
                                   '#d7b14a',
                                   '#f42b2b',
                                   '#432bf4',
                                   '#826d6d'),
                          drop=FALSE) +
        theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
              axis.title.x=element_blank(), axis.title.y=element_blank(),
              panel.background=element_blank(), panel.border=element_blank(),
              panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.background=element_blank(), axis.ticks=element_blank(),
              plot.margin=unit(c(.1, .1, .1, .1), 'cm')) +
        geom_path(data=aoi_df, aes(long, lat), color='yellow', 
                  size=.8*size_scale, alpha=.7, linetype=2) +
        ggtitle(title_string)
}

#' Plot an animation of forest change within a given AOI
#'
#' @export
#' @importFrom tools file_ext
#' @importFrom plyr join
#' @import animation
#' @param gfc_stack a Global Forest Change product subset as a 
#' \code{RasterStack} (as output by \code{\link{gen_stack}})
#' @param out_file filename for animation
#' @param height desired height of the animation GIF in inches
#' @param width desired width of the animation GIF in inches
#' @param dpi dots per inch for the output image
#' @param aoi an Area of Interest (AOI) as a \code{SpatialPolygons*} object.  
#' If the AOI is not in the WGS84 geographic coordinate system, it will be 
#' reprojected to WGS84.
#' @examples
#' #TODO: Add examples
gfc_animate <- function(aoi, gfc_stack, out_file, height=6, width=6, dpi=300) {
    ani.options(outdir=normalizePath(dirname(out_file)),
                ani.width=width*2*dpi, 
                ani.height=height*.5*2*dpi)

    if (tolower(file_ext(out_file)) != 'gif') {
        stop('out_file must end in ".gif"')
    }

    aoi  <- spTransform(aoi, CRS(proj4string(gfc_stack)))
    aoi@data$id <- rownames(aoi@data)
    aoi_points <- fortify(aoi, region="id")
    aoi_df <- join(aoi_points, aoi@data, by="id")

    dates <- seq(2000, 2012, 1)
    saveGIF({
                for (n in 1:nlayers(gfc_stack)) {
                    p <- plot_year(gfc_stack[[n]], aoi_df, "Forest cover", 
                                   dates[n], size_scale=5); print(p)
                }
            }, interval=0.5, movie.name=out_file)
}

# aoi <- readOGR('H:/Data/TEAM/BCI/Vectors', 'ZOI_BCI_2013')
# gfc_stack <- brick('C:/Users/azvoleff/Code/TEAM/gfcanalysis_scripts/ZOI_BCI_2013_gfcstack.envi')
# plot_year(gfc_stack[[5]], aoi_df, 'Forest cover', '', 2)

