#' @import ggplot2
#' @importFrom grid unit
#' @importFrom rasterVis gplot
plot_gfc <- function(data_raster, aoi_df, aoi_label, variable, 
                      title_string='', size_scale=1, maxpixels=50000) {
    theme_set(theme_bw(base_size=8*size_scale))
    long=lat=value=NULL # For R CMD CHECK
    gplot(data_raster, maxpixels=maxpixels) +
        geom_tile(aes(fill=factor(value, levels=c(1, 2, 3, 4, 5, 0)))) +
        coord_fixed() + 
        scale_fill_manual("Cover",
                         breaks=c("1", "2", "3", "4", "5", "0"),
                         labels=c('Forest',
                                  'Non-forest',
                                  'Forest loss',
                                  'Forest gain',
                                  'Water',
                                  'No data'),
                          values=c('#008000', # forest
                                   '#ffa500', # non-forest
                                   '#ff0000', # forest loss
                                   '#0000ff', # forest gain
                                   '#c0c0c0', # water
                                   '#101010'), # no data
                          drop=FALSE) +
        theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
              axis.title.x=element_blank(), axis.title.y=element_blank(),
              panel.background=element_blank(), panel.border=element_blank(),
              panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
              plot.background=element_blank(), axis.ticks=element_blank(),
              plot.margin=unit(c(.1, .1, .1, .1), 'cm')) +
        geom_path(aes(long, lat), color='black', data=aoi_df, 
                  size=.9*size_scale, alpha=.3) +
        geom_path(aes(long, lat, color=Region), data=aoi_df, 
                  size=.5*size_scale, linetype=2) +
        scale_color_manual(values='black') +
        ggtitle(title_string)
}

#' Plot an animation of forest change within a given AOI
#'
#' @export
#' @importFrom tools file_ext
#' @importFrom plyr join
#' @import animation
#' @param aoi an Area of Interest (AOI) as a \code{SpatialPolygons*} object.  
#' If the AOI is not in the WGS84 geographic coordinate system, it will be 
#' reprojected to WGS84.
#' @param aoi_label label to use in legend for AOI polygon
#' @param out_name basename for animation output
#' @param gfc_stack a Global Forest Change product subset as a 
#' \code{RasterStack} (as output by \code{\link{gen_stack}})
#' @param out_name basename for animation output
#' @param site_name name of the site (used in making title)
#' @param type type of animation to make. Can be either "gif" or "html"
#' @param height desired height of the animation GIF in inches
#' @param width desired width of the animation GIF in inches
#' @param dpi dots per inch for the output image
gfc_animate <- function(aoi, gfc_stack, out_name, site_name='', 
                        type='gif', height=3, width=3, dpi=300) {
    out_name <- basename(out_name)
    out_dir <- dirname(out_name)
    if (!file_test('-d', out_dir)) {
        dir.create(out_dir)
    }
    out_dir <- normalizePath(out_dir)
    ani.options(outdir=out_dir, ani.width=width*dpi, ani.height=height*dpi, 
                verbose=FALSE)


    if (tolower(file_ext(out_name)) != '') {
        stop('out_name should not have an extension')
    }

    if (!(type %in% c('gif', 'html'))) {
        stop('type must be gif or html')
    }

    aoi <- spTransform(aoi, CRS(proj4string(gfc_stack)))
    aoi@data$id <- rownames(aoi@data)
    aoi_points <- fortify(aoi, region="id")
    aoi_df <- join(aoi_points, aoi@data, by="id")
    aoi_df$Region <- aoi_label

    dates <- seq(2000, 2012, 1)

    # Round maxpixels to nearest 1000
    maxpixels <- ceiling((width * height * dpi^2)/1000) * 1000
    if (type == 'gif') {
        out_file <- paste(out_name, '.gif')
        saveGIF({
                    for (n in 1:nlayers(gfc_stack)) {
                        p <- plot_gfc(gfc_stack[[n]], aoi_df, "Forest cover", 
                                      dates[n], size_scale=4, maxpixels)
                        print(p)
                    }
                }, interval=0.5, movie.name=out_file)
    } else if (type == 'html') {
        saveHTML({
                    for (n in 1:nlayers(gfc_stack)) {
                        p <- plot_gfc(gfc_stack[[n]], aoi_df, "Forest cover", 
                                      dates[n], size_scale=4, maxpixels)
                        print(p)
                    }
                 },
                 img.name=out_name, imgdir=out_name, outdir=out_dir,
                 htmlfile=paste0(out_name, ".html"), autobrowse=FALSE,
                 title=paste("Forest change at", site_name))
    }
}

# library(gfcanalysis)
# library(raster)
# library(rasterVis)
# library(rgdal)
# library(plyr)
# #aoi <- readOGR('H:/Data/TEAM/BCI/Vectors', 'ZOI_BCI_2013')
# aoi <- readOGR('D:/azvoleff/Data/BCI/Vectors', 'ZOI_BCI_2013')
# #gfc_stack <- brick('C:/Users/azvoleff/Code/TEAM/gfcanalysis_scripts/ZOI_BCI_2013_gfcstack.envi')
#
# gfc_animate(aoi, "ZOI", gfc_stack, 'BCI_new/BCI', 'Barro Colorado Nature Monument', 'html')
#
# plot_gfc(gfc_stack[[5]], aoi_df, "ZOI", 'Forest cover', 2000, 2, 40000)
