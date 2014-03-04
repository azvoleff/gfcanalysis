#' @import ggplot2
#' @importFrom grid unit
#' @importFrom rasterVis gplot
plot_gfc <- function(data_raster, aoi_df, variable, title_string='', 
                     size_scale=1, maxpixels=50000) {
    theme_set(theme_bw(base_size=8*size_scale))
    long=lat=value=Region=NULL # For R CMD CHECK
    gplot(data_raster, maxpixels=maxpixels) +
        geom_tile(aes(fill=factor(value, levels=c(1, 2, 3, 4, 5, 6, 0)))) +
        coord_fixed() + 
        scale_fill_manual("Cover",
                         breaks=c("1", "2", "3", "4", "5", "6", "0"),
                         labels=c('Forest', # 1
                                  'Non-forest', # 2
                                  'Forest loss', # 3
                                  'Forest gain', # 4
                                  'Loss and gain', # 5
                                  'Water', # 6
                                  'No data'), # 0
                          values=c('#008000', # forest
                                   '#ffa500', # non-forest
                                   '#ff0000', # forest loss
                                   '#0000ff', # forest gain
                                   '#ff00ff', # loss and gain
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
                  size=.6*size_scale, alpha=.3) +
        geom_path(aes(long, lat, color=Region), data=aoi_df, 
                  size=.4*size_scale, linetype=2) +
        scale_color_manual(values='black') +
        ggtitle(title_string)
}

#' Plot an animation of forest change within a given AOI
#'
#' Produces an animation of annual forest change in the area bounded by the 
#' extent of a given AOI. The AOI polygon is also plotted on the image. The 
#' \code{gfc_stack} must be pre-calculated using the \code{\link{annual_stack}} 
#' function. The animation can be either an animated GIF (if \code{type} is set 
#' to 'gif') or a series of '.png' files with a corresponding '.html' webpage 
#' showing a simple viewer and the forest change animation (if \code{type} is 
#' set to 'html'). The HTML option is recommended as it requires no additional 
#' software to produce it. The animated GIF option will only work if the 
#' imagemagicK software package is installed beforehand (this is done outside 
#' of R).
#'
#' @seealso \code{\link{annual_stack}}
#'
#' @export
#' @import ggplot2
#' @importFrom tools file_ext
#' @importFrom plyr join
#' @import animation
#' @param aoi an Area of Interest (AOI) as a \code{SpatialPolygons*} object.  
#' If the AOI is not in the WGS84 geographic coordinate system, it will be 
#' reprojected to WGS84.
#' @param gfc_stack a GFC product subset as a 
#' \code{RasterStack} (as output by \code{\link{annual_stack}})
#' @param out_dir folder for animation output
#' @param out_basename basename to use when naming animation files
#' @param site_name name of the site (used in making title)
#' @param aoi_label label to use in legend for AOI polygon
#' @param type type of animation to make. Can be either "gif" or "html"
#' @param height desired height of the animation GIF in inches
#' @param width desired width of the animation GIF in inches
#' @param dpi dots per inch for the output image
animate_annual <- function(aoi, gfc_stack, out_dir, out_basename, site_name='', 
                           aoi_label='AOI', type='html', height=3, width=3, 
                           dpi=300) {
    if (nlayers(gfc_stack) != 13) {
        warning('gfc_stack has ', nlayers(gfc_stack),
                ' layers - full annual GFC product stack should have 13 layers')
    }
    if (!file_test('-d', out_dir)) {
        dir.create(out_dir)
    }
    out_dir <- normalizePath(out_dir)
    ani.options(outdir=out_dir, ani.width=width*dpi, ani.height=height*dpi, 
                verbose=FALSE)

    if (tolower(file_ext(out_basename)) != '') {
        stop('out_basename should not have an extension')
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
        out_file <- paste(out_basename, '.gif')
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
                 img.name=out_basename,
                 imgdir=paste0(out_basename, '_imgs'),
                 outdir=out_dir,
                 htmlfile=paste0(out_basename, ".html"),
                 autobrowse=FALSE,
                 title=paste(site_name, 'forest change'))
    }
}
