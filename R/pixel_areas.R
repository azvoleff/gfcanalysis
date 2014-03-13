#' Calculate the area of each pixel of a raster
#'
#' Calculates the area of each pixel of a raster, and returns the result as a 
#' \code{RasterLayer}. Useful for calculating areas based on a classified 
#' raster in a geographic coordinate system.
#'
#' @import raster
#' @importFrom geosphere areaPolygon
#' @param x a \code{Raster*} object
#' @return \code{RasterLayer} with pixel areas (in meters)
pixel_areas <- function(x) {
    x <- raster(x, layer=1)
    # Construct polygons for a single column of the raster
    xleft <- xmin(x)
    xright <- xmin(x) + xres(x)
    # Note that ylower and yupper are setup so that the polygons are ordered
    # from high latitude to lowest latitude - needed because rasters are 
    # addressed starting from the upper left corner (highest latitude)
    ylower <- seq(from=(ymax(x) - yres(x)), by=(-yres(x)), length.out=nrow(x))
    yupper <- seq(from=ymax(x), by=(-yres(x)), length.out=nrow(x))

    poly_areas <- function(xl, xr, yl, yu) {
        areaPolygon(matrix(c(xl, yl,
                             xr, yl,
                             xr, yu,
                             xl, yu), ncol=2, byrow=TRUE))
    }
    areas <- mapply(poly_areas, xleft, xright, ylower, yupper)

    areas <- setValues(x, rep(areas, each=ncol(x)))
    names(areas) <- 'area'

    return(areas)
}
