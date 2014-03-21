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
    out <- raster(x)
    # Construct polygons for a single column of the raster
    xleft <- xmin(out)
    xright <- xmin(out) + xres(out)
    # Note that ylower and yupper are setup so that the polygons are ordered
    # from high latitude to lowest latitude - needed because rasters are 
    # addressed starting from the upper left corner (highest latitude)
    ylower <- seq(from=(ymax(out) - yres(out)), by=(-yres(out)), length.out=nrow(out))
    yupper <- seq(from=ymax(out), by=(-yres(out)), length.out=nrow(out))

    poly_areas <- function(xl, xr, yl, yu) {
        areaPolygon(matrix(c(xl, yl,
                             xr, yl,
                             xr, yu,
                             xl, yu), ncol=2, byrow=TRUE))
    }
    areas <- mapply(poly_areas, xleft, xright, ylower, yupper)
    
    # Write areas by block to avoid running out of memory with very large 
    # rasters
    bs <- blockSize(out)
    out <- writeStart(out, rasterTmpFile())
    for (block_num in 1:bs$n) {
        start_row <- bs$row[block_num]
        end_row <- start_row + bs$nrows[block_num]
        writeValues(out, rep(areas[start_row:end_row], each=ncol(out)), bs$row[block_num])
    }
    out <- writeStop(out)
    names(out) <- 'area'

    return(out)
}
