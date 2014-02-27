#' Generates an annual stack of forest/non-forest
#'
#' Uses the datamask, treecover2000, loss, gain, and lossyear layers to produce 
#' a raster  stack within the given AOI coded as: 0 - nodata, 1 - forest 
#' 2 - non-forest, 3 - forest loss, 4 - forest gain, 5 - water
#'
#' @export
#' @import raster
#' @param aoi an Area of Interest (AOI) as a \code{SpatialPolygons*} object.  
#' If the AOI is not in the WGS84 geographic coordinate system, it will be 
#' reprojected to WGS84.
#' @param gfc extract of Global Forest Change product for a given AOI (see 
#' \code{\link{gfc_extract}})
#' @param forest_threshold percent woody vegetation to use as a threshold for 
#' mapping forest/non-forest
gen_stack <- function(aoi, gfc, forest_threshold=50) {
    # Code forest as 1, non-forest as 2
    forest <- (gfc$treecover2000 > forest_threshold) & (gfc$datamask == 1)
    forest[forest != 1] <- 2
    # Code water as 5
    forest[gfc$datamask == 2] <- 5

    recode_values <- function(this_year, floss, fgain, lossyear, datamask) {
        # Recode according to loss, gain, and lossyear values
        this_year[floss == 1 & lossyear == year] <- 3
        this_year[fgain == 1 & lossyear == year] <- 4
        # Code water as 5
        this_year[datamask == 2] <- 5
        # Code no data as 0
        this_year[datamask == 0] <- 0
        return(this_year)
    }

    out <- raster(gfc)
    out <- addLayer(out, forest) # Initial observation (year 2000)
    names(out) <- 'y2000'
    for (year in 1:12) {
        # Start with obs from past year
        this_year <- overlay(raster(out, layer=year),
                             gfc$loss, 
                             gfc$gain, 
                             gfc$lossyear, 
                             gfc$datamask, 
                             fun=recode_values, datatype='INT1U')
        out <- addLayer(out, this_year)
        names(out)[year + 1] <- paste0('y20', sprintf('%02i', year))
    }

    out <- setZ(out, seq(as.Date('2000-1-1'), as.Date('2012-1-1'), by='year'))

    return(out)
}
