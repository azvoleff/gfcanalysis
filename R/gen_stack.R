recode_gfc <- function(this_year, floss, fgain, lossyear, datamask) {
    this_year[floss == 1 & lossyear == year] <- 3 # loss
    this_year[fgain] <- 4 # gain (no years attached to gain)
    this_year[fgain & floss] <- 5 # loss and gain
    this_year[datamask == 2] <- 6 # water
    this_year[datamask == 0] <- 0 # no data
    return(this_year)
}

#' Generates an annual stack of forest/non-forest
#'
#' Uses the datamask, treecover2000, loss, gain, and lossyear layers to produce 
#' a raster stack coded as: 0 - nodata, 1 - forest, 2 - non-forest, 3 - forest 
#' loss, 4 - forest gain, 5 - forest loss and gain, 6 - water
#'
#' @export
#' @import raster
#' @param gfc extract of Global Forest Change product for a given AOI (see 
#' \code{\link{gfc_extract}})
#' @param forest_threshold percent woody vegetation to use as a threshold for 
#' mapping forest/non-forest
gen_stack <- function(gfc, forest_threshold=50) {
    out <- raster(gfc)
    names(out) <- 'y2000'
    for (year in 0:12) {
        if (year == 0) {
            # Code forest as 1, non-forest as 2
            forest <- (gfc$treecover2000 > forest_threshold) & (gfc$datamask == 1)
            forest[forest != 1] <- 2
            last_year <- forest
        } else {
            last_year <- raster(out, layer=year)
        }
        this_year <- overlay(last_year,
                             gfc$loss, 
                             gfc$gain, 
                             gfc$lossyear, 
                             gfc$datamask, 
                             fun=recode_gfc, datatype='INT1U')
        out <- addLayer(out, this_year)
        names(out)[year + 1] <- paste0('y20', sprintf('%02i', year))
    }
    out <- setZ(out, seq(as.Date('2000-1-1'), as.Date('2012-1-1'), by='year'))
    return(out)
}
