recode_gfc <- function(this_year, floss, fgain, datamask) {
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
    layer_names <- paste0('y', seq(2000, 2012, 1))
    for (n in 1:length(layer_names)) {
        if (n == 1) {
            # Code forest as 1, non-forest as 2
            this_year <- (gfc$treecover2000 > forest_threshold) & (gfc$datamask == 1)
            this_year[this_year != 1] <- 2
            this_year <- this_year
        } else {
            this_year <- raster(out, layer=(n-1))
        }
        # First code forest loss
        this_year[gfc$loss & gfc$lossyear == n] <- 3 # loss
        # Now code forest gain, loss/gain, water, and no data
        this_year <- overlay(this_year,
                             gfc$loss, 
                             gfc$gain, 
                             gfc$datamask, 
                             fun=recode_gfc, datatype='INT1U')
        names(this_year) <- layer_names[n]
        out <- addLayer(out, this_year)

    }
    out <- setZ(out, seq(as.Date('2000-1-1'), as.Date('2012-1-1'), by='year'))
    return(out)
}
