recode_gfc <- function(this_year, floss, fgain, datamask) {
    this_year[fgain] <- 4 # gain (no years attached to gain)
    this_year[fgain & floss] <- 5 # loss and gain
    this_year[datamask == 2] <- 6 # water
    this_year[datamask == 0] <- 0 # no data
    return(this_year)
}

#' Generate an annual stack of forest change from GFC product
#'
#' Uses the datamask, treecover2000, loss, gain, and lossyear layers to produce 
#' an annual raster stack from a \code{gfc_extract}. See Details for the class 
#' codes used in the annual raster stack.
#' 
#' The output raster stack uses the following codes to describe forest change 
#' at each pixel:
#' \tabular{lc}{
#'     Nodata               \tab 0 \cr
#'     Forest               \tab 1 \cr
#'     Non-forest           \tab 2 \cr
#'     Forest loss          \tab 3 \cr
#'     Forest gain          \tab 4 \cr
#'     Forest loss and gain \tab 5 \cr
#'     Water                \tab 6 \cr
#' }
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
