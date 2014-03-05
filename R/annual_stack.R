#' Generate an annual stack of forest change from GFC product
#'
#' Uses the GFC data output from \code{\link{extract_gfc}} to make an 
#' annualized layer stack of forest change. This function uses the datamask, 
#' treecover2000, loss, gain, and lossyear layers from the GFC product to 
#' produce an annual raster stack from a \code{\link{extract_gfc}}.  See 
#' Details for the class codes used in the annual raster stack. The 
#' \code{\link{animate_annual}} function can be used to produce an animation of 
#' forest change from the generated layer stack.
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
#' @seealso \code{\link{extract_gfc}}, \code{\link{animate_annual}}
#'
#' @export
#' @import raster
#' @param gfc extract of GFC product for a given AOI (see 
#' \code{\link{extract_gfc}})
#' @param forest_threshold percent woody vegetation to use as a threshold for 
#' mapping forest/non-forest
annual_stack <- function(gfc, forest_threshold=25) {
    out <- raster(gfc)
    layer_names <- paste0('y', seq(2000, 2012, 1))
    for (n in 1:length(layer_names)) {
        if (n == 1) {
            # Code forest as 1, non-forest as 2
            this_year <- gfc$treecover2000 > forest_threshold
            this_year[this_year == 0] <- 2
            # Code missing data
            this_year[gfc$datamask == 0] <- 0
            # Code gain as 4 (no years are attributed to gain). Gain can only 
            # occur on initially non-forested pixels (this_year == 2) where 
            # loss does not also occur (loss == 0)
            this_year[(this_year == 2) & gfc$gain & (gfc$loss == 0)] <- 4
            # Code forest loss/gain, and water
            this_year[gfc$gain & (gfc$loss != 0)] <- 5 # loss and gain
            this_year[gfc$datamask == 2] <- 6 # water
        } else {
            this_year <- raster(out, layer=(n-1))
        }
        # Code forest loss
        this_year[(this_year == 1) & gfc$loss & gfc$lossyear == n] <- 3 # loss
        names(this_year) <- layer_names[n]
        out <- addLayer(out, this_year)
    }
    out <- setZ(out, seq(as.Date('2000-1-1'), as.Date('2012-1-1'), by='year'))
    return(out)
}
