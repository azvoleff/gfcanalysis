#' Threshold the GFC product
#'
#' Uses the GFC data output from \code{\link{extract_gfc}} to make an 
#' thresholded layer stack with five layers: treecover2000, loss, gain,
#' lossyear, and datamask layers. See Details for the coding used in each 
#' layer.
#' 
#' The output uses the following codes to describe forest change at each pixel:
#'
#' \bold{Band 1 (forest2000)}
#' \tabular{lc}{
#'     Non-forest           \tab 0 \cr
#'     Forest               \tab 1 \cr
#' }
#'
#' \bold{Band 2 (loss)}
#' \tabular{lc}{
#'     No loss \tab 0 \cr
#'     Loss    \tab 1 \cr
#' }
#'
#' \bold{Band 3 (gain)}
#' \tabular{lc}{
#'     No gain \tab 0 \cr
#'     Gain    \tab 1 \cr
#' }
#'
#' \bold{Band 4 (lossyear)}
#' \tabular{lc}{
#'     No loss      \tab 0  \cr
#'     Loss in 2001 \tab 1  \cr
#'     Loss in 2002 \tab 2  \cr
#'     Loss in 2003 \tab 3  \cr
#'     Loss in 2004 \tab 4  \cr
#'     Loss in 2005 \tab 5  \cr
#'     Loss in 2006 \tab 6  \cr
#'     Loss in 2007 \tab 7  \cr
#'     Loss in 2008 \tab 8  \cr
#'     Loss in 2009 \tab 9  \cr
#'     Loss in 2010 \tab 10 \cr
#'     Loss in 2011 \tab 11 \cr
#'     Loss in 2012 \tab 12 \cr
#' }
#'
#' \bold{Band 5 (datamask)}
#' \tabular{lc}{
#'     No data \tab 0 \cr
#'     Land    \tab 1 \cr
#'     Water   \tab 2 \cr
#' }
#'
#' @seealso \code{\link{extract_gfc}}
#'
#' @export
#' @import raster
#' @param gfc extract of GFC product for a given AOI (see 
#' \code{\link{extract_gfc}})
#' @param forest_threshold percent woody vegetation to use as a threshold for 
#' mapping forest/non-forest
threshold <- function(gfc, forest_threshold=25) {
    # Code forest as 1, non-forest as 0
    forest2000 <- gfc$treecover2000 > forest_threshold
    # Code gain as 0 for forest pixels (forested can't gain forest)
    gfc$gain[forest2000 == 1] <- 0
    # Code loss as 0 for non-forest pixels (nonforested can't lose forest)
    gfc$loss[forest2000 == 0] <- 0
    # Code lossyear as 0 for non-forest pixels (nonforested can't lose forest)
    gfc$lossyear[gfc$forest2000 == 0] <- 0
    thresholded <- stack(forest2000, gfc$loss, gfc$gain, gfc$lossyear, 
                         gfc$datamask)
    names(thresholded) <- c('forest2000', 'loss', 'gain', 'lossyear', 
                            'datamask')
    return(thresholded)
}
