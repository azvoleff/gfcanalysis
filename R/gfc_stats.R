#' Produce a table of forest cover change statistics for a given AOI
#'
#' For a given AOI, this function produces two tables: an annual forest loss 
#' table (in hectares), and a table specifying 1) the total area of pixels that 
#' experienced forest gain and, 2) the total area of pixels that experienced 
#' both loss and gain over the full 2010-2012 period. Note that forest gain and 
#' combined loss and gain are not available in the GFC product on an annualized 
#' basis. Use \code{\link{extract_gfc}} to extract the GFC data for the AOI prior to 
#' running this function.
#'
#' @seealso \code{\link{extract_gfc}}
#'
#' @export
#' @import raster
#' @import rgdal
#' @importFrom sp spTransform CRS proj4string
#' @param aoi an Area of Interest (AOI) as a \code{SpatialPolygons*} object.  
#' If the AOI is not in the WGS84 geographic coordinate system, it will be 
#' reprojected to WGS84
#' @param gfc extract of GFC product for a given AOI (see 
#' \code{\link{extract_gfc}})
#' @param forest_threshold percent woody vegetation to use as a threshold for 
#' mapping forest/non-forest
#' @return \code{list} with three elements "loss_table", a \code{data.frame} 
#' with statistics on forest loss, "gain", the area of forest gain, and 
#' "lossgain", the area that experienced both loss and gain. The units of the 
#' output are hectares.
#' @examples
#' #TODO: Add examples
gfc_stats <- function(aoi, gfc, forest_threshold=25) {
    gfc_boundpoly <- as(extent(gfc), 'SpatialPolygons')
    proj4string(gfc_boundpoly) <- proj4string(gfc)
    gfc_boundpoly_wgs84 <- spTransform(gfc_boundpoly, CRS('+init=epsg:4326'))
    aoi_wgs84 <- spTransform(aoi, CRS('+init=epsg:4326'))
    if (!gIntersects(gfc_boundpoly_wgs84, aoi_wgs84)) {
        stop('aoi does not intersect supplied GFC extract')
    }

    names(gfc) <- c('treecover2000', 'loss', 'gain', 'lossyear', 'datamask')

    aoi <- spTransform(aoi, CRS(proj4string(gfc)))
    #TODO: Rewrite to handle multiple polygons, with appended suffixes to stat 
    # columns for each. Using extract likely best bet for this.
    gfc <- mask(gfc, aoi)

    # Use the included pixel_areas function to calculate the area of each 
    # raster cell, allowing for areal estimates of deforestation in square 
    # meters even from the original imagery in WGS84.
    cell_areas <- pixel_areas(gfc)

    years <- seq(2000, 2012, 1)
    NAs <- rep(NA, length(years))
    loss_table <- data.frame(year=years, cover=NAs, loss=NAs)

    forest2000 <- gfc$treecover2000 > forest_threshold
    # Don't count as loss pixels that also had gain
    loss_pixels <- gfc$lossyear * (!gfc$gain) * forest2000
    # Similarly, don't count as gain pixels that also had loss
    gain_pixels <- gfc$gain & !gfc$loss & !forest2000
    lossgain_pixels <- gfc$gain & gfc$loss

    # Note that areas are converted to square meters using pixel size, then 
    # converted to hectares
    initial_cover <- cellStats((gfc$treecover2000 > forest_threshold) * 
                               cell_areas,
                         'sum') / 10000
    loss_table$cover[1] <- initial_cover

    # Freq loss is a table of the number of pixels lost by year. Entry '0' is 
    # no data value and can be ignored. 
    freq_loss <- freq(loss_pixels, useNA='no')
    loss_table$loss <- c(0, freq_loss[2:nrow(loss_table), 2])
    loss_table$loss <- loss_table$loss * cell_areas / 10000

    for (n in 2:nrow(loss_table)) {
        loss_table$cover[n] <- loss_table$cover[n-1] - loss_table$loss[n]
    }

    gainarea <- cellStats(gain_pixels, 'sum') * cell_areas / 10000
    lossgainarea <- cellStats(lossgain_pixels, 'sum') * cell_areas / 10000
    return(list(loss_table=loss_table, gain=gainarea, 
                lossgain=lossgainarea))
}
