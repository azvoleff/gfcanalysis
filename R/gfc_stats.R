#' Make a table of forest cover change for a given AOI
#'
#' Output is in hectares.
#'
#' @export
#' @import raster
#' @import rgdal
#' @importFrom sp spTransform CRS proj4string
#' @param aoi an Area of Interest (AOI) as a \code{SpatialPolygons*} object.  
#' If the AOI is not in the WGS84 geographic coordinate system, it will be 
#' reprojected to WGS84
#' @param gfc extract of Global Forest Change product for a given AOI (see 
#' \code{\link{gfc_extract}})
#' @param forest_threshold percent woody vegetation to use as a threshold for 
#' mapping forest/non-forest
#' @examples
#' #TODO: Add examples
gfc_stats <- function(aoi, gfc, forest_threshold=50) {
    gfc_boundpoly <- as(extent(gfc), 'SpatialPolygons')
    proj4string(gfc_boundpoly) <- proj4string(gfc)
    if (!gIntersects(gfc_boundpoly, aoi)) {
        stop('aoi does not intersect supplied GFC extract')
    }

    names(gfc) <- c('treecover2000', 'loss', 'gain', 'lossyear', 'datamask')

    aoi <- spTransform(aoi, CRS(proj4string(gfc)))
    #TODO: Rewrite to handle multiple polygons, with appended suffixes to stat 
    # columns for each. Using extract likely best bet for this.
    gfc <- mask(gfc, aoi)

    years <- seq(2000, 2012, 1)
    NAs <- rep(NA, length(years))
    loss_table <- data.frame(year=years, fcover=NAs, loss=NAs)

    # Will need to convert pixels counts to areas. The gfc_stack is in UTM, so 
    # unit of pixel area is square meters.
    pixel_area <- res(gfc)[1] * res(gfc)[2]

    # Don't count as loss pixes that also had gain
    loss_pixels <- gfc$lossyear * (!gfc$gain)
    # Similarly, don't count as gain pixes that also had loss
    gain_pixels <- gfc$gain & !gfc$loss
    lossgain_pixels <- gfc$gain & gfc$loss

    # Note that areas are converted to square meters using pixel size, then 
    # converted to hectares
    init_fc <- cellStats(gfc$treecover2000 > forest_threshold,
                         'sum') * pixel_area / 10000
    loss_table$fcover[1] <- init_fc

    # Freq loss is a table of the number of pixels lost by year. Entry '0' is 
    # no data value and can be ignored. 
    freq_loss <- freq(loss_pixels, useNA='no')
    loss_table$loss <- c(0, freq_loss[2:nrow(loss_table), 2])
    loss_table$loss <- loss_table$loss * pixel_area / 10000

    for (n in 2:nrow(loss_table)) {
        loss_table$fcover[n] <- loss_table$fcover[n-1] - loss_table$loss[n]
    }
    loss_table$loss_pct <- (loss_table$loss/loss_table$fcover) * 100

    gainarea <- cellStats(gain_pixels, 'sum') * pixel_area / 10000
    lossgainarea <- cellStats(lossgain_pixels, 'sum') * pixel_area / 10000
    return(list(loss_table=loss_table, gainarea=gainarea, 
                lossgainarea=lossgainarea))
}

library(rgdal)
aoi <- readOGR('H:/Data/TEAM/COU/Vectors', 'ZOI_COU_2012')
gfc <- brick('C:/Users/azvoleff/Code/TEAM/gfcanalysis_scripts/ZOI_COU_2012_gfcextract.envi')

gfc_stats(aoi, gfc)
