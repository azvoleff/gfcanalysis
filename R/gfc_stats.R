#' Produce a table of forest cover change statistics for a given AOI
#'
#' For a given AOI, this function produces two tables: an annual forest loss 
#' table (in hectares, by default), and a table specifying 1) the total area of 
#' pixels that experienced forest gain and, 2) the total area of pixels that 
#' experienced both loss and gain over the full 2010-2012 period. Note that 
#' forest gain and combined loss and gain are not available in the GFC product 
#' on an annualized basis. Use \code{\link{extract_gfc}} to extract the GFC 
#' data for the AOI prior to running this function.
#'
#' @seealso \code{\link{extract_gfc}}
#'
#' @export
#' @import raster
#' @import rgdal
#' @importFrom spatial.tools rasterEngine
#' @importFrom rgeos gIntersects
#' @importFrom sp spTransform CRS proj4string
#' @param aoi one or more Area of Interest (AOI) polygon(s) as a 
#' \code{SpatialPolygons*} object.  If the \code{SpatialPolygons*} object is 
#' not in the coordinate system of the procided gfc extract, it will be 
#' reprojected. If there is a "label" attribute, it will be used to label the 
#' output statistics. Otherwise, unique names ("AOI 1", "AOI 2", etc.) will be 
#' generated and used to label the output.
#' @param gfc extract of GFC product for a given AOI (see 
#' \code{\link{extract_gfc}})
#' @param forest_threshold percent woody vegetation to use as a threshold for 
#' mapping forest/non-forest
#' @param scalefactor how to scale the output data (from meters). Defaults to 
#' .0001 for output in hectares.
#' @return \code{list} with two elements "loss_table", a \code{data.frame} with 
#' statistics on forest loss, and "gain_table", with the area of forest gain, 
#' and area that experienced both loss and gain. The units of the output are 
#' hectares (when \code{scalefactor} is set to .0001).
gfc_stats <- function(aoi, gfc, forest_threshold=25, scalefactor=.0001) {
    gfc_boundpoly <- as(extent(gfc), 'SpatialPolygons')
    proj4string(gfc_boundpoly) <- proj4string(gfc)
    gfc_boundpoly_wgs84 <- spTransform(gfc_boundpoly, CRS('+init=epsg:4326'))
    aoi_wgs84 <- spTransform(aoi, CRS('+init=epsg:4326'))
    if (!gIntersects(gfc_boundpoly_wgs84, aoi_wgs84)) {
        stop('aoi does not intersect supplied GFC extract')
    }

    names(gfc) <- c('treecover2000', 'loss', 'gain', 'lossyear', 'datamask')

    if ((((xmin(gfc) >=-180) & (xmax(gfc) <=180)) || ((xmin(gfc) >=0) & (xmax(gfc) <=360))) &&
        (ymin(gfc) >=-90) & (ymax(gfc) <= 90)) {
        # Use the included pixel_areas function to calculate the area of each 
        # raster cell, allowing for areal estimates of deforestation in square 
        # meters even from the original imagery in WGS84.
        message('Data appears to be in latitude/longitude - calculating cell areas on a sphere...')
        cell_areas <- pixel_areas(gfc)
    } else {
        cell_areas <- xres(gfc) * yres(gfc)
    }

    gain_table <- data.frame(aoi=NA, gain=NA, lossgain=NA)

    recode_gfc <- function(treecover2000, lossyear, gain, forest_threshold, 
                           ...) {
        forest2000 <- treecover2000 > forest_threshold
        # Don't count as loss pixels that also had gain
        loss_recode <- lossyear * (!gain) * forest2000
        # Similarly, don't count as gain pixels that also had loss
        gain_recode <- gain & (!loss_recode) & (!forest2000)
        lossgain <- gain & (lossyear > 0)
        array(c(forest2000, loss_recode, gain_recode, lossgain),
              c(nrow(forest2000), ncol(forest2000), 4))
    }

    gfc_recode <- rasterEngine(treecover2000=gfc$treecover2000, 
                               lossyear=gfc$lossyear, gain=gfc$gain, 
                               args=list(forest_threshold=forest_threshold), 
                               fun=recode_gfc, outbands=4, outfiles=1, 
                               setMinMax=1)
    names(gfc_recode) <- c('forest2000', 'loss', 'gain', 'lossgain')

    calc_loss_table <- function(gfc_recode) {
        years <- seq(2000, 2012, 1)
        NAs <- rep(NA, length(years))
        loss_table <- data.frame(year=years, cover=NAs, loss=NAs)
        # Note that areas are converted to square meters using pixel size, then 
        # converted to hectares
        initial_cover <- cellStats(gfc_recode$forest2000 * cell_areas, 'sum') * scalefactor 
        loss_table$cover[1] <- initial_cover
        for (i in 1:12) {
            # n + 1 because first row is year 2000, with zero loss
            loss_table$loss[i + 1] <- cellStats((gfc_recode$loss == i) * cell_areas, 'sum') * scalefactor
        }
        for (i in 2:nrow(loss_table)) {
            loss_table$cover[i] <- loss_table$cover[i - 1] - loss_table$loss[i]
        }
        return(loss_table)
    }

    calc_gain_table <- function(gfc_recode) {
        gainarea <- cellStats(gfc_recode$gain * cell_areas, 
                              'sum') * scalefactor
        lossgainarea <- cellStats(gfc_recode$lossgain * 
                                  cell_areas, 'sum') * scalefactor
        this_gain_table <- data.frame(period='2000-2012',
                                      gain=gainarea, lossgain=lossgainarea)
        return(this_gain_table)
    }

    aoi <- spTransform(aoi, CRS(proj4string(gfc)))
    if (!('label' %in% names(aoi))) {
        aoi$label <- paste('AOI', seq(1:nrow(aoi@data)))
    }

    for (n in 1:nrow(aoi)) {
        gfc_recode_cropped <- crop(gfc_recode, aoi[n, ])
        gfc_recode_cropped <- mask(gfc_recode_cropped, aoi[n, ])
        this_loss_table <- calc_loss_table(gfc_recode_cropped)
        this_loss_table$aoi <- aoi[n, ]$label
        this_gain_table <- calc_gain_table(gfc_recode_cropped)
        this_gain_table$aoi <- aoi[n, ]$label
        if (n == 1) {
            loss_table <- this_loss_table
            gain_table <- this_gain_table
        } else {
            loss_table <- rbind(loss_table, this_loss_table)
            gain_table <- rbind(gain_table, this_gain_table)
        }
    }

    return(list(loss_table=loss_table, gain_table=gain_table))
}
