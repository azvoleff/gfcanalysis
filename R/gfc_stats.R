#' Produce a table of forest cover change statistics for a given AOI
#'
#' For a given AOI, this function produces two tables: an annual forest loss 
#' table (in hectares, by default), and a table specifying 1) the total area of 
#' pixels that experienced forest gain and, 2) the total area of pixels that 
#' experienced both loss and gain over the full 2010-2012 period. Note that 
#' forest gain and combined loss and gain are not available in the GFC product 
#' on an annualized basis. Use \code{\link{extract_gfc}} to extract the GFC 
#' data for the AOI, and threshold it using \code{\link{threshold_gfc}} prior to 
#' running this function.
#'
#' @seealso \code{\link{extract_gfc}}, \code{\link{threshold_gfc}}
#'
#' @export
#' @import raster
#' @import rgdal
#' @importFrom rgeos gIntersects
#' @importFrom sp spTransform CRS proj4string
#' @param aoi one or more Area of Interest (AOI) polygon(s) as a 
#' \code{SpatialPolygons*} object.  If the \code{SpatialPolygons*} object is 
#' not in the coordinate system of the procided gfc extract, it will be 
#' reprojected. If there is a "label" attribute, it will be used to label the 
#' output statistics. Otherwise, unique names ("AOI 1", "AOI 2", etc.) will be 
#' generated and used to label the output.
#' @param gfc extract of GFC product for a given AOI (see 
#' \code{\link{extract_gfc}}), recoded using \code{\link{threshold_gfc}}.
#' @param scale_factor how to scale the output data (from meters). Defaults to 
#' .0001 for output in hectares.
#' @return \code{list} with two elements "loss_table", a \code{data.frame} with 
#' statistics on forest loss, and "gain_table", with the area of forest gain, 
#' and area that experienced both loss and gain. The units of the output are 
#' hectares (when \code{scale_factor} is set to .0001).
gfc_stats <- function(aoi, gfc, scale_factor=.0001, ...) {
    names(gfc) <- c('forest2000', 'lossyear', 'gain', 'lossgain', 'datamask')
    gfc_boundpoly <- as(extent(gfc), 'SpatialPolygons')
    proj4string(gfc_boundpoly) <- proj4string(gfc)
    gfc_boundpoly_wgs84 <- spTransform(gfc_boundpoly, CRS('+init=epsg:4326'))
    aoi_wgs84 <- spTransform(aoi, CRS('+init=epsg:4326'))
    if (!gIntersects(gfc_boundpoly_wgs84, aoi_wgs84)) {
        stop('aoi does not intersect supplied GFC extract')
    }

    if ((((xmin(gfc) >=-180) & (xmax(gfc) <=180)) || ((xmin(gfc) >=0) & (xmax(gfc) <=360))) &&
        (ymin(gfc) >=-90) & (ymax(gfc) <= 90)) {
        # Use the included calc_pixel_area function to calculate the area of 
        # one cell in each line of the raster, allowing for accurate areal 
        # estimates of deforestation in square meters even when imagery is in 
        # WGS84.
        message('Data appears to be in latitude/longitude. Calculating cell areas on a sphere.')
        spherical_areas <- TRUE
        # Calculate the area of a single pixel in each line of the image (to 
        # avoid repeating this calculation later on)
        pixel_areas <- calc_pixel_areas(gfc)
    } else {
        spherical_areas <- FALSE
        pixel_areas <- xres(gfc) * yres(gfc)
    }

    gain_table <- data.frame(aoi=NA, gain=NA, lossgain=NA)

    calc_loss_table <- function(gfc) {
        years <- seq(2000, 2012, 1)
        NAs <- rep(NA, length(years))
        loss_table <- data.frame(year=years, cover=NAs, loss=NAs)
        # Note that areas are converted to square meters using pixel size, then 
        # converted to hectares
        if (spherical_areas) {
            initial_cover_weighted <- scale_by_pixel_area(gfc$forest2000,
                                                          pixel_areas=pixel_areas, 
                                                          scale_factor=scale_factor)
            initial_cover <- cellStats(initial_cover_weighted, 'sum')
        } else {
            initial_cover <- cellStats(gfc$forest2000 * pixel_areas, 'sum') * scale_factor 
        }
        loss_table$cover[1] <- initial_cover
        for (i in 1:12) {
            # n + 1 because first row is year 2000, with zero loss
            if (spherical_areas) {
                loss_weighted <- scale_by_pixel_area(gfc$lossyear == i, 
                                                     pixel_areas=pixel_areas, 
                                                     scale_factor=scale_factor)
                loss_table$loss[i + 1] <- cellStats(loss_weighted, 'sum')
            } else {
                loss_table$loss[i + 1] <- cellStats((gfc$lossyear == i) * pixel_areas, 'sum') * scale_factor
            }
        }
        for (i in 2:nrow(loss_table)) {
            loss_table$cover[i] <- loss_table$cover[i - 1] - loss_table$loss[i]
        }

        return(loss_table)
    }

    calc_gain_table <- function(gfc) {
        if (spherical_areas) {
            gainarea_weighted <- scale_by_pixel_area(gfc$gain, 
                                                     pixel_areas=pixel_areas, 
                                                     scale_factor=scale_factor)
            gainarea <- cellStats(gainarea_weighted, 'sum')
            lossgainarea_weighted <- scale_by_pixel_area(gfc$lossgain, 
                                                         pixel_areas=pixel_areas, 
                                                         scale_factor=scale_factor)
            lossgainarea <- cellStats(lossgainarea_weighted, 'sum')
        } else {
            gainarea <- cellStats(gfc$gain * pixel_areas, 'sum') * scale_factor
            lossgainarea <- cellStats(gfc$lossgain * pixel_areas, 'sum') * scale_factor
        }
        this_gain_table <- data.frame(period='2000-2012',
                                      gain=gainarea, lossgain=lossgainarea)
        return(this_gain_table)
    }

    aoi <- spTransform(aoi, CRS(proj4string(gfc)))
    if (!('label' %in% names(aoi))) {
        aoi$label <- paste('AOI', seq(1:nrow(aoi@data)))
    }

    for (n in 1:nrow(aoi)) {
        gfc_cropped <- crop(gfc, aoi[n, ], datatype='INT1U')
        gfc_cropped <- mask(gfc_cropped, aoi[n, ], datatype='INT1U')
        this_loss_table <- calc_loss_table(gfc_cropped)
        this_loss_table$aoi <- aoi[n, ]$label
        this_gain_table <- calc_gain_table(gfc_cropped)
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
