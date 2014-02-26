#' Generates an annual stack of forest/non-forest
#'
#' Uses the datamask, treecover2000, loss, gain, and lossyear layers to produce 
#' a raster  stack within the given AOI coded as: 0 - nodata, 1 - forest 
#' 2 - non-forest, 3 - forest loss, 4 - forest gain, 5 - water
#'
#' @export
#' @import rgdal
#' @import raster
#' @importFrom sp bbox spTransform CRS proj4string proj4string<-
#' @importFrom rgeos gBuffer
#' @param aoi an Area of Interest (AOI) as a \code{SpatialPolygons*} object.  
#' If the AOI is not in the WGS84 geographic coordinate system, it will be 
#' reprojected to WGS84.
#' @param data_folder folder where downloaded GFC product tiles are located 
#' (see \code{download_tiles} function.
#' @param aoi_buffer a distance in meters to buffer the AOI by prior to 
#' intersecting it with the GFC grid.
#' @param forest_threshold percent woody vegetation to use as a threshold for 
#' mapping forest/non-forest
gen_stack <- function(aoi, data_folder, aoi_buffer=0, forest_threshold=50) {
    if (aoi_buffer > 0) {
        aoi_utm <- spTransform(aoi, CRS(utm_zone(aoi, proj4string=TRUE)))
        aoi_utm <- gBuffer(aoi_utm, width=aoi_buffer, byid=TRUE)
        aoi <- aoi_utm
    }

    tiles <- calc_gfc_tiles(aoi)

    aoi <- spTransform(aoi, CRS(proj4string(gfc_tiles)))
    file_root <- 'Hansen_GFC2013_'
    bands <- c('treecover2000', 'loss', 'gain', 'lossyear', 'datamask')
    tile_stacks <- c()
    for (n in 1:length(tiles)) {
        tile <- tiles[n]
        min_x <- bbox(tile)[1, 1]
        max_y <- bbox(tile)[2, 2]
        if (min_x < 0) {
            min_x <- paste0(sprintf('%03i', abs(min_x)), 'W')
        } else {
            min_x <- paste0(sprintf('%03i', min_x), 'E')
        }
        if (max_y < 0) {
            max_y <- paste0(sprintf('%02i', abs(max_y)), 'S')
        } else {
            max_y <- paste0(sprintf('%02i', max_y), 'N')
        }
        file_suffix <- paste0('_', max_y, '_', min_x, '.tif')
        filenames <- file.path(data_folder, paste0(file_root, bands, 
                                                   file_suffix))
        tile_stack <- crop(stack(filenames), aoi, datatype='INT2S')
        names(tile_stack) <- bands
        tile_stacks <- c(tile_stacks, list(tile_stack))
    }

    if (length(tile_stacks) > 1) {
        # See http://bit.ly/1dJPIeF re issue in raster that necessitates below 
        # workaround TODO: Contact Hijmans re possible fix
        mosaicargs <- tile_stacks
        mosaicargs$fun <- mean
        mosaicargs$datatype <- 'INT1U'
        tile_mosaic <- do.call(mosaic, mosaicargs)
        names(tile_mosaic) <- names(tile_stacks[[1]])
    } else {
        tile_mosaic <- tile_stacks[[1]]
    }
    NAvalue(tile_mosaic) <- 0

    # Code forest as 1, non-forest as 2
    forest <- (tile_mosaic$treecover2000 > forest_threshold) & (tile_mosaic$datamask == 1)
    forest[forest != 1] <- 2
    # Code water as 5
    forest[tile_mosaic$datamask == 2] <- 5

    recode_values <- function(this_year, floss, fgain, lossyear, datamask) {
        # Recode according to loss, gain, and lossyear values
        this_year[floss == 1 & lossyear == year] <- 3
        this_year[fgain == 1 & lossyear == year] <- 4
        # Code water as 5
        this_year[datamask == 2] <- 5
        return(this_year)
    }

    out <- raster(tile_mosaic)
    out <- addLayer(out, forest) # Initial observation (year 2000)
    names(out) <- 'y2000'
    for (year in 1:12) {
        # Start with obs from past year
        this_year <- overlay(raster(out, layer=year),
                             tile_mosaic$loss, 
                             tile_mosaic$gain, 
                             tile_mosaic$lossyear, 
                             tile_mosaic$datamask, 
                             fun=recode_values, datatype='INT1U')
        out <- addLayer(out, this_year)
        names(out)[year + 1] <- paste0('y20', sprintf('%02i', year))
    }

    # Project to utm for plotting and analysis of change in forest area
    bounding_poly <- as(extent(out), "SpatialPolygons")
    proj4string(bounding_poly) <- proj4string(out)
    utm_proj4string <- utm_zone(bounding_poly, proj4string=TRUE)
    # Use nearest neighbor since the data is categorical
    out <- projectRaster(out, crs=utm_proj4string, method='ngb', datatype='INT1U')

    out <- setZ(out, seq(as.Date('2000-1-1'), as.Date('2012-1-1'), by='year'))

    return(out)
}
# gen_stack(test_poly, "H:/Data/TEAM/GFC_Product")
# aoi <- test_poly
# data_folder <- "H:/Data/TEAM/GFC_Product"
#

# Note gplot is from rasterVis package
# gplot(out) + geom_tile(aes(fill=value))
