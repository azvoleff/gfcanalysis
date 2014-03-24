#' @import methods
#' @import rgdal
#' @import raster
#' @importFrom sp bbox spTransform CRS proj4string
make_tile_mosaic <- function(aoi, data_folder, filename="", ...) {
    tiles <- calc_gfc_tiles(aoi)
    # Transform aoi to match tiles CRS so it can be used later for cropping
    aoi <- spTransform(aoi, CRS(proj4string(tiles)))
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
        tile_stack <- crop(stack(filenames), aoi, datatype='INT1U')
        names(tile_stack) <- bands
        tile_stacks <- c(tile_stacks, list(tile_stack))
    }

    if (length(tile_stacks) > 1) {
        # See http://bit.ly/1dJPIeF re issue in raster that necessitates below 
        # workaround TODO: Contact Hijmans re possible fix
        mosaic_list <- function(x, fun, datatype, overwrite, tolerance=0.05, 
                                filename="") {
            mosaic_args <- x
            if (!missing(fun)) mosaic_args$fun <- fun
            if (!missing(tolerance)) mosaic_args$tolerance <- tolerance
            if (!missing(datatype)) mosaic_args$datatype <- datatype
            if (!missing(overwrite)) mosaic_args$overwrite <- overwrite
            mosaic_args$filename <- filename
            do.call(mosaic, mosaic_args)
        }
        tile_mosaic <- mosaic_list(tile_stacks, fun='mean', filename=filename, 
                                   datatype='INT1U', ...)
    } else {
        tile_mosaic <- tile_stacks[[1]]
        if (filename != '') {
            tile_mosaic <- writeRaster(tile_mosaic, filename=filename, datatype="INT1U", ...)
        }
    }
    names(tile_mosaic) <- names(tile_stacks[[1]])
    NAvalue(tile_mosaic) <- -1

    return(tile_mosaic)
}

#' Extracts GFC data for a given AOI
#'
#' This function extracts a dataset for a given AOI from a series of 
#' pre-downloaded GFC tiles. The \code{\link{download_tiles}} function should 
#' be used beforehand in order to download the necessary data to the specified
#' \code{data_folder}.
#'
#' @seealso \code{\link{download_tiles}}, \code{\link{annual_stack}}, 
#' \code{\link{gfc_stats}}
#'
#' @export
#' @import rgdal
#' @importFrom sp spTransform CRS proj4string proj4string<-
#' @importFrom rgeos gBuffer
#' @param aoi an Area of Interest (AOI) as a \code{SpatialPolygons*} object.  
#' If the AOI is not in WGS 1984 (EPSG:4326), it will be reprojected to WGS84.
#' @param data_folder folder where downloaded GFC product tiles are located 
#' (see \code{\link{download_tiles}} function.
#' @param to_UTM if TRUE, then reproject the output into the UTM zone of the 
#' AOI centroid. If FALSE, retain the original WGS84 projection of the GFC 
#' tiles.
#' @param ... additional arguments to pass to writeRaster, such as 
#' \code{filename}, or \code{overwrite}
#' @return \code{RasterStack} with GFC layers
extract_gfc <- function(aoi, data_folder, to_UTM=FALSE, ...) {
    if (to_UTM) {
        tile_mosaic <- make_tile_mosaic(aoi, data_folder)
        # Project to UTM for plotting and analysis of change in forest area.  
        # Calculate UTM zone based on bounding polygon of tile mosaic.
        bounding_poly <- as(extent(tile_mosaic), "SpatialPolygons")
        proj4string(bounding_poly) <- proj4string(tile_mosaic)
        utm_proj4string <- utm_zone(bounding_poly, proj4string=TRUE)
        # Use nearest neighbor since the data is categorical
        tile_mosaic <- projectRaster(tile_mosaic, crs=utm_proj4string, 
                                     method='ngb', datatype='INT1U', ...)
        NAvalue(tile_mosaic) <- -1
    } else {
        tile_mosaic <- make_tile_mosaic(aoi, data_folder, ...)
        NAvalue(tile_mosaic) <- -1
    }

    names(tile_mosaic) <- c('treecover2000', 'loss', 'gain', 'lossyear', 
                            'datamask')
    return(tile_mosaic)
}
