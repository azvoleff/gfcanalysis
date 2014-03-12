#' @import rgdal
#' @import raster
#' @importFrom sp bbox spTransform CRS proj4string
make_tile_mosaic <- function(aoi, data_folder) {
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
#' @param aoi_buffer a distance in meters to buffer the AOI by prior to 
#' intersecting it with the GFC grid.
#' @param to_UTM if TRUE, then reproject the output into the UTM zone of the 
#' AOI centroid. If FALSE, retain the original WGS84 projection of the GFC 
#' tiles.
#' @return \code{RasterStack} with GFC layers
extract_gfc <- function(aoi, data_folder, aoi_buffer=0, to_UTM=FALSE) {
    if (aoi_buffer > 0) {
        aoi <- spTransform(aoi, CRS(utm_zone(aoi, proj4string=TRUE)))
        aoi <- gBuffer(aoi, width=aoi_buffer, byid=TRUE)
    }
    aoi <- spTransform(aoi, CRS(proj4string(tile_mosaic)))

    # Add an additional small buffer avoid having missing areas on the image 
    # edge after the below reprojection - this buffer will be removed prior to 
    # returning the output.
    aoi_buffered <- gBuffer(spTransform(aoi,
                                        CRS(utm_zone(aoi, proj4string=TRUE))), 
                            width=500, byid=TRUE)
    tile_mosaic <- make_tile_mosaic(aoi_buffered, data_folder)

    if (to_UTM) {
        # Project to utm for plotting and analysis of change in forest area
        bounding_poly <- as(extent(tile_mosaic), "SpatialPolygons")
        proj4string(bounding_poly) <- proj4string(tile_mosaic)
        utm_proj4string <- utm_zone(bounding_poly, proj4string=TRUE)
        # Use nearest neighbor since the data is categorical
        tile_mosaic <- projectRaster(tile_mosaic, crs=utm_proj4string, 
                                     method='ngb', datatype='INT1U')
    }
    # Crop to the original AOI, as "tile_mosaic" currently includes 500m buffer
    aoi <- spTransform(aoi, CRS(proj4string(tile_mosaic)))
    NAvalue(tile_mosaic) <- -1
    tile_mosaic <- crop(tile_mosaic, aoi)
    names(tile_mosaic) <- c('treecover2000', 'loss', 'gain', 'lossyear', 
                            'datamask')
    return(tile_mosaic)
}
