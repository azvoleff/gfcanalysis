#' Calculate the GFC product tiles needed for a given AOI
#'
#' Intersects an AOI with the GFC 
#' product grid to determine what tiles are need to cover the AOI.
#'
#' @export
#' @importFrom sf st_crs st_transform st_intersects st_convex_hull st_touches
#' @param aoi an Area of Interest (AOI) as a \code{SpatialPolygons*} or \code{sf} object.  
#' If the AOI is not in the WGS84 geographic coordinate system, it will be 
#' reprojected to WGS84.
#' @return a \code{sf} of the GFC tiles needed to cover 
#' the AOI
#' @examples
#' tiles <- calc_gfc_tiles(test_poly)
#' plot(tiles)
#' plot(test_poly, lt=2, add=TRUE)
calc_gfc_tiles <- function(aoi) {
    aoi <- check_aoi(aoi)
    if (!identical(st_crs(aoi), st_crs(gfc_tiles))){
      warning("aoi and gfc do not have identical crs. aoi will be reprojected for analysis, 
              but may not overlap with results when plotting")
      aoi <- st_transform(aoi, st_crs(gfc_tiles))
    }

    intersecting <- apply(st_intersects(gfc_tiles, st_convex_hull(aoi), sparse=FALSE),
                          FUN=any,
                          MARGIN=1)

    if (sum(intersecting) == 0) {
        stop('no intersecting GFC tiles found')
    } else {
        gfc_tiles <- gfc_tiles[intersecting, ]
    }
    return(gfc_tiles)
}


