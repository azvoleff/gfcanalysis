#' Calculate the Global Forest Change product tiles needed for a given AOI
#'
#' Intersects an (optionally buffered) AOI with the Global Forest Change 
#' product grid to determine what tiles are need to cover the AOI.
#'
#' @export
#' @import sp
#' @import rgdal
#' @importFrom rgeos gBuffer gIntersects gUnaryUnion
#' @param aoi an Area of Interest (AOI) as a \code{SpatialPolygons*} object.  
#' If the AOI is not in the WGS84 geographic coordinate system, it will be 
#' reprojected to WGS84.
#' @param aoi_buffer a distance in meters to buffer the AOI by prior to 
#' intersecting it with the GFC grid.
#' @return a \code{SpatialPolygonsDataFrame} of the GFC tiles needed to cover 
#' the AOI
calc_gfc_tiles <- function(aoi, aoi_buffer=0) {
    if (aoi_buffer > 0) {
        aoi_utm <- spTransform(aoi, CRS(utm_zone(aoi, proj4string=TRUE)))
        aoi_utm <- gBuffer(aoi_utm, width=aoi_buffer, byid=TRUE)
    }
    aoi <- spTransform(aoi, CRS(proj4string(gfc_tiles)))
    intersecting <- as.logical(gIntersects(gfc_tiles, 
                                           gUnaryUnion(aoi), 
                                           byid=TRUE))
    if (sum(intersecting) == 0) {
        stop('no intersecting Global Forest Change tiles found')
    } else {
        gfc_tiles <- gfc_tiles[intersecting, ]
    }
    return(gfc_tiles)
}
