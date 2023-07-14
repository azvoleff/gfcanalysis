#' Check if aoi is an sf or sp type object
#' If sp, convert to sf and return
#' @param aoi the area of interest object
#' @import sf
#' @NoRd
check_aoi <- function(aoi) {
  if (is(aoi, 'SpatialPolygonsDataFrame')){
    suppressWarnings(aoi <- st_as_sf(aoi))
  } else{
    return(aoi)
  }
}
