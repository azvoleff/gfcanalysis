#' Check if aoi is an sf or sp type object
#' If sf, convert to sp and return
#' @param aoi the area of interest object
#' @NoRd
check_aoi <- function(aoi) {
  if ('sf' %in% class(aoi)){
    #Check if sf is installled
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Package \"sf\" needed for this function to work. Please install it.",
           call. = FALSE)
    }

    suppressWarnings(aoi <- as(aoi, "Spatial"))

  } else{
    return(aoi)
  }
}
