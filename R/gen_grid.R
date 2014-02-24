#' Function to generate grid
#'
#' Used for producing the 10x10 degree WGS84 grid that the GFC product is tiled 
#' on, so that an AOI polygon can be intersected with the grid to calculate the 
#' appropriate tiles to download.
#'
#' @export
#' @import sp
#' @param origin_x x coordinate of the origin
#' @param dx cell size in the x direction
#' @param max_x maximum value in x direction
#' @param origin_y y coordinate of the origin
#' @param dy cell size in the y direction
#' @param max_y maximum value in y direction
#' @param crs coordinate system as a crs object (defaults to WGS-84)
#' @examples
#' gfc_grid <- gen_grid(-180, 10, 180, -90, 10, 90)
gen_grid <- function(origin_x, dx, max_x, origin_y, dy, max_y, 
                           crs=CRS("+init=epsg:4326")) {
    # Based on code at http://bit.ly/1lfUOnV
    cells.dim <- c((max_x - origin_x) / dx,
                   (max_y - origin_y) / dy)
    gt <- GridTopology(c(origin_x+dx/2, origin_y+dy/2), c(dx, dy), cells.dim)
    grd <- SpatialGrid(gt, proj4string=CRS("+init=epsg:4326"))
    spix <- as(grd, "SpatialPixels")
    spol <- as(spix, "SpatialPolygons")
    return(spol)
}
