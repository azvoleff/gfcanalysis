\name{calc_gfc_tiles}
\alias{calc_gfc_tiles}
\title{Calculate the Global Forest Change product tiles needed for a given AOI}
\usage{
calc_gfc_tiles(aoi, aoi_buffer = 0)
}
\arguments{
  \item{aoi}{an Area of Interest (AOI) as a
  \code{SpatialPolygons*} object. If the AOI is not in the
  WGS84 geographic coordinate system, it will be
  reprojected to WGS84.}

  \item{aoi_buffer}{a distance in meters to buffer the AOI
  by prior to intersecting it with the GFC grid.}
}
\value{
a \code{SpatialPolygonsDataFrame} of the GFC tiles needed
to cover the AOI
}
\description{
Intersects an (optionally buffered) AOI with the Global
Forest Change product grid to determine what tiles are need
to cover the AOI.
}
\examples{
library(sp)
tiles <- calc_gfc_tiles(test_poly)
plot(tiles)
plot(test_poly, add=TRUE)
}

