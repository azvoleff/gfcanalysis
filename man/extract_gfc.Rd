\name{extract_gfc}
\alias{extract_gfc}
\title{Extracts GFC data for a given AOI}
\usage{
extract_gfc(aoi, data_folder, aoi_buffer = 0)
}
\arguments{
  \item{aoi}{an Area of Interest (AOI) as a
  \code{SpatialPolygons*} object. If the AOI is not in the
  WGS84 geographic coordinate system, it will be
  reprojected to WGS84.}

  \item{data_folder}{folder where downloaded GFC product
  tiles are located (see \code{\link{download_tiles}}
  function.}

  \item{aoi_buffer}{a distance in meters to buffer the AOI
  by prior to intersecting it with the GFC grid.}
}
\value{
\code{RasterStack} with GFC layers
}
\description{
This function extracts a dataset for a given AOI from a
series of pre-downloaded GFC tiles. The
\code{\link{download_tiles}} function should be used
beforehand in order to download the necessary data to the
specified \code{data_folder}.
}
\seealso{
\code{\link{download_tiles}}, \code{\link{annual_stack}},
\code{\link{gfc_stats}}
}

