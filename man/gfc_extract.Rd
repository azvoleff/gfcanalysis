\name{gfc_extract}
\alias{gfc_extract}
\title{Extracts GFC data for a given AOI}
\usage{
gfc_extract(aoi, data_folder, aoi_buffer = 0)
}
\arguments{
  \item{aoi}{an Area of Interest (AOI) as a
  \code{SpatialPolygons*} object. If the AOI is not in the
  WGS84 geographic coordinate system, it will be
  reprojected to WGS84.}

  \item{data_folder}{folder where downloaded GFC product
  tiles are located (see \code{download_tiles} function.}

  \item{aoi_buffer}{a distance in meters to buffer the AOI
  by prior to intersecting it with the GFC grid.}
}
\value{
\code{RasterStack} with GFC layers
}
\description{
Extracts GFC data for a given AOI
}

