\name{pixel_areas}
\alias{pixel_areas}
\title{Calculate the area of each pixel of a raster}
\usage{
pixel_areas(x)
}
\arguments{
  \item{x}{a \code{Raster*} object}
}
\value{
\code{RasterLayer} with pixel areas (in meters)
}
\description{
Calculates the area of each pixel of a raster, and returns
the result as a \code{RasterLayer}. Useful for calculating
areas based on a classified raster in a geographic
coordinate system.
}

