\name{gfc_animate}
\alias{gfc_animate}
\title{Plot an animation of forest change within a given AOi}
\usage{
gfc_animate(aoi, gfc_stack, out_file, height = 6, width = 6, dpi = 300)
}
\arguments{
  \item{gfc_stack}{a Global Forest Change product subset as
  a \code{RasterStack} (as output by
  \code{\link{gen_stack}})}

  \item{out_file}{filename for animation}

  \item{height}{desired height of the animation GIF in
  inches}

  \item{width}{desired width of the animation GIF in
  inches}

  \item{dpi}{dots per inch for the output image}

  \item{aoi}{an Area of Interest (AOI) as a
  \code{SpatialPolygons*} object. If the AOI is not in the
  WGS84 geographic coordinate system, it will be
  reprojected to WGS84.}
}
\description{
Plot an animation of forest change within a given AOi
}
\examples{
#TODO: Add examples
}

