\name{gfc_animate}
\alias{gfc_animate}
\title{Plot an animation of forest change within a given AOI}
\usage{
gfc_animate(aoi, gfc_stack, out_name, site_name = "", aoi_label = "ZOI",
  type = "gif", height = 3, width = 3, dpi = 300)
}
\arguments{
  \item{aoi}{an Area of Interest (AOI) as a
  \code{SpatialPolygons*} object. If the AOI is not in the
  WGS84 geographic coordinate system, it will be
  reprojected to WGS84.}

  \item{gfc_stack}{a Global Forest Change product subset as
  a \code{RasterStack} (as output by
  \code{\link{gen_stack}})}

  \item{out_name}{basename for animation output}

  \item{site_name}{name of the site (used in making title)}

  \item{aoi_label}{label to use in legend for AOI polygon}

  \item{type}{type of animation to make. Can be either
  "gif" or "html"}

  \item{height}{desired height of the animation GIF in
  inches}

  \item{width}{desired width of the animation GIF in
  inches}

  \item{dpi}{dots per inch for the output image}
}
\description{
Plot an animation of forest change within a given AOI
}

