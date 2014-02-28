\name{gfc_animate}
\alias{gfc_animate}
\title{Plot an animation of forest change within a given AOI}
\usage{
gfc_animate(aoi, gfc_stack, out_name, site_name = "", aoi_label = "ZOI",
  type = "html", height = 3, width = 3, dpi = 300)
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
Produces an animation of annual forest change in the area
bounded by the extent of a given AOI. The AOI polygon is
also plotted on the image. The \code{gfc_stack} must be
pre-calculated using the \code{\link{gen_stack}} function.
The animation can be either an animated GIF (if \code{type}
is set to 'gif') or a series of '.png' files with a
corresponding '.html' webpage showing a simple viewer and
the forest change animation (if \code{type} is set to
'html'). The HTML option is recommended as it requires no
additional software to produce it. The animated GIF option
will only work if the imagemagicK software package is
installed beforehand (this is done outside of R).
}
\seealso{
\code{\link{gen_stack}}, \code{\link{gfc_stats}}
}

