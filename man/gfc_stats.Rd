\name{gfc_stats}
\alias{gfc_stats}
\title{Produce a table of forest cover change statistics for a given AOI}
\usage{
gfc_stats(aoi, gfc, forest_threshold = 25, scalefactor = 1e-04, ...)
}
\arguments{
  \item{aoi}{one or more Area of Interest (AOI) polygon(s)
  as a \code{SpatialPolygons*} object.  If the
  \code{SpatialPolygons*} object is not in the coordinate
  system of the procided gfc extract, it will be
  reprojected. If there is a "label" attribute, it will be
  used to label the output statistics. Otherwise, unique
  names ("AOI 1", "AOI 2", etc.) will be generated and used
  to label the output.}

  \item{gfc}{extract of GFC product for a given AOI (see
  \code{\link{extract_gfc}})}

  \item{forest_threshold}{percent woody vegetation to use
  as a threshold for mapping forest/non-forest}

  \item{scalefactor}{how to scale the output data (from
  meters). Defaults to .0001 for output in hectares.}

  \item{...}{additional arguments to pass to
  \code{threshold}, such as \code{filename} or
  \code{overwrite}}
}
\value{
\code{list} with two elements "loss_table", a
\code{data.frame} with statistics on forest loss, and
"gain_table", with the area of forest gain, and area that
experienced both loss and gain. The units of the output are
hectares (when \code{scalefactor} is set to .0001).
}
\description{
For a given AOI, this function produces two tables: an
annual forest loss table (in hectares, by default), and a
table specifying 1) the total area of pixels that
experienced forest gain and, 2) the total area of pixels
that experienced both loss and gain over the full 2010-2012
period. Note that forest gain and combined loss and gain
are not available in the GFC product on an annualized
basis. Use \code{\link{extract_gfc}} to extract the GFC
data for the AOI prior to running this function.
}
\seealso{
\code{\link{extract_gfc}}
}

