\name{gfc_stats}
\alias{gfc_stats}
\title{Produce a table of forest cover change statistics for a given AOI}
\usage{
gfc_stats(aoi, gfc, forest_threshold = 50)
}
\arguments{
  \item{aoi}{an Area of Interest (AOI) as a
  \code{SpatialPolygons*} object. If the AOI is not in the
  WGS84 geographic coordinate system, it will be
  reprojected to WGS84}

  \item{gfc}{extract of Global Forest Change product for a
  given AOI (see \code{\link{gfc_extract}})}

  \item{forest_threshold}{percent woody vegetation to use
  as a threshold for mapping forest/non-forest}
}
\value{
\code{list} with three elements "loss_table", a
\code{data.frame} with statistics on forest loss, "gain",
the area of forest gain, and "lossgain", the area that
experienced both loss and gain.
}
\description{
For a given AOI, this function produces two tables: an
annual forest loss table (in hectares), and a table
specifying 1) the total area of pixels that experienced
forest gain and, 2) the total area of pixels that
experienced both loss and gain over the full 2010-2012
period. Note that forest gain and combined loss and gain
are not available in the GFC product on an annualized
basis. Use \code{gfc_extract} to extract the GFC data for
the AOI prior to running this function.
}
\examples{
#TODO: Add examples
}

