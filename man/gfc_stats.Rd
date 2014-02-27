\name{gfc_stats}
\alias{gfc_stats}
\title{Make a table of forest cover change for a given AOI}
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
Output is in hectares.
}
\examples{
#TODO: Add examples
}

