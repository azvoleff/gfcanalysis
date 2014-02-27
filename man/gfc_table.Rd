\name{gfc_table}
\alias{gfc_table}
\title{Make a table of forest cover change for a given AOI}
\usage{
gfc_table(aoi, gfc, out_file)
}
\arguments{
  \item{aoi}{an Area of Interest (AOI) as a
  \code{SpatialPolygons*} object. If the AOI is not in the
  WGS84 geographic coordinate system, it will be
  reprojected to WGS84}

  \item{gfc}{extract of Global Forest Change product for a
  given AOI (see \code{\link{gfc_extract}})}

  \item{out_file}{name for output data table (should end in
  ".csv")}

  \item{forest_threshold}{percent woody vegetation to use
  as a threshold for mapping forest/non-forest}
}
\description{
Make a table of forest cover change for a given AOI
}
\examples{
#TODO: Add examples
}

