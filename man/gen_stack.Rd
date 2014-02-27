\name{gen_stack}
\alias{gen_stack}
\title{Generates an annual stack of forest/non-forest}
\usage{
gen_stack(aoi, gfc, forest_threshold = 50)
}
\arguments{
  \item{aoi}{an Area of Interest (AOI) as a
  \code{SpatialPolygons*} object. If the AOI is not in the
  WGS84 geographic coordinate system, it will be
  reprojected to WGS84.}

  \item{gfc}{extract of Global Forest Change product for a
  given AOI (see \code{\link{gfc_extract}})}

  \item{forest_threshold}{percent woody vegetation to use
  as a threshold for mapping forest/non-forest}
}
\description{
Uses the datamask, treecover2000, loss, gain, and lossyear
layers to produce a raster stack within the given AOI coded
as: 0 - nodata, 1 - forest 2 - non-forest, 3 - forest loss,
4 - forest gain, 5 - water
}

