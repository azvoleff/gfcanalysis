\name{gen_stack}
\alias{gen_stack}
\title{Generate an annual stack of forest change from GFC product}
\usage{
gen_stack(gfc, forest_threshold = 50)
}
\arguments{
  \item{gfc}{extract of Global Forest Change product for a
  given AOI (see \code{\link{gfc_extract}})}

  \item{forest_threshold}{percent woody vegetation to use
  as a threshold for mapping forest/non-forest}
}
\description{
Uses the datamask, treecover2000, loss, gain, and lossyear
layers to produce an annual raster stack from a
\code{\link{gfc_extract}}. See Details for the class codes
used in the annual raster stack.
}
\details{
The output raster stack uses the following codes to
describe forest change at each pixel: \tabular{lc}{ Nodata
\tab 0 \cr Forest \tab 1 \cr Non-forest \tab 2 \cr Forest
loss \tab 3 \cr Forest gain \tab 4 \cr Forest loss and gain
\tab 5 \cr Water \tab 6 \cr }
}
\seealso{
\code{\link{download_tile}}, \code{\link{gfc_animate}},
\code{\link{gfc_stats}}
}

