\name{annual_stack}
\alias{annual_stack}
\title{Generate an annual stack of forest change from GFC product}
\usage{
annual_stack(gfc, forest_threshold = 25)

annual_stack(gfc, forest_threshold = 25)
}
\arguments{
  \item{gfc}{extract of GFC product for a given AOI (see
  \code{\link{extract_gfc}})}

  \item{forest_threshold}{percent woody vegetation to use
  as a threshold for mapping forest/non-forest}

  \item{gfc}{extract of GFC product for a given AOI (see
  \code{\link{extract_gfc}})}

  \item{forest_threshold}{percent woody vegetation to use
  as a threshold for mapping forest/non-forest}
}
\description{
Uses the GFC data output from \code{\link{extract_gfc}} to
make an annualized layer stack of forest change. This
function uses the datamask, treecover2000, loss, gain, and
lossyear layers from the GFC product to produce an annual
raster stack from a \code{\link{extract_gfc}}.  See Details
for the class codes used in the annual raster stack. The
\code{\link{animate_annual}} function can be used to
produce an animation of forest change from the generated
layer stack.

Uses the GFC data output from \code{\link{extract_gfc}} to
make an annualized layer stack of forest change. This
function uses the datamask, treecover2000, loss, gain, and
lossyear layers from the GFC product to produce an annual
raster stack from a \code{\link{extract_gfc}}.  See Details
for the class codes used in the annual raster stack. The
\code{\link{animate_annual}} function can be used to
produce an animation of forest change from the generated
layer stack.
}
\details{
The output raster stack uses the following codes to
describe forest change at each pixel: \tabular{lc}{ Nodata
\tab 0 \cr Forest \tab 1 \cr Non-forest \tab 2 \cr Forest
loss \tab 3 \cr Forest gain \tab 4 \cr Forest loss and gain
\tab 5 \cr Water \tab 6 \cr }

The output raster stack uses the following codes to
describe forest change at each pixel: \tabular{lc}{ Nodata
\tab 0 \cr Forest \tab 1 \cr Non-forest \tab 2 \cr Forest
loss \tab 3 \cr Forest gain \tab 4 \cr Forest loss and gain
\tab 5 \cr Water \tab 6 \cr }
}
\seealso{
\code{\link{extract_gfc}}, \code{\link{animate_annual}}

\code{\link{extract_gfc}}, \code{\link{animate_annual}}
}

