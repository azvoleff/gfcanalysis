\name{threshold}
\alias{threshold}
\title{Threshold the GFC product}
\usage{
threshold(gfc, forest_threshold = 25)
}
\arguments{
  \item{gfc}{extract of GFC product for a given AOI (see
  \code{\link{extract_gfc}})}

  \item{forest_threshold}{percent woody vegetation to use
  as a threshold for mapping forest/non-forest}
}
\description{
Uses the GFC data output from \code{\link{extract_gfc}} to
make an thresholded layer stack with five layers:
treecover2000, loss, gain, lossyear, and datamask layers.
See Details for the coding used in each layer. The
\code{\link{annual_stack}} function can be used to produce
an annual layer stack of forest change.
}
\details{
The output raster stack uses the following codes to
describe forest change at each pixel.

\bold{Band 1 (forest2000)} \tabular{lc}{ Non-forest \tab 0
\cr Forest \tab 1 \cr }

\bold{Band 2 (loss)} \tabular{lc}{ No loss \tab 0 \cr Loss
\tab 1 \cr }

\bold{Band 3 (gain)} \tabular{lc}{ No gain \tab 0 \cr Gain
\tab 1 \cr }

\bold{Band 4 (lossyear)} \tabular{lc}{ No loss \tab 0 \cr
Loss in 2001 \tab 1 \cr Loss in 2002 \tab 2 \cr Loss in
2003 \tab 3 \cr Loss in 2004 \tab 4 \cr Loss in 2005 \tab 5
\cr Loss in 2006 \tab 6 \cr Loss in 2007 \tab 7 \cr Loss in
2008 \tab 8 \cr Loss in 2009 \tab 9 \cr Loss in 2010 \tab
10 \cr Loss in 2011 \tab 11 \cr Loss in 2012 \tab 12 \cr }

\bold{Band 5 (datamask)} \tabular{lc}{ No data \tab 0 \cr
Land \tab 1 \cr Water \tab 2 \cr }
}
\seealso{
\code{\link{extract_gfc}}, \code{\link{annual_stack}}
}

