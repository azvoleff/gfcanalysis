\name{download_tiles}
\alias{download_tiles}
\title{Download a set of GFC tiles}
\usage{
download_tiles(tiles, output_folder, first_and_last = FALSE)
}
\arguments{
  \item{tiles}{\code{SpatialPolygonsDataFrame} with Global
  Forest Change product tiles to download, as calculated by
  the \code{calc_gfc_tiles} function.}

  \item{output_folder}{the folder to save output data in}

  \item{first_and_last}{whether to download the composite
  images for 2000 and 2010 (which are substantially larger
  than the other layers)}
}
\value{
used for the side effect of downloading GFC tiles
}
\description{
Checkes whether each tile a set Global Forest Change (GFC)
product tiles, is present locally, with file sizes matching
the files available on the Google server hosting the GFC
product. Downloads all tiles that either are not present
locally, or that are present but have file sizes differing
from the file on the Google server.
}
\examples{
\dontrun{
output_folder <- 'H:/Data/TEAM/GFC_Product'
tiles <- calc_gfc_tiles(test_poly)
download_tiles(tiles, output_folder)
}
}

