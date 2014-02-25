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
Function to download a set of Global Forest Change product
tiles, after verifying that they are not present locally.
}
\examples{
\dontrun{
output_folder <- 'H:/Data/TEAM/GFC_Product'
tiles <- calc_gfc_tiles(test_poly)
download_tiles(tiles, output_folder)
}
}

