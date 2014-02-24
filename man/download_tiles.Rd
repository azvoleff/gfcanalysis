\name{download_tiles}
\alias{download_tiles}
\title{Download a set of GFC tiles}
\usage{
download_tiles(gfc_tiles, output_folder)
}
\arguments{
  \item{gfc_tiles}{\code{SpatialPolygonsDataFrame} with
  Global Forest Change product tiles to download, as
  calculated by the \code{calc_gfc_tiles} function.}

  \item{output_folder}{the folder to save output data in}
}
\value{
used for the side effect of downloading GFC tiles
}
\description{
Function to download a set of Global Forest Change product
tiles, after verifying that are not present locally.
}

