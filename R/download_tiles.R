#' @importFrom RCurl getUrl
#' @importFrom stringr str_extract
verify_download <- function(tile_url, local_path) {
    header <- getURL(tile_url, nobody=1L, header=1L)
    header <- strsplit(header, "\r\n")[[1]]
    content_length <- header[grepl('Content-Length: ', header)]
    remote_size <- as.numeric(str_extract(content_length, '[0-9]+'))
    local_size <- file.info(local_path)$size
    if (remote_size != local_size) {
        return(3)
    } else {
        return(0)
    }
}

download_tile <- function(tile_url, local_path) {
    ret_code <- download.file(tile_url, local_path, mode="wb")
    if (ret_code != 0) {
        message(paste('Warning: problem downloading', local_path))
        return(1)
    } else if (verify_download(tile_url, local_path) != 0) {
        message(paste("Warning: verification failed on", local_path))
        return(2)
    } else {
        return(0)
    }
}

#' Download a set of GFC tiles
#'
#' Function to download a set of Global Forest Change product tiles, after 
#' verifying that are not present locally.
#' @export
#' @importFrom stringr str_extract
#' @param tiles \code{SpatialPolygonsDataFrame} with Global Forest Change 
#' product tiles to download, as calculated by the \code{calc_gfc_tiles} 
#' function.
#' @param output_folder the folder to save output data in
#' @return used for the side effect of downloading GFC tiles
#' @examples
#' output_folder <- 'H:/Data/TEAM/GFC_Product'
#' tiles <- calc_gfc_tiles(test_poly, 'H:/Data/TEAM/GFC_Product')
#' download_tiles(tiles, output_folder)
download_tiles <- function(tiles, output_folder) {
    message(paste(nrow(tiles), 'tiles to download/check.'))
    successes <- 0
    failures <- 0
    skips <- 0

    for (n in 1:length(tiles)) {
        gfc_tile <- tiles[n,]
        min_x <- bbox(gfc_tile)[1, 1]
        max_y <- bbox(gfc_tile)[2, 2]
        if (min_x < 0) {
            min_x <- paste0(sprintf('%03i', abs(min_x)), 'W')
        } else {
            min_x <- paste0(sprintf('%03i', min_x), 'E')
        }
        if (max_y < 0) {
            max_y <- paste0(sprintf('%02i', abs(max_y)), 'S')
        } else {
            max_y <- paste0(sprintf('%02i', max_y), 'N')
        }
        file_root <- 'Hansen_GFC2013_'
        file_suffix <- paste0('_', max_y, '_', min_x, '.tif')
        filenames <- paste0(file_root, c('treecover2000', 'loss', 'gain', 
                                        'lossyear', 'datamask', 'first', 
                                        'last'),
                            file_suffix)
        tile_urls <- paste0('http://commondatastorage.googleapis.com/earthenginepartners-hansen/GFC2013/',
                      filenames)
        local_paths <- file.path(output_folder, filenames)

        for (i in 1:length(filenames)) {
            tile_url <- tile_urls[i]
            local_path <- local_paths[i]
            if (file.exists(local_path)) {
                if (verify_download(tile_url, local_path)) {
                    message(paste(local_path, "exists but doesn't match remote - re-downloading file"))
                } else {
                    message(paste(local_path, 'exists and matches remote - skipping download'))
                    skips <- skips + 1
                    next
                }
            }
            if (download_tile(tile_url, local_path) == 0) {
                successes <- successes + 1
            } else {
                failures <- failures + 1
            }

        }
    }

    message(paste(successes, "tile(s) succeeded,", skips, "tile(s) skipped,", 
                failures, "tile(s) failed."))
}
