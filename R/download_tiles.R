verify_download <- function(tile_url, local_path) {
    cksum_file <- tempfile()
    ret_code <- download.file(gsub('\\.tar\\.gz$', '.cksum', tile_url), 
                              cksum_file, mode="w", quiet=TRUE)
    if (ret_code != 0) {
        message(paste('Warning: problem downloading cksum for', local_path))
        return(1)
    } else {
        # TODO: Check return code and handle accordingly
        # The first element is the cksum, second is the expected file size in 
        # bytes, and the third is the filename
        espa_checksum <- scan(cksum_file, what=c('integer', 'integer', 
                                                 'character'), quiet=TRUE)
        unlink(cksum_file)
        local_size <- file.info(local_path)$size
        if (espa_checksum[2] != local_size) {
            return(3)
        } else {
            return(0)
        }
    }
}

download_tile <- function(tile_url, output_path) {
    ret_code <- download.file(tile_url, output_path, mode="wb")
    if (ret_code != 0) {
        message(paste('Warning: problem downloading', output_path))
        return(1)
    } else if (verify_download(tile_url, output_path) != 0) {
        message(paste("Warning: checksum mismatch on", output_path))
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
#' @param gfc_tiles \code{SpatialPolygonsDataFrame} with Global Forest Change 
#' product tiles to download, as calculated by the \code{calc_gfc_tiles} 
#' function.
#' @param output_folder the folder to save output data in
#' @return used for the side effect of downloading GFC tiles
download_tiles <- function(gfc_tiles, output_folder) {
    successes <- 0
    failures <- 0
    skips <- 0
    message(paste('Found', length(tile_urls), 'tiles to download.'))
    for (n in 1:length(tile_urls)) {
        tile_url <- tile_urls[n]
        img_file <- basename(tile_url)
        output_path <- file.path(output_folder, img_file)
        if (file.exists(output_path)) {
            if (verify_download(tile_url, output_path)) {
                message(paste(img_file, 'exists but has bad checksum - re-downloading file'))
            } else {
                message(paste(img_file, 'exists and has good checksum - skipping download'))
                skips <- skips + 1
                next
            }
        }
        if (download_tile(tile_url, output_path) == 0) {
            successes <- successes + 1
        } else {
            failures <- failures + 1
        }

    }
    message(paste(successes, "tile(s) succeeded,", skips, "tile(s) skipped,", 
                failures, "tile(s) failed."))
}
