###############################################################################
# This script shows an example of how to use the "gfcanalysis" R package put 
# together by Alex Zvoleff (azvoleff@conservation.org) for working with the 
# Hansen et al. 2013 Global Forest Change dataset. Contact Alex if you notice 
# any issues or have problems using the package.
#
# See the help files for the functions below for more information. For example, 
# type "?download_tiles" in R to see the help file for the "download_tiles" 
# function.
# 
# NOTE: the gfcanalysis package must be installed before this script will run.  
# Run the "install_gfcanalysis.R" script to install/update the gfcanalysis 
# package.
###############################################################################

# Load the gfcanalysis package
library(gfcanalysis)
# Load 'rgdal' package, which is used to read/write shapefiles and rasters
library(rgdal)

# Indicate where we want to save GFC tiles downloaded from Google. For any 
# given AOI, the script will first check to see if these tiles are available 
# locally (in the below folder) before downloading them from the server - so I 
# recommend storing ALL of your GFC tiles in the same folder.
output_folder <- 'C:/Users/azvoleff/Desktop/gfcanalysis_demo'

# Set the threshold for forest/non-forest based on the treecover2000 layer in 
# the GFC product
forest_threshold <- 90

###############################################################################
# Downloading data from Google server for a given AOI
###############################################################################

# Load a demo AOI from the P drive - notice that first we specify the folder 
# the shapefile is in, and then the name of the shapefile without the '.shp'
aoi <- readOGR('P:/Alex Zvoleff/gfcanalysis_demo', 'ZOI_NAK_2012')

# Calculate the google server URLs for the tiles needed to cover the AOI
tiles <- calc_gfc_tiles(aoi)
# Check to see if these tiles are already present locally, and download them if 
# they are not.
download_tiles(tiles, output_folder, first_and_last=FALSE)
# Extract the GFC data for this AOI from the downloaded GFC tiles, mosaicing 
# multiple tiles as necessary (if needed to cover the AOI).
gfc_data <- extract_gfc(aoi, output_folder)
# Save the output data to a GeoTIFF (can also save in ENVI format, Erdas 
# format, etc.)
writeRaster(gfc_data, filename='test_gfc_extract.tif')

###############################################################################
# Performing thresholding and calculating basic statistics
###############################################################################

# Calculate annual statistics on forest loss/gain
gfc_stats <- gfc_stats(aoi, gfc_data, forest_threshold=forest_threshold)
# Save these statistics to CSV files for use in Excel, etc.
write.csv(gfc_stats$loss_table, file='test_gfc_extract_losstable.csv', row.names=FALSE)
write.csv(gfc_stats$gain_table, file='test_gfc_extract_gaintable.csv', row.names=FALSE)

# Calculate and save a thresholded version of the GFC product
gfc_thresholded <- threshold(gfc_data, forest_threshold=forest_threshold)
writeRaster(gfc_thresholded, filename='test_gfc_extract_thresholded.tif')

# Calculate and save a thresholded annual layer stack from the GFC product 
# (useful for simple visualizations, etc.)
gfc_thresholded_annual <- annual_stack(gfc_data, forest_threshold=forest_threshold)
writeRaster(gfc_thresholded_annual, filename='test_gfc_extract_thresholded_annual.tif')

# Save a simple visualization of the thresholded annual layer stack (this is 
# just an example, and is using the data in WGS84. The data should be projected 
# for this).
animate_annual(aoi, gfc_thresholded_annual)
