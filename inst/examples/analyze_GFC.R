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

# Start a cluster for parallel processing. You must install the "snow" package 
# for this to work. Comment out this line, and the endCluster() line at the end 
# of this script, if you do NOT want gfcanalysis to run in parallel.
if (require(snow)) beginCluster()

# Indicate where we want to save GFC tiles downloaded from Google. For any 
# given AOI, the script will first check to see if these tiles are available 
# locally (in the below folder) before downloading them from the server - so I 
# recommend storing ALL of your GFC tiles in the same folder. For this example 
# we will save files in the current working directory folder.
output_folder <- '.'

# Set the threshold for forest/non-forest based on the treecover2000 layer in 
# the GFC product
forest_threshold <- 90

###############################################################################
# Download data from Google server for a given AOI
###############################################################################

# Load a demo AOI from the P drive - notice that first we specify the folder 
# the shapefile is in, and then the name of the shapefile without the '.shp'
aoi <- readOGR(system.file('extdata', package='gfcanalysis'), 'ZOI_NAK_2012')

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
gfc_data <- writeRaster(gfc_data, filename='gfc_NAK_extract.tif')

###############################################################################
# Performing thresholding and calculate basic statistics
###############################################################################

# Calculate and save a thresholded version of the GFC product
gfc_thresholded <- threshold_gfc(gfc_data, forest_threshold=forest_threshold, 
                                 filename="gfc_NAK_extract_thresholded.tif")

# Calculate annual statistics on forest loss/gain
gfc_stats <- gfc_stats(aoi, gfc_thresholded)

# Save statistics to CSV files for use in Excel, etc.
write.csv(gfc_stats$loss_table, file='gfc_NAK_extract_losstable.csv', row.names=FALSE)
write.csv(gfc_stats$gain_table, file='gfc_NAK_extract_gaintable.csv', row.names=FALSE)

###############################################################################
# Make visualization of forest change
###############################################################################

# Calculate and save a thresholded annual layer stack from the GFC product 
# (useful for simple visualizations, etc.)
gfc_thresholded_annual <- annual_stack(gfc_thresholded)
writeRaster(gfc_thresholded_annual, filename='gfc_NAK_extract_thresholded_annual.tif')

# Save a simple visualization of the thresholded annual layer stack (this is 
# just an example, and is using the data in WGS84. The data should be projected 
# for this).
animate_annual(aoi, gfc_thresholded_annual)

# Stop the parallel processing cluster
if (require(snow)) endCluster()
