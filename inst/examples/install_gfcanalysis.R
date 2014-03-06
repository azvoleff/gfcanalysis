###############################################################################
# This script installs the "gfcanalysis" R package created by Alex Zvoleff 
# (azvoleff@conservation.org) for working with the Hansen et al. 2013 Global 
# Forest Change dataset.
#
# After installing the package, see the "analyze_gfc.R" script for an example 
# of how to use the package.
################################################################################

# Install the devtools package (if not already installed). devtools is used to 
# install Alex's "gfcanalysis" package.
if (!require(devtools)) install.packages("devtools")

# Install Alex's gfcanalysis package - you only need to do this once.
library(devtools)
install_github('azvoleff/gfcanalysis')
