# gfcanalysis

[![Build Status](https://travis-ci.org/azvoleff/gfcanalysis.png)](https://travis-ci.org/azvoleff/gfcanalysis)

## Overview
The `gfcanalysis` package facilitates analyses of forest change using the 
[Global Forest 
Change](http://earthenginepartners.appspot.com/science-2013-global-forest) 
dataset released by Hansen et al. 2013, and subsequent versions of that data. 
The package was originally designed to support the work of the [Tropical 
Ecology Assessment & Monitoring (TEAM) Network](http://www.teamnetwork.org).

## Package Installation

## Release Version

To install the release version of `gfcanalysis` in R, just run:

```R
install.packages('gfcanalysis')
```

## Development Version

The easiest way to install the development version of the `gfcanalysis` package 
is using the 
[`devtools`](https://CRAN.R-project.org/package=devtools)
package. After installing `devtools`, type:

```R
install_github('azvoleff/gfcanalysis')
```

## Usage

There are two example scripts demonstrating how to use the `gfcanalysis` 
package:

- For those new to R, see the 
[install_gfcanalysis.R](https://raw.github.com/azvoleff/gfcanalysis/master/inst/examples/install_gfcanalysis.R)
script. This script provides guidance on how to download and install the 
(development version) of the package in R.

- For an example of how to run the package to calculate forest 
change statistics for a given area of interest (AOI), see the examples in
[analyze_gfc.R](https://raw.github.com/azvoleff/gfcanalysis/master/inst/examples/analyze_GFC.R).

## Author Contact Information

[Matt Cooper](mailto:mw.coop.r@gmail.com) (current author and maintainer)  
PhD Student  
Department of Geographical Sciences  
University of Maryland  

[Alex Zvoleff](mailto:azvoleff@conservation.org) (original author)  
Senior Director, Resilience Science  
Moore Center for Science
Conservation International  
2011 Crystal Dr. Suite 600  
Arlington, VA 22202  
USA

## References
Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A. Turubanova, A. 
Tyukavina, D. Thau, S. V. Stehman, S. J. Goetz, T. R. Loveland, A. Kommareddy, 
A. Egorov, L. Chini, C. O. Justice, and J. R. G. Townshend. 2013. 
High-Resolution Global Maps of 21st-Century Forest Cover Change. Science 342, 
(15 November): 850--853. Data available on-line from: 
http://earthenginepartners.appspot.com/science-2013-global-forest.
