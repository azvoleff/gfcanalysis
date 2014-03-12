# gfcanalysis

[![Build Status](https://travis-ci.org/azvoleff/gfcanalysis.png)](https://travis-ci.org/azvoleff/gfcanalysis)

## Tools for working with Hansen et al. 2013 Global Forest Change dataset

The `gfcanalysis` package facilitates analyses of forest change using the 
[Global Forest 
Change](http://earthenginepartners.appspot.com/science-2013-global-forest) 
dataset released by Hansen et al. The package was designed to support the work 
of the [Tropical Ecology Assessment & Monitoring (TEAM) 
Network](http://www.teamnetwork.org/).

## Package Installation

`gfcanalysis` is not yet listed on [CRAN](http://cran.r-project.org).  The 
easiest way to install the `gfcanalysis` package is using the 
[`devtools`](http://cran.r-project.org/web/packages/devtools/index.html) 
package. After installing `devtools` from CRAN, type:

```R
install_github('gfcanalysis', username='azvoleff')
```

at the R prompt to install `gfcanalysis`.

## Usage

There are two example scripts demostrating how to use the `gfcanalysis` 
package.  For those new to R, see the 
[install_gfcanalysis.R](https://raw.github.com/azvoleff/gfcanalysis/master/inst/examples/install_gfcanalysis.R)
script. This script provides guidance on how to download and install the 
package in R. For an example of how to run the package to calculate forest 
change statistics for a given area of interest (AOI), see the examples in
[analyze_gfc.R](https://raw.github.com/azvoleff/gfcanalysis/master/inst/examples/analyze_GFC.R).

## Author Contact Information

[Alex Zvoleff](mailto:azvoleff@conservation.org)  
Postdoctoral Associate  
Tropical Ecology Assessment and Monitoring (TEAM) Network  
Conservation International  
2011 Crystal Dr. Suite 500  
Arlington, VA 22202  
USA

## References
Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A. Turubanova, A. 
Tyukavina, D. Thau, S. V. Stehman, S. J. Goetz, T. R. Loveland, A. Kommareddy, 
A. Egorov, L. Chini, C. O. Justice, and J. R. G. Townshend. 2013. 
High-Resolution Global Maps of 21st-Century Forest Cover Change. Science 342, 
(15 November): 850--853. Data available on-line from: 
http://earthenginepartners.appspot.com/science-2013-global-forest.
