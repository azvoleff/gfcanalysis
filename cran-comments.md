## gfcanalysis 1.8.0

-   Released primarily to remove dependencies on **rgdal** and **rgeos**
-   Minor bugfixes and url fixes incorporated as well

## Tested platforms

-   Local install x86_64-w64-mingw32 (Windows, R 4.2.3)
-   Winbuilder
    -   devtools::check_win_devel() - Windows Server 2022 x64, R Under development (unstable) (2023-10-08 r85282 ucrt)
    -   devtools::check_win_release() - Windows Server 2022 x64, R version 4.3.1 (2023-06-16 ucrt)
-   rhub::check_for_cran()
    -   Windows Server 2022, R-devel, 64 bit
    -   Windows Server 2022, R-devel, 64 bit
    -   Fedora Linux, R-devel
    -   Ubuntu Linux 20.04.1 LTS, R-release

## R CMD check results

-   Winbuilder - 0 errors \| 0 warnings \| 0 notes
-   rhub:
    -   Ubuntu Linux 20.04.1 LTS
        -   PREPRERROR comes up that seems to relate to an error installing dependencies on the server - [posted as an issue here](https://github.com/r-hub/rhub/issues/569)
    -   Windows Server 2022 - two notes come up on the checks run on Windows Server 2022, both of which seem to relate to ongoing issues within rhub, and so likely can be ignored
        -   Found the following files/directories: ''NULL'' - seems ok to ignore, and to be related to [this known rhub issue](https://github.com/r-hub/rhub/issues/560)
        -   Found the following files/directories:'lastMiKTeXException''' - seems ok to ignore, and to be related to [this known rhub issue](https://github.com/r-hub/rhub/issues/503)
