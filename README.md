---
editor_options: 
  markdown: 
    wrap: 72
---
 
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rRINEX

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

## STILL BEING DEVELOPED, NOT YET OPERATIONAL !!

The goal of rRINEX is to provide [***simple tools***]{.underline} to
analyse your RINEX observation files.

The idea is that the user simply uploads one or more RINEX OBS files,
compressed or uncompressed and the app will try to:

-   show an initial rough position

-   create plots, provide SNR statistics etc….

-   download CORS data (NAV files) and other precise Ephemeris

-   provide user with some feedback on how to best post-process the data

We did not reinvent the wheel, and thus [RTKLIB](http://www.rtklib.com)
is used as backend. Download the observation data of a CORS (continuous
operating reference stations) network from a GNSS data archive in the
net. For now it connects to a user-provided list of CORS repositories,
using a templated string just like RTKLIB’s URL_LIST.txt

<http://www.epncb.oma.be/ftp/center/data/BKGI.RDC>
<https://igs.bkg.bund.de/root_ftp/IGS/highrate/2020/263/l/>

## Installation and use example

You can install the released version of rRINEX from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rRINEX")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fpirotti/rRINEX")
```

To run the interface this software makes sure that you have all
necessary packages installed so it might take a while to start the first
time....

``` r
shiny::runApp(appDir = "data-raw/rtklibShinyApp/")
```

## Examples

### Some benchmarking

Reading a 5 MB (uncompressed) observation file with a total of 900
epochs and a total of 26701 readings takes **1.3s**
