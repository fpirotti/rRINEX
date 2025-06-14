
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rRINEX

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

## STILL BEING DEVELOPED, NOT YET OPERATIONAL !!

The following command might work and will download and run the **web
interface…** - shiny library is required.

shiny::runGitHub(“rRINEX”, “fpirotti”, subdir =
“data-raw/rtklibShinyApp/”)

The goal of rRINEX is to provide simple tools to analyse your RINEX
observation files.

Just upload them and it will try to find NAV files and Ephemeris data
and create plots, statistics etc….

Some advantages is that it can use parallel reading of large files.

It interfaces with
<a href="http://www.rtklib.com" target="_blank"><em>RTKLIB</em></a> ,
which is easily downloaded from the link, both for linux and windows.
Download the observation data of a CORS (continuous operating reference
stations) network froma GNSS data archive in the net. For now it
connects to a user-provided list of CORS repositories, using a templated
string just like RTKLIB’s URL_LIST.txt

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

To run the interface this software makes sure
that you have all necessary packages installed so it might take a while to start
the first time....

``` r
shiny::runGitHub(“rRINEX”, “fpirotti”, subdir = “data-raw/rtklibShinyApp/”)

```

## Examples

### Some benchmarking

Reading a 5 MB (uncompressed) observation file with a total of 900
epochs and a total of 26701 readings takes **1.3s**
