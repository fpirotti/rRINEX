
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rRINEX

<!-- badges: start -->
<!-- badges: end -->

The goal of rRINEX is to provide simple tools to analyse your RINEX
observation files.

It tries to interface with
<a href="http://www.rtklib.com" target="_blank"><em>RTKLIB</em></a> , so
you should download it and provide rRINEX with the path to the **bin**
folder. Download the observation data of a CORS (continuous operating
reference stations) network froma GNSS data archive in the net. For now
it connects to a user-provided list of CORS repositories, using a
templated string just like RTKLIB’s URL_LIST.txt

<http://www.epncb.oma.be/ftp/center/data/BKGI.RDC>
<https://igs.bkg.bund.de/root_ftp/IGS/highrate/2020/263/l/>

## Installation

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

## Examples

Check out the vignettes to see examples.
