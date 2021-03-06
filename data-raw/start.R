usethis::use_package('sf')
usethis::use_package('tools')
usethis::use_package('R.utils')
usethis::use_package('utils') 
usethis::use_package('qpdf')
usethis::use_package('tmap')
usethis::use_package('lattice')
usethis::use_data_raw()

source("DATASET.R")
usethis::git_vaccinate()
usethis::use_version()
#usethis::use_rcpp()

devtools::document()

usethis::use_vignette("rRINEX-examples")
