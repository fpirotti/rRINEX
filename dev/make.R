usethis::use_package('sf')
usethis::use_package('tools')
usethis::use_package('R.utils')
usethis::use_package('utils')
usethis::use_package('mapview')

usethis::git_vaccinate()
usethis::use_version()
#usethis::use_rcpp()

devtools::document()
