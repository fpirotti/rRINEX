.onLoad <- function(libname, pkgname) {
  # Example: check for updates
  packageStartupMessage("Loading package ", pkgname, " (", utils::packageVersion(pkgname), ")")
  # download.file("https://raw.githubusercontent.com/tomojitakasu/RTKLIB/refs/heads/rtklib_2.4.3/data/URL_LIST.txt", "inst/extdata/URL_LIST.txt")
  # Optional: check for updates (e.g., GitHub version vs. local)
  # check_for_package_update(pkgname)
}
