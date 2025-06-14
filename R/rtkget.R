#' Download GNSS Data Based on a RINEX Observation File
#'
#' Downloads BRDC navigation, SP3 precise ephemeris, and CLK clock files
#' for the date found in a given RINEX observation file.
#'
#' @param rinex_obs_path Character. Path to a RINEX observation file (.obs)
#' @param output_dir Character. Directory to save the downloaded files
#'
#' @return A list with downloaded file paths (if successful)
#' @import httr lubridate stringr
#' @export
rtkget <- function(rinex_obs_path, output_dir = "gnss_data") {
  if (!file.exists(rinex_obs_path)) stop("RINEX file not found: ", rinex_obs_path)
  
  if (!dir.exists(output_dir))  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  header <- readLines(rinex_obs_path, n = 200)
  date_line <- header[grepl("TIME OF FIRST OBS", header)]
  
  if (length(date_line) == 0) stop("TIME OF FIRST OBS not found in RINEX header")
  
  date_parts <- as.numeric(unlist(stringr::str_extract_all(date_line, "\\d+")))
  obs_date <- lubridate::make_datetime(year = date_parts[1], month = date_parts[2], day = date_parts[3])
  
  doy <- lubridate::yday(obs_date)
  yy <- format(obs_date, "%y")
  yyyy <- format(obs_date, "%Y")
   
  brdc_url <- sprintf("https://igs.bkg.bund.de/root_ftp/IGS/BRDC/%s/%03d/",
                      yyyy, doy)
  
  tryCatch({
    response <- httr::GET(brdc_url,   httr::timeout(60))
    if (httr::http_error(response)) {
      stop("Failed to access URL: ", brdc_url)
    }
    html <- httr::content(response, as = "parsed", encoding = "UTF-8")
    links <- rvest::html_nodes(html, "a")
    files <- rvest::html_attr(links, "href")
  }, error = function(e) {
    stop("Error downloading: ", url, " - ", e$message)
    return(NULL)
  })
  # File URLs  BRDC00IGS_R_20251630000_01D_MN.rnx.gz
  brdc_url <- sprintf("https://igs.bkg.bund.de/root_ftp/IGS/BRDC/%s/%03d/brdc%03d0.%sN.Z",
                      yyyy, doy, doy, yy)
  sp3_url <- sprintf("https://ftp.aiub.unibe.ch/CODE/%s/%03d/CO%03d0.%s.sp3.Z",
                     yyyy, doy, doy, yy)
  clk_url <- sprintf("https://ftp.aiub.unibe.ch/CODE/%s/%03d/CO%03d0.%s.clk.Z",
                     yyyy, doy, doy, yy)
  browser()
  download_file <- function(url, dest_dir) {
    fname <- basename(url)
    destfile <- file.path(dest_dir, fname)
    tryCatch({
      httr::GET(url, httr::write_disk(destfile, overwrite = TRUE), httr::timeout(60))
      if (file.exists(destfile)) {
        message("Downloaded: ", fname)
        return(destfile)
      } else {
        warning("Download failed: ", url)
        return(NULL)
      }
    }, error = function(e) {
      warning("Error downloading: ", url, " - ", e$message)
      return(NULL)
    })
  }
  
  files_downloaded <- list(
    brdc = download_file(brdc_url, output_dir),
    sp3  = download_file(sp3_url, output_dir),
    clk  = download_file(clk_url, output_dir)
  )
  
  return(files_downloaded)
}
