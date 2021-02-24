stistko_base_url <- "http://stistko.uiv.cz/registr/"
msmtod_base_url <- "https://rejstriky.msmt.cz/opendata/"
msmt_ckan_base_url <- "https://data.msmt.cz"
register_ckan_id <- "rejstrik-skol-a-skolskych-zarizeni-cela-cr"

ua <- "github.com/petrbouchal/vsezved"
ua_string <- paste0("User-Agent: ", ua)
ua_list <- list(`User-Agent` = ua)



#' Get URL of file in CKAN dataset
#'
#' Currently assumes we are getting register XML data
#'
#' @param package_id package ID. Leave as NULL for whole-country school register.
#' @param base_url CKAN base URL. Leave as NULL for MSMT CKAN.
#'
#' @return a URL, character of length 1
#' @export
vz_get_ckan_url <- function(package_id = NULL, base_url = NULL) {

  if(is.null(package_id)) package_id <- register_ckan_id
  if(is.null(base_url)) base_url <- msmt_ckan_base_url

  check_server(base_url)
  pkg_meta <- ckanr::package_show(package_id,
                      url = msmt_ckan_base_url, as = "table")
  if(length(pkg_meta$resources$format) == 1 & pkg_meta$resources$format == "XML") {
    url <- pkg_meta$resources$url
  } else {
    usethis::ui_stop("x")
  }
  return(url)
}
