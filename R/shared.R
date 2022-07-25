stistko_base_url <- "http://stistko.uiv.cz/registr/"
msmtod_base_url <- "https://rejstriky.msmt.cz/opendata/"
msmt_ckan_base_url <- "https://data.msmt.cz"
register_ckan_id <- "rejstrik-skol-a-skolskych-zarizeni-cela-cr"

ua <- "github.com/petrbouchal/vsezved"
ua_string <- paste0("User-Agent: ", ua)
ua_list <- list(`User-Agent` = ua)



#' Get URL of file in MSMT data store
#'
#' Currently assumes we are getting register XML data
#'
#' @param nuts3_kod NUTS code for region, e.g. CZ010 for Prague. Leave as NULL for whole-country school register.
#' @param base_url Base URL. Leave as NULL for MSMT data store URL.
#'
#' @return a URL, character of length 1
#' @export
vz_get_xml_url <- function(nuts3_kod = NULL, base_url = NULL) {

  if(is.null(nuts3_kod)) nuts3_kod <- "celk"
  if(is.null(base_url)) base_url <- msmtod_base_url

  check_server(base_url)

  url <- paste0(base_url, "/VREJ", nuts3_kod, ".xml")
  print(url)

  return(url)
}
