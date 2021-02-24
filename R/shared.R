stistko_base_url <- "http://stistko.uiv.cz/registr/"
msmtod_base_url <- "https://rejstriky.msmt.cz/opendata/"
msmt_ckan_base_url <- "https://data.msmt.cz"
ua <- "github.com/petrbouchal/vsezved"
ua_string <- paste0("User-Agent: ", ua)
ua_list <- list(`User-Agent` = ua)

vz_get_ckan_url <- function(package_id, base_url) {
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
