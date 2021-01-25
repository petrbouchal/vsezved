
check_server <- function(url) {

  if(!curl::has_internet()) ui_stop("No internet. Cannot continue; stopping.")

  url <- as.character(url)

  url_parsed <- httr::parse_url(url)
  url_modified <- url_parsed
  url_modified$path <- NULL
  url_modified$query <- NULL
  url_modified$params <- NULL
  host <- httr::build_url(url_modified)

  host_check <- !httr::http_error(host)

  if (!host_check) {
    host_resp <- httr::HEAD(host)
    host_status <- httr::http_status(host_resp)
    ui_stop("Host {ui_path(host)} not reachable or returns error on '/' (error {ui_value(host_status)}). Stopping.")
  }

  url_check <- !httr::http_error(url)

  if (!url_check) {
    url_resp <- httr::HEAD(url)
    url_status <- httr::http_status(url_resp)
    ui_stop("Resource {ui_path(url)} returns error {ui_value(url_status)}. Stopping.")
  }

  return(TRUE)
}

get_download_size <- function(x) {

  if(is.character(x)) {
    response = httr::HEAD(x, config = httr::add_headers(`User-Agent` = ua))
    rslt <- httr::headers(response)[["Content-Length"]] %>% as.numeric()
  } else if("response" %in% class(x)) {
    rslt <- httr::headers(x)[["Content-Length"]] %>% as.numeric()
  } else {
    stop("wrong type")
  }
}

msg_download_size <- function(x, past = F) {
  dl_size <- get_download_size(x)

  verb <- ifelse(past, "Downloaded", "Downloading")

  ui_info("{verb} {prettyunits::pretty_bytes(dl_size)}")
}
