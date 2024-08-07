#' Get search page for directory search
#'
#' @param base_url If left unset, defaults to internally recorded base URL
#' @return an rvest_session object containing the session for the search page.
#'   Can be passed on to `vz_get_search_form()`.
#' @export
vz_get_search_page <- function(base_url = NULL) {

  if(is.null(base_url)) base_url <- stistko_base_url

  url <- paste0(base_url, "vybskolrn.asp")
  check_server(url)
  return(rvest::session(url, httr::user_agent(ua)))
}


#' Get search form for school directory
#'
#' @param search_page search page session as returned by `vz_get_search_page()`
#'
#' @return An rvest_form object to be passed on to `vz_get_directory_responses()`.
vz_get_search_form <- function(search_page = NULL) {
  if(is.null(search_page)) {
    search_page <- vz_get_search_page()
  }
  # the base_url param should become unnecessary once
  # https://github.com/tidyverse/rvest/issues/302 is resolved
  # to make this more self-contained
  return(rvest::html_form(search_page, base_url = stistko_base_url)[[1]])
}

vz_get_search_fields <- function(search_form = NULL) {
  if(is.null(search_form)) {
    search_form <- vz_get_search_form()
  }

  return(search_form[['fields']])
}


#' Get school directory responses
#'
#' Key low-level code for getting school directory data: crawl through layers of forms
#' and return HTTP response containing quasi-XLS attachments with data exports.
#'
#' @inheritParams vz_get_directory
#' @inheritSection vz_get_directory Tables
#'
#' @return HTTP response parsable with response_to_quasixls or generally with {httr}.
#' @export
vz_get_directory_responses <- function(tables = c("addresses", "schools",
                                                  "locations", "specialisations"),
                                       ...) {

  if(missing(tables)) {
    tables <- "addresses"
    ui_info(c("{ui_field('tables')} is not set. Using {ui_value('addresses')}."))
    }

  tryCatch({tabs <- match.arg(tables, several.ok = T)},
           error = function(e) {
             ui_stop("Table(s) {ui_value(tables)} not available")
           })
  if(all(!all.equal(tabs, tables) == TRUE)) {
    diff_tables <- setdiff(tables, tabs)
    ui_stop("Table(s) {ui_value(diff_tables)} not available")
  }

  search_page <- vz_get_search_page()
  search_form <- vz_get_search_form(search_page)

  # the form does not indicate defaults (even though it seems like it
  # in the browser), so we have to set them manually - otherwise no
  # results are returned.

  # build list with field names and the same default value
  # passed to html_set_form via "!!!"
  form_fields <- c("uzemi", "zrizovatel", "organ", "typ", "jazs", "delka",
                   "forma", "jazob", "skupobor", "kmobor", "obor")
  form_vals <- rep("NIC", times = length(form_fields)) %>%
    purrr::set_names(form_fields) %>% as.list()


  search_form_with_defaults <- rvest::html_form_set(search_form, !!!form_vals)

  if(!missing(...)) {
    search_form_with_userinput <- rvest::html_form_set(search_form_with_defaults,
                                                       ...)
    search_form_filled <- search_form_with_userinput
  } else {
    search_form_filled <- search_form_with_defaults
  }

  results_page <- rvest::session_submit(search_page, search_form_filled, "XX")

  results_forms <- results_page %>% xml2::read_html() %>%
    rvest::html_elements("form") %>%
    # the base_url param should become unnecessary once
    # https://github.com/tidyverse/rvest/issues/302 is resolved
    # to make this more self-contained
    rvest::html_form(base_url = stistko_base_url)

  export_page <- rvest::session_submit(results_page, results_forms[[2]], "EX")

  # the base_url param should become unnecessary once
  # https://github.com/tidyverse/rvest/issues/302 is resolved
  # to make this more self-contained
  export_forms <- rvest::html_form(export_page, base_url = stistko_base_url)

  responses <- list()

  add_dir_table <- function(responses, form, name, submit) {
    new_names <- c(names(responses), name)
    new_response <- rvest::session_submit(export_page, form, submit)
    resp <- append(responses, list(new_response))
    msg_download_size(new_response$response, T)
    names(resp) <- new_names
    return(resp)
  }

  if("addresses" %in% tabs) responses <- add_dir_table(responses, export_forms[[1]], "addresses", "EX")
  if("schools" %in% tabs) responses <- add_dir_table(responses, export_forms[[3]], "schools", "EXX")
  if("locations" %in% tabs) responses <- add_dir_table(responses, export_forms[[4]], "locations", "EXM")
  if("specialisations" %in% tabs) responses <- add_dir_table(responses, export_forms[[5]], "specialisations", "EXO")

  return(responses)
}


#' Turn a httr response created by `vz_get_directory_responses()` into and XLS file
#'
#' @param response a httr respons returned by `vz_get_directory_responses()`
#' @param write_file Whether to write the XLS files locally.
#' @inheritParams vz_get_directory
#'
#' @return character of length 1: path to XLS file
#' @export
vz_write_directory_quasixls <- function(response, write_file = FALSE, dest_dir = getwd()) {

  if(!write_file) {
    path <- tempfile(fileext = ".xls")
  } else {
    attachment_string <- response$response$headers$`content-disposition`
    filename <- regmatches(attachment_string,
                       regexpr("(?<=filename\\=)(\\w*.xls$)",
                               attachment_string, perl = T))
    path <- file.path(dest_dir, filename)
  }

  r3_c_bin <- httr::content(response$response)
  writeBin(r3_c_bin, path)

  return(path)
}

#' Get school directory
#'
#' \lifecycle{experimental}
#' This function performs a search on the [school directory at uiv.cz](http://stistko.uiv.cz/registr/vybskolrn.asp) and returns
#' the resulting export -  either the XLS file or the data, or both.
#' The school directory is a version of the school register: unlike the core
#' register, it contains contact information but lacks some other information
#' (such as unique address identification.) Use `vz_get_register()` for the core
#' register.
#'
#' @param tables a character vector of tables to retrieve. See ** Tables** below.
#' @param ... key-value pairs of search fields. Use `vz_get_search_fields()`
#' to see a list of fields and their potential values.
#' @param return_tibbles Whether to return the data (if TRUE) or only download the files (if FALSE).
#' @param write_files Whether to write the XLS files locally.
#' @param dest_dir Directory in which to write XLS files. Defaults to working directory.
#'
#' @return A list of a [tibbles][tibble::tibble-package] if return_tibbles = TRUE, a single tibble if only
#'   one table name is passed `tables`, otherwise a character vector of paths
#'   to the downloaded *.xls files.
#'
#' @section Tables:
#'
#' Tables can include "addresses", "schools", "locations", "specialisations".
#' If you need more tables based on the same query (fields), pass them into
#' a single function call in order to avoid burdening the data provider's
#' server (the server needs to perform a search for each function call; there is no caching
#' and no data dumps are made available).
#'
#' @section What this does:
#'
#' The function
#'
#'- performs a search on the school directory at uiv.cz
#'- by default the search is for all schools,
#'  unless ... params are set to narrow down the search
#'- traverses the results to the export links
#'- downloads the XLS files
#'- loads them into tibbles if return_tibbles is TRUE
#'
#' This is the only way to get to the data - there are no static dumps available.
#' At the same time, no intense web scraping takes place - only individual
#' export files (max 4 per call) are downloaded the same way as
#' it would be done manually.
#'
#' @section Note:
#'
#' To avoid blitzing the data provider's server with many heavy requests:
#'
#' 1. If you need more tables based on the same search, pass it in one call,
#' using the `tables` argument. This means that only one initial search is
#' peformed.
#' 1. Only ask for the tables you need.
#' 1. If you need a subset of the data, use the `fields` (...) argument
#' 1. If you need multiple subsets of the data,
#' try to do that via the `fields` (...) argument too, though that may not always be
#' possible.
#' 1. If you are downloading a large dump and reusing it in a
#' pipeline, keep the downloaded XLS files (or your own export) locally (setting
#' `write_files` to TRUE), use caching and avoid calling this function repeatedly
#' (ideally make any reruns conditional on the age of the stored export
#' or use a pipeline management framework such as {targets}.
#'
#' @return if return_tibbles is TRUE, a named list of
#'   [tibbles][tibble::tibble-package], with a tibble for each table in `tables`
#'   with the corresponding name, unless the function was called with a `tables`
#'   parameter of length one, in which case the result is a tibble;
#'   if return_tibbles is FALSE, the result is a character vector of file paths.
#'   Note that the downloaded XLS files are in fact HTML files and you are best
#'   off loading them using `vz_load_directory()` and tidying with
#'   `vz_load_directory`, though they can be opened in Excel too.
#'
#' @examples
#' vz_get_directory("addresses", uzemi = "CZ010", return_tibbles = TRUE, write_files = TRUE)
#' @export
vz_get_directory <- function(tables = c("addresses", "schools",
                                        "locations", "specialisations"),
                             ...,
                             return_tibbles = FALSE,
                             write_files = TRUE,
                             dest_dir = getwd()) {

  if(missing(tables)) {
    tables <- "addresses"
    ui_info(c("{ui_field('tables')} is not set. Using {ui_value('addresses')}."))
  }

  tryCatch({tabs <- match.arg(tables, several.ok = T)},
           error = function(e) {
             ui_stop("Table(s) {ui_value(tables)} not available")
           })
  if(all(!all.equal(tabs, tables) == TRUE)) {
    diff_tables <- setdiff(tables, tabs)
    ui_stop("Table(s) {ui_value(diff_tables)} not available")
  }

  responses <- vz_get_directory_responses(tables = tabs, ...)

  paths <- purrr::map_chr(responses, vz_write_directory_quasixls, write_files)
  names(paths) <- names(responses)

  if(return_tibbles) {
    rslt <- purrr::map(paths, vz_load_directory)
    if(length(rslt) == 1) rslt <- rslt[[1]]
  } else {
    rslt <- paths
  }

  return(rslt)
}



#' Load directory XLS file
#'
#' Read and clean up quasi-XLSX file retrieved by `vz_write_directory_quasixls`
#'
#' @param path Path to .xls file retrieved by `vz_write_directory_quasixls`
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
vz_load_directory <- function(path) {
  tbl_html <- xml2::read_html(path)
  df <- rvest::html_table(tbl_html, header = TRUE, convert = FALSE)[[1]]
  df_tbl <- suppressMessages(tibble::as_tibble(df, .name_repair = janitor::make_clean_names))
  df_tbl2 <- df_tbl %>%
    mutate(dplyr::across(dplyr::matches("\\w"),
                         ~dplyr::na_if(., "")))
  return(df_tbl2)
}

