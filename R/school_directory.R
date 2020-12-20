sk_get_search_page <- function() {
  rvest::html_session("http://stistko.uiv.cz/registr/vybskolrn.asp")
}

sk_get_search_form <- function(search_page = NULL) {
  if(is.null(search_page)) {
    search_page <- sk_get_search_page()
  }

  rvest::html_form(search_page)[[1]]
}

sk_get_search_fields <- function(search_form = NULL) {
  if(is.null(search_form)) {
    search_form <- sk_get_search_form()
  }

  search_form[['fields']]

}



#' Get school directory
#'
#' \lifecycle{experimental}
#' This function performs a search on the [school directory at uiv.cz](http://stistko.uiv.cz/registr/vybskolrn.asp) and returns
#' the resulting export -  either the XLS file or the data, or both.
#' The school directory is a version of the school register: unlike the core
#' register, it contains contact information but lacks some other information
#' (such as unique address identification.) Use `sk_get_register()` for the core
#' register.
#'
#' @param tables a character vector of tables to retrieve. See Details.
#' @param fields a named character vector of search fields. Use `sk_get_search_fields()`
#' to see a list of fields and their potential values.
#' @param return_tibbles Whether to return the data (if TRUE) or only download the files (if FALSE).
#' @param keep_files Whether to write the XLS files locally.
#'
#' @details
#'## Available tables
#'
#' Tables can include "addresses", "schools", "locations", "specialisations".
#' If you need more tables based on the same query (fields), pass them into
#' a single function call in order to avoid burdening the data provider's
#' server (the server needs to perform a search for each call; there is no caching
#' and no data dumps are made available).
#'
#' ## What this does
#'
#' The function
#'
#'- performs a search on the school directory at uiv.cz
#'- traverses the results to the export links
#'- downloads the XLS files
#'- loads them into tibbles if return_tibbles is TRUE
#'
#' This is the only way to get to the data - there are no static dumps available.
#' At the same time, no intense web scraping takes place - only individual
#' export files (max 4 per call) are downloaded the same way as
#' it would be done manually.
#'
#' ## Note
#'
#' To avoid blitzing the data provider's server with many heavy requests:
#'
#' 1. If you need more tables based on the same search, pass it in one call,
#' using the `tables` argument. This means that only one initial search is peformed.
#' 1. Only ask for the tables you need.
#' 1. If you need a subset of the data, use the `fields` argument
#' 1. If you need multiple subsets of the data, try to do that via the `fields`
#' argument too, though that may not always be possible.
#' 1. If you are downloading a large dump and reusing it in a pipeline, keep the
#' downloaded XLS files (or your own export) locally (setting `keep_files` to TRUE),
#' use caching and avoid calling this function repeatedly (ideally make any reruns
#' conditional on the age of the stored export).
#'
#' @return if return_tibbles is TRUE, a named list of [tibbles][tibble::tibble-package],
#' with a tibble for each table in `tables` with the corresponding name; otherwise a character vector of file paths.
#' Note that the resulting XLS files are in fact HTML files and you are best off
#' loading them using `sk_load_directory()` and tidying with `sk_process_directory`,
#' though they can be opened in Excel too.
#'
#' @examples
#' sk_get_directory("addresses", fields = c(uzemi = "CZ010"), return_tibbles = T, write_files = T)
sk_get_directory <- function(tables = NULL,
                           fields = NULL,
                           return_tibbles = FALSE,
                           keep_files = TRUE) {

  if(is.null(tables)) {
    tabs <- c("addresses", "schools", "locations", "specialisations")
  } else {
    tabs <- tables
  }

  search_page <- sk_get_search_page()
  search_form <- sk_get_search_form(search_page)

  fields_to_make_default <- c("uzemi","zrizovatel", "organ","typ", "jazs","delka",
                              "forma","jazob","skupobor","kmobor","obor")

  search_form_fields <- sk_get_search_fields(search_form)

  field_names <- names(search_form_fields)

  search_form_fields_with_defaults <- list()
  for (form_field in search_form_fields) {
    if(form_field[['name']] %in% fields_to_make_default) form_field['value'] <- "NIC"
    search_form_fields_with_defaults <- append(search_form_fields_with_defaults,
                                               list(form_field))
  }

  names(search_form_fields_with_defaults) <- field_names

  if(!is.null(fields)) {
    fields_for_userupdate <- names(fields)
    search_form_fields_userupdated <- list()
    for (form_field in search_form_fields_with_defaults) {
      if(form_field[['name']] %in% fields_for_userupdate) {
        form_field['value'] <- fields[form_field[['name']]]
      }
      search_form_fields_userupdated <- append(search_form_fields_userupdated,
                                                 list(form_field))
    }
    search_form_fields_final <- search_form_fields_userupdated
  } else {
    search_form_fields_final <- search_form_fields_with_defaults
  }

  names(search_form_fields_final) <- field_names
  search_form_filled <- search_form
  search_form_filled$fields <- search_form_fields_final

  results_page <- rvest::submit_form(search_page, search_form_filled, "XX")

  results_forms <- results_page %>% xml2::read_html() %>%
    rvest::html_nodes("form") %>%
    rvest::html_form()

  export_page <- rvest::submit_form(results_page, results_forms[[2]], "EX")

  export_forms <- rvest::html_form(export_page)

  print("downloading data")
  responses <- list()
  if("addresses" %in% tabs) {
    responses <- append(responses, list(rvest::submit_form(export_page, export_forms[[1]], "EX")))
    names(responses) <- c(names(responses), "addresses")
  }
  if("schools" %in% tabs) {
    responses <- append(responses, list(rvest::submit_form(export_page, export_forms[[2]], "EXX")))
    names(responses) <- c(names(responses), "schools")
  }
  if("locations" %in% tabs) {
    responses <- append(responses, list(rvest::submit_form(export_page, export_forms[[3]], "EXM")))
    names(responses) <- c(names(responses), "locations")
    }
  if("specialisations" %in% tabs) {
    responses <- append(responses, list(rvest::submit_form(export_page, export_forms[[4]], "EXO")))
    names(responses) <- c(names(responses), "specialisations")
    }

    print("writing files")
  paths <- purrr::map_chr(responses, response_to_quasixls, !keep_files)
  names(paths) <- names(responses)

  if(return_tibbles) {
    print("loading data")
    rslt <- purrr::map(paths, sk_load_stistko)
  } else {
    rslt <- paths
  }

  return(rslt)

}

response_to_quasixls <- function(response, tempfile = TRUE) {

  if(tempfile) {
    path <- tempfile(fileext = ".xls")
  } else {
    attachment_string <- response$response$headers$`content-disposition`
    path <- regmatches(attachment_string,
                       regexpr("(?<=filename\\=)(\\w*.xls$)",
                               attachment_string, perl = T))
  }

  r3_c_bin <- httr::content(response$response)
  writeBin(r3_c_bin, path)

  return(path)
}

sk_load_stistko <- function(path) {
  df <- (xml2::read_html(path) %>% rvest::html_table(header = TRUE))[[1]]
  return(tibble::as_tibble(df))
}

