#' Get URL for Stistko Ciselnik
#'
#' Constructs the URL for the specified Stistko codelist code.
#'
#' @param code A character string representing the ciselnik code.
#' @return A character string containing the URL for the specified ciselnik code.
#' @examples
#' vz_get_codelist_url("BASO")
#' @export
vz_get_codelist_url <- function(code) {
  code <- toupper(code)
  url <- paste0("http://stistko.uiv.cz/katalog/ciselnik11x.asp?idc=", code, "&aap=on")
  return(url)
}

#' Get Stistko Ciselnik
#'
#' Downloads the HTML page for the specified Stistko codelist code.
#'
#' @param code A character string representing the ciselnik code.
#' @param dest_dir A character string specifying the destination directory. Defaults to tempdir().
#' @return A character string containing the path to the downloaded HTML file.
#' @examples
#' vz_grab_codelist("BASO")
#' @export
vz_grab_codelist <- function(code, dest_dir = NULL) {
  if (is.null(dest_dir)) {
    dest_dir <- tempdir()
  }
  url <- paste0("http://stistko.uiv.cz/katalog/ciselnik11x.asp?idc=", code, "&aap=on")
  destfile = file.path(dest_dir, paste0(code, ".html"))
  download.file(url, destfile)
  return(destfile)
}

#' Download Stistko Ciselnik
#'
#' Downloads the HTML page from the given URL.
#'
#' @param url A character string representing the URL to download the HTML page from.
#' @param dest_dir A character string specifying the destination directory. Defaults to tempdir().
#' @return A character string containing the path to the downloaded HTML file.
#' @examples
#' vz_download_codelist("http://stistko.uiv.cz/katalog/ciselnik11x.asp?idc=BASO&aap=on")
#' @export
vz_download_codelist <- function(url, dest_dir = NULL) {
  if (is.null(dest_dir)) {
    dest_dir <- tempdir()
  }
  code <- stringr::str_extract(url, "(?<=(idc\\=))([A-Z]*)(?=\\&)")
  destfile = file.path(dest_dir, paste0(code, ".html"))
  download.file(url, destfile)
  return(destfile)
}

#' Read Stistko Ciselnik
#'
#' Reads and processes the HTML file of a Stistko ciselnik.
#'
#' @param path A character string representing the path to the HTML file.
#' @return A data frame containing the processed data from the ciselnik.
#' @export
vz_read_codelist <- function(path) {
  ch <- rvest::read_html(path)

  dt0 <- ch |>
    rvest::html_elements("p.lft") |>
    rvest::html_text(trim = TRUE)

  dt_r <- readr::read_delim(I(dt0), delim = ";")

  dt_c <- dt_r |>
    dplyr::slice_head(n = nrow(dt_r) - 1) |> # remove nonsense bottom row
    dplyr::mutate(dplyr::across(dplyr::everything(), stringr::str_squish),
                  datkp = stringr::str_remove(datkp, "\\;"),
                  dplyr::across(c(datkp, datzp), lubridate::parse_date_time, orders = "d.m.Y"),
    )
  return(dt_c)
}

#' Get Stistko Ciselnik (df from code)
#'
#' Reads and processes the HTML file of a Stistko ciselnik based on a code
#'
#' @param code A character string representing the code of the codelist.
#' @param dest_dir Where to save the downloaded file. Defaults to `tempdir()`.
#' @return A data frame containing the processed data from the ciselnik.
#' @examples
#' vz_get_codelist("BASO")
#' @export
vz_get_codelist <- function(code, dest_dir = NULL) {
  if (is.null(dest_dir)) {
    dest_dir <- tempdir()
  }
  fl <- vz_grab_codelist(code, dest_dir = dest_dir)
  vz_read_codelist(fl)
}



