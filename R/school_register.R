

#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param dataset_id DESCRIPTION.
#' @param write_file DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' @importFrom tidyr unnest_wider unnest_longer unnest unnest_auto
vz_get_register_xml <- function(dataset_id = "rejstrik-skol-a-skolskych-zarizeni-cela-cr",
                                write_file = F) {
  url <- vz_get_url("register", register_id = dataset_id)
  dl_path <- ifelse(write_file, "registr.xml", tempfile(fileext = ".xml"))

  msg_download_size(url)

  curl::curl_download(url, dl_path, handle = curl::new_handle() %>%
                        curl::handle_setheaders(`User-Agent` = ua))
  ui_done("Data downloaded. Reading (this may take a while)")

  return(dl_path)
}


#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param dl_path DESCRIPTION.
#' @param tables DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
vz_load_register <- function(dl_path, tables = "organisations") {

  available_tables <- c("organisations", "schools", "locations",
                        "specialisations")

  if(any(!tables %in% available_tables)) {
    wrong_tables <- tables[!tables %in% available_tables]
    ui_stop("Table(s) {ui_value(wrong_tables)} not available")
  }

  doc <- xml2::as_list(xml2::read_xml(dl_path))
  doc[[1]][1] <- NULL

  doc_f <- doc[[1]]

  sklf <- tibble(skola = doc_f) %>%
    unnest_wider(skola, simplify = T) %>%
    unnest_longer(RedIzo)

  vz_skolskeosoby <- sklf %>%
    unnest_longer(ICO) %>%
    unnest_wider(Reditelstvi, simplify = T) %>%
    unnest(c(RedPlnyNazev, RedZkracenyNazev, RedRUAINKod,
             PravniForma, DruhZrizovatele)) %>%
    unnest(c(RedPlnyNazev, RedZkracenyNazev, RedRUAINKod,
             PravniForma, DruhZrizovatele)) %>%
    unnest_wider(Reditel, simplify = T) %>%
    unnest_longer(c(ReditelJmeno)) %>%
    select(-matches("Adresa|^Okres$|^ORP$"),
           -ReditelJeStatutar,
           -SkolyZarizeni, -StatutarniOrgany, -DobaZrizeniSubjektu) %>%
    mutate(zriz_ICO = purrr::map(Zrizovatele, `[[`, "Zrizovatel") %>%
             purrr::map(`[[`, "ZrizICO") %>% purrr::map(`[[`, 1),
           zriz_ICO = ifelse(is.null(zriz_ICO[[1]]), NA, zriz_ICO) %>%
             purrr::map_chr(`[[`, 1)) %>%
    select(-Zrizovatele) %>%
    rename(redizo = RedIzo)

  # write_parquet(vz_skolskeosoby, "stistko/od_skolskeosoby.parquet")

  vz_zarizeni <- sklf %>%
    select(redizo = RedIzo, SkolyZarizeni) %>%
    unnest_longer(SkolyZarizeni) %>%
    unnest_wider(SkolyZarizeni) %>%
    unnest_longer(SkolaPlnyNazev) %>%
    unnest_longer(SkolaDruhTyp) %>%
    unnest_longer(SkolaKapacita) %>%
    unnest_longer(SkolaKapacitaJednotka) %>%
    unnest_longer(SkolaJazyk) %>%
    unnest_longer(IZO) %>%
    mutate(SkolaKapacita = dplyr::if_else(grepl("[a-zA-Z]",
                                                         SkolaKapacita),
                                          NA_character_,
                                          SkolaKapacita)) %>%
    mutate(SkolaKapacita = dplyr::if_else(!is.na(SkolaKapacita),
                                          as.integer(SkolaKapacita),
                                          NA_integer_)) %>%
    rename(izo = IZO) %>%
    select(-dplyr::starts_with("SkolaDatum"),
           -dplyr::matches("Mista|Obor"))

  # write_parquet(vz_zarizeni, "stistko/od_zarizeni.parquet")

  vz_mista <- sklf %>%
    select(redizo = RedIzo, SkolyZarizeni) %>%
    filter(redizo != "691014086") %>%
    unnest_longer(SkolyZarizeni) %>%
    unnest_wider(SkolyZarizeni) %>%
    unnest_longer(IZO) %>%
    select(redizo, IZO, SkolaMistaVykonuCinnosti) %>%
    unnest_longer(SkolaMistaVykonuCinnosti) %>%
    unnest_wider(SkolaMistaVykonuCinnosti) %>%
    unnest_longer(IDMista) %>%
    unnest_longer(MistoDruhTyp) %>%
    unnest_longer(MistoRUAINKod) %>%
    select(redizo, izo = IZO, IDMista, MistoDruhTyp, ADM_KOD = MistoRUAINKod)

  return(list(vz_skolskeosoby, vz_zarizeni, vz_mista))

}


#' Download and read school register
#'
#' This is the high-level function for getting data from the online XML export
#' of the school register. It downloads the file (whole country by default) and
#' turns it into a tibble, cleaning up names and dropping some uninteresting
#' columns (this may change as the package matures.)
#'
#' @param dataset_id which dataset to download; used to select data dumps for individual regions or whole country.
#'  Currently only the default is implemented (whhole country).
#' @param tables Currently ignored, the first three tables are returned. Which tables to return. Can be one or more of "organisations",
#'   "schools", "locations" or "specialisations".
#' @param write_file Whether to keep the downloaded XML file.
#'   Currently only writing to the working directory is supported.
#'
#' @return a [tibble][tibble::tibble-package] or list of tibbles if multiple
#'   table names are passed to `tables`.
vz_get_register <- function(dataset_id = "rejstrik-skol-a-skolskych-zarizeni-cela-cr",
                            tables = "organisations", write_file = TRUE) {

  dl_path <- vz_get_register_xml(dataset_id = dataset_id, write_file = write_file)
  return(vz_load_register(dl_path, tables = tables))
  # write_parquet(vz_mista, "stistko/od_mista.parquet")
}

vz_list_registers <- function() {
  ckanr::package_list(url = msmt_ckan_base_url, as = "table")
}

