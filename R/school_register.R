

#' Download (XML) file of register
#'
#' Uses CKAN to find the correct URL in the education ministry's [open data catalogue](https://data.msmt.cz/) and retrieve the file.
#'
#' @inheritParams vz_get_register
#' @param url URL; if left to NULL, will use internal default
#' @param nuts3_kod used to point to per-region datasets; if left unset, defaults to state-wide data
#'
#' @return Path to downloaded (XML) file.
#' @importFrom tidyr unnest_wider unnest_longer unnest unnest_auto
#' @export
vz_get_register_xml <- function(url = NULL,
                                nuts3_kod = NULL,
                                write_file = F,
                                dest_dir = getwd()) {

  if(is.null(url) & is.null(nuts3_kod)) {
    urlf <- vz_get_xml_url()
    ui_info("Neither {ui_field('nuts3_kod')} nor {ui_field('url')} set.",
            "Getting data package {ui_value(register_ckan_id} from {ui_path(msmt_ckan_base_url)}")
  } else if(is.null(url) & !is.null(nuts3_kod)) {
    urlf <- vz_get_xml_url(nuts3_kod = nuts3_kod)
  } else if(!is.null(url) & !is.null(nuts3_kod)) {
    urlf <- url
    ui_info("Both {ui_field('nuts3_kod')} and {ui_field('url')} set.",
            "Using {ui_field('nuts3_kod')} ({ui_value(nuts3_kod)})")
  } else {
    urlf <- url
  }

  dl_path <- ifelse(write_file,
                    file.path(dest_dir, "registr.xml"),
                    tempfile(fileext = ".xml"))

  msg_download_size(urlf)

  curl::curl_download(urlf, dl_path, handle = curl::new_handle() %>%
                        curl::handle_setheaders(`User-Agent` = ua))
  return(dl_path)
}


#' Load register
#'
#' Read XML register and return tibble(s) with the register tables.
#'
#' @param dl_path Path to XML file output by `vz_get_register_xml()`.
#' @inheritParams vz_get_register
#'
#' @inherit vz_get_register return
#' @export
vz_load_register <- function(dl_path, tables = c("organisations", "schools", "locations",
                                                 "specialisations")) {

  # available_tables <- c("organisations", "schools", "locations",
  #                       "specialisations")
  available_tables <- c("organisations", "schools", "locations")

  if(missing(tables)) {
    tables <- "organisations"
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

  doc <- xml2::as_list(xml2::read_xml(dl_path))
  doc[[1]][1] <- NULL

  doc_f <- doc[[1]]

  sklf <- tibble(skola = doc_f) %>%
    unnest_wider(skola, simplify = T) %>%
    unnest_longer(RedIzo)

  reg_list <- list()

  if("organisations" %in% tabs) {
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
      rename(red_izo = RedIzo) %>%
      janitor::clean_names()


    reg_list <- append(reg_list, list(vz_skolskeosoby))
    names(reg_list) <- c(names(reg_list), "organisations")
  }

  if("schools" %in% tabs) {

    vz_zarizeni <- sklf %>%
      select(red_izo = RedIzo, SkolyZarizeni) %>%
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
             -dplyr::matches("Mista|Obor")) %>%
      janitor::clean_names()

    reg_list_names_orig <- names(reg_list)
    reg_list <- append(reg_list, list(vz_zarizeni))
    names(reg_list) <- c(reg_list_names_orig, "schools")
  }

  if("locations" %in% tabs) {
    vz_mista <- sklf %>%
      select(red_izo = RedIzo, SkolyZarizeni) %>%
      unnest_longer(SkolyZarizeni) %>%
      unnest_wider(SkolyZarizeni) %>%
      unnest_longer(IZO) %>%
      select(red_izo, IZO, SkolaMistaVykonuCinnosti) %>%
      unnest_longer(SkolaMistaVykonuCinnosti) %>%
      unnest_wider(SkolaMistaVykonuCinnosti) %>%
      unnest_longer(IDMista) %>%
      unnest_longer(MistoDruhTyp) %>%
      unnest_longer(MistoRUAINKod) %>%
      select(red_izo, izo = IZO, IDMista, MistoDruhTyp, ADM_KOD = MistoRUAINKod) %>%
      janitor::clean_names()

    reg_list_names_orig <- names(reg_list)
    reg_list <- append(reg_list, list(vz_mista))
    names(reg_list) <- c(reg_list_names_orig, "locations")
  }

  reg_list <- reg_list[tabs]

  return(reg_list)

}


#' Download and read school register
#'
#' This is the high-level function for getting data from the online XML export
#' of the school register. It downloads the file (whole country by default) and
#' turns it into a tibble, cleaning up names and dropping some uninteresting
#' columns (this may change as the package matures.)
#'
#' @param tables Which tables to return. Can be one or more of "organisations",
#'   "schools", "locations" or "specialisations" (specialisations not yet available via the package).
#' @param write_file Whether to keep the downloaded XML file.
#'   Currently only writing to the working directory is supported.
#' @param dest_dir Where to write the resulting XML
#' @inheritParams vz_get_register_xml
#'
#' @return a [tibble][tibble::tibble-package] or list of tibbles if multiple
#'   table names are passed to `tables`.
#' @export
vz_get_register <- function(nuts3_kod = NULL,
                            url = NULL,
                            tables = c("organisations", "schools", "locations",
                                       "specialisations"),
                            write_file = TRUE,
                            dest_dir = getwd()) {

  # available_tables <- c("organisations", "schools", "locations",
  #                       "specialisations")
  available_tables <- c("organisations", "schools", "locations")

  if(missing(tables)) {
    tables <- "organisations"
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

  dl_path <- vz_get_register_xml(nuts3_kod = nuts3_kod,
                                 write_file = write_file,
                                 dest_dir = dest_dir)
  ui_done("Data downloaded. Reading (this may take a while)")
  return(vz_load_register(dl_path, tables = tables))
}

