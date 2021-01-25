

#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param dataset_id DESCRIPTION.
#' @param keep_file DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' @importFrom tidyr unnest_wider unnest_longer unnest unnest_auto
vz_get_register_xml <- function(dataset_id = "rejstrik-skol-a-skolskych-zarizeni-cela-cr",
                                 keep_file = F) {
  url <- vz_get_url("register")
  dl_path <- ifelse(keep_file, "registr.xml", tempfile(fileext = ".xml"))

  msg_download_size(url)

  curl::curl_download(url, dl_path, handle = curl::new_handle() %>%
                        curl::handle_setheaders(`User-Agent` = usr))
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

  available_tables <- c("organisations", "schools", "locations", "specialisations")

  if(any(!tables %in% available_tables)) {
    wrong_tables <- tables[!tables %in% available_tables]
    ui_stop("Table(s) {ui_value(wrong_tables)} not available")
  }

  doc <- xml2::as_list(xml2::read_xml(dl_path))
  doc[[1]][1] <- NULL

  doc_f <- doc[[1]]

  sklf <- tibble(skola = doc_f)

  stop()

  vz_skolskeosoby <- sklf %>%
    unnest_wider(skola, simplify = T) %>%
    unnest_auto(RedIzo) %>%
    unnest_auto(ICO) %>%
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
    mutate(zriz_ICO = map(Zrizovatele, `[[`, "Zrizovatel") %>%
             map(`[[`, "ZrizICO") %>% map(`[[`, 1),
           zriz_ICO = ifelse(is.null(zriz_ICO), NA, zriz_ICO) %>%
             map_chr(`[[`, 1)) %>%
    select(-Zrizovatele) %>%
    rename(redizo = RedIzo)

  # write_parquet(vz_skolskeosoby, "stistko/od_skolskeosoby.parquet")

  vz_zarizeni <- sklf %>%
    unnest_wider(skola, simplify = T) %>%
    unnest_auto(RedIzo) %>%
    select(redizo = RedIzo, SkolyZarizeni) %>%
    filter(redizo != "691014086") %>%
    unnest_longer(SkolyZarizeni) %>%
    unnest_wider(SkolyZarizeni) %>%
    unnest_auto(SkolaPlnyNazev) %>%
    unnest_auto(SkolaDruhTyp) %>%
    unnest_auto(SkolaKapacita) %>%
    unnest_auto(SkolaKapacitaJednotka) %>%
    unnest_auto(SkolaJazyk) %>%
    unnest_auto(IZO) %>%
    mutate(SkolaKapacita = as.numeric(SkolaKapacita)) %>%
    rename(izo = IZO) %>%
    select(-starts_with("SkolaDatum"), -matches("Mista|Obor"))

  # write_parquet(vz_zarizeni, "stistko/od_zarizeni.parquet")

  vz_mista <- sklf %>%
    unnest_wider(skola, simplify = T) %>%
    unnest_auto(RedIzo) %>%
    select(redizo = RedIzo, SkolyZarizeni) %>%
    filter(redizo != "691014086") %>%
    unnest_longer(SkolyZarizeni) %>%
    unnest_wider(SkolyZarizeni) %>%
    unnest_auto(IZO) %>%
    select(redizo, IZO, SkolaMistaVykonuCinnosti) %>%
    unnest_longer(SkolaMistaVykonuCinnosti) %>%
    unnest_wider(SkolaMistaVykonuCinnosti) %>%
    unnest_auto(IDMista) %>%
    unnest_auto(MistoDruhTyp) %>%
    unnest_auto(MistoRUAINKod) %>%
    select(redizo, izo = IZO, IDMista, MistoDruhTyp, ADM_KOD = MistoRUAINKod)
}


#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param which DESCRIPTION.
#' @param tables DESCRIPTION.
#' @param keep_file DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
vz_get_register <- function(dataset_id = "rejstrik-skol-a-skolskych-zarizeni-cela-cr",
                            tables = "organisations", keep_file = T) {

  dl_path <- vz_get_register_xml(dataset_id = dataset_id, keep_file = keep_file)
  return(vz_load_register(dl_path, tables = tables))
  # write_parquet(vz_mista, "stistko/od_mista.parquet")
}

vz_list_registers <- function() {
  ckanr::package_list(url = msmt_ckan_base_url, as = "table")
}

