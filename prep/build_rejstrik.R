library(xml2)
library(purrr)
library(tidyr)
library(tidyverse)
library(arrow)

download.file("https://rejstriky.msmt.cz/opendata/vrejcelk.xml", destfile = "registr.xml")

doc <- xml2::as_list(read_xml("registr.xml"))

doc[[1]][1] <- NULL

doc_s <- doc[[1]][1:20]
doc_f <- doc[[1]]

skls <- tibble(skola = doc_s)
sklf <- tibble(skola = doc_f)

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

write_parquet(vz_skolskeosoby, "stistko/od_skolskeosoby.parquet")

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

write_parquet(vz_zarizeni, "stistko/od_zarizeni.parquet")

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

write_parquet(vz_mista, "stistko/od_mista.parquet")

