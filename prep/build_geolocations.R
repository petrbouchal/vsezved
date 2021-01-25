adb <- read_parquet("adm_vazby.parquet")
adm <- read_parquet("temp/ab.parquet")

r_skoly <- read_parquet("stistko/Skoly.parquet")

skoly_coords <- r_skoly %>%
  filter(druh_typ %in% c("Základní škola", "Střední škola",
                         "Konzervatoř", "Vyšší odborná škola",
                         "Základní škola - Szkoła Podstawowa",
                         "Středisko praktického vyučování",
                         "Domov mládeže")) %>%
  left_join(adm) %>%
  mutate(geometry = map2(-sour_y, -sour_x,
                         ~st_point(x = c(..1, ..2), dim = "XY"))) %>%
  st_as_sf() %>%
  st_set_crs(5514) %>%
  st_transform(4326) %>%
  mutate(coord_lon = map_dbl(geometry, `[[`, 1),
         coord_lat = map_dbl(geometry, `[[`, 2),
         point_wkt = st_as_text(geometry)) %>%
  select(redizo, izo, ico, kod_obce, kod_adm, starts_with("sour_"),
         starts_with("coord_"), point_wkt)
