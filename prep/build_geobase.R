library(tidyverse)
library(arrow)

download.file("https://vdp.cuzk.cz/vymenny_format/csv/20200930_OB_ADR_csv.zip",
              destfile = "ruian_adm_all.zip")
adm_files <- unzip("ruian_adm_all.zip")
download.file("https://vdp.cuzk.cz/vymenny_format/csv/20200930_strukt_ADR.csv.zip",
              destfile = "ruian_hierarchie.zip")
hier <- unzip("ruian_hierarchie.zip")

rradm <- map_dfr(adm_files, read_delim, delim = ";",
             locale = locale(encoding = "Windows-1250", decimal_mark = ".", grouping_mark = " "),
             col_types = list(`Číslo orientační` = "-",
                              `Kód ADM` = "c",
                              `Kód MOP` = "c",
                              `Kód obce` = "c",
                              `Název obce` = "c",
                              `Kód ulice` = "c",
                              `Kód MOMC` = "c",
                              `Název MOMC` = col_skip(),
                              `Číslo domovní` = col_skip(),
                              `Souřadnice X` = "d",
                              `Souřadnice Y` = "d",
                              `Název ulice` = col_skip(),
                              `Typ SO` = col_skip(),
                              `Název části obce` = col_skip(),
                              `Platí Od` = col_skip(),
                              `PSČ` = col_skip(),
                              `Kód části obce` = "c",
                              `Znak čísla orientačního` = col_skip())) %>%
  select(kod_adm = `Kód ADM`,
         kod_obce = `Kód obce`,
         kod_momc = `Kód MOMC`,
         sour_x = `Souřadnice X`,
         sour_y = `Souřadnice Y`)

arrow::write_parquet(rradm, "temp/ab.parquet")

ds <- arrow::open_dataset("temp")

arrow::write_dataset(ds, path = "adresni-mista",
                     format = "parquet",
                     # codec = arrow::Codec$create("LZ4_FRAME"),
                     partitioning = c("kod_obce"))

ds <- arrow::open_dataset("adresni-mista")


adm_vazby <- read_csv2("ruian_hierarchie/adresni-mista-vazby-cr.csv")

arrow::write_parquet(adm_vazby, "adm_vazby.parquet")
