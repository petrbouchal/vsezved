library(rvest)
library(xml2)
library(arrow)
library(tidyverse)
library(janitor)

process_stistko <- function(stub) {
  print(stub)

  print("reading html")
  xls_path <- here::here("stistko", str_glue("{stub}.xls"))
  htmldoc <- read_html(xls_path)

  print("reading table")
  table0 <- html_table(htmldoc)[[1]]
  names(table0) <- table0[1,]
  table <- table0[-1,] %>%
    as_tibble(.name_repair = make_clean_names)

  print(names(table))

  table <- table %>%
    rename(redizo = red_izo) %>%
    rename_with(~str_replace_all(.x, "_ruian|_ruain", "_adm"))

  print("writing parquet")
  write_parquet(table, here::here("stistko", str_glue("{stub}.parquet")))
}

# adresar_tbl <- read_parquet(here::here("stistko", "adresar.parquet"))

walk(c("Skoly", "Adresar", "SkolyAMista", "SkolyAObory"), process_stistko)
