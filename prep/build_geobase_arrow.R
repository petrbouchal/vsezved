download.file("https://vdp.cuzk.cz/vymenny_format/csv/20200930_OB_ADR_csv.zip",
              destfile = "ruian_adm_all.zip")
adm_files <- unzip("ruian_adm_all.zip", overwrite = F)

adm_files <- list.files("CSV")

resave <- function(file_in, dir_in, dir_out) {
  rd <- read_lines(file.path(dir_in, file_in),
                   locale = locale(encoding = "Windows-1250"))
  path_out <- file.path(dir_out, file_in)
  write_lines(rd, path_out)
  return(path_out)
}

dir.create("csv_fixed")
out <- purrr::map_chr(adm_files, resave, "CSV", "csv_fixed")

out <- list.files("csv_fixed")

reorg_files <- function(path) {
  obec <- paste0("obec=", str_extract(path, "5[0-9]{5}"))
  dir.create(file.path("CSV2", obec), recursive = T, showWarnings = F)
  file.copy(path, file.path("CSV2", obec, "file.csv"))
}

purrr::map(file.path("csv_fixed", out), reorg_files)

ads <- arrow::open_dataset(sources = "CSV2",
                           format = "text",
                           delimiter = ";",
                           # skip_rows = 1066000
) %>%
  select(obec, `Kód obce`) %>%
  # head(1000) %>%
  collect()

head(ads) %>% collect()

ads2 <- open_dataset(sources =
                       list(dataset_factory(x = "CSV2",
                                            partitioning = hive_partition(obec = string()),
                                            format = CsvFileFormat$create(delim = ";",
                                                                          opts = CsvConvertOptions$create(check_utf8 = F)))))

ads2 %>%
  # head() %>%
  # select(obec, 1, 2, 4, 18, 19) %>%
  select(obec, x = 1) %>%
  head() %>%
  collect()
