library(arrow)

csvtr <- CsvTableReader$create(file = ReadableFile$create(path = "CSV2/obec=500071/file.csv"),
                               read_options = CsvReadOptions$create(skip_rows = 0),
                               parse_options = CsvParseOptions$create(delimiter = ";"),
                               convert_options = CsvConvertOptions$create(check_utf8 = F,
                                                                          include_columns = ))
csvtr$Read()

