# This script is used to convert .xlsx file to .rds file

pacman::p_load(tidyverse, magrittr, devtools, readxl, janitor, plotly,
               lubridate, flexdashboard, rlist, plotly, rmarkdown)

# function to clean df names
clean_df <- function(df) {
  df %>%
  clean_names() %>%
  remove_empty_cols() %>%
  remove_empty_rows()
}

# given a xlsx file put all it's table to *.rds file
# concanate table name and column name with underscore
xlsx_to_list <- function(xlsx_file_path) {
  # reaplace " " in table names to "_"
  sheetnames <-
    readxl::excel_sheets(xlsx_file_path) %>%
    trimws() %>%
    gsub(pattern = " ", x = ., replacement = "_")

  listfile <- list()

  for (i in seq_along(sheetnames)) {
    d <- read_excel(xlsx_file_path, sheet = i) %>%
      clean_df()

    # change dttm to chr
    # for (j in seq_along(length(d))) {
    #   c <- d[, j][[1]] %>% class()
    #   if("POSIXct" %in% c | "POSIXt" %in% c | "Date" %in% c) {
    #     d[, j][[1]] %<>% as.character()
    #   }
    # }

    if(sheetnames[i] %in% names(listfile) == F) {
      listfile <- list.append(listfile, d)
    }
  }
  names(listfile) <- sheetnames
  return(listfile)
}

# commodity <- xlsx_to_list("data/Commodity price.xlsx")
# currency <- xlsx_to_list("data/Currency cross rates.xlsx")
# bond <- xlsx_to_list("data/Government bond 10 year yield.xlsx")
# index <- xlsx_to_list("data/INDEX.xlsx")
# etf <- xlsx_to_list("data/Sector specific ETF.xlsx")
#
# saveRDS(commodity, "commodity.rds")
# saveRDS(currency, "currency.rds")
# saveRDS(index, "index.rds")
# saveRDS(etf, "etf.rds")
# saveRDS(bond, "bond.rds")
