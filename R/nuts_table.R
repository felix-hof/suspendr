#' Data set containing NUTS-Classifications
#'
#' @description A function that returns a reference table for NUTS regions including IDs and names.
#' @param cache_dir Object of class \code{character}. Indicates a path where the return object is cached. Defaults to \code{\link{tempdir}}.
#'
#' @return An object of class \code{data.frame} containing NUTS-Codes and names of the regions considered in the SUSPend project.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter arrange select mutate rename left_join



get_nuts_table_from_source <- function(cache_dir = tempdir()){
  # download, unzip and import necessary file
  nuts_path <- "https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/ref-nuts-2021-60m.shp.zip"
  temp1 <- tempfile(tmpdir = cache_dir)
  temp2 <- tempfile(tmpdir = cache_dir)
  utils::download.file(nuts_path, temp1)
  file <- utils::unzip(temp1, files = "NUTS_AT_2021.csv", exdir = temp2)
  nuts <- utils::read.csv(file, encoding = "UTF-8")
  unlink(c(temp1, temp2), recursive = TRUE)

  # create tables for nuts levels
  nuts_lvl3 <- nuts %>%
    filter(CNTR_CODE %in% c("CH", "AT", "DE", "FR", "IT", "LI"), nchar(NUTS_ID) == 5) %>%
    arrange(CNTR_CODE) %>%
    select(CNTR_CODE, NUTS_ID, NUTS_NAME) %>%
    mutate(NUTS_2 = substr(NUTS_ID, 1, 4)) %>%
    rename(NUTS_3_NAME = NUTS_NAME, NUTS_3 = NUTS_ID)
  nuts_lvl2 <- nuts %>%
    filter(CNTR_CODE %in% c("CH", "AT", "DE", "FR", "IT", "LI"), nchar(NUTS_ID) == 4) %>%
    select(NUTS_ID, NUTS_NAME) %>%
    rename(NUTS_2 = NUTS_ID, NUTS_2_NAME = NUTS_NAME)
  nuts_lvl1 <- nuts %>%
    filter(CNTR_CODE %in% c("CH", "AT", "DE", "FR", "IT", "LI"), nchar(NUTS_ID) == 3) %>%
    select(NUTS_ID, NUTS_NAME) %>%
    rename(NUTS_1 = NUTS_ID, NUTS_1_NAME = NUTS_NAME)

  # join tables
  nuts <- left_join(nuts_lvl3, nuts_lvl2, by = "NUTS_2") %>%
    mutate(NUTS_1 = substr(NUTS_2, 1, 3)) %>%
    left_join(., nuts_lvl1, by = "NUTS_1")

  # throw out french oversea departments
  nuts <- nuts[-which(nuts$NUTS_1 == "FRY"), ]

  # edit naming (replace apostrophe)
  nuts$NUTS_3_NAME <- vapply(nuts$NUTS_3_NAME, function(x) gsub("\u2019", "'", x), character(1L))
  nuts$NUTS_2_NAME <- vapply(nuts$NUTS_2_NAME, function(x) gsub("\u2019", "'", x), character(1L))
  nuts$NUTS_1_NAME <- vapply(nuts$NUTS_1_NAME, function(x) gsub("\u2019", "'", x), character(1L))

  # edit naming (replace dash)
  nuts$NUTS_3_NAME <- vapply(nuts$NUTS_3_NAME, function(x) gsub("\u2014", "-", x), character(1L))
  nuts$NUTS_2_NAME <- vapply(nuts$NUTS_2_NAME, function(x) gsub("\u2014", "-", x), character(1L))
  nuts$NUTS_1_NAME <- vapply(nuts$NUTS_1_NAME, function(x) gsub("\u2014", "-", x), character(1L))

  # edit naming (delete trailing whitespaces)
  nuts$NUTS_3_NAME <- vapply(nuts$NUTS_3_NAME, function(x) gsub("\\s$", "", x), character(1L))
  nuts$NUTS_2_NAME <- vapply(nuts$NUTS_2_NAME, function(x) gsub("\\s$", "", x), character(1L))
  nuts$NUTS_1_NAME <- vapply(nuts$NUTS_1_NAME, function(x) gsub("\\s$", "", x), character(1L))

  # edit level 1 names
  nuts$NUTS_1_NAME[nuts$NUTS_1_NAME == "Schweiz/Suisse/Svizzera"] <- "Schweiz"
  nuts$NUTS_1_NAME[nuts$NUTS_1_NAME == "Bourgogne-Franche-Comt\u00E9"] <- "Bourgogne - Franche-Comt\u00E9"
  nuts$NUTS_1_NAME[nuts$NUTS_1_NAME == "Auvergne-Rh\u00F4ne-Alpes"] <- "Auvergne - Rh\u00F4ne-Alpes"

  # order this nicely
  nuts <- nuts %>% arrange(CNTR_CODE, NUTS_3)

  # save data as .RDS to cache_dir
  saveRDS(object = nuts, file = paste0(cache_dir, "/nuts_table.RDS"))

  return(nuts)
}
