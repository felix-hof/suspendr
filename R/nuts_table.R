#' Data set containing NUTS-Classifications
#'
#' @description A function that returns a reference table of NUTS regions including
#' IDs and names. The data is cached in \code{cache_dir} and read from there if the file
#' already exists. If the cached file is older than a week it is updated from the data sources.
#' @template cache_dir
#' @return An object of class \code{data.frame} containing NUTS-Codes and names of the regions considered in the SUSPend project.
#' @seealso \code{\link[base:tempdir]{tempdir()}}
#' @examples
#' nuts_table()
#' @export
#'
nuts_table <- function(cache_dir = NULL){

  filename <- "nuts_table.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 7, units = "days")
  if(from_cache){
    nuts <- readRDS(make_path(cache_dir, filename))
  } else {
    nuts <- get_nuts_table_from_source(cache_dir = cache_dir, filename = filename)
  }

  return(nuts)
}


# helper functions ----

#' Constructs the table from the Eurostat's data set.
#'
#' @template cache_dir
#' @param filename A string declaring what the name of the cache file should be. Comes from the
#' wrapper function.
#' @return A \code{data.frame} with NUTS classifications.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter arrange select mutate rename left_join
#'
get_nuts_table_from_source <- function(cache_dir, filename){

  # download, unzip and import necessary file
  nuts_path <- "https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/ref-nuts-2021-60m.shp.zip"
  temp1 <- tempfile(tmpdir = tempdir())
  temp2 <- tempfile(tmpdir = tempdir())
  utils::download.file(nuts_path, temp1)
  file <- utils::unzip(temp1, files = "NUTS_AT_2021.csv", exdir = temp2)
  nuts <- utils::read.csv(file, encoding = "UTF-8", stringsAsFactors = FALSE)
  unlink(c(temp1, temp2), recursive = TRUE)

  # create tables for nuts levels
  nuts_lvl3 <- nuts %>%
    filter(CNTR_CODE %in% c("CH", "AT", "DE", "FR", "IT", "LI"), nchar(NUTS_ID) == 5) %>%
    arrange(CNTR_CODE) %>%
    select(NUTS_ID, NUTS_NAME) %>%
    mutate(lvl2 = substr(NUTS_ID, 1, 4)) %>%
    rename(lvl3_name = NUTS_NAME, lvl3 = NUTS_ID)
  nuts_lvl2 <- nuts %>%
    filter(CNTR_CODE %in% c("CH", "AT", "DE", "FR", "IT", "LI"), nchar(NUTS_ID) == 4) %>%
    select(NUTS_ID, NUTS_NAME) %>%
    rename(lvl2 = NUTS_ID, lvl2_name = NUTS_NAME) %>%
    mutate(lvl1 = substr(lvl2, 1, 3))
  nuts_lvl1 <- nuts %>%
    filter(CNTR_CODE %in% c("CH", "AT", "DE", "FR", "IT", "LI"), nchar(NUTS_ID) == 3) %>%
    select(NUTS_ID, NUTS_NAME) %>%
    rename(lvl1 = NUTS_ID, lvl1_name = NUTS_NAME) %>%
    mutate(lvl0 = substr(lvl1, 1, 2))
  nuts_lvl0 <- nuts %>%
    filter(CNTR_CODE %in% c("CH", "AT", "DE", "FR", "IT", "LI"), nchar(NUTS_ID) == 2) %>%
    select(NUTS_ID, NUTS_NAME) %>%
    rename(lvl0 = NUTS_ID, lvl0_name = NUTS_NAME)

  # join tables
  nuts <- left_join(nuts_lvl3, nuts_lvl2, by = "lvl2") %>%
    left_join(., nuts_lvl1, by = "lvl1") %>%
    left_join(., nuts_lvl0, by = "lvl0")

  # throw out french oversea departments
  nuts <- nuts[-which(nuts$lvl1 == "FRY"), ]

  # create vector to only make one call to gsub()
  dims <- dim(nuts)
  df_vec <- do.call(c, nuts)

  # edit naming (replace apostrophe, dash, trailing whitespaces, some names)
  df_vec <- vapply(df_vec, function(x) gsub("\u2019", "'", x), character(1L))
  df_vec <- vapply(df_vec, function(x) gsub("\u2014", "-", x), character(1L))
  df_vec <- vapply(df_vec, function(x) gsub("\\s+$", "'", x), character(1L))
  df_vec <- vapply(df_vec, function(x) gsub("Schweiz/Suisse/Svizzera", "Schweiz", x),
                   character(1L))
  df_vec <- vapply(df_vec, function(x) gsub("Bourgogne-Franche-Comt\u00E9", "Bourgogne - Franche-Comt\u00E9", x),
                   character(1L))
  df_vec <- vapply(df_vec, function(x) gsub("Auvergne-Rh\u00F4ne-Alpes", "Auvergne - Rh\u00F4ne-Alpes", x),
                   character(1L))

  # reallocate vector entries to data frame
  for(i in 0:(dims[2]-1)){
    nuts[, i+1] <- df_vec[1:dims[1] + i*dims[1]]
  }

  # order this nicely
  nuts <- nuts %>% arrange(lvl3)

  # save data as .rds to cache_dir if user wants this
  filepath <- make_path(cache_dir, filename)
  saveRDS(object = nuts, file = filepath)

  return(nuts)
}

