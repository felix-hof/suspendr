#' Get population data from Eurostat (in general)
#'
#' @description This function gets data from Eurostat on NUTS-3 level and pre-processes it.
#' However, this data is from 31st of December 2019 and the NUTS-3 regions in Sardegna (Italy)
#' changed. Therefore the population number for regions ITG2, ITG2D, ITG2E, ITG2F, ITG2G and
#' ITG2H come from IStat (the Italian statistics office).
#' @param country Object of class \code{character}. One or more of
#' \code{c("at", "austria", "de", "germany", "fr", "france", "it", "italy", "ch",
#' "switzerland", "li", "liechtenstein", "all")}.
#' @param nuts Object of class \code{numeric} and length 1. Indicates the NUTS level
#' @template cache_dir
#' @return A \code{data.frame} containing three columns:
#' \describe{
#'   \item{nuts_id}{The NUTS IDs of the regions in the specified countries.}
#'   \item{time}{The most recent year which data is available for. Reference date is
#'   the 31st of December.}
#'   \item{population}{The number of inhabitants of the respective NUTS region.}
#' }
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select rename arrange
#' @export
#'
#' @examples
#' \donttest{pop_data(country = c("at", "ch"), nuts = 2)}
#'
pop_data <- function(country = c("at", "austria", "de", "germany", "fr", "france",
                                 "it", "italy", "ch", "switzerland", "li", "liechtenstein",
                                 "all"),
                     nuts = 2,
                     cache_dir = NULL){

  ### identify what user wants ###

  # Return error if invalid nuts argument
  if(!(nuts %in% 0:3)) stop("Nuts level has to be in 0:3")

  # create data frame for regular expressions
  df <- data.frame(abbr = c("at", "de", "fr", "it", "ch", "li", "all"),
                   name = c("austria", "germany", "france", "italy",
                            "switzerland", "liechtenstein", "all"),
                   regex = c("^AT", "^DE", "^FR", "^IT", "^CH", "^LI", "^(FR|DE|CH|LI|IT|AT)"))

  # throw an error if country input invalid
  valid_inputs <- unname(do.call(c, df[, 1:2]))
  if(any(!(country %in% valid_inputs))) stop("Invalid country input.")

  # get regex according to country argument
  idx <- vapply(country, function(x) which(df == x, arr.ind = TRUE)[1,1], integer(1L))
  idx <- unique(idx)
  regex <- df$regex[idx]

  # get regular expression to filter countries we are interested in
  # this is only used if data is fetched from source
  regex_all <- df$regex[7]

  ### get the data ###

  # get path to cache directory
  cache_dir <- get_cache_dir(cache_dir)

  # set file name for caching
  filename <- "pop_data.rds"

  # check if raw data already exists in cache directory
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 7, units = "days")

  # construct path to file
  # if file has been cached before import it, otherwise get it from source
  path_to_file <- make_path(cache_dir, filename)

  # if raw data has been cached import it, otherwise get it from source
  if(from_cache){
    pop_raw <- readRDS(path_to_file)
  } else {
    pop_raw <- get_pop_data_from_source(path_to_file, regex_all)
  }

  ### process data according to what user wants ###

  # process raw data according to user specifications
  dat <- process_raw_pop_data(raw_data = pop_raw, nuts = nuts, regex = regex)

  return(dat)
}


# helper functions ----


#' Get population data from Eurostat (in general).
#'
#' @description This function gets data from Eurostat on NUTS-3 level and pre-processes it.
#' However, this data is from 31st of December 2019 and the NUTS-3 regions in Sardegna (Italy)
#' changed. Therefore the population number for regions ITG2, ITG2D, ITG2E, ITG2F, ITG2G and
#' ITG2H come from IStat (the Italian statistics office).
#' @param path_to_file Character string indicating where to save the pre-processed file.
#' @param regex_all Character string. Contains a regular expression to filter out countries
#' we are not interested in from the original data file from Eurostat.
#' @return Returns a \code{data.frame} containing
#' @importFrom restatapi get_eurostat_bulk
#' @importFrom magrittr %>%
#' @importFrom dplyr rename filter select
#'
get_pop_data_from_source <- function(path_to_file, regex_all){

  # get data from source
  dat <- get_eurostat_bulk("demo_r_pjangrp3", cache = FALSE, stringsAsFactors = FALSE)

  # preprocess it
  dat[["values"]] <- as.numeric(dat[["values"]])
  dat[["time"]] <- as.numeric(dat[["time"]])
  year <- max(dat$time)
  dat <- dat %>%
    rename(nuts_id = geo, population = values) %>%
    filter(sex == "T",
           grepl(regex_all, nuts_id),
           !grepl("^FRY", nuts_id), # remove French oversea departments
           !grepl("^ITG2", nuts_id), # remove Sardegna regions: see reason below
           age == "TOTAL",
           population != 0,
           time == year) %>%
    select(nuts_id, time, population)

  # Italy has seen some changes in NUTS classification
  # therefore adjust manually
  # Data taken from: I.Stat
  # Link: http://dati.istat.it/?lang=en#
  # Dataset name: Resident population on 1st January
  # Year considered: 2020
  # Accessed: 2020-11-28
  # Data:
  # ITG2: Sardegna : 1630474
  # ITG2D: Sassari: 489634
  # ITG2E: Nuoro : 206843
  # ITG2F: Cagliari : 430914
  # ITG2G: Oristano : 156078
  # ITG2H: Sud Sardegna : 347005

  sard <- data.frame(nuts_id = c("ITG2", "ITG2D", "ITG2E", "ITG2F", "ITG2G", "ITG2H"),
                     time = 2020,
                     population = c(1630474, 489634, 206843, 430914, 156078, 347005))
  dat <- rbind(dat, sard)


  # save preprocessed file as rds to cache directory
  saveRDS(dat, path_to_file)

  return(dat)
}


#' Process raw population data
#'
#' @param raw_data The preprocessed data file. Basically this is the output from
#' \code{\link[suspendr:get_pop_data_from_source]{get_pop_data_from_source()}}, whether it comes from a cached file or from
#' the function itself.
#' @param nuts The nuts level specified in the main function.
#' @param regex The regular expressions corresponding to the countries specified in the main
#' function.
#' @return A \code{data.frame} containing three columns:
#' \describe{
#'   \item{nuts_id}{The NUTS IDs of the regions in the specified countries.}
#'   \item{time}{The most recent year which data is available for. Reference date is
#'   the 31st of December.}
#'   \item{population}{The number of inhabitants of the respective NUTS region.}
#' }
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select arrange rename
#'
process_raw_pop_data <- function(raw_data, nuts, regex){

  # if length(regex) > 1, combine regexes
  if(length(regex) > 1){
    regex <- paste0("(", paste0(regex, collapse = "|"), ")")
  }

  # get number of characters according to requested nuts level
  if(nuts == 3) {
    chars <- 5
    name <- "lvl3"
  } else if (nuts == 2) {
    chars <- 4
    name <- "lvl3"
  } else if (nuts == 1) {
    chars <- 3
    name <- "lvl1"
  } else {
    chars <- 2
    name <- "lvl0"
  }

  # process data according to user specifications
  df <- raw_data %>%
    filter(grepl(regex, nuts_id),
           nchar(nuts_id) == chars) %>%
    select(nuts_id, time, population) %>%
    arrange(nuts_id) %>%
    rename(!!name := nuts_id)

  return(df)
}
