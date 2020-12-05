#' Get Austrian case data from AGES website or from cached file.
#'
#' @param level The desired NUTS-level. Given as object of class \code{numeric}.
#' One of \code{c(0, 1, 2, 3)}.
#' @param ref The reference table. The output of \code{\link[suspendr:nuts_table]{nuts_table()}}.
#' @template cache_dir
#' @return A \code{data.frame} with the country specific case data aggregated to the desired
#' NUTS level.
#'
get_austria_cases <- function(level, ref, cache_dir = NULL){

  # set parameters for cacheing
  filename <- "austrian_cases.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 1, units = "days")

  # get pre-processed data from file or from source
  if(from_cache){
    dat <- readRDS(make_path(cache_dir, filename))
  } else {
    dat <- get_austria_cases_from_source(cache_dir = cache_dir, filename = filename)
  }

  # aggregate pre-processed data to desired NUTS level
  dat <- aggregate_to_nuts(level = level, ref = ref, dat = dat)

  return(dat)
}

#' Get pre-processed Austrian COVID-19 case data from the AGES dashboard website.
#'
#' @description This function gets the Austrian case data from AGES. And returns
#' it aggregated to the desired NUTS level.
#' @param cache_dir The directory where to save the output of the function.
#' @param filename What the name of the file to cache is.
#' @return A \code{data.frame} containing Austrian case data.
#' @importFrom dplyr mutate select rename left_join
#' @importFrom utils download.file unzip read.csv
#'
get_austria_cases_from_source <- function(cache_dir, filename){

  # link to data set from AGES
  url <- "https://covid19-dashboard.ages.at/data/data.zip"

  # download zip archive, unzip it, read in data and delete archive
  temp <- tempfile(tmpdir = cache_dir)
  download.file(url = url, destfile = temp, exdir = cache_dir)
  p <- unzip(zipfile = temp, exdir = cache_dir, files = "CovidFaelle_Timeline_GKZ.csv")
  dat <- read.csv(p, header = TRUE, sep = ";")
  files_rm <- c(temp, make_path(cache_dir, "CovidFaelle_Timeline_GKZ.csv"))
  unlink(files_rm)

  # get date into the right format
  dat$date <- as.Date(gsub("^(\\d{2}).(\\d{2}).(\\d{4})(.+)", "\\3-\\2-\\1", dat$Time))

  # add nuts info
  nuts_info <- data.frame(bezirk = c(sort(unique(dat$Bezirk))),
                          lvl3 = c("AT121", "AT127", "AT341", "AT311", "AT341", "AT127",
                                   "AT223", "AT225", "AT342", "AT312", "AT112", "AT112",
                                   "AT342", "AT212", "AT313", "AT125", "AT124", "AT315",
                                   "AT221", "AT221", "AT311", "AT113", "AT323", "AT224",
                                   "AT212", "AT125", "AT124", "AT334", "AT332", "AT332",
                                   "AT113", "AT314", "AT335", "AT211", "AT211", "AT126",
                                   "AT124", "AT124", "AT335", "AT334", "AT225", "AT223",
                                   "AT333", "AT222", "AT122", "AT312", "AT312", "AT112",
                                   "AT121", "AT125", "AT127", "AT226", "AT226", "AT122",
                                   "AT112", "AT111", "AT113", "AT313", "AT331", "AT311",
                                   "AT313", "AT112", "AT323", "AT323", "AT322", "AT123",
                                   "AT123", "AT213", "AT311", "AT121", "AT335", "AT212",
                                   "AT314", "AT314", "AT224", "AT321", "AT126", "AT312",
                                   "AT211", "AT211", "AT315", "AT225", "AT213", "AT124",
                                   "AT121", "AT224", "AT312", "AT312", "AT130", "AT122",
                                   "AT122", "AT213", "AT322", "AT124"))

  # join nuts info to data frame
  dat <- dat %>%
    left_join(nuts_info, by = c("Bezirk" = "bezirk")) %>%
    mutate(lvl2 = substr(lvl3, 1, nchar(lvl3) - 1),
           lvl1 = substr(lvl3, 1, nchar(lvl3) - 2),
           lvl0 = substr(lvl3, 1, nchar(lvl3) - 3)) %>%
    select(date, Bezirk, AnzahlFaelle, lvl3, lvl2, lvl1, lvl0) %>%
    rename(bezirk = Bezirk,
           new_cases = AnzahlFaelle)

  # save cache file
  path_to_file <- make_path(cache_dir, filename)
  saveRDS(dat, path_to_file)

  return(dat)
}

