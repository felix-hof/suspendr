#' Get German case data from cache or from source
#'
#' @param level A vector of class \code{numeric} and length 1. Indicates the desired NUTS level.
#' @param ref The reference table. The output of \code{\link[suspendr:nuts_table]{nuts_table()}}.
#' @template cache_dir
#'
#' @return A \code{data.frame} with a time series of COVID-19 cases in Germany on the desired NUTS level.
#'
get_germany_cases <- function(level, ref, cache_dir = NULL){

  # set parameters for cacheing
  filename <- "german_cases.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 1, units = "days")

  # get pre-processed data from file or from source
  if(from_cache){
    dat <- readRDS(make_path(cache_dir, filename))
  } else {
    dat <- get_germany_cases_from_source(ref = ref, cache_dir = cache_dir, filename = filename)
  }

  # aggregate pre-processed data to desired NUTS level
  dat <- aggregate_to_nuts(level = level, ref = ref, dat = dat)

  return(dat)
}



#' Get German case data from RKI and pre-process it
#'
#' @param ref The reference table. The output of \code{\link[suspendr:nuts_table]{nuts_table()}}.
#' @template cache_dir
#' @param filename The name of the pre-processed data file that is save to \code{cache_dir}.
#'
#' @return A \code{data.frame} with pre-processed data originally from RKI
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise rename mutate arrange ungroup left_join select
#' @importFrom utils read.csv
#' @importFrom lubridate as_date ymd_hms
#'
get_germany_cases_from_source <- function(ref, cache_dir, filename){

  # set url
  url <- "https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data"

  # read data
  de_data <- read.csv(file = url, header = TRUE,
                      fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)

  # group by day and Landkreis and add up cases
  dat <- de_data[de_data$NeuerFall >= 0, ] %>%
    group_by(Meldedatum, Landkreis) %>%
    summarise(new_cases = sum(AnzahlFall), .groups = "drop") %>%
    ungroup() %>%
    rename(date = 1, lvl3_name = Landkreis) %>%
    mutate(date = as_date(ymd_hms(date)),
           lvl3_name = gsub("(^[SL]K) (.*)", "\\2 \\(\\1\\)", lvl3_name)) %>%
    arrange(date)

  # as dataset doesn't include any NUTS code, we need to join by Landkreis names
  # adjust naming of Landkreise from RKI dataset to make "joinable" with Eurostat data
  dat$lvl3_name[grepl("Berlin", dat$lvl3_name)] <- "Berlin"
  dat$lvl3_name <- gsub(" \\(LK\\)$", ", Landkreis", dat$lvl3_name)
  dat$lvl3_name <- gsub(" \\(SK\\)$", ", Kreisfreie Stadt", dat$lvl3_name)
  dat$lvl3_name[dat$lvl3_name == "Bitburg-Pr\u00fcm, Landkreis"] <- "Eifelkreis Bitburg-Pr\u00fcm"
  dat$lvl3_name[dat$lvl3_name == "Rostock, Landkreis"] <- "Landkreis Rostock"
  dat$lvl3_name[dat$lvl3_name == "Heilbronn, Kreisfreie Stadt"] <- "Heilbronn, Stadtkreis"
  dat$lvl3_name[dat$lvl3_name == "Karlsruhe, Kreisfreie Stadt"] <- "Karlsruhe, Stadtkreis"
  dat$lvl3_name[dat$lvl3_name == "Leipzig, Landkreis"] <- "Leipzig"
  dat$lvl3_name[dat$lvl3_name == "Sankt Wendel, Landkreis"] <- "St. Wendel"
  dat$lvl3_name[dat$lvl3_name == "Stadtverband Saarbr\u00fccken, Landkreis"] <- "Regionalverband Saarbr\u00fccken"
  dat$lvl3_name[dat$lvl3_name == "Saar-Pfalz-Kreis, Landkreis"] <- "Saarpfalz-Kreis"
  dat$lvl3_name[dat$lvl3_name == "G\u00e4ttingen (alt), Landkreis"] <- "G\u00e4ttingen, Landkreis"
  dat$lvl3_name[dat$lvl3_name == "Aachen, Landkreis"] <- "St\u00e4dteregion Aachen"
  dat$lvl3_name[dat$lvl3_name == "StadtRegion Aachen"] <- "St\u00e4dteregion Aachen"

  # edit reference table to only include german regions for next step
  ref <- ref[ref$lvl0 == "DE", ]

  # make reference table for join
  df <- cbind(arrange(ref[, 1:2], lvl3_name), Landkreis = sort(unique(dat$lvl3_name)))

  # insert days without cases to get complete time series, add cumulative cases,
  # add level c(2, 1, 0) NUTS codes
  ts <- seq(min(dat$date), max(dat$date), by = 1)
  dat <- expand.grid(date = ts, lvl3_name = unique(df$lvl3_name), stringsAsFactors = FALSE) %>%
    left_join(x = ., y = df, by = "lvl3_name") %>%
    left_join(x = ., y = dat, by = c("Landkreis" = "lvl3_name", "date" = "date")) %>%
    select(date, lvl3, lvl3_name, new_cases) %>%
    mutate(new_cases = ifelse(is.na(new_cases), 0, new_cases),
           lvl2 = substr(lvl3, 1, nchar(lvl3)-1),
           lvl1 = substr(lvl3, 1, nchar(lvl3)-2),
           lvl0 = substr(lvl3, 1, nchar(lvl3)-3)) %>%
    arrange(lvl3, date)

  # save this data to cache_dir
  path_to_file <- make_path(dir_path = cache_dir, filename = filename)
  saveRDS(dat, path_to_file)

  return(dat)
}

