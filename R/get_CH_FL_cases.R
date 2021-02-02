#' Get Swiss and Liechtensteinian COVID-19 case data from source or from cache.
#'
#' @param level A vector of class \code{numeric} and length 1. Indicates the desired NUTS level.
#' @param merge_li A binary indicator whether to merge Liechtenstein to the canton of St. Gallen on NUTS
#' level 3. This also implicates adding the population as well as cases to whichever region St. Gallen is in.
#' @param ref The reference table. The output of \code{\link[suspendr:nuts_table]{nuts_table()}}.
#' @template cache_dir
#'
#' @return A \code{data.frame} with a time series of COVID-19 cases in Switzerland/Liechtenstein
#' on the desired NUTS level.
#'
get_CH_FL_cases <- function(level, merge_li, ref, cache_dir = NULL){

  # set parameters for cacheing
  filename <- "CH_FL_cases.rds"
  cache_dir <- get_cache_dir(cache_dir)

  # make decision whether to get data from cached file or from source
  from_cache <- read_from_cache(cache_dir = cache_dir, filename = filename,
                                cutoff = 1, units = "days")

  # get pre-processed data from file or from source
  if(from_cache){
    dat <- readRDS(make_path(cache_dir, filename))
  } else {
    dat <- get_CH_FL_cases_from_source(cache_dir = cache_dir, filename = filename)
  }

  # in case of merging Liechtenstein to Switzerland:
  # add attribute merge_li such that aggregate_to_nuts() also adds the population of Liechtenstein
  # to the corresponding NUTS region of Switzerland (lvl3: St. Gallen, lvl2: Ostschweiz, lvl1 & lvl0: Schweiz)
  if(merge_li){
    row_idx_li <- which(dat$region == "FL")
    row_idx_sg <- which(dat$region == "SG")[1]
    replacement <- dat[row_idx_sg, 4:7]
    dat[row_idx_li, 4:7] <- replacement
    attr(dat, "merge_li") <- TRUE
  }

  # aggregate pre-processed data to desired NUTS level
  dat <- aggregate_to_nuts(level = level, ref = ref, dat = dat)

  test <- aggregate_to_nuts(level = level, ref = ref, dat = dat)

  return(dat)
}


#' Get Swiss and Liechtensteinian COVID-19 case data from source.
#'
#' @template cache_dir
#' @param filename The name of the file to cache without any preceding directories but with fileextension.
#' Must be given as a character string.
#'
#' @return A code \code{data.frame} containing the number of cases per canton and day as well as the respective
#' NUTS codes. Also contains data from Liechtenstein.
#' @importFrom openxlsx read.xlsx
#' @importFrom dplyr mutate filter rename select group_by summarise ungroup %>%
#'
get_CH_FL_cases_from_source <- function(cache_dir, filename){

  # path to file
  path <- "https://www.bag.admin.ch/dam/bag/en/dokumente/mt/k-und-i/aktuelle-ausbrueche-pandemien/2019-nCoV/covid-19-basisdaten-fallzahlen.xlsx.download.xlsx/Dashboards_1&2_COVID19_swiss_data_pv.xlsx"

  # import data
  ch <- openxlsx::read.xlsx(path)

  # pre-process data
  ch <- ch %>%
    mutate(fall_dt = as.Date(fall_dt)) %>%
    filter(!is.na(fall_dt)) %>%
    rename(date = fall_dt, region = ktn, new_cases = fallklasse_3) %>%
    select(date, region, new_cases) %>%
    group_by(date, region) %>%
    summarise(new_cases = sum(new_cases), .groups = "drop")

  # add NUTS level 3 info
  df <- data.frame("canton" = sort(unique(ch$region)),
                   "lvl3" = c("CH033", "CH054", "CH053", "CH021", "CH032", "CH031", "LI000", # last entry on this line is actually Liechtenstein
                              "CH022", "CH013", "CH051", "CH056", "CH025", "CH061", "CH024",
                              "CH065", "CH064", "CH055", "CH052", "CH023", "CH063", "CH057",
                              "CH070", "CH062", "CH011", "CH012", "CH066", "CH040"))

  # add other NUTS levels
  df <- df %>%
    mutate(lvl2 = substr(lvl3, 1, nchar(lvl3)-1),
           lvl1 = substr(lvl3, 1, nchar(lvl3)-2),
           lvl0 = substr(lvl3, 1, nchar(lvl3)-3))

  ch <- left_join(ch, df, by = c("region" = "canton")) %>% ungroup

  # save file to cache directory
  filepath <- make_path(dir_path = cache_dir, filename = filename)
  saveRDS(ch, filepath)

  return(ch)
}
