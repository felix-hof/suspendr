#' Get IOM travel restriction matrix data
#'
#' @param countries A vector of class \code{character}. Valid inputs are ISO-3 country codes (case insensitive).
#' Defaults to all countries.
#' @param date A vector of class \code{Date}. Matrices are returned for each day in \code{dates} on which
#' such a matrix is available.
#' @template cache_dir
#'
#' @return A \code{list} containing processed IOM travel restriction matrices for days requested
#' in \code{date} if available. Rows refer to countries imposing restrictions to travellers/citizens from
#' countries in the columns.
#'
#' @export
#'
#' @examples
get_iom_matrix <- function(countries = NULL, date, cache_dir = NULL){

  # throw error if date is not of class "Date"
  if(class(date) != "Date") stop("'date' must be a vector of class 'Date'.")

  # get dates for which a matrix is available
  datepath <- "https://migration.iom.int/sites/all/themes/fmp/pages/heatmap/matrix.php"
  avail_dates <- get_iom_dates(datepath)

  # if there's no matrix for any of the dates supplied in 'date', throw error
  requested_avail <- which(avail_dates %in% date)
  if(length(requested_avail) == 0) stop("There is no travel restriction matrix available for any of the dates requested.")

  # print message for which dates there are no matrices
  if(length(requested_avail) != length(date)){
      message(paste0("No matrices available for the following dates: ",
                     paste0(date[-requested_avail], collapse = ", "), "."))
  }

  # check countries argument if it is not NULL
  if(!is.null(countries)){
    maxdate <- max(avail_dates)
    path_latest <- paste0("https://migration.iom.int/sites/all/themes/fmp/pages/heatmap/matrix.php?d=",
                          as.character(maxdate))
    avail_countries <- get_country_lookup(path_latest)[["iso_3"]]
    if(any(!(toupper(countries) %in% avail_countries))){
      not_found_idx <- which(!(toupper(countries) %in% avail_countries))
      not_found <- countries[not_found_idx]
      stop(paste0("The following ISO-3 country code was not found: ",
                  not_found))
    }
  }

  # set date to only the dates for which there are matrices available
  date <- sort(avail_dates[requested_avail])

  # check whether files can be loaded from cache directory or whether we need to get them from source
  filenames <- paste0("iom_trmat_", date, ".rds")
  cache_dir <- get_cache_dir(cache_dir)

  from_cache <- vapply(filenames, function(x){
    read_from_cache(cache_dir = cache_dir, filename = x,
                    cutoff = 30, units = "days")
  }, logical(1L))


  # construct paths needed as input for get_iom_matrix_raw
  paths <- vapply(date, function(x){
    paste0("https://migration.iom.int/sites/all/themes/fmp/pages/heatmap/js/heatmap_",
           as.character(x),
           ".js")
  }, character(1L))

  # if matrix for a day is in cache, load it from there
  # otherwise scrape from source
  mats <- lapply(seq_along(paths), function(x){
    if(from_cache[x]){
      mat <- readRDS(make_path(cache_dir, filenames[x]))
    } else {
      raw_mat <- get_iom_matrix_raw(paths[x])
      mat <- get_iom_matrix_color_code(raw_mat)
      saveRDS(mat, make_path(cache_dir, filenames[x]))
    }
    if(!is.null(countries)){
      idx <- which(rownames(mat) %in% countries)
      mat <- mat[idx, idx]
    }
    return(mat)
  })
  names(mats) <- as.character(date)

  # return
  return(mats)
}


#' Convert raw IOM travel matrix to matrix of color codes.
#'
#' @param mat_raw The output of \code{get_iom_matrix_raw}.
#'
#' @return A \code{data.frame} with recoded color codes of the IOM travel restriction matrix.
#'
get_iom_matrix_color_code <- function(mat_raw){
  # get rid of individual measure codes and convert to numeric
  out <- vapply(1:ncol(mat_raw), function(x){
    res <- gsub("^(\\d)\\-.+", "\\1", mat_raw[, x])
    return(as.numeric(res))
  }, numeric(nrow(mat_raw)))
  # recode entries
  # until now:
  # 0: green (No official restrictions reported)
  # 1: yellow (Partial restriction)
  # 2: red (Total restriction)
  # 3: blue (No restriction)
  idx1 <- which(out == 1, arr.ind = TRUE)
  idx2 <- which(out == 2, arr.ind = TRUE)
  idx3 <- which(out == 3, arr.ind = TRUE)
  # recode to:
  # 0: green (No official restrictions reported)
  # 1: blue (No restriction)
  # 2: yellow (Partial restriction)
  # 3: red (Total restriction)
  out[idx1] <- 2
  out[idx2] <- 3
  out[idx3] <- 1
  # return
  out <- as.data.frame(out)
  colnames(out) <- rownames(out) <- rownames(mat_raw)
  return(out)
}


#' Get raw IOM travel restriction matrix
#'
#' @param path The path to the javascript file where the matrix is defined.
#'
#' @return A data frame containing the level of restriction as well as the codes for concrete measures imposed.
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr %>%
#'
#'
get_iom_matrix_raw <- function(path){
  # read .js file
  lst <- suppressWarnings(readLines(path)) %>%
    # paste single lines together
    paste0(collapse = "") %>%
    # remove javascript variable declaration
    gsub("^var RestrictionMatrix \\=|\\;$", "", .) %>%
    # use JSON parser to read data
    jsonlite::fromJSON()
  # assemble data frame
  df <- do.call(rbind.data.frame, lst[2:length(lst)])
  rownames(df) <- colnames(df) <- lst[[1]]
  return(df)
}


#' Get the dates for which IOM travel restriction matrices are available
#'
#' @param path The path to the overview page which shows for what dates IOM travel matrices are available.
#'
#' @return A vector of class \code{Date} containing dates for which IOM travel matrices are available.
#' @importFrom rvest html_nodes html_text
#' @importFrom xml2 read_html
#'
get_iom_dates <- function(path){
  dates <- xml2::read_html(path) %>%
    rvest::html_nodes(".heatmap-index-item") %>%
    rvest::html_text() %>%
    strsplit(" ") %>%
    vapply(function(x){
      x[2] <- gsub("\\D", "", x[2])
      return(paste(x, collapse = " "))
    }, character(1L)) %>%
    as.Date(format = "%B %d %Y")
  return(dates)
}


# path needed as input for functions below
#path <- "https://migration.iom.int/sites/all/themes/fmp/pages/heatmap/matrix.php?d=2020-08-17"

# Get an overview of what the detailed codes (not the color codes) mean.
#
# @param path The path to one of the pages displaying a concrete IOM travel restriction matrix.
#
# @return A \code{data.frame} with restriction codes and their signification.
# @importFrom stringr str_remove_all str_split str_trim
# @importFrom dplyr %>%
#
#
#get_status_lookup <- function(path){
#  # read page content and get part where status codes are defined
#  lookup <- suppressWarnings(readLines(path)) %>%
#    paste(collapse = "") %>%
#    gsub(".+var rest.+tionTypes \\= \\{(.*?)\\}\\;.+", "\\1", .) %>%
#    # parse javascript dictionary
#    stringr::str_split('\\"\\s*,', simplify = TRUE) %>%
#    stringr::str_remove_all('"') %>%
#    stringr::str_split(":", simplify = TRUE)
#  # remove leading and trailing whitespaces
#  lookup <- vapply(1:ncol(lookup), function(x){
#    stringr::str_trim(lookup[, x], side = "both")
#  }, character(dim(lookup)[1]))
#  # return
#  out <- as.data.frame(lookup)
#  colnames(out) <- c("code", "meaning")
#  return(out)
#}

#' Get ISO-3 country codes used in the IOM travel restriction matrix
#'
#' @param path The path to one of the pages displaying a concrete IOM travel restriction matrix.
#'
#' @return A \code{data.frame} with ISO-3 country codes and the countries' full name.
#' @importFrom stringr str_remove_all str_split str_trim
#' @importFrom dplyr %>%
#'
#'
get_country_lookup <- function(path){
  lookup <- suppressWarnings(readLines(path)) %>%
    paste(collapse = "") %>%
    gsub(".+var countriesLookup \\= \\{(.*?)\\}\\;.+", "\\1", .) %>%
    # parse javascript dictionary
    stringr::str_split('\\"\\s*,', simplify = TRUE) %>%
    stringr::str_remove_all('"') %>%
    stringr::str_split(":", simplify = TRUE)
  # remove leading and trailing whitespaces
  lookup <- vapply(1:ncol(lookup), function(x){
    stringr::str_trim(lookup[, x], side = "both")
  }, character(dim(lookup)[1]))
  # return
  out <- as.data.frame(lookup)
  colnames(out) <- c("iso_3", "name")
  return(out)
}










