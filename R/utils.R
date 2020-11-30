#' Get cache directory
#' @description Handles cache directory for data files used within the \code{suspendr} package.
#' @template cache_dir
#' @return The path to the cache directory. Object of class \code{character}.
#'
get_cache_dir <- function(cache_dir){
  if(!is.null(cache_dir)){
    return(cache_dir)
  } else if(!is.null(getOption("suspendr_cache_dir"))){
    return(getOption("suspendr_cache_dir"))
  } else {
    return(tempdir())
  }
}


#' Check whether a file is older than a defined cutoff
#' @description Returns \code{TRUE} if a file should be updated. Mainly to decide whether to
#' ignore cached versions of the file.
#' @param filepath Object of class \code{character}. Indicates the path to the file to check.
#' @param cutoff Object of class \code{numeric}. How many days/hours/minutes etc. we accept the
#' file to be old.
#' @param units Object of class \code{character}. Unit of \code{cutoff}. One of the possibilities
#' listed in \code{\link[base:difftime]{base::difftime()}}.
#' @return \code{TRUE} if the file is older than the cutoff and \code{FALSE} otherwise.
#' Object of class \code{logical}.
#'
needs_update <- function(filepath, cutoff, units){
  age <- as.numeric(difftime(Sys.time(), file.info(filepath)$mtime, units = units))
  if(age > cutoff){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' Construct path to a file from input directory and a file name
#' @description This function constructs valid paths from a
#' path to a directory and the file name.
#' @param dir_path a string determining the path to the file directory.
#' @param filename a string determining the file name
#' @return Path to file for further use. Object of class \code{character}.
#'
make_path <- function(dir_path, filename){
  ifelse(endsWith(dir_path, "/"),
         out <- paste0(dir_path, filename),
         out <- paste0(dir_path, "/", filename))
}


#' Read file from cache directory or get data from source
#' @description Returns a decision whether to get data from a cached file (\code{TRUE}) or
#' from source (\code{FALSE}). If \code{cache_dir} does not exist yet, the function also creates
#' the directory.
#' @template cache_dir
#' @param filename Object of class \code{character}. Name of the cached file.
#' @param cutoff Object of class \code{numeric}. How many days/hours/minutes etc. we accept the
#' file to be old.
#' @param units Object of class \code{character}. Unit of \code{cutoff}. One of the possibilities
#' listed in \code{\link[base:difftime]{difftime()}}.
#' @return Object of class \code{logical}. Returns \code{TRUE} if the cached file exists and is
#' not older than \code{cutoff} in the specified \code{units}. Otherwise returns \code{FALSE}.
#'
read_from_cache <- function(cache_dir, filename, cutoff, units){
  filepath <- make_path(dir_path = cache_dir, filename = filename)
  # check whether cache directory exists, if not create it and return false
  if(!dir.exists(cache_dir)){
    dir.create(cache_dir, recursive = TRUE)
    cat("Created the following cache directory:\n", normalizePath(cache_dir))
    return(FALSE)
  }
  # if cached directory exists but the cache file does not, return false
  if(!file.exists(filepath)) return(FALSE)
  if(needs_update(filepath = filepath, cutoff = cutoff, units = units)){
    return(FALSE)
  } else {
    return(TRUE)
  }
}



# Handle global variables for R CMD check ----

utils::globalVariables(c("CNTR_CODE", "NUTS_ID", "NUTS_NAME", ".", "lvl3", "lvl3_name", "lvl2",
                         "lvl2_name", "lvl1", "lvl1_name", "lvl0", "lvl0_name", "age", "geo",
                         "nuts_id", "population", "sex", "time", "values", ":=", "Bezirk",
                         "AnzahlFaelle", "AnzahlFaelleSum", "new_cases", "cum_cases"))
