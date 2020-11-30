
#' Aggregate pre-processed data to the desired NUTS level
#'
#' @param level The desired NUTS level to aggregate data to. Given as \code{numeric}.
#' @param ref The reference table for joining. This is the output of \code{\link[suspendr:nuts_table]{nuts_table()}}.
#' @param dat Object of class \code{data.frame}. The data set with country specific case data. Needs to include columns with the following
#' names:
#' \describe{
#'   \item{date}{A vector of class \code{Date} with the given dates.}
#'   \item{new_cases}{A vector of class \code{numeric}. Contains the new cases of a given day and region.}
#'   \item{cum_cases}{A vector of class \code{numeric}. Contains the cumulative cases of a given day and region.}
#'   \item{lvl_3}{A vector of class \code{character}. Contains the NUTS 3 code of the region.}
#'   \item{lvl_2}{A vector of class \code{character}. Contains the NUTS 2 code of the region.}
#'   \item{lvl_1}{A vector of class \code{character}. Contains the NUTS 1 code of the region.}
#'   \item{lvl_0}{A vector of class \code{character}. Contains the NUTS 0 code of the region.}
#' }
#' The order does not matter.
#' @return A \code{data.frame} with the country specific case data aggregated to the desired
#' NUTS level.
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise left_join select arrange sym
#' @importFrom rlang as_name
#'
aggregate_to_nuts <- function(level, ref, dat){

  # variables needed for aggregating data to desired nuts level
  if(level == 3){
    var <- "lvl3"
    idx <- c(1:2, 9)
    nam <- "lvl3_name"
  } else if (level == 2){
    var <- "lvl2"
    idx <- c(3:4, 9)
    nam <- "lvl2_name"
  } else if (level == 1){
    var <- "lvl1"
    idx <- c(5:6, 9)
    nam <- "lvl1_name"
  } else {
    var <- "lvl0"
    idx <- c(7:8, 9)
    nam <- "lvl0_name"
  }

  # get population to desired level
  ref <- ref[idx] %>%
    group_by(!!sym(var), !!sym(nam)) %>%
    summarise(population = sum(population), .groups = "drop")

  # aggregate case data to desired level
  dat <- dat %>%
    group_by(date, !!sym(var)) %>%
    summarise(new_cases = sum(new_cases),
              cum_cases = sum(cum_cases), .groups = "drop") %>%
    left_join(x = ., y = ref, by = rlang::as_name(var)) %>%
    select(date, !!sym(var), !!sym(nam), new_cases, cum_cases, population) %>%
    arrange(date, !!sym(var))

  return(dat)
}
