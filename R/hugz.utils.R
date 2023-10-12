
# Path functions ----------------------------------------------------------

#' List groups in MSF onedrive.
#'
#' @return
#' @export
#'
#' @examples
list_msf_onedrive <- function(){

  #list all directories in the MSF drive
  dir <- stringr::str_remove(as.character(fs::dir_ls("~/MSF/")), "(.*)/")

  ls_dir <- split(dir, x = dir)

  get("ls_dir")
}

#' Construct path to MSF onedrive.
#'
#' @param project specify the projects from MSF one drive.
#' @param path lower level path to fetch from.
#'
#' @return
#' @export
#'
#' @examples
get_msf_onedrive <- function(project = NULL, path = NULL ) {

  fs::path("~/MSF/", project, path)
}

# Formatting functions -----------------------------------------------------------

#' Inline format of counts with proportion.
#'
#' @param x a dataframe object.
#' @param ... a condition for filtering x.
#'
#' @return inline counts and proportions for condition.
#' @export
#'
#' @examples
fmt_count_prop <- function (x, ...) {

  stopifnot(is.data.frame(x))
  f <- dplyr::filter(dplyr::as_tibble(x), ...)
  f <- dplyr::count(f)
  prop <- f$n/nrow(x)
  sprintf("%d (%s)", f$n, scales::percent(prop, accuracy = 0.1))
}


#' Format counts using K and M symbols.
#'
#' @param n a numeric vector.
#'
#' @return
#' @export
#'
#' @examples
fmt_n <- function(n) {

  if (is.na(n)) {
    out <- "(Unknown)"
    return(out)

  } else if (n < 1000) {
    out <-  n

    return(out)

  } else {
    out <- scales::number(
      n,
      accuracy = .1,
      scale_cut = c(0, K = 1e3, M = 1e6))

      return( stringr::str_remove(out, "\\.0"))
  }
}
#fmt_n <- Vectorize(fmt_n)

# Epi functions -----------------------------------------------------------


#' Time Interval between two dates.
#'
#' @param x1 first date vector.
#' @param x2 second date vector.
#' @param unit units of time for interval. One of "days", "weeks", "months", "years".
#'
#' @return date interval using {lubridate}.
#' @export
#'
#' @examples
get_interval <- function(x1, x2, unit) {

  #check arguments and dates
  unit <- match.arg(unit, c("days", "weeks", "months", "years"))
  stopifnot("x1 and x2 must be dates" = lubridate::is.Date(x1) & lubridate::is.Date(x2))

  lubridate::time_length(lubridate::interval(x1, x2), unit)
}

#' Get epiweeks.
#'
#' @param x a date vector.
#'
#' @return a character vector of formatted epiweeks.
#' @export
#'
#' @examples
get_epiweek <- function(x) {
  year <- lubridate::isoyear(x)
  week <- lubridate::isoweek(x)
  paste0(year, "-S",week)
}

#' get a vector of id from df given condition.
#'
#' @param df a dataframe.
#' @param ... a condition to filter a dataframe.
#' @param id_var specifiy id variable, default = pid.
#'
#' @return a character vector of unique id_var after filtering for condition.
#' @export
#'
#' @examples
#'
#' dat <- data.frame( "pid" = c("xx-1", "xx-2", "xx-3"), "age" = c(12, 20, 39) )
#'
#' get_id_vec(dat, age < 30)
get_id_vec <- function(df, ..., id_var = "pid" ){

  id_var <- rlang::sym(id_var)

  if(missing(...)){

    message(paste0("returning all unique ",  substitute(id_var) , " of ", deparse(substitute(df)) ))

    id <- unique(dplyr::pull( dplyr::filter(df, !is.na(!!id_var)), !!id_var ))

  } else {

    fil_df <- dplyr::filter(df, !is.na(!!id_var), ... )
    id <-  unique(dplyr::pull(fil_df, !!id_var))
  }
  return(id)
}
