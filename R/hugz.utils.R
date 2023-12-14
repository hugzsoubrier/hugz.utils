
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
#' @param x a data frame object.
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
#' @return formatted values using M for millions and K for thousands
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

#' get a vector of id from a df given condition.
#'
#' @param df a data frame.
#' @param ... a condition to filter a dataframe.
#' @param id_var specify id variable, default = pid.
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


# Write flags -------------------------------------------------------------

#' Title
#'
#' @param df a df to save to .xlsx
#' @param df_name a name for the main sheet
#' @param extra_sheet a df to be added as a second sheet
#' @param extra_sheet_name a name for the second sheet
#' @param file_path path to save the file
#' @param file_name file name using .xlsx
#'ss
#' @return
#' @export
#'
#' @examples
write_multi_xlsx <- function(
    df,
    df_name,
    extra_sheet,
    extra_sheet_name,
    file_path,
    file_name) {

  if(!grepl(".xlsx", file_name)) {stop("Only provide a .xlsx filename")}

  if(nrow(df)){

    xlsx <- openxlsx::createWorkbook()

    openxlsx::addWorksheet(xlsx, sheetName = df_name)
    openxlsx::writeData(xlsx, sheet = df_name, df)

    if(!missing(extra_sheet)){
      openxlsx::addWorksheet(xlsx, sheetName = extra_sheet_name)
      openxlsx::writeData(xlsx, sheet = extra_sheet_name, extra_sheet)
    }
    #save the workbook
    openxlsx::saveWorkbook(xlsx, fs::path(file_path, file_name), overwrite = TRUE )

    message(nrow(df), " records written in ", file_name)

  } else {

    message(" No records written in ", file_name)

  }

}



# pull_unique() -----------------------------------------------------------

# pulls unique values of a vector

pull_unique <- function(dat, x){

  values <- dat %>%
    distinct({{x}}) %>%

    pull({{x}})

  return(values)

}

# Ggplot theme ------------------------------------------------------------

my_theme <- function(base_size = 10) {

  hrbrthemes::theme_ipsum(base_size = base_size,
                          strip_text_size = 6,
                          axis_title_size = 10,
                          plot_margin = ggplot2::margin(10, 10, 10, 10),
                          plot_title_size = 10,
                          subtitle_size = 9,
                          subtitle_face = "italic",
                          axis_text_size = 10
  )

}

my_theme()
