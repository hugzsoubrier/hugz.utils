# ---------------------------
# Script name: hugz-utils.R
#
# Purpose of script: Hugz utils functions
#
# Author: Hugo Soubrier
#
# Date Created: 2023-10-12
#
# Copyright (c) Hugo SOUBRIER, 2023
# Email: soubrierhugo@gmail.com

# ---------------------------
# Notes:
#   
#
#
# ---------------------------

# Path functions ----------------------------------------------------------

#list all directories in the MSF one drive

list_msf_onedrive <- function(){
  
  #list all directories in the MSF drive
  dir <- str_remove(as.character(fs::dir_ls("~/MSF/")), "(.*)/")
  
  ls_dir <- split(dir, x = dir)
  
  get("ls_dir")
}


get_msf_onedrive <- function(project = NULL, path = NULL, ) {
  
  fs::path("~/MSF/", project, path)
  
}

# Formatting functions -----------------------------------------------------------

#format count with proportion inline
fmt_count_prop <- function (x, ...) {
  
  stopifnot(is.data.frame(x))
  f <- dplyr::filter(dplyr::as_tibble(x), ...)
  f <- dplyr::count(f)
  prop <- f$n/nrow(x)
  sprintf("%d (%s)", f$n, scales::percent(prop, accuracy = 0.1))
}

#formatting counts with K and M numbers

fmt_n <- function(n) {
  if (is.na(n)) {
    "(Unknown)"
  } else if (n < 1000) {
    n
  } else {
    scales::number(
      n,
      accuracy = .1,
      scale_cut = c(0, K = 1e3, M = 1e6)
    ) %>% str_remove("\\.0")
  }
}
fmt_n <- Vectorize(fmt_n)

# Epi functions -----------------------------------------------------------

#function for getting interval in days between two dates 
get_interval <- function(x1, x2, unit) {
  
  #check arguments and dates
  unit <- match.arg(unit, c("days", "weeks", "years"))
  stopifnot("x1 and x2 must be dates" = is.Date(x1) & is.Date(x2))
  
  time_length(interval(x1, x2), unit)
}

#get epiweek
get_epiweek <- function(x) {
  year <- lubridate::isoyear(x)
  week <- lubridate::isoweek(x)
  glue::glue("{year}-S{week}")
}

#get a vector of id by passing a df and a condition
get_id_vec <- function(df, ..., id_var = pid ){
  
  if(missing(...)){ 
    
    message(paste0("returning all unique ",  substitute(id_var) , " of ", deparse(substitute(df)) ))
    
    id <- unique(pull( df %>% drop_na({{id_var}}), {{id_var}} ))
    
  } else {
    
    id <-  unique(df %>%  drop_na({{id_var}}) %>% filter(... ) %>% pull({{id_var}}))
  }
  
  return(id)
}
