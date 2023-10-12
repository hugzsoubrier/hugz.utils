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
#' dat <- data.frame(
#' "pid" = c("xx-1", "xx-2", "xx-3"),
#' "age" = c(12, 20, 39)
#' )
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
