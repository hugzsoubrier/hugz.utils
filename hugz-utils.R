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
