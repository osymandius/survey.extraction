#' Create individual ID for DHS surveys
#' @param cluster Cluster ID
#' @param household Household ID
#' @param line Line number
#' @export

dhs_individual_id <- function(cluster, household, line) {
  sprintf("%4d%4d%3d", cluster, household, line)
}

#' Convert DHS country code to ISO3C code
#' @param dhscc Two letter DHS country code
#' @export

dhscc_to_iso3 <- function(dhscc) {
  dhsc <- rdhs::dhs_countries()
  dictionary <- setNames(dhsc$ISO3_CountryCode, dhsc$DHS_CountryCode)
  val <- dplyr::recode(dhscc, !!!dictionary)
  
  if (any(!val %in% dictionary)) {
    stop(
      "DHS Country Code not found: ", 
      paste(val[!val %in% dictionary], collapse = ", ")
    )
  }
  val
}