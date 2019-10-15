#' A S4 Class to store raw data from Intake24 Experiments
#'
#' @slot meta a list
#' @slot food a `tibble`
#' @slot nutrients `tibble`
#'
#' @export

setOldClass(c('tbl_df', 'tbl', 'data.frame'))

setClass(
  Class = 'Intake24',
  representation = representation(
    meta = 'list',
    food = 'tbl_df',
    nutrients = 'tbl_df'
  )
)
