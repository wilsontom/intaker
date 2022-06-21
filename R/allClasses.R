#' Intake24 Class
#'
#' @description A S4 Class to store raw data from Intake24 Experiments
#'
#' @slot meta a list
#' @slot food a `tbl_df`
#' @slot nutrients a `tbl_df`
#'
#' @export

setClass(
  Class = 'Intake24',
  representation = representation(
    meta = 'list',
    food = 'tbl_df',
    nutrients = 'tbl_df'
  )
)

setOldClass(c('tbl_df', 'tbl', 'data.frame'))
