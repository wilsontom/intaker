#' @rdname filter_survey

setGeneric(
  name = 'filter_survey',
  def = function(object,
                 energy_range = c(400, 4000),
                 min_time = 2)
  {
    standardGeneric('filter_survey')
  }
)


#' @rdname total

setGeneric(
  name = 'totals',
  def = function(object)
  {
    standardGeneric('totals')
  }
)
