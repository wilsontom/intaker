#' @rdname filterSurvey

setGeneric(
  name = 'filterSurvey',
  def = function(object,
                 energy_range = c(400, 4000),
                 min_time = 2)
  {
    standardGeneric('filterSurvey')
  }
)
