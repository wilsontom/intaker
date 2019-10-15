#' show-Intake24
#' @rdname show
#' @param object an `Intake24` object
#' @export

setMethod('show', signature = 'Intake24',
         function(object) {
           cat(cli::rule(
             left = crayon::bold('Intake24 Object'),
             right = paste0('tidyIntake24 v', packageVersion('tidyIntake24'))
           ), '\n')

           cat('Object Size:', format(utils::object.size(object), units = 'Mb'), '\n', '\n')
         })
