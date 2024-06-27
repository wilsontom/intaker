#' Totals
#'
#' Retrieve nutrient totals for daily intake and individual meals
#'
#' @rdname totals
#' @param object an `Intake24` object
#' @return an `Intake24` object
#' @export
#'

setMethod('totals',
          signature = 'Intake24',
          function(object)
          {

            mutate_idx <-
              object@nutrients %>% dplyr::select(-c(RecordID, SurveyID, UserID, StartDate, MealName)) %>% names()


            nutrients <- object@nutrients %>%
              suppressWarnings(dplyr::mutate_at(dplyr::all_of(mutate_idx), as.numeric))


            daily_totals <-
              nutrients %>% dplyr::group_by(SurveyID) %>%
              dplyr::summarise_if(is.numeric, sum, na.rm = TRUE)



            meal_totals <-
              nutrients %>% dplyr::group_by(SurveyID, MealName) %>%
              dplyr::summarise_if(is.numeric, sum, na.rm = TRUE)


            return(list(daily_totals, meal_totals))


          })
