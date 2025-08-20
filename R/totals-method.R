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


            nutrients <- suppressWarnings(object@nutrients %>%
                                            dplyr::mutate_at(dplyr::vars(Water:TotalFS), as.numeric))


            survery_nutrient_totals <-
              nutrients %>% dplyr::group_by(SurveyID) %>%
              dplyr::summarise_if(is.numeric, sum, na.rm = TRUE)


            survery_meal_totals <-
              nutrients %>% dplyr::group_by(SurveyID, MealName) %>%
              dplyr::summarise_if(is.numeric, sum, na.rm = TRUE)


            foods <- suppressWarnings(object@food %>%
                                        dplyr::mutate_at(dplyr::vars(Fruit:Other_Cheese), as.numeric))









            return(list(survery_nutrient_totals, survery_meal_totals))


          })
