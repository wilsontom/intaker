#' Filter Surveys
#'
#' @description Perform basic quality check on completed surveys by filtering out surveys which are extreme outlier's in total daily energy intake and have
#' been completed quickly (ie < 3mins)
#'
#' @rdname filterSurvey
#' @param object an `Intake24` object
#' @param energy_range a numeric vector of minimum and maximum total daily energy intake (KCals). Surveys with total daily energy intake outside of the specified range will be removed
#' @param min_time a numeric value of minimum time to filter out
#' @return an `Intake24` object
#' @export


setMethod('filterSurvey',
          signature = 'Intake24',
          function(object, energy_range, min_time)
          {
            survey_out_time <- object@meta$CompletionTime %>%
              dplyr::filter(CompletionTime <= !!min_time)

            message(
              crayon::yellow(
                'Removing',
                nrow(survey_out_time),
                'surveys with completion time <',
                min_time,
                'mins'
              )
            )

            object@food <- object@food %>%
              dplyr::filter(!SurveyID %in% survey_out_time$SurveyID)

            object@nutrients <- object@nutrients %>%
              dplyr::filter(!SurveyID %in% survey_out_time$SurveyID)


            Total_Daily_Energy <- object@nutrients %>%
              dplyr::select(SurveyID, EnergyKCAL)

            Total_Daily_Energy$EnergyKCAL <-
              stringr::str_replace_all(Total_Daily_Energy$EnergyKCAL, 'N/A', 'NA')

            Total_Daily_Energy$EnergyKCAL <-
              suppressWarnings(as.numeric(Total_Daily_Energy$EnergyKCAL))

            Total_Daily_Energy$EnergyKCAL[is.na(Total_Daily_Energy$EnergyKCAL)] <-
              0


            Total_Daily_Energy <-
              Total_Daily_Energy %>%
              dplyr::mutate(EnergyKCAL = as.numeric(EnergyKCAL)) %>%
              dplyr::group_by(SurveyID) %>%
              dplyr::summarise(TotalEnergy = sum(EnergyKCAL), .groups = 'keep')

            Total_Daily_Energy_Out <- Total_Daily_Energy %>%
              dplyr::filter(TotalEnergy < !!energy_range[1] |
                              TotalEnergy > !!energy_range[2])


            message(
              crayon::yellow(
                'Removing',
                nrow(Total_Daily_Energy_Out),
                'surveys with Total Daily Energy Intake <',
                energy_range[1],
                'or >',
                energy_range[2],
                'kcals'
              )
            )

            object@food <- object@food %>%
              dplyr::filter(!SurveyID %in% Total_Daily_Energy_Out$SurveyID)

            object@nutrients <- object@nutrients %>%
              dplyr::filter(!SurveyID %in% Total_Daily_Energy_Out$SurveyID)


            return(object)

          })
