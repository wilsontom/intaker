#' Open Intake24 Experiment
#'
#' @param input a `tibble` of the raw Intake24 data export
#' @return a `Intake24` S4 object
#'
#' @export

openIntake24 <- function(input)
{
  object <- methods::new('Intake24')

  names(input) <- INDEX$NEW_NAME

  remove_ids <-
    INDEX %>% dplyr::filter(INDEX == 'REMOVE') %>% dplyr::select(NEW_NAME) %>% dplyr::pull()

  RowHash <-
    paste0(input$SurveyID, '_', 1:nrow(input)) %>% openssl::md5()

  input_clean <-
    input %>% dplyr::select(-remove_ids) %>% tibble::add_column('RecordID' = RowHash, .before = 'SurveyID')

  input_clean <-
    input_clean %>% dplyr::mutate(
      StartDate = lubridate::ymd(as.Date(StartTime)),
      StartTime =  as.POSIXct(StartTime) %>% format(., format = '%H:%M:%S'),
      EndDate = lubridate::ymd(as.Date(SubmissionTime)),
      EndTime =  as.POSIXct(SubmissionTime) %>% format(., format = '%H:%M:%S'),
      CompletionTime =  gsub('min', '', CompletionTime) %>% as.numeric()
    ) %>% dplyr::select(-SubmissionTime)


  user_meta <- input_clean %>% dplyr::select(
      SurveyID,
      UserID,
      StartDate,
      StartTime,
      EndDate,
      EndTime,
      CompletionTime,
      SearchTerm,
      MealName
    ) %>%
    dplyr::group_by(UserID)


  user_split <- user_meta %>% dplyr::group_split()


  UserSummary <- purrr::map(user_split, ~ {
    dplyr::group_by(., SurveyID, UserID) %>%
      dplyr::count()
  })


  ItemsPerSurvey <-
    user_meta %>% dplyr::group_by(SurveyID, UserID) %>% dplyr::count() %>%
    dplyr::summarise(value = sum(n)) %>% dplyr::group_by(UserID) %>% dplyr::summarise(AverageItems = mean(value)) %>% dplyr::ungroup()

  ItemsPerMeal <-
    user_meta %>% dplyr::group_by(SurveyID, UserID, MealName) %>% dplyr::count() %>%
    dplyr::group_by(MealName, UserID) %>% dplyr::summarise(AverageItems = mean(n)) %>% dplyr::ungroup()


  TimePerSurvey <-
    user_meta %>% dplyr::select(SurveyID, UserID, CompletionTime) %>% dplyr::distinct() %>%
    dplyr::summarise(AverageTime = mean(CompletionTime))  %>% dplyr::ungroup()


  UserOverview <-
    dplyr::full_join(ItemsPerSurvey, TimePerSurvey, by = 'UserID')



  object@meta <- list(User = UserOverview, Survery = ItemsPerMeal)



  NutrientID <-
    INDEX %>% dplyr::filter(INDEX == 'NUTRIENTS') %>% dplyr::select(NEW_NAME) %>% dplyr::pull()

  object@nutrients <-
    input_clean %>% dplyr::select(RecordID, SurveyID, UserID, StartDate, MealName, NutrientID) %>%
    tibble::as_tibble()


  object@food <-
    input_clean %>% dplyr::select(
      RecordID,
      SurveyID,
      UserID,
      StartDate,
      MealID,
      MealName,
      FoodID,
      SearchTerm,
      FoodCode,
      DescriptionEN,
      DescriptionLocal,
      FoodGroupCode,
      FoodGroupEN,
      FoodGroupLocal,
      ServingSize,
      PortionSize
    ) %>% tibble::as_tibble()



  return(object)

}
