#' Open Intake24 Experiment
#'
#' @param input a `tibble` of the raw Intake24 data export
#' @return a `Intake24` S4 object
#'
#' @importFrom magrittr %>%
#' @export

openIntake24 <- function(input)
{
  object <- methods::new('Intake24')

  names(input) <- column_index$NEW_NAME

  remove_ids <-
    column_index %>% dplyr::filter(INDEX == 'REMOVE') %>% dplyr::select(NEW_NAME) %>% dplyr::pull()

  RowHash <-
    paste0(input$SurveyID, '_', 1:nrow(input)) %>% openssl::md5()

  input_clean <-
    input %>% dplyr::select(-dplyr::all_of(remove_ids)) %>% tibble::add_column('RecordID' = RowHash, .before = 'SurveyID')

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
  )

  user_split <-
    user_meta %>% dplyr::group_by(UserID) %>% dplyr::group_split()


  UserSummary <- purrr::map(user_split, ~ {
    dplyr::group_by(., SurveyID, UserID) %>%
      dplyr::count()
  })


  ItemsPerSurvey <-
    user_meta %>% dplyr::group_by(SurveyID, UserID) %>% dplyr::count() %>%
    dplyr::summarise(value = sum(n), .groups = 'keep') %>% dplyr::group_by(UserID) %>% dplyr::summarise(AverageItems = mean(value)) %>% dplyr::ungroup()

  ItemsPerMeal <-
    user_meta %>% dplyr::group_by(SurveyID, UserID, MealName) %>% dplyr::count() %>%
    dplyr::group_by(MealName, UserID) %>% dplyr::summarise(AverageItems = mean(n), .groups = 'keep') %>% dplyr::ungroup()

  TimePerSurvey <-
    user_meta %>% dplyr::select(SurveyID,UserID, CompletionTime) %>% dplyr::distinct() %>%
    dplyr::group_by(UserID) %>%
    dplyr::summarise(AverageTime = mean(CompletionTime),
                     .groups = 'keep')  %>% dplyr::ungroup()


  UserOverview <-
    dplyr::full_join(ItemsPerSurvey, TimePerSurvey, by = 'UserID')


  UserIDX <-
    user_meta %>% dplyr::group_by(UserID) %>% dplyr::group_split() %>% purrr::map_chr(., ~ {
      .$UserID[1]
    })

  SurverysPerUser <-
    user_meta %>% dplyr::group_by(UserID) %>% dplyr::group_split() %>% purrr::map_dbl(., ~ {
      length(unique(.$SurveyID))
    }) %>% tibble::tibble(UserID = UserIDX, SurveyCount = .) %>% dplyr::left_join(UserOverview, ., by = 'UserID')


  time_to_complete <-
    user_meta %>% dplyr::ungroup() %>% dplyr::select(SurveyID, CompletionTime) %>% dplyr::distinct()



  object@meta <-
    list(User = SurverysPerUser,
         Survery = ItemsPerMeal,
         CompletionTime = time_to_complete)


  NutrientID <-
    column_index %>% dplyr::filter(INDEX == 'NUTRIENTS') %>% dplyr::select(NEW_NAME) %>% dplyr::pull()

  object@nutrients <-
    input_clean %>% dplyr::select(RecordID,
                                  SurveyID,
                                  UserID,
                                  StartDate,
                                  MealName,
                                  dplyr::all_of(NutrientID)) %>%
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
