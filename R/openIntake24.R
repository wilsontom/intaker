#' Open Intake24 Experiment
#'
#' @param input a `tibble` of the raw Intake24 data export
#' @return an `Intake24` S4 object
#'
#' @importFrom magrittr %>%
#' @export

openIntake24 <- function(input)
{
  object <- methods::new('Intake24')

  raw_names <- tibble::tibble(Original = names(input))

  ref_index <- intaker::intake24_index

  reformat_index <-
    raw_names %>% dplyr::left_join(., ref_index, by = 'Original')

  names(input) <- reformat_index$New

  remove_ids <-
    ref_index %>% dplyr::filter(Type == 'REMOVE') %>% dplyr::select(New) %>% dplyr::pull()

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
    user_meta %>% dplyr::ungroup() %>% dplyr::select(SurveyID,UserID,CompletionTime) %>% dplyr::distinct()



  object@meta <-
    list(User = SurverysPerUser,
         Survery = ItemsPerMeal,
         CompletionTime = time_to_complete)


  NutrientID <-
    intake24_index %>% dplyr::filter(Type == 'NUTRIENTS') %>% dplyr::select(New) %>% dplyr::pull()

  object@nutrients <-
    input_clean %>% dplyr::select(RecordID,
                                  SurveyID,
                                  UserID,
                                  StartDate,
                                  MealName,
                                  dplyr::all_of(NutrientID)) %>%
    tibble::as_tibble()


  FoodID <-
    intake24_index %>% dplyr::filter(Type == 'FOOD') %>% dplyr::select(New) %>% dplyr::pull()



  object@food <-
    input_clean %>% dplyr::select(RecordID,
                                  SurveyID,
                                  UserID,
                                  StartDate,
                                  dplyr::all_of(FoodID)) %>% tibble::as_tibble()


  return(object)

}
