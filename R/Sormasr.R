
#' Connect and pull/get data from a SORMAS
#' @format An R6 class called Sormasr.
#' @description
#' To create a SORMAS connection, you need a SORMAS base URL, username, password, and an API version
#' The R6 Class called `Sormasr` representing a SORMAS instance connection
#'
#' @details
#' You can use a SORMAS connection to get data several times without needing to manually supply your user credentials on each API call.
#'
#' @import httr2 R6 curl
#' @importFrom dplyr  select filter  arrange
#' @importFrom tidyr unnest
#' @export
#'
#'
Sormasr <- R6::R6Class(

  "Sormasr",
  public = list(


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @field request_sent The request used to perform an API call
    #' @field name Name of the user
    #'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     request_sent = NULL,
      name = NULL,
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description
    #' Initialize the Sormas API request
    #'
    #' This function initializes the Sormas API request by setting the base URL, request headers,
    #' and user agent.
    #'
    #' @importFrom httr2 req_headers req_retry req_user_agent
    #'
    #' @export
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function() {

      # base_url
      # username
      # password
      # api_version = NULL

      #args <- list(base_url = base_url, username = username, password = password)
      #Check that at least one argument is not null

      #attempt::stop_if_any(args, is.null,"You need to specify all arguements")
      #attempt::stop_if_none(args, is.character, "All arguements should be type character")

      self$request_sent <- request(base_url = "https://sormas.org.ng/ncdc-sormas-cases-api/") |>
        # req_url_path_append(api_version) |>
        # req_url_path_append("api") |>
        # req_auth_basic(username = username, password = password ) |>
        # req_url_query(paging = "false") |>
        req_headers("Accept" = "application/json") |>
        httr2::req_user_agent("Sormasr (https://www.amanyiraho.com/sormasr/") |>
        httr2::req_retry(max_tries = 5)

    },

    #' @description
    #' Get states information from remote API
    #'
    #' @param page  the page number of the states to retrieve.
    #' @param size  the limit of the number of states to retrieve.
    #' @return      a data frame with state and LGA information.
    #'
    #' @export
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_states = function(page, size = 37) {

      # Check for internet
      check_internet()

        response_object <- self$request_sent |>
          req_url_path_append("/lookup/getStatesInfo") |>
          req_url_query(page = page) |>
          req_url_query(size = size) |>
          req_perform()

        print(response_object$url)

        response_data  <-  response_object |>
          resp_body_json(simplifyVector = TRUE)

        #response_data

        response_data[[1]] |>
          unnest(lgas, names_repair = "check_unique") |>
          setNames(c("state_name", "state_id", "lga_id", "lga_name" ))

    },
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    #' @description
    #' Get disease data from remote API
    #'
    #' @param disease  the type of disease to retrieve data for. Choose from "COVID", "CSM", "Cholera",
    #'                  "Lassa", "Measles", "MonkeyPox", and "YellowFever".
    #' @param stateId  the ID of the state to retrieve disease data for.
    #' @param month    the name of the month to retrieve disease data for. Choose from "JANUARY", "FEBRUARY",
    #'                  "MARCH", "APRIL", "MAY", "JUNE", "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER",
    #'                  and "DECEMBER".
    #' @param year     the year to retrieve disease data for.
    #' @param page     (optional) the page number of the results to retrieve. Default is "0".
    #' @param size     (optional) the size of each page of results. Default is "1".
    #' @return         a tibble with disease data for the specified parameters.
    #' @export
    #'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    get_disease_data = function(disease = c("COVID", "CSM", "Cholera", "Lassa", "Measles", "MonkeyPox", "YellowFever"),
                                stateId , month = c("JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER"),
                                year, page = "0", size = "1") {
      # Check for internet
      check_internet()
      args <- list(disease  = disease, stateId = stateId , year = year)
      #Check that at least one argument is not null

      attempt::stop_if_any(args, is.null,"You need to specify all arguements")
      attempt::stop_if_none(args, is.character, "All arguements should be type character")

      disease <- match.arg(disease)

      response_object <- self$request_sent |>
        req_url_path_append("api/getDiseaseData") |>
        req_url_query(diseaseType = disease) |>
        req_url_query(stateId = stateId) |>
        req_url_query(year = year) |>
        req_url_query(month = month) |>
        req_url_query(page = page) |>
        req_url_query(size = size) |>
        req_perform()

      print(response_object$url)

      response_data  <-  response_object |>
        resp_body_json(simplifyVector = TRUE, flatten = TRUE)

      tibble(response_data$data$casesData$data) |> unnest(`response_data$data$casesData$data`) |>  unnest(lgaBreakDown) |> unnest(epiBreakDown ) |>
        select("stateName", "stateCode",  "lgaName",  "lgaCode", "epiData.totalNoOfConfirmedCases") |>
        setNames(c( "state", "state_id", "lga", "lga_id", "confirmed_cases")) |>
        arrange(desc(confirmed_cases))


    },

    #' @description
    #' Fetches age and sex data for specified disease
    #'
    #' @param disease A character string specifying the disease. Defaults to "COVID".
    #' @param vaccinationStatus A character string specifying whether to include vaccination status information. Defaults to "true".
    #' @param stateId A character string specifying the state ID for which to fetch data.
    #' @param month A character string specifying the month for which to fetch data. Defaults to all months.
    #' @param year A character string specifying the year for which to fetch data.
    #' @param page A character string specifying the page number of results to fetch. Defaults to "0".
    #' @param size A character string specifying the number of results to fetch per page. Defaults to "1".
    #'
    #' @return A tibble data frame with age and sex data for the specified disease .
    #'
    #' @importFrom tidyr pivot_longer
    #' @importFrom dplyr filter arrange select
    #' @importFrom purrr map_lgl
    #' @importFrom tibble tibble
    #' @import attempt
    #'
    #' @export
    #'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    get_age_sex = function(disease = c("COVID", "CSM", "Cholera", "Lassa", "Measles", "MonkeyPox", "YellowFever"),
                                vaccinationStatus = "true",
                                stateId , month = c("JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE", "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER"),
                                year, page = "0", size = "1") {
      # Check for internet
      check_internet()
      args <- list(disease  = disease, stateId = stateId , year = year)
      #Check that at least one argument is not null

      attempt::stop_if_any(args, is.null,"You need to specify all arguements")
      attempt::stop_if_none(args, is.character, "All arguements should be type character")

      disease <- match.arg(disease)

      response_object <- self$request_sent |>
        req_url_path_append("age-sex") |>
        req_url_query(diseaseType = disease) |>
        req_url_query(stateId = stateId) |>
        req_url_query(year = year) |>
        req_url_query(month = month) |>
        req_url_query(page = page) |>
        req_url_query(size = size) |>
        req_url_query(getVaccinationStatus =  vaccinationStatus)|>
        req_perform()

      print(response_object$url)

      response_data  <-  response_object |>
        resp_body_json(simplifyVector = TRUE, flatten = TRUE)

      tibble(response_data$data$data) |> unnest(cols = c(lgas)) |> unnest(cols = c(epiBreakdown)) |> unnest(ageBreakdown) |>
        select(-c("epiWeek", "ageGroupCode",   "ageBreakdown")) |>
        setNames(c( "state", "state_id", "lga", "lga_id", "age_group", "unvaccinated", "vaccinated", "unknown" , "uncategorised")) |>
        filter(age_group %in% c("0 - 9", "9 - 59", "60 - 180", "180 - 1600")) |>
        tidyr::pivot_longer(cols = c("unvaccinated", "vaccinated", "unknown", "uncategorised"), values_to = "confirmed_cases", names_to = "vaccination_status") |>
        arrange(desc(confirmed_cases))

    }


  )
)

