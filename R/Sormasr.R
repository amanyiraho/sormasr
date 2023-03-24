#' Connect and pull/get data from a SORMAS
#' @format An R6 class called Sormasr.
#' @description
#' To create a SORMAS connection, you need a SORMAS base URL, username, password, and an API version
#' The R6 Class called `Sormasr` representing a SORMAS instance connection
#'
#' @details
#' You can use a SORMAS connection to get data several times without needing to manually supply your user credentials on each API call.
#'
#' @export
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
    #' Create a connection to SORMAS using basic authentication
    #' @param base_url Base url e.g https://
    #' @param username Registered username e.g
    #' @param password Registered password e.g
    #' @param api_version The api version e.g
    #' @return A new `Sormasr` object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(base_url , username ,  password , api_version = NULL) {

      #args <- list(base_url = base_url, username = username, password = password)
      #Check that at least one argument is not null

      #attempt::stop_if_any(args, is.null,"You need to specify all arguements")
      #attempt::stop_if_none(args, is.character, "All arguements should be type character")

      self$request_sent <- request(base_url = "http://staging.sormas.org.ng/ncdc-sormas-cases-api/") |>
        # req_url_path_append(api_version) |>
        # req_url_path_append("api") |>
        # req_auth_basic(username = username, password = password ) |>
        # req_url_query(paging = "false") |>
        req_headers("Accept" = "application/json") |>
        httr2::req_user_agent("Sormasr (http://www.amanyiraho.com/Sormasr/") |>
        httr2::req_retry(max_tries = 5)

    },

    #' @description
    #' Get states info from SORMAS
    #'
    #' @return A data frame
    #'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_states = function() {

      # Check for internet
      check_internet()

        response_object <- self$request_sent |>
          req_url_path_append("/lookup/getStatesInfo") |>
          req_perform()

        print(response_object$url)

        response_data  <-  response_object |>
          resp_body_json(simplifyVector = TRUE)

        response_data
        #tibble::tibble( response_data[[1]])
    },
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    #' @description Get disease from SORMAS
    #'
    #' @return A vector of all possible fields for a specific metadata
    #'
    #' @param disease  vector of disease from "COVID", "CSM", "Cholera", "Lassa", "Measles", "MonkeyPox", "YellowFever"
    #' @param stateId  vector of stateId
    #' @param week  vector of
    #' @param year vector of
    #'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    get_disease_data = function(disease = c("COVID", "CSM", "Cholera", "Lassa", "Measles", "MonkeyPox", "YellowFever"),
                                stateId , week , year ) {
      # Check for internet
      check_internet()
      args <- list(disease  = disease, stateId = stateId ,week = week, year = year)
      #Check that at least one argument is not null

      attempt::stop_if_any(args, is.null,"You need to specify all arguements")
      attempt::stop_if_none(args, is.character, "All arguements should be type character")

      disease <- match.arg(disease)

      response_object <- self$request_sent |>
        req_url_path_append("/api/getDiseaseData") |>
        req_url_query(diseaseType = disease) |>
        req_url_query(stateId = stateId) |>
        req_url_query(week = week) |>
        req_url_query(year = year) |>

        req_perform()

      print(response_object$url)

      # response_data  <-  response_object |>
      #   resp_body_json(simplifyVector = TRUE, flatten = TRUE)
      #
      # response_data

    }


  )
)

