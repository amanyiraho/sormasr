library(testthat)
library(dplyr)

sormas_server <- Sormasr$new()

test_that("initialize function sets up request correctly to the live server", {
  expect_equal(sormas_server$request_sent$url, "https://sormas.org.ng/ncdc-sormas-cases-api/")
})


test_that("get_states returns the expected output",{

            states <- sormas_server$get_states(page = 0, size = 37)

            expect_equal(names(states), c("state_name", "state_id", "lga_id", "lga_name"))

            expect_equal(n_distinct(states$state_name), 37)

            })



test_that("get_disease_data returns the expected output",
          {
            disease_data <-  sormas_server$get_disease_data(disease = "Measles", stateId =  "102324",month ="MARCH",
                                   year = "2022", page = "0", size = "1")

            expect_equal(names(disease_data), c("state", "state_id" , "lga", "lga_id", "confirmed_cases"))


 })


test_that("get_age_sex returns the expected output",
          {
            age_sex_data <-  sormas_server$get_age_sex(disease = "Measles", stateId = "102324",month ="MARCH",vaccinationStatus = "true",
                                                       year = "2022", page = "0", size = "1")

            expect_equal(names(age_sex_data), c("state", "state_id" , "lga", "lga_id", "age_group", "vaccination_status", "confirmed_cases"))


          })
