#
# ###
# sormas_connection<- Sormasr$new()
# yyy <- sormas_connection$get_states(page = 0, size = 10)|>
#   count(state_name)
#
#
#
# ff <- sormas_connection$get_disease_data(disease = "Measles", stateId = "121877",month ="JANUARY",
#                                          year = "2022", page = "0", size = "10000")
#
# ttt <- tibble(ff$data$casesData$data) |> unnest(`ff$data$casesData$data`) |>  unnest(lgaBreakDown) |> unnest(epiBreakDown ) |>
#   select("stateName", "stateCode",  "lgaName",  "lgaCode", "epiData.totalNoOfConfirmedCases") |>
#   arrange(desc(`epiData.totalNoOfConfirmedCases`))
#
#
# f2 <- sormas_connection$get_age_sex(disease = "Measles", stateId = "121877",month ="JANUARY",vaccinationStatus = "true",
#                                          year = "2022", page = "0", size = "10000")
#
#
#
#
#
# tf <- tibble(f2$data$data) |> unnest(cols = c(lgas)) |> unnest(cols = c(epiBreakdown)) |> unnest(ageBreakdown) |>
#   select(-c("epiWeek", "ageGroupCode",   "ageBreakdown")) |>
#   setNames(c( "state", "state_id", "lga_name", "lga_id", "age_group", "unvaccinated", "vaccinated", "unknown" )) |>
#   filter(age_group %in% c("0 - 9", "9 - 59", "60 - 180", "180 - 1600")) |>
#   tidyr::pivot_longer(cols = c("unvaccinated", "vaccinated", "unknown"), values_to = "confirmed_cases", names_to = "vaccination_status") |>
#   group_by(state, state_id, lga_name, lga_id, age_group, vaccination_status) |>
#   summarise(across(c("confirmed_cases"), sum), .groups = "drop") |>
#   arrange(desc(confirmed_cases))
#
# sum(tf$confirmed_cases)
#
