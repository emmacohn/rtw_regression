policy_rtw <- read.csv("./input/policy_rtw_status.csv")

## weighted averages of various policies by rtw status
policy_outcomes <- policy_rtw |> 
  summarize(medicaid = weighted.mean(medicaid_expansion, w = population),
            medicaid_raw = mean(medicaid_expansion),
           .by = rtw_status)

#function to do this for all the variables 
policy_fun <- function(x) {
  policy_rtw |>
    summarise("{x}_wgt" := weighted.mean(.data[[x]], w = population),
              "{x}_raw" := mean(.data[[x]]),
              .by = rtw_status)
}

# just population weighted numbers
policy_table <- map(.x = c("medicaid", "ui_rate", "uninsured", "perpupil_spend", "fed_min", "min_wage", "change_2009"), .f = ~ policy_fun(.x)) |>
  reduce(bind_rows) |>
  pivot_longer(cols = -rtw_status, names_to = "policy", values_to = "value", values_drop_na = TRUE) |>
  filter(!str_detect(policy, "_raw$")) |>
  mutate(policy = str_remove(policy, "_wgt$")) |>
  pivot_wider(names_from = rtw_status, values_from = value) |>
  rename(non_rtw = `0`, rtw = `1`)

# just unweighted numbers
policy_table2 <- map(.x = c("medicaid", "ui_rate", "uninsured", "perpupil_spend", "fed_min", "min_wage", "change_2009"), .f = ~ policy_fun(.x)) |>
  reduce(bind_rows) |>
  pivot_longer(cols = -rtw_status, names_to = "policy", values_to = "value", values_drop_na = TRUE) |>
  filter(!str_detect(policy, "_wgt$")) |>
  mutate(policy = str_remove(policy, "_raw$")) |>
  pivot_wider(names_from = rtw_status, values_from = value) |>
  rename(non_rtw = `0`, rtw = `1`)
