### this is our final regression model ###
# it separates out the five switchers and runs an updated set of controls on two groups: 
## RTW always (pre 2011) and RTW since 2011 (the switchers, and MI)
## it also uses contemporary EPI AGE restrictions (16+ rather than 18-64),
## removes UNION as a control, adds in PUBSEC as a control, and clusters SEs at the state level
## finally, it codes missing METSTAT values as 2, which prevents R from dropping them 

df_f <- df |> filter(year %in% c(2010:2025)) |> 
  mutate(rtw_status = case_when(
             statefips == "IN" ~ 2,
             statefips == "WI" ~ 2,
             statefips == "WV" ~ 2,
             statefips == "KY" ~ 2,
             statefips == "MI" ~ 2,
             #statefips == "CO" ~ 1, 
             # ## we tested moving CO to RTW, but ultimately ruled it out. 
             # we may use this for robustness testing in the paper
             TRUE ~ rtw_status),
          metstat = case_when(
             is.na(metstat) ~ 2,          
             TRUE ~ metstat)
  )

#############################
## MODEL 1 (no controls) ###
#############################

### REGRESSION VARIABLES ####
#### Set treatment variables ####
treatment_vars <- "i(rtw_status, ref = '0')"
fe_vars <- "year"

#### Set fixed variables ####
fe_vars = paste("year", collapse=" + ")

#### Regression formula
regression_formula <- as.formula(paste(
  "lnwage ~", treatment_vars, "|", fe_vars
))

### OUTPUT ####
model1f <- df_f |>
  filter(year %in% c(2023:2025)) |> 
  (\(d) feols(regression_formula, data = d, weights = ~ wgt, vcov = ~ statefips))()

model1f_results <- broom::tidy(model1f) |>
  filter(term == "rtw_status::1") |>
  mutate(value = exp(estimate) - 1)

##########################################################
## MODEL 2 (demographic + indiv labor market controls) ###
##########################################################

treatment_vars <- "i(rtw_status, ref = '0')"

fe_vars <- paste(c("year", "wbhao", "educ", "female", "metstat", "pubsec",
                   "married", "ft", "paidhre", "mind03", "mocc03", "age", "age2"),
                 collapse = " + ")

regression_formula <- as.formula(paste("lnwage ~", treatment_vars, "|", fe_vars))

### OUTPUT ####
model2f <- df_f |>
  filter(year %in% c(2023:2025)) |> 
  (\(d) feols(regression_formula, data = d, weights = ~ wgt, vcov = ~ statefips))()

model2f_results <- broom::tidy(model2f) |>
  filter(term == "rtw_status::1") |>
  mutate(value = exp(estimate) - 1)

#########################################################################
## MODEL 4 (demographic + indiv lmc + state lmc + BEA RPPs) ###
#########################################################################

treatment_vars <- "i(rtw_status, ref = '0') + urate + lnrpp"

fe_vars <- paste(c("year", "wbhao", "educ", "female", "metstat", "pubsec",
                   "married", "ft", "paidhre", "mind03", "mocc03", "age", "age2"),
                 collapse = " + ")

regression_formula <- as.formula(paste("lnwage ~", treatment_vars, "|", fe_vars))

### OUTPUT ####
model4f <- df_f |>
  filter(year %in% c(2023:2025)) |> 
  (\(d) feols(regression_formula, data = d, weights = ~ wgt, vcov = ~ statefips))()

model4f_results <- broom::tidy(model4f) |>
  filter(term %in% c("rtw_status::1", "rtw_status::2")) |>
  mutate(value = exp(estimate) - 1)

#########################################################################
## MODEL 4 BY YEAR (same specification as model 4, run separately for  ##
## each year 2012-2025; "year" is dropped from the FEs since there's   ##
## no within-year variation to absorb)                                 ##
#########################################################################

treatment_vars <- "i(rtw_status, ref = '0') + urate + lnrpp"

fe_vars <- paste(c("wbhao", "educ", "female", "metstat", "pubsec",
                   "married", "ft", "paidhre", "mind03", "mocc03", "age", "age2"),
                 collapse = " + ")

regression_formula <- as.formula(paste("lnwage ~", treatment_vars, "|", fe_vars))

### OUTPUT ####
model4f_by_year <- list()

for (yr in 2010:2025) {
  model4f_by_year[[as.character(yr)]] <- df_f |>
    filter(year == yr) |>
    (\(d) feols(regression_formula, data = d, weights = ~ wgt, vcov = ~ statefips))()
}

model4f_by_year_results <- imap_dfr(model4f_by_year, \(model, yr) {
  broom::tidy(model) |>
    filter(term %in% c("rtw_status::1", "rtw_status::2")) |>
    mutate(value = exp(estimate) - 1, year = as.integer(yr))
})

#########################################################################
## MODEL 4 BY YEAR, 3-YEAR MOVING AVERAGE (same specification as model ##
## 4, run separately for each year 2012-2025, pooling that year with   ##
## the prior two years, e.g. 2012 pools 2010-2012; "year" is included  ##
## in the FEs since each window spans multiple years)                  ##
#########################################################################

treatment_vars <- "i(rtw_status, ref = '0') + urate + lnrpp"

fe_vars <- paste(c("year", "wbhao", "educ", "female", "metstat", "pubsec",
                   "married", "ft", "paidhre", "mind03", "mocc03", "age", "age2"),
                 collapse = " + ")

regression_formula <- as.formula(paste("lnwage ~", treatment_vars, "|", fe_vars))

### OUTPUT ####
model4f_by_year_ma <- list()

for (yr in 2012:2025) {
  model4f_by_year_ma[[as.character(yr)]] <- df_f |>
    filter(year %in% (yr - 2):yr) |>
    (\(d) feols(regression_formula, data = d, weights = ~ wgt, vcov = ~ statefips))()
}

model4f_by_year_ma_results <- imap_dfr(model4f_by_year_ma, \(model, yr) {
  broom::tidy(model) |>
    filter(term %in% c("rtw_status::1", "rtw_status::2")) |>
    mutate(value = exp(estimate) - 1, year = as.integer(yr))
})

wb$
  # add new worksheet
  add_worksheet(sheet = "final_reg")$
  # add data to worksheet
  add_data(x = "model 1")$
  add_data(x = model1f_results, start_row = 2)$
  add_data(x = "model 2", start_row = 5)$
  add_data(x = model2f_results, start_row = 6)$
  add_data(x = "model 4", start_row = 9)$
  add_data(x = model4f_results, start_row = 10)$
  # add new worksheet for model 4 run separately by year
  add_worksheet(sheet = "final_reg_by_year")$
  add_data(x = model4f_by_year_results, start_row = 1)$
  # add new worksheet for model 4 run by year with 3-year moving average pooling
  add_worksheet(sheet = "final_reg_by_year_ma")$
  add_data(x = model4f_by_year_ma_results, start_row = 1)