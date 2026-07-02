# this script runs 2025 data but with 2012 designations

df_c <- df |> 
  filter(year %in% c(2023:2025)) |> 
# need to switch IN, WI, WV, MI, and KY to non-RTW in 2023-2025
  mutate(rtw_status = case_when(
             statefips == "IN" ~ 0,
             statefips == "WI" ~ 0,
             statefips == "WV" ~ 0,
             statefips == "KY" ~ 0,
             statefips == "MI" ~ 0,
             TRUE ~ rtw_status
  ))

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
model1c <- df_c |>
  (\(d) feols(regression_formula, data = d, weights = ~ wgt))()

model1c_results <- broom::tidy(model1c) |>
  filter(term == "rtw_status::1") |>
  mutate(value = exp(estimate) - 1)

##########################################################
## MODEL 2 (demographic + indiv labor market controls) ###
##########################################################

treatment_vars <- "i(rtw_status, ref = '0')"

fe_vars <- paste(c("year", "wbhao", "educ", "female", "age", "age2", "metstat",
                   "married", "ft", "paidhre", "union", "mind03", "mocc03"),
                 collapse = " + ")

regression_formula <- as.formula(paste("lnwage ~", treatment_vars, "|", fe_vars))

### OUTPUT ####
model2c <- df_c |>
  (\(d) feols(regression_formula, data = d, weights = ~ wgt))()

model2c_results <- broom::tidy(model2c) |>
  filter(term == "rtw_status::1") |>
  mutate(value = exp(estimate) - 1)

#########################################################################
## MODEL 4 (demographic + indiv lmc + state lmc + BEA RPPs) ###
#########################################################################

treatment_vars <- "i(rtw_status, ref = '0') + urate + lnrpp"

fe_vars <- paste(c("year", "wbhao", "educ", "female", "age", "age2", "metstat",
                   "married", "ft", "paidhre", "union", "mind03", "mocc03"),
                 collapse = " + ")

regression_formula <- as.formula(paste("lnwage ~", treatment_vars, "|", fe_vars))

### OUTPUT ####
model4c <- df_c |>
  (\(d) feols(regression_formula, data = d, weights = ~ wgt))()

model4c_results <- broom::tidy(model4c) |>
  filter(term == "rtw_status::1") |>
  mutate(value = exp(estimate) - 1)

wb$
  # add new worksheet
  add_worksheet(sheet = "new_2012desig")$
  # add data to worksheet
  add_data(x = "model 1")$
  add_data(x = model1c_results, start_row = 2)$
  add_data(x = "model 2", start_row = 5)$
  add_data(x = model2c_results, start_row = 6)$
  add_data(x = "model 4", start_row = 9)$
  add_data(x = model4c_results, start_row = 10)