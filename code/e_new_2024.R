# this runs the regular model with 2024 data and states' status as of 2024
df_e <- df |> filter(year %in% c(2022:2024))

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
model1e <- df_e |>
  (\(d) feols(regression_formula, data = d, weights = ~ wgt))()

model1e_results <- broom::tidy(model1e) |>
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
model2e <- df_e |>
  (\(d) feols(regression_formula, data = d, weights = ~ wgt))()

model2e_results <- broom::tidy(model2e) |>
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
model4e <- df_e |>
  (\(d) feols(regression_formula, data = d, weights = ~ wgt))()

model4e_results <- broom::tidy(model4e) |>
  filter(term == "rtw_status::1") |>
  mutate(value = exp(estimate) - 1)

wb$
  # add new worksheet
  add_worksheet(sheet = "new_2024")$
  # add data to worksheet
  add_data(x = "model 1")$
  add_data(x = model1e_results, start_row = 2)$
  add_data(x = "model 2", start_row = 5)$
  add_data(x = model2e_results, start_row = 6)$
  add_data(x = "model 4", start_row = 9)$
  add_data(x = model4e_results, start_row = 10)