# this script removes the five switcher states entirely and runs the 2025 data

df_d <- df |> filter(year %in% c(2023:2025), !statefips %in% c("IN", "KY", "WI", "WV", "MI"))

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
model1d <- df_d |>
  (\(d) feols(regression_formula, data = d, weights = ~ wgt, vcov = "hetero"))()

model1d_results <- broom::tidy(model1d) |>
  filter(term == "rtw_status::1") |>
  mutate(value = exp(estimate) - 1)

##########################################################
## MODEL 2 (demographic + indiv labor market controls) ###
##########################################################

treatment_vars <- "i(rtw_status, ref = '0') + age + age2"

fe_vars <- paste(c("year", "wbhao", "educ", "female", "metstat",
                   "married", "ft", "paidhre", "union", "mind03", "mocc03"),
                 collapse = " + ")

regression_formula <- as.formula(paste("lnwage ~", treatment_vars, "|", fe_vars))

### OUTPUT ####
model2d <- df_d |>
  (\(d) feols(regression_formula, data = d, weights = ~ wgt, vcov = "hetero"))()

model2d_results <- broom::tidy(model2d) |>
  filter(term == "rtw_status::1") |>
  mutate(value = exp(estimate) - 1)

#########################################################################
## MODEL 4 (demographic + indiv lmc + state lmc + BEA RPPs) ###
#########################################################################

treatment_vars <- "i(rtw_status, ref = '0') + urate + lnrpp + age + age2"

fe_vars <- paste(c("year", "wbhao", "educ", "female", "metstat",
                   "married", "ft", "paidhre", "union", "mind03", "mocc03"),
                 collapse = " + ")

regression_formula <- as.formula(paste("lnwage ~", treatment_vars, "|", fe_vars))

### OUTPUT ####
model4d <- df_d |>
  (\(d) feols(regression_formula, data = d, weights = ~ wgt, vcov = "hetero"))()

model4d_results <- broom::tidy(model4d) |>
  filter(term == "rtw_status::1") |>
  mutate(value = exp(estimate) - 1)

wb$
  # add new worksheet
  add_worksheet(sheet = "noswitch_2025")$
  # add data to worksheet
  add_data(x = "model 1")$
  add_data(x = model1d_results, start_row = 2)$
  add_data(x = "model 2", start_row = 5)$
  add_data(x = model2d_results, start_row = 6)$
  add_data(x = "model 4", start_row = 9)$
  add_data(x = model4d_results, start_row = 10)