# ok, future emma (or others). this script is recreating table 2 in gould & kimball 2015

df_a <- df |> filter(year %in% c(2010:2012), age >= 18 & age <=64)

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
model1a <- df_a |>
  (\(d) feols(regression_formula, data = d, weights = ~ wgt))()

model1a_results <- broom::tidy(model1a) |>
  filter(term == "rtw_status::1") |>
  mutate(value = exp(estimate) - 1)

##########################################################
## MODEL 2 (demographic + indiv labor market controls) ###
##########################################################

treatment_vars <- "i(rtw_status, ref = '0')"

fe_vars <- paste(c("year", "wbhao", "educ", "female", "metstat", "union",
                   "married", "ft", "paidhre", "mind03", "mocc03", "age", "age2"),
                 collapse = " + ")

regression_formula <- as.formula(paste("lnwage ~", treatment_vars, "|", fe_vars))

### OUTPUT ####
model2a <- df_a |>
  (\(d) feols(regression_formula, data = d, weights = ~ wgt))()

model2a_results <- broom::tidy(model2a) |>
  filter(term == "rtw_status::1") |>
  mutate(value = exp(estimate) - 1)

#########################################################################
## MODEL 4 (demographic + indiv lmc + state lmc + BEA RPPs) ###
#########################################################################

## NOTE: including age and age2 in treatment variables is what i do elsewhere, but not how Gould & Kimball 2015 did it. 
# including age and age2 in fixed effects gets us closer to benchmark

treatment_vars <- "i(rtw_status, ref = '0') + urate + lnrpp"

fe_vars <- paste(c("year", "wbhao", "educ", "female", "metstat", "union", "age", "age2",
                   "married", "ft", "paidhre", "mind03", "mocc03"),
                 collapse = " + ")

regression_formula <- as.formula(paste("lnwage ~", treatment_vars, "|", fe_vars))

### OUTPUT ####
model4a <- df_a |>
  (\(d) feols(regression_formula, data = d, weights = ~ wgt))()

model4a_results <- broom::tidy(model4a) |>
  filter(term == "rtw_status::1") |>
  mutate(value = exp(estimate) - 1)

wb$
  # add new worksheet
  add_worksheet(sheet = "benchmark_2012")$
  # add data to worksheet
  add_data(x = "model 1")$
  add_data(x = model1a_results, start_row = 2)$
  add_data(x = "model 2", start_row = 5)$
  add_data(x = model2a_results, start_row = 6)$
  add_data(x = "model 4", start_row = 9)$
  add_data(x = model4a_results, start_row = 10)