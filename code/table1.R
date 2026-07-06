# table 1, demographic counts

###### 2012 ######
demos_a <- df |> filter(year %in% c(2010:2012), metstat == 0 | metstat == 1)

demos_a_fun <- function(x) {
  demos_a |>
    mutate(!!x := to_factor(.data[[x]])) |>
    summarise(pop = sum(wgt/3),
              n = n(),
              .by = all_of(c(x, "rtw_status"))
             ) |>
    mutate(share = pop/sum(pop),
           .by = "rtw_status") |>
    pivot_wider(id_cols = all_of(x), names_from = rtw_status, values_from = share)
}

# calculate demos results for non-share statistics
demos12 <- demos_a |> 
  summarize(age = weighted.mean(age, w = (wgt/3)),
            sex = 1-(weighted.mean(female, w= (wgt/3))), ## share male
            metstat = weighted.mean(metstat, w = (wgt/3)),
            paidhre = weighted.mean(paidhre, w = (wgt/3)),
            ft = weighted.mean(ft, w = (wgt/3)),
            union = weighted.mean(union, w = (wgt/3)),
            avg_wage = weighted.mean(realwage14, w = (wgt/3)),
            med_wage = weighted_median(realwage14, w = (wgt/3)),
            urate = weighted.mean(urate, w = (wgt/3)),
            rpp = weighted.mean(rpp, w = (wgt/3)),
            n=n(),
           .by = rtw_status) |> 
  pivot_longer(cols = -rtw_status, names_to = "demographic") |>
  pivot_wider(names_from = rtw_status, values_from = value)

# use demos_fun to calculate share by value of remaining variables
demos_table12 <- map(.x = c("wbhao", "educ"), .f = ~ demos_a_fun(.x)) |>
  reduce(bind_rows) |>
  unite(col = "demographic", c(wbhao, educ), na.rm = TRUE) |> 
  bind_rows(demos12) |> 
  rename(rtw = 2, non_rtw = 3)


###### 2024 ######
demos_b <- df |> filter(year %in% c(2022:2024), metstat == 0 | metstat == 1)

demos_b_fun <- function(x) {
  demos_b |>
    mutate(!!x := to_factor(.data[[x]])) |>
    summarise(pop = sum(wgt/3),
              n = n(),
              .by = all_of(c(x, "rtw_status"))
             ) |>
    mutate(share = pop/sum(pop),
           .by = "rtw_status") |>
    pivot_wider(id_cols = all_of(x), names_from = rtw_status, values_from = share)
}

# calculate demos results for non-share statistics
demos24 <- demos_b |> 
  summarize(age = weighted.mean(age, w = (wgt/3)),
            sex = 1-(weighted.mean(female, w= (wgt/3))), ## share male
            metstat = weighted.mean(metstat, w = (wgt/3)),
            paidhre = weighted.mean(paidhre, w = (wgt/3)),
            ft = weighted.mean(ft, w = (wgt/3)),
            union = weighted.mean(union, w = (wgt/3)),
            avg_wage = weighted.mean(realwage24, w = (wgt/3)),
            med_wage = weighted_median(realwage24, w = (wgt/3)),
            urate = weighted.mean(urate, w = (wgt/3)),
            rpp = weighted.mean(rpp, w = (wgt/3)),
            n=n(),
           .by = rtw_status) |> 
  pivot_longer(cols = -rtw_status, names_to = "demographic") |>
  pivot_wider(names_from = rtw_status, values_from = value)

# use demos_fun to calculate share by value of remaining variables
demos_table24 <- map(.x = c("wbhao", "educ"), .f = ~ demos_b_fun(.x)) |>
  reduce(bind_rows) |>
  unite(col = "demographic", c(wbhao, educ), na.rm = TRUE) |> 
  bind_rows(demos24) |> 
  rename(rtw = 2, non_rtw = 3)

###### 2025 ######
demos_c <- df |> filter(year %in% c(2023:2025), metstat == 0 | metstat == 1)

demos_c_fun <- function(x) {
  demos_c |>
    mutate(!!x := to_factor(.data[[x]])) |>
    summarise(pop = sum(wgt/3),
              n = n(),
              .by = all_of(c(x, "rtw_status"))
             ) |>
    mutate(share = pop/sum(pop),
           .by = "rtw_status") |>
    pivot_wider(id_cols = all_of(x), names_from = rtw_status, values_from = share)
}

# calculate demos results for non-share statistics
demos25 <- demos_c |> 
  summarize(age = weighted.mean(age, w = (wgt/3)),
            sex = 1-(weighted.mean(female, w= (wgt/3))), ## share male
            metstat = weighted.mean(metstat, w = (wgt/3)),
            paidhre = weighted.mean(paidhre, w = (wgt/3)),
            ft = weighted.mean(ft, w = (wgt/3)),
            union = weighted.mean(union, w = (wgt/3)),
            avg_wage = weighted.mean(realwage25, w = (wgt/3)),
            med_wage = weighted_median(realwage25, w = (wgt/3)),
            urate = weighted.mean(urate, w = (wgt/3)),
            rpp = weighted.mean(rpp, w = (wgt/3)),
            n=n(),
           .by = rtw_status) |> 
  pivot_longer(cols = -rtw_status, names_to = "demographic") |>
  pivot_wider(names_from = rtw_status, values_from = value)

# use demos_fun to calculate share by value of remaining variables
demos_table25 <- map(.x = c("wbhao", "educ"), .f = ~ demos_c_fun(.x)) |>
  reduce(bind_rows) |>
  unite(col = "demographic", c(wbhao, educ), na.rm = TRUE) |> 
  bind_rows(demos25) |> 
  rename(rtw = 2, non_rtw = 3)

###### Switchers for 2023-2025 ######
demos_d <- df |> filter(year %in% c(2023:2025), metstat == 0 | metstat == 1, statefips %in% c("IN", "WI", "WV", "KY"))

demos_d_fun <- function(x) {
  demos_d |>
    mutate(!!x := to_factor(.data[[x]])) |>
    summarise(pop = sum(wgt/3),
              n = n(),
              .by = all_of(c(x, "rtw_status"))
             ) |>
    mutate(share = pop/sum(pop),
           .by = "rtw_status") |>
    pivot_wider(id_cols = all_of(x), names_from = rtw_status, values_from = share)
}

# calculate demos results for non-share statistics
demos_switch <- demos_d |> 
  summarize(age = weighted.mean(age, w = (wgt/3)),
            sex = 1-(weighted.mean(female, w= (wgt/3))), ## share male
            metstat = weighted.mean(metstat, w = (wgt/3)),
            paidhre = weighted.mean(paidhre, w = (wgt/3)),
            ft = weighted.mean(ft, w = (wgt/3)),
            union = weighted.mean(union, w = (wgt/3)),
            avg_wage = weighted.mean(realwage25, w = (wgt/3)),
            med_wage = weighted_median(realwage25, w = (wgt/3)),
            urate = weighted.mean(urate, w = (wgt/3)),
            rpp = weighted.mean(rpp, w = (wgt/3)),
            n=n(),
           .by = rtw_status) |> 
  pivot_longer(cols = -rtw_status, names_to = "demographic") |>
  pivot_wider(names_from = rtw_status, values_from = value)

# use demos_fun to calculate share by value of remaining variables
demos_table3 <- map(.x = c("wbhao", "educ"), .f = ~ demos_d_fun(.x)) |>
  reduce(bind_rows) |>
  unite(col = "demographic", c(wbhao, educ), na.rm = TRUE) |> 
  bind_rows(demos_switch) |> 
  rename(switch = 2)


wb$
  # add new worksheet
  add_worksheet(sheet = "Table 1")$
  # add data to worksheet
  add_data(x = "2012")$
  add_data(x = demos_table12, start_row = 2)$
  add_data(x = "2024", start_col = 5)$
  add_data(x = demos_table24, start_col = 5, start_row = 2)$
  add_data(x = "2025", start_col = 9)$
  add_data(x = demos_table25, start_col = 9, start_row = 2)$
  add_data(x = "switchers", start_col = 13)$
  add_data(x = demos_table3, start_col = 13, start_row = 2)