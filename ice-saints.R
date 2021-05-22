library(tidyverse)
library(lubridate)
library(clock)
library(stringr)
library(magrittr)
library(gt)

# Data import
#==================================

temperature_potsdam_hist <-
    # load data
    read_delim(file = "data/produkt_tu_stunde_18930101_20201231_03987.txt",
               delim = ";") %>%
    rename(temperature = TT_TU) %>%
    mutate(datetime = lubridate::ymd_h(MESS_DATUM),
           temperature = as.numeric(stringr::str_trim(temperature)),
           temperature = ifelse(temperature == -999, NA, temperature)) %>%
    select(datetime, temperature) %>%
    mutate(location = "Potsdam")

temperature_potsdam_recent <-
    # load data
    read_delim(file = "data/produkt_zehn_min_tu_20191117_20210519_03987.txt",
               delim = ";") %>%
    # extract the temperature column, add NA's, and format the datetime column
    rename(temperature = TM5_10) %>%
    mutate(datetime = lubridate::ymd_hm(MESS_DATUM),
           temperature = as.numeric(stringr::str_trim(temperature)),
           temperature = ifelse(temperature == -999, NA, temperature)) %>%
    select(datetime, temperature) %>%
    # filter for the current year's data only
    filter(datetime > "2021-01-01 00:00:00") %>%
    # group into hourly averages
    group_by(datetime = lubridate::ceiling_date(datetime, unit = "hour")) %>%
    summarise(temperature = round(mean(temperature, na.rm = TRUE), 1)) %>%
    # add location label
    mutate(location = "Potsdam")

temperature_leipzig_hist <- 
    read_delim(file = "data/produkt_tu_stunde_19510101_20201231_02928.txt",
               delim = ";") %>%
    rename(temperature = TT_TU) %>%
    mutate(datetime = lubridate::ymd_h(MESS_DATUM),
           temperature = as.numeric(stringr::str_trim(temperature)),
           temperature = ifelse(temperature == -999, NA, temperature)) %>%
    select(datetime, temperature) %>%
    mutate(location = "Leipzig")

temperature_leipzig_recent <-
    # load data
    read_delim(file = "data/produkt_zehn_min_tu_20191117_20210519_02928.txt",
               delim = ";") %>%
    # extract the temperature column, add NA's, and format the datetime column
    rename(temperature = TM5_10) %>%
    mutate(datetime = lubridate::ymd_hm(MESS_DATUM),
           temperature = as.numeric(stringr::str_trim(temperature)),
           temperature = ifelse(temperature == -999, NA, temperature)) %>%
    select(datetime, temperature) %>%
    # filter for the current year's data only
    filter(datetime > "2021-01-01 00:00:00") %>%
    # group into hourly averages
    group_by(datetime = lubridate::ceiling_date(datetime, unit = "hour")) %>%
    summarise(temperature = round(mean(temperature, na.rm = TRUE), 1)) %>%
    # add location label
    mutate(location = "Leipzig")

temperature_hourly =
    bind_rows(
        temperature_potsdam_hist,
        temperature_leipzig_hist,
        temperature_potsdam_recent,
        temperature_leipzig_recent
    ) %>%
    mutate(location = factor(location))



# Calculate daily minimum temperature
#==================================

temperature_daily_min = 
    temperature_hourly %>%
    # calculate deaily min temperature for each location
    drop_na() %>%
    group_by(location, 
             date = as_date(datetime)) %>%
    summarise(temp_min = min(temperature)) %>%
    ungroup() %>%
    # add marker if daily min temperaure is below zero
    mutate(subzero = ifelse(temp_min < 0, TRUE, FALSE)) %>%
    mutate(year = lubridate::year(date),
           day_of_year = lubridate::yday(date))



# Calculate last frost day for each year and location
#==================================

last_frost = 
    temperature_daily_min %>%
    filter(day_of_year <= 180,
           subzero) %>%
    group_by(location, year) %>%
    summarise(date = max(date),
              day_of_year = lubridate::yday(date)) %>%
    ungroup() %>%
    mutate(day_month = as.Date(day_of_year, origin = "0000-01-01")) %>%
    complete(location, year)


# Plot historic data on last frost day for each year and location
#==================================

ice_saints_start = as.Date(131, origin = "0000-01-01")
ice_saints_end = as.Date(135, origin = "0000-01-01")

ggplot(last_frost) +
    geom_rect(aes(xmin = -Inf, 
                  xmax = Inf, 
                  ymin = ice_saints_start, 
                  ymax = ice_saints_end), 
              fill = 'skyblue1') + 
    geom_hline(aes(yintercept = ice_saints_start)) +
    geom_hline(aes(yintercept = ice_saints_end)) +
    geom_line(aes(x = year,
                  y = day_month,
                  color = location),
              size = 0.8) +
    facet_wrap(~location, 
               ncol = 1) +
    scale_x_continuous(breaks = seq(0, 10000, 10)) +
    scale_color_manual(values = c("navy", "peru")) +
    labs(title = "When is it safe to plant your seedlings outside?",
         subtitle = "Last day of frost in relation to the Ice Saints (blue ribbon).",
         x = NULL,
         y = NULL,
         caption = "Data: Deutscher Wetterdienst (DWD)") +
    theme_minimal() +
    theme(legend.position = "none")


# Calculate probability of last frost having passed for each given day
#==================================

last_frost_probability <-
    last_frost %>%
    # remove years without data
    drop_na() %>%
    # calculate the cumulative probability of last frost having passed
    group_by(location, day_month) %>%
    summarise(prob_abs = n()) %>%
    mutate(prob_rel = prob_abs / sum(prob_abs),
           prob_cum = cumsum(prob_rel)) %>%
    ungroup() %>%
    # bring date ranges to a common start and end point
    complete(location, day_month) %>%
    # add explicit missing values and set start end end of the curve (prob = 0 or 1)
    group_by(location) %>%
    padr::pad(interval = "day") %>%
    mutate(prob_cum = ifelse(day_month == min(day_month) & is.na(prob_cum), 0, prob_cum),
           prob_cum = ifelse(day_month == max(day_month) & is.na(prob_cum), 1, prob_cum)) %>%
    ungroup() %>%
    # add a day-of-year-count
    mutate(day_of_year = lubridate::yday(day_month))


# Fit the logit model
#==================================

# define logit function 
logit_model  <- function(df) {
    glm(prob_cum ~ day_month, 
        data = df, 
        family = binomial(logit))
}  

# create a date range for the model prediction/interpolation
fit_dates <-
    tibble(day_month = 
               date_seq(from = as.Date("0000-01-01"), 
                        to = as.Date("0000-05-31"), 
                        by = 1))

# fit the model and interpolate
last_frost_model <-
    last_frost_probability %>%
    # subset relevant columns
    select(location, prob_cum, day_month) %>%
    # nest data by location
    group_by(location) %>%
    nest() %>%
    # run logit model and predict on the entire date range
    mutate(model = map(data, logit_model)) %>%
    mutate(fit = map(model, predict, type = "response", newdata = fit_dates)) %>%
    unnest(fit) %>%
    select(location, fit) %>%
    # add prediction date range
    mutate(day_month = fit_dates$day_month) %>%
    # add original prob_cum column
    left_join(last_frost_probability %>%
                  select(location, day_month, prob_cum),
              by = c("location", "day_month"))


# Plot probabilities and model
#==================================

ggplot(last_frost_probability) +
    geom_rect(aes(xmin = ice_saints_start,
                  xmax = ice_saints_end,
                  ymin = -Inf,
                  ymax = Inf),
              fill = 'skyblue1') +
    geom_vline(aes(xintercept = ice_saints_start)) +
    geom_vline(aes(xintercept = ice_saints_end)) +
    geom_text(aes(x = ice_saints_start + 2,
                  y = 0.25),
              angle = 90,
              color = "darkblue",
              label = "Ice Saints") +
    geom_smooth(aes(x = day_month,
                    y = prob_cum,
                    color = location),
                alpha = 0.15,
                method = "glm", 
                method.args = list(family = binomial(logit))) +
    geom_point(aes(x = day_month,
                   y = prob_cum,
                   color = location),
               size = 1.5) +
    scale_color_manual(values = c("navy", "peru")) +
    labs(title = "When is it safe to plant your seedlings outside?",
         subtitle = "Probability of the last frost day having already passed",
         x = NULL,
         y = NULL,
         color = "Logit fit for",
         caption = "Data: Deutscher Wetterdienst (DWD)") +
    theme_minimal() +
    theme(legend.position = "top",
          legend.justification ='left',
          legend.key.width = unit(1.5,"cm")) +
    guides(color = guide_legend(override.aes = list(size = 2)))


# Create summary table based on model results
#==================================


prob_table = 
    last_frost_model %>%
    group_by(location) %>%
    mutate(above_50 = fit >= 0.5,
           above_90 = fit >= 0.9,
           above_95 = fit >= 0.95,
           above_98 = fit >= 0.98,
           above_99 = fit >= 0.99) %>%
    ungroup() %>%
    select(location, day_month, starts_with("above")) %>%
    gather(key = prob,
           value = response,
           starts_with("above")) %>%
    filter(response) %>%
    group_by(location, prob) %>%
    summarise(threshold = min(day_month)) %>%
    ungroup() %>%
    mutate(prob = str_remove(prob, "above_") %>%
               as.numeric() %>%
               magrittr::divide_by(100)) %>%
    spread(key = location, 
           value = threshold) %>%
    mutate(explainer = paste0("i.e., you can expect a frost day after this date once in ", round(1/(1-prob), 0), " years"))

gt(prob_table) %>%
    tab_header(title =  md("**When is it safe to plant your seedlings outside?**"),
               subtitle = "Probability of the last frost day having already passed") %>%
    opt_align_table_header(align = "left") %>%
    tab_spanner(label = "City",
                columns = vars("Leipzig", "Potsdam")) %>%
    cols_label(prob = "Probability",
               explainer = "") %>%
    fmt_percent(columns = vars(prob),
                decimals = 0) %>%
    fmt_date(columns = vars("Leipzig", "Potsdam"),
             date_style = 9) %>%
    cols_align(align = "center",
               columns = everything())
