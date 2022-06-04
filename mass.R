# libraries
if("pacman" %in% installed.packages()){
  library(pacman)
}else{
  install.packages("pacman")
  library(pacman)
}

p_load(tidyverse)
p_load(lubridate)
p_load(pracma)
p_load(scales)
p_load(RColorBrewer)
p_load(fontawesome)
p_load(plotly)
p_load(data.table)
p_load(janitor)
p_load(tidymodels)
p_load(anytime)
#devtools::install_github("wilkelab/ungeviz")
p_load(ungeviz)
p_load(httr)
p_load(readxl)

# fetch

#curr_date <- mdy(Sys.Date())

before_5 <- ifelse(hour(Sys.time()) >=17, FALSE, TRUE)

month <- lubridate::month(Sys.Date(), label = TRUE, abbr = FALSE) %>%
  as.character() %>%
  str_to_lower()
day <- lubridate::day(Sys.Date())
day <- ifelse(before_5 == TRUE, day - 1, day)
yr <- lubridate::year(Sys.Date())

date <- paste(month, day, yr, sep = "-")
mass_data_link <- paste0("https://www.mass.gov/doc/covid-19-raw-data-", date, "/download")

target <- "/Users/conorkelly/Downloads/mass_raw.xlsx"
sheet <- "CasesByDate (Test Date)"

download.file(mass_data_link, target)
mass_raw <- read_excel(target, sheet = sheet) %>%
  clean_names() %>%
  mutate(date = ymd(date)) %>%
  mutate(all_7_day = x7_day_confirmed_case_average,
         pos_7d_avg = x7_day_confirmed_case_average,
         x7_day_confirmed_case_average = ifelse(date >= (max(date, na.rm = TRUE) - 3), 
                                                NA, x7_day_confirmed_case_average)) %>%
  filter(date < max(date, na.rm = TRUE),
         date >= "2021-10-01")

max_date <- max(mass_raw$date, na.rm = T)

## testing
ma_test_raw <- read_excel(target, sheet = "TestingByDate (Test Date)") %>%
  clean_names() 

ma_test <- ma_test_raw%>%
  select(date, all_molecular_tests, all_positive_molecular_tests)%>%
  mutate(daily_pct_pos = all_positive_molecular_tests / all_molecular_tests,
         pct_pos_7d = (movavg(all_positive_molecular_tests, 7) * 7) / (movavg(all_molecular_tests, 7) * 7))

# plot
mass_case_plot <- mass_raw %>%

  ggplot() +
  geom_col(aes(x = date, y = positive_new), fill = "coral1", colour = NA, alpha = 0.4) +
  geom_line(aes(x = date, y = all_7_day), size = 0.2) +
  geom_line(aes(x = date, y = x7_day_confirmed_case_average), size = 0.9, color = "coral1") +
  
  
  scale_y_continuous(labels = comma) +
  labs(title = "Massachusetts New Cases by Test Date",
       subtitle = paste0("Daily raw and 7-day average values through ", max_date),
       caption = "Source: Massachusetts Department of Public Health. Recent days (black line) are likely underreported.",
       y = "",
       x  = "") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"),
        text = element_text(size = 10),
        plot.caption = element_text(hjust = 0)) +
  scale_color_continuous(na.value = "transparent")

mass_case_plot
ggsave(paste0("/Users/conorkelly/Documents/COVID/mass/mass_log_", 
              date, ".png"),
       plot = mass_case_plot)
ggplotly(mass_case_plot)

# log 
ma_log <- mass_case_plot +
  geom_point(aes(x = date, y = positive_new), color = "coral1", alpha = 0.4) +
  scale_y_continuous(trans = "log2",
                     labels = comma,
                     breaks = c(500, 1000, 2000, 4000, 8000, 16000, 32000))
ma_log$layers[1] <- NULL
ma_log
ggplotly(ma_log)

# ratios
mass_ratio <- mass_raw %>%
  mutate(case_ratio_daily = positive_new / lag(positive_new, 7),
         case_ratio_7d = x7_day_confirmed_case_average / lag(x7_day_confirmed_case_average, 7),
         case_ratio_7d_all = pos_7d_avg / lag(pos_7d_avg, 7)) %>%
  filter(date >= "2021-11-01") %>%
  
  ggplot() +
  geom_point(aes(x = date, y = case_ratio_daily), color = "coral1", alpha = 0.4) +
  geom_line(aes(x = date, y = case_ratio_7d), size = 0.9) +
  geom_line(aes(x = date, y = case_ratio_7d_all), size = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  
  labs(title = "Massachusetts New Case Ratios by Test Date",
       subtitle = "Daily raw and 7-day average values",
       caption = "Source: Massachusetts Department of Public Health. Recent days are likely underreported.",
       y = "",
       x  = "") +
  ylim(c(0, 3)) +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"),
        text = element_text(size = 10),
        plot.caption = element_text(hjust = 0))
mass_ratio
ggplotly(mass_ratio)

# testing
ma_test_plt <- ma_test %>%
  filter(date >= "2021-07-01",
         date < max(date, na.rm = TRUE)) %>%
  ggplot() +
  geom_point(aes(x = date, y = daily_pct_pos, label = all_molecular_tests), shape = 1, color = "steelblue") +
  geom_line(aes(x = date, y = pct_pos_7d), color = "navy", size = 1) +
  
  labs(title = "Massachusetts Positive Test Percentage (PCR)",
       subtitle = "Daily raw and 7-day total values",
       caption = "Source: Massachusetts Department of Public Health. Recent days are likely underreported.",
       y = "",
       x  = "") +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"),
        text = element_text(size = 10),
        plot.caption = element_text(hjust = 0))

ma_test_plt
ggplotly(ma_test_plt)

####################
# DEATHS
####################

sheet = "DateofDeath"
mass_deaths_raw <- read_excel(target, sheet = sheet) %>%
  clean_names() %>%
  mutate(date_of_death = ymd(date_of_death))

mass_deaths <- mass_deaths_raw %>%
  mutate(check = movavg(confirmed_deaths, 7),
         deaths_7d_no_prov = ifelse(date_of_death >= max(date_of_death, na.rm = TRUE) - 5, NA,
                                    check))

mass_deaths_plot <- mass_deaths %>%
  filter(date_of_death >= "2021-05-01") %>%
  ggplot() +
  #geom_point(aes(x = date_of_death, y = confirmed_deaths), shape = 1) +
  geom_line(aes(x = date_of_death, y = check), size = 0.3) +
  geom_line(aes(x = date_of_death, y = deaths_7d_no_prov), size = 0.8) +
  scale_y_continuous(labels = comma) +
  labs(title = "Massachusetts Deaths by Date of Date",
       subtitle = "Daily raw and 7-day total values",
       caption = "Source: Massachusetts Department of Public Health. Recent days are likely underreported.",
       y = "",
       x  = "") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"),
        text = element_text(size = 10),
        plot.caption = element_text(hjust = 0))

mass_deaths_plot
ggplotly(mass_deaths_plot)


#########
# CFR
#########

cfr <- mass_deaths %>%
  rename(date = date_of_death)%>%
  left_join(mass_raw, by = "date") %>%
  select(date, all_7_day, x7_day_confirmed_death_average, deaths_7d_no_prov) %>%
  mutate(cfr14 = x7_day_confirmed_death_average / lag(all_7_day, 14)) %>%
  mutate(cfr14_no_prov = deaths_7d_no_prov / lag(all_7_day, 14),
         cfr14_no_prov_7day = movavg(cfr14_no_prov, 7),
         cfr14_lab = paste0(round(cfr14 * 100, 3), "%"),
         cfr14_7d = movavg(cfr14, 7))

mass_cfr_plot <- cfr %>%
  
  filter(!is.na(cfr14)) %>%
  
  ggplot() +
  #geom_point(aes(x = date, y = cfr14, label = cfr14_lab, label2 = lag(all_7_day, 14)), shape = 1) +
  geom_line(aes(x = date, y = cfr14_7d), size = 0.3) +
  geom_line(aes(x = date, y = cfr14_no_prov_7day), size = 1.1) +
  scale_y_continuous(labels = percent_format(),
                     limits = c(0, NA)) +
  labs(title = "Massachusetts Case Fatality Rate Estimate",
       subtitle = "Deaths by date of death divided by cases (date of test) 14 days earlier",
       caption = "Source: Massachusetts Department of Public Health. Recent days are likely underreported.",
       y = "",
       x  = "") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"),
        text = element_text(size = 10),
        plot.caption = element_text(hjust = 0))

mass_cfr_plot
ggplotly(mass_cfr_plot)

#####################
# HOSPITALIZATIONS
#####################

sheet = "Hospitalization from Hospitals"
mass_hosps_raw <- read_excel(target, sheet = sheet) %>%
  clean_names() 

mass_hosps <- mass_hosps_raw %>%
  select(date, Hospitalized = total_number_of_covid_patients_in_hospital_today,
         Admissions = new_covid_19_hospitalizations,
         ICU = icu, Intubated = intubated) %>%

  filter(date >= "2021-07-01") %>%
  pivot_longer(cols = -date) %>%
  group_by(name) %>%
  mutate(avg7 = movavg(value, 7)) %>%
  ungroup() %>%
  mutate(weekday = lubridate::wday(date, label = TRUE, abbr = FALSE))

mass_hosps_plt <- mass_hosps %>%
  ggplot() +
  geom_col(aes(x = date, y = value, fill = name, label = weekday), alpha = 0.5,  show.legend = F) +
  geom_line(aes(x = date, y = avg7, color = name, label = weekday), size = 1, show.legend = F) +
  facet_wrap(~name, scales = "free") +
  labs(title = "Massachusetts Hospitalization Metrics",
       #subtitle = "Deaths by date of death divided by cases (date of test) 14 days earlier",
       caption = "Source: Massachusetts Department of Public Health",
       y = "",
       x  = "") +
  theme_minimal() +
  theme(plot.title=element_text(size = 14, face="bold"),
        text = element_text(size = 10),
        plot.caption = element_text(hjust = 0),
        strip.text = element_text(size = 12, face = "bold"),
        legend.title = element_blank())

mass_hosps_plt
ggplotly(mass_hosps_plt)

ma_he <- ma_test_raw %>%
  select(date,
         all_positive_molecular_tests,
         all_molecular_tests_higher_ed_only,
         all_molecular_tests_ma_without_higher_ed,
         all_molecular_tests,
         
         all_positive_molecular_tests,
         all_positive_molecular_tests_higher_ed_only,
         all_positive_molecular_tests_ma_without_higher_ed,
         
         x7_day_weighted_average_positive_test_rate_all_molecular_tests_in_ma,
         x7_day_weighted_average_positive_test_rate_all_molecular_tests_higher_ed_only,
         x7_day_weighted_average_positive_test_rate_all_molecular_tests_ma_without_higher_ed,
         
         x7_day_average_daily_positive_molecular_tests_ma_without_higher_ed,
         x7_day_average_daily_positive_molecular_tests_higher_ed_only) 

ggplotly(ma_he %>%
  filter(date >= "2021-11-01") %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = x7_day_weighted_average_positive_test_rate_all_molecular_tests_in_ma), linetype = "dotted") +
  geom_line(aes(y = x7_day_weighted_average_positive_test_rate_all_molecular_tests_ma_without_higher_ed), color = "maroon") +
  geom_line(aes(y = x7_day_weighted_average_positive_test_rate_all_molecular_tests_higher_ed_only), color = "navy")
)
ma_he %>%
  filter(date >= "2021-11-01") %>%
  ggplot(aes(x = date)) +
  #geom_line(aes(y = all_positive_molecular_tests)) +
  geom_line(aes(y = x7_day_average_daily_positive_molecular_tests_higher_ed_only), color = "red") +
  geom_line(aes(y = x7_day_average_daily_positive_molecular_tests_ma_without_higher_ed), color = "blue")



