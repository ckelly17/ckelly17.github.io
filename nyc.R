# libraries

library(tidyverse)
library(lubridate)
library(pracma)
library(scales)
library(RColorBrewer)
library(plotly)
library(data.table)
library(janitor)
library(anytime)

#setwd("/Users/conorkelly/Documents/COVID")


## load urls for historical analysis
replace_text <- "https://github.com/nychealth/coronavirus-data/commit/"
link_part1 <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/"
link_part2 <- "/trends/data-by-day.csv"
link_part2d <- "/trends/deaths-by-day.csv"
#
# nyc_links <- fread("/Users/conorkelly/Documents/COVID/nyc_github_links.csv") %>%
#   as_tibble()%>%
# 
#   mutate(upload_date = anydate(paste0(upload_date, "-2021"))) %>%
#   mutate(commit = str_remove_all(commit_link, replace_text)) %>%
#   mutate(commit = str_replace(commit,"\\#.*","")) %>%
#   mutate(upload_wday = lubridate::wday(upload_date, label = TRUE)) %>%
#   mutate(data_link = ifelse(!is.na(commit), paste0(link_part1, commit, link_part2), NA)) %>%
#   mutate(death_data_link = ifelse(!is.na(commit), paste0(link_part1, commit, link_part2d), NA)) %>%
#   filter(!is.na(commit)) %>%
#   filter(commit != "")
# 
# # load dataframe
# hist_raw <- tibble()
# for(i in 1:nrow(nyc_links)){
#   upload_date <- nyc_links$upload_date[i]
#   upload_wday <- nyc_links$upload_wday[i]
#   url <- nyc_links$data_link[i]
#   print(upload_date)
#   print(url)
# 
#   df <- fread(url) %>%
#     as_tibble %>%
#     clean_names %>%
#     mutate(case_count_all = case_count + probable_case_count,
#            date_of_interest = anydate(date_of_interest)) %>%
#     select(date_of_interest, case_count_all) %>%
#     mutate(upload_date = upload_date,
#            upload_wday = upload_wday) %>%
#     mutate(upload_wday = factor(upload_wday,
#                                 levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
#                                 ordered = FALSE),
#            doi_wday = lubridate::wday(date_of_interest, label = TRUE),
#            doi_wday = factor(doi_wday,
#                              levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
#                              ordered = FALSE)) %>%
#     filter(date_of_interest >= ymd("2021-01-01"))
# 
#   hist_raw <- bind_rows(hist_raw, df)
# }
# 
# # target value: max case count within date
# hist <- hist_raw %>%
#   filter(date_of_interest <= ymd(Sys.Date() - 11)) %>%
#   group_by(date_of_interest) %>%
#   mutate(max_cases = max(case_count_all, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(pct_of_max = case_count_all / max_cases) %>%
#   group_by(upload_date) %>%
#   mutate(max_date_in_upload = max(date_of_interest, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(gap = max_date_in_upload - date_of_interest) %>%
#   filter(gap <= 12)
# 
# avg_gap <- hist %>%
#   group_by(gap) %>%
#   summarize(mean_pct = mean(pct_of_max, na.rm = TRUE),
#             p95 = quantile(pct_of_max, .975),
#             p5 = quantile(pct_of_max, .025)) %>%
#   mutate(days_from_complete = ifelse(gap > 12, 0, 12 - gap))
#
# write_csv(avg_gap, "nyc_gaps.csv")
avg_gap <- read_csv("nyc_gaps.csv")

# function
clean_nyc <- function(url){
  nyc_raw <- fread(url) %>%
    as_tibble() %>%
    clean_names()
  
  nyc <- nyc_raw %>%
    mutate(date_of_interest = mdy(date_of_interest)) %>%
    mutate(case_count_all = case_count + probable_case_count) %>%
    select(date = date_of_interest,
           case_count_7d = all_case_count_7day_avg,
           case_count_all) %>%
    mutate(Status = ifelse(date >= max(date, na.rm = TRUE) - 11, "Provisional", "Complete"),
           days_from_complete = ifelse(Status %in% "Provisional", date - max(date, na.rm = TRUE) + 12, 0))
  
  return(nyc)
}

# load main
nyc_url <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/cases-by-day.csv"
nyc <- clean_nyc(nyc_url) %>%
  left_join(avg_gap, by = "days_from_complete")

nyc <- nyc %>%
  mutate(PredictedCases = ifelse(Status %in% "Complete", 
                                    case_count_all,
                                 case_count_all / mean_pct),
         High = ifelse(Status %in% "Complete", 
                        case_count_all,
                      case_count_all / p5),
         Low = ifelse(Status %in% "Complete", 
                      case_count_all,
                      case_count_all / p95)) %>%
  mutate(pred7 = movavg(PredictedCases, 7),
         low7 = movavg(Low, 7),
         high7 = movavg(High, 7))
  
# log plot
nyc_log <- nyc %>%
  filter(date >= "2021-07-01") %>%
  mutate(Status = ifelse(Status %in% "Provisional", "Incomplete (estimate)", ">99% reporting")) %>%
  
  ggplot() +
  
  geom_point(aes(x = date, y = PredictedCases, color = Status), alpha = 0.8, size = 1) +
  geom_point(aes(x = date, y = Low, color = Status), size = 0.5, alpha = 0.3) +
  geom_point(aes(x = date, y = High, color = Status), size = 0.5, alpha = 0.3) +
  geom_linerange(aes(x = date, ymin = Low, ymax = High, group = date, color = Status),
                 alpha = 0.5, show.legend = FALSE) +
  geom_ribbon(aes(x = date, ymin=low7, ymax=high7), fill="gray", col="white", alpha=0.7) +
  geom_line(aes(x = date, y = low7), linetype = "dashed", color = "black") +
  geom_line(aes(x = date, y = high7), linetype = "dashed", color = "black") +
  geom_line(aes(x = date, y = pred7), size = 0.9, color = "black") +
  scale_color_manual(values = c(">99% reporting" = "#c72f26",
                                "Incomplete (estimate)" = "#f2837c")) +
  
  scale_y_continuous(labels = comma,
                     trans = "log2",
                     breaks = c(250, 500, 1000, 2000, 4000, 8000, 16000, 32000, 64000)) +
  theme_minimal() + 
  labs(title = "NYC New Cases by Date of Diagnosis",
       subtitle = paste0("Daily values (circles) and 7-day average (line) on log scale from 2021-11-01 to ", max(nyc$date, na.rm = TRUE)),
       caption = 'Source: NYC Health. 
       
Cases for past dates can be added continuously. This chart displays estimates of final case counts for dates where reporting is likely to
still be incomplete. These estimates use the counts already reported for these dates, adjusted upwards based on how recent the date is. 
Dashed lines and shaded areas show a 95% confidence interval for final case counts dates with possibly incomplete reporting.',
       x = "",
       y = "") +
  theme(plot.title=element_text(face="bold"),
        text = element_text(size = 10),
        plot.caption = element_text(hjust = 0))

nyc_log
ggplotly(nyc_log)


# regular axis
nonlog <- nyc_log +
  scale_y_continuous(labels = comma,
                     breaks = seq(0, 100000, 10000)) + 
  labs(subtitle = paste0("Daily values (circles) and 7-day average (line) from 2021-11-01 to ", max(nyc$date, na.rm = TRUE)))
nonlog
ggplotly(nonlog)

# ratio plot
nyc_ratio <- nyc %>%
  filter(date >= "2021-11-01") %>%
  mutate(ratio_7d = pred7 / lag(pred7, 7),
         ratio = ifelse(Status %in% "Provisional", case_count_all / lag(case_count_all, 7), NA),
         
         ratio_pred = PredictedCases / lag(PredictedCases, 7),
         ratio_low = ifelse(Status %in% "Provisional", Low / lag(Low, 7),NA),
         ratio_high = ifelse(Status %in% "Provisional", High / lag(High, 7),NA)) %>%
  mutate(Status = ifelse(Status %in% "Provisional", "Incomplete (estimate)", ">99% reporting")) %>%
  
  ggplot() +
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  #geom_point(aes(x = date, y = ratio, color = Status), shape = 8, 
  #           size = 2, alpha = 0.5) +
  #scale_shape_manual(values=c(8)) +
  geom_point(aes(x = date, y = ratio_pred, color = Status), size = 1) +
  geom_linerange(aes(x = date, ymin = ratio_low, ymax = ratio_high, group = date, color = Status),
                 alpha = 0.5, show.legend = FALSE) +
  geom_line(aes(x = date, y = ratio_7d), size = 1) +
  geom_point(aes(x = date, y = ratio_low, color = Status), size = 0.5, alpha = 0.3) +
  geom_point(aes(x = date, y = ratio_high, color = Status), size = 0.5, alpha = 0.3) +
  scale_color_manual(values = c(">99% reporting" = "#c72f26",
                                "Incomplete (estimate)" = "#f2837c")) +
  #geom_hpline(aes(x = date, y = ratio_low, color = Status), 
  #            width = 1, size = 0.5, alpha = 0.5, show.legend = FALSE) +
  #geom_hpline(aes(x = date, y = ratio_high, color = Status), 
              #width = 1, size = 0.5, alpha = 0.5, show.legend = FALSE) +
  theme_minimal() +
  scale_fill_manual(values = c("white", "black")) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = paste0("NYC Week-Over-Week Case Ratios by Date of Diagnosis through ", max(nyc$date)),
       subtitle = "Ratio of new cases to cases a week prior. Daily values (circles) and 7-day average (line) shown.",
       caption = 'Source: NYC Health GitHub.
       
Cases for past dates can be added continuously. Orange dots display estimates of final case counts for dates where reporting is likely to
still be incomplete. These estimates use the counts already reported for these dates, adjusted upwards based on how recent the date is. 
Vertical lines (orange) show 95% confidence intervals for final case counts on these incomplete dates.',
       x = "",
       y = "") +
  theme(plot.title=element_text(face="bold"),
        text = element_text(size = 10),
        plot.caption = element_text(hjust = 0))
nyc_ratio
ggplotly(nyc_ratio)


##############
# CFR
##############

d_url <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/deaths-by-day.csv"
nyc_deaths_raw <- fread(d_url) %>%
  as_tibble() %>%
  clean_names()

nyc_deaths <- nyc_deaths_raw %>%
  #select(date_of_interest, death_count, probable_death_count) %>%
  mutate(death_count_all = death_count + probable_death_count,
         date = mdy(date_of_interest)) %>%
  left_join(nyc, "date") %>%
  select(date, death_count_all, all_death_count_7day_avg, pred7, PredictedCases) %>%
  mutate(cases_18_days_prior = lag(pred7, 18),
         cfr18 = all_death_count_7day_avg / cases_18_days_prior,
         cfr18_label = paste0(round(cfr18, 4) * 100, "%"),
         case_date = date - 18,
         ratio18 = death_count_all / lag(PredictedCases, 18))

nyc_cfr_plt <- nyc_deaths %>%
  filter(date >= "2021-11-01") %>%
  mutate(Status = ifelse(date >= max(date, na.rm = TRUE) - 7, "P", "C"),
         cfr18 = ifelse(Status %in% "P", NA, cfr18)) %>%
  
  ggplot() +

  #geom_point(aes(x = date, y = ratio18, color = Status), size = 2, shape = 1) +
  geom_line(aes(x = date, y = cfr18, label = cfr18_label, label2 = case_date), size = 0.9) +
  theme_minimal() +
  scale_color_manual(values = c("black", "gray")) +
  scale_y_continuous(limits = c(0, .015),
                     labels = percent_format()) +
  labs(title = paste0("NYC CFR estimates (data through ", max(nyc$date), ")"),
       subtitle = "18 day lag",
       caption = 'Source: NYC Health GitHub',
       x = "",
       y = "") +
  theme(plot.title=element_text(face="bold"),
        text = element_text(size = 10),
        plot.caption = element_text(hjust = 0))

ggplotly(nyc_cfr_plt)
nyc_cfr_plt

nyc_deaths_plt <- nyc_deaths %>%
  filter(date >= "2021-11-01") %>%
  mutate(Status = ifelse(date >= max(date, na.rm = TRUE) - 4, "P", "C")) %>%
  #mutate(all_death_count_7day_avg = ifelse(Status %in% "P", NA, all_death_count_7day_avg)) %>%
  mutate(all_death_count_7day_avg_no_prov = ifelse(Status %in% "P", NA, all_death_count_7day_avg)) %>%
  ggplot() +
  #geom_point(aes(x = date, y = death_count_all, color = Status), size = 2, shape = 1, show.legend = F) +
  geom_line(aes(x = date, y = all_death_count_7day_avg), size = 0.3) +
  geom_line(aes(x = date, y = all_death_count_7day_avg_no_prov), size = 0.9) +
  labs(title = "New York City COVID-19 Deaths",
       subtitle = paste0("Daily values and seven-day average lines by date of death through ", max(nyc_deaths$date), "."),
       caption = "Source: NYC Health. Deaths can be backfilled continuously. Last five data points are marked as provisional (gray) and the seven-day average line is thin.",
       x = "",
       y= "") +
  scale_color_manual(values = c("black", "gray")) +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", size = 15),
        text = element_text(size = 10),
        plot.caption = element_text(hjust = 0),
        axis.text = element_text(size = 10, color = "black"))

ggplotly(nyc_deaths_plt)
nyc_deaths_plt


# ####################
# # BREAKTHROUGHS
# ####################
# 
# link <- "https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/weekly-breakthrough.csv"
# 
# nyc_vax_raw <- fread(link) %>%
#   as_tibble() %>%
#   clean_names()
# 
# nyc_vax <- nyc_vax_raw %>%
#   select(week_of_diagnosis, vax_case_count, vax_hosp_count, vax_death_count,
#          unvax_case_count, unvax_hosp_count, unvax_death_count) %>%
#   mutate(week_of_diagnosis = mdy(week_of_diagnosis))
# 
# # percent in each
# nyc_pct_vax <- nyc_vax %>%
#   mutate(case_vax_pct = vax_case_count / (vax_case_count + unvax_case_count),
#          death_vax_pct = vax_death_count / (vax_death_count + unvax_death_count),
#          hosp_vax_pct = vax_hosp_count / (vax_hosp_count + unvax_hosp_count))
# 
# nyc_vax_pct_plt <- nyc_pct_vax %>%
#   
#   ggplot() +
#   geom_line(aes(x = week_of_diagnosis, y = case_vax_pct, color = "Cases")) +
#   geom_line(aes(x = week_of_diagnosis, y = hosp_vax_pct, color = "Hospitalized")) +
#   geom_line(aes(x = week_of_diagnosis, y = death_vax_pct, color = "Deaths")) +
#   #geom_line(aes(x = week_of_diagnosis, y = death_vax_pct), color = "black") +
#   scale_y_continuous(labels = percent_format()) +
#   scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
#   
#   theme_minimal() +
#   theme(plot.title=element_text(face="bold"),
#         text = element_text(size = 10),
#         plot.caption = element_text(hjust = 0),
#         legend.position = c(.80, 0.20),
#         #panel.grid = element_blank(),
#         legend.text = element_text(size = 10),
#         strip.text = element_text(size = 12, face = "bold"),
#         legend.title = element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   labs(title = "Percentage of NYC COVID-19 Cases, Hospitalizations, and Deaths in Vaccinated Persons",
#        subtitle = "Unadjusted counts by date of diagnosis",
#        caption = "Source: NYC Health",
#        x = "",
#        y= "")
# nyc_vax_pct_plt
# #ggplotly(nyc_vax_pct_plt)
# 
# 
# ## total outcomes by vax status
# nyc_vax_long <- nyc_vax %>%
#   pivot_longer(cols = -week_of_diagnosis) %>%
#   mutate(`Vaccination Status` = ifelse(str_detect(name, "unvax"), "Not Fully Vaccinated", "Fully Vaccinated"),
#          metric = ifelse(str_detect(name, "death"), "Deaths", NA),
#          metric = ifelse(str_detect(name, "case"), "Cases", metric),
#          metric = ifelse(str_detect(name, "hosp"), "Hospitalized", metric)) %>%
#   #mutate(week_of_diagnosis = mdy(week_of_diagnosis)) %>%
#   select(week_of_diagnosis, `Vaccination Status`, metric, value)
# 
# nyc_vax_plt <- nyc_vax_long %>%
# 
#   ggplot() +
#   geom_col(aes(x = week_of_diagnosis, y = value, fill = `Vaccination Status`,
#                color = `Vaccination Status`)) +
#   facet_wrap(~metric, scales = "free", nrow = 2) +
#   labs(title = "NYC COVID-19 Cases, Hospitalizations, and Deaths by Vaccination Status",
#        subtitle = "Unadjusted counts by date of diagnosis",
#        caption = "Source: NYC Health",
#        x = "",
#        y= "") +
#   scale_fill_manual(values = c("navy", "maroon")) +
#   scale_color_manual(values = c("navy", "maroon")) +
#   scale_y_continuous(labels = comma) +
#   scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
#   
#   theme_minimal() +
#   theme(plot.title=element_text(face="bold"),
#         text = element_text(size = 10),
#         plot.caption = element_text(hjust = 0),
#         legend.position = c(.80, 0.20),
#         panel.grid = element_blank(),
#         legend.text = element_text(size = 10),
#         strip.text = element_text(size = 12, face = "bold"),
#         legend.title = element_blank()) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# nyc_vax_plt
# #ggplotly(nyc_vax_plt)
# 
# cfr_vax <- nyc_pct_vax %>%
#   arrange(week_of_diagnosis) %>%
#   filter(week_of_diagnosis >= "2021-10-01") %>%
#   mutate(vax_cfr = vax_death_count / lag(vax_case_count, 2),
#          unvax_cfr = unvax_death_count / lag(unvax_case_count, 2)) %>%
#   select(week_of_diagnosis, ends_with("cfr")) %>%
#   
#   ggplot(aes(x = week_of_diagnosis)) +
#   geom_line(aes(y = vax_cfr, color = "Fully Vaccinated")) +
#   geom_point(aes(y = vax_cfr)) +
#   geom_line(aes(y = unvax_cfr, color = "Not Fully Vaccinated")) +
#   geom_point(aes(y = unvax_cfr)) +
#   scale_y_continuous(labels = percent_format()) +
#   labs(title = "New York City CFR Estimates by Vaccination Status",
#        subtitle = "Weekly total deaths divided by weekly total cases two weeks earlier",
#        caption = "Source: NYC Health",
#        x = "Week of Death",
#        y = "CFR Estimate") +
#   theme_minimal() +
#   theme(plot.title=element_text(face="bold"),
#         text = element_text(size = 10),
#         plot.caption = element_text(hjust = 0),
#         legend.title = element_blank())
# cfr_vax
# #ggplotly(cfr_vax)


#############
# HOSP
#############

# function for hospitalizations
load_nyc <- function(){
  url <- "https://health.data.ny.gov/api/views/jw46-jpb7/rows.csv?accessType=DOWNLOAD"
  ny_hosps_raw <- fread(url) %>%
    as_tibble() %>%
    clean_names()
  
  nyc_hosp <- ny_hosps_raw %>%
    mutate(weekday = lubridate::wday(mdy(as_of_date), label = TRUE, abbr = FALSE)) %>%
    group_by(facility_name) %>%
    arrange(mdy(as_of_date)) %>%
    mutate(patients_currently_hospitalized = ifelse(is.na(patients_currently_hospitalized),
                                                    lag(patients_currently_hospitalized),
                                                    patients_currently_hospitalized),
           patients_newly_admitted = ifelse(is.na(patients_newly_admitted),
                                            lag(patients_newly_admitted),
                                            patients_newly_admitted),
           hosp_change = patients_currently_hospitalized - lag(patients_currently_hospitalized)) %>%
    ungroup()
  
  return(nyc_hosp)
}

# load
nyc_hosp <- load_nyc()

# did it update?
print(max(mdy(nyc_hosp$as_of_date), na.rm = TRUE))

# write
fwrite(nyc_hosp, "nyc_hosp.csv")

############
## NY CASES
############
url <- "https://health.data.ny.gov/api/views/xdss-u53e/rows.csv?accessType=DOWNLOAD"
ny_cases_raw <- fread(url) %>%
  as_tibble() %>%
  clean_names() %>%
  group_by(county) %>%
  arrange(mdy(test_date)) %>%
  mutate(new_tests = cumulative_number_of_tests_performed - lag(cumulative_number_of_tests_performed),
         new_tests_7d = movavg(new_tests, 7),
         new_pos_7d_avg = movavg(new_positives, 7))

print(max(mdy(ny_cases_raw$test_date), na.rm = TRUE))

# write
fwrite(ny_cases_raw, "nyc_cases.csv")


##########
## DEATHS
##########

ny_deaths_link <- "https://health.data.ny.gov/api/views/xymy-pny5/rows.csv?accessType=DOWNLOAD"

ny_deaths_raw <- fread(ny_deaths_link) %>%
  clean_names() %>%
  as_tibble()

ny_deaths <- ny_deaths_raw %>%
  mutate(date = mdy(report_date)) %>%
  
  group_by(county) %>%
  arrange(date) %>%
  mutate(new_deaths_county = deaths_by_county_of_residence - lag(deaths_by_county_of_residence),
         deaths_county_7d = movavg(new_deaths_county, 7)) %>%
  ungroup()

ny_deaths_plt <- ny_deaths %>%
  filter(county %in% "Statewide Total") %>%
  filter(date >= "2021-09-01") %>%
  
  ggplot() +
  
  geom_col(aes(x = date, y = new_deaths_county), alpha = 0.3) +
  geom_line(aes(x = date, y = deaths_county_7d))

#ggplotly(ny_deaths_plt)

cdc_ny <- cd_raw %>%
  filter(state %in% c("NY", "NYC")) %>%
  select(submission_date, state, new_death, pnew_death) %>%
  mutate(new_deaths_total = new_death) %>%
  mutate(date = mdy(submission_date) - 1) %>%
  group_by(state) %>%
  arrange(date) %>%
  mutate(new_death_7d_avg_cdc = movavg(new_deaths_total, 7)) %>%
  ungroup() %>%
  mutate(county = ifelse(state %in% "NY", "Albany", "Manhattan"))

ny_deaths_comb <- ny_deaths %>%
  left_join(cdc_ny, by = c("date", "county")) %>%
  mutate(weekday = lubridate::wday(date, label =TRUE, abbr = FALSE))

print(max(ny_deaths$date,na.rm = T))

# write
fwrite(ny_deaths_comb, "ny_deaths.csv")




