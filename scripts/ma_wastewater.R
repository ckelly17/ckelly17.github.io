if("pacman" %in% installed.packages()){
  library(pacman)
}else{
  install.packages("pacman")
  library(pacman)
}

p_load(devtools)
devtools::install_github("r-lib/xml2")
system("sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev")
#print("command went through")

p_load(pdftools)
p_load(dplyr)
p_load(readr)
p_load(stringr)
p_load(rvest)
p_load(xml2)
p_load(lubridate)
p_load(scales)
p_load(plotly)

#setwd("/Users/conorkelly/Documents/COVID")

# get url of PDF
mwra_link <- "https://www.mwra.com/biobot/biobotdata.htm"
cntnt <- xml2::read_html(mwra_link)
links <- html_attr(html_nodes(cntnt, "a"), "href") %>%
  as_tibble() %>%
  filter(str_detect(value, "data.pdf"))

data_url <- paste0("https://www.mwra.com/biobot/", min(links$value, na.rm = TRUE))

# scrape pdf
pdf_link <- data_url
raw_pdf <- pdftools::pdf_text(pdf_link)
lines <- read_lines(raw_pdf)

ww_df <- tibble()

for(i in (541:length(lines))){
  line <- lines[i] %>%
    str_trim() %>%
    str_replace_all(" ", ",") %>%
    str_replace_all(",,", ",") %>%
    str_replace_all(",,", ",") %>%
    str_replace_all(",,", ",") %>%
    str_replace_all(",,", ",")
  
  line <- ifelse(nchar(lines[i]) > 1, line, "NO_DATA")
  line_df <- read.csv(text = line, header = FALSE, stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    mutate(across(everything(), as.character))
  
  ww_df <- bind_rows(ww_df, line_df) %>%
    filter(!V1 %in% c("NO_DATA", "Omicron"),
           !is.na(V2))
  
}

ww_df$V10 <- NULL
ww_df$V11 <- NULL

cols <- c("sample_date", "north", "south", "north7", "south7", 
          "south_low", "south_high", "north_low", "north_high")

names(ww_df) <- cols

reg_names <- c("sample_date", "count", "avg7", "low", "high", "region")

# get max date
max_mwra_date <- max(mdy(ww_df$sample_date), na.rm = T)

# split into north and south
north <- ww_df %>%
  select(sample_date, starts_with("north")) %>%
  mutate(region = "North")

names(north) <- reg_names

south <- ww_df %>%
  select(sample_date, starts_with("south")) %>%
  mutate(region = "South")

names(south) <- reg_names

# combine
ww_final <- bind_rows(north, south) %>%
  filter(sample_date != 'BA.2') %>%
  mutate(date = mdy(sample_date),
         count = as.numeric(count),
         avg7 = as.numeric(avg7),
         low = as.numeric(low),
         high = as.numeric(high)) 

# plot
ww_plot <- ww_final %>%
  filter(date >= "2021-06-01") %>%
  ggplot() +
  geom_point(aes(x = date, y = count, color = region), 
             shape = 1, size = 0.5, show.legend = FALSE, alpha = 0.5) +
  geom_line(aes(x = date, y = avg7, color = region), size = 1, show.legend = FALSE) +
  facet_wrap(~region) +
  theme_minimal() +
  scale_y_continuous(labels = comma,
                     breaks = seq(0,20000, 2500)) +
  scale_x_date(breaks = "2 months", date_labels =  "%b 1") +
  labs(title = paste0("COVID-19 Levels in Boston-Area Wastewater"),
       subtitle = paste0("RNA copies per mL in North- and South-Shore wastewater samples at Deer Island Treatment Plant through ",
                        max_mwra_date, "."),    
       caption = 'Source: Massachusetts Water Resource Authority and Biobot Analytics. https://www.mwra.com/biobot/biobotdata.htm',
       x = "",
       y = "") +
  theme(plot.title=element_text(face="bold", size = 15),
        text = element_text(size = 10),
        plot.caption = element_text(hjust = 0),
        strip.text = element_text(size = 12, face = "bold"))


ww_log <- ww_plot +
  scale_y_continuous(labels = comma,
                     trans = "log2",
                     breaks = c(15, 30, 60, 125, 250, 500, 1000, 2000, 4000, 8000, 16000)) +
  labs(title = "COVID-19 Levels in Boston-Area Wastewater (Log Scale)")

ww_plot
ww_log
fig <- ggplotly(ww_plot, width = 800)
log_fig <- ggplotly(ww_log, width = 800)

### week over week ratio
ww_ratio_df <- ww_final %>%
  group_by(region) %>%
  mutate(ratio = count / lag(count, 7),
         ratio7 = avg7 / lag(avg7, 7))

# plot
ww_ratio <- ww_ratio_df %>%
  filter(date >= "2021-09-01") %>%
  ggplot() +
  geom_point(aes(x = date, y = ratio, color = region), shape = 1, size = 0.5, 
             alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(x = date, y = ratio7, color = region), size = 1, show.legend = FALSE) +
  geom_hline(aes(yintercept = 1), linetype = 'dashed', size = 0.9, color = 'black') +
  facet_wrap(~region) +
  theme_minimal() +
  # scale_y_continuous(labels = comma,
  #                    breaks = seq(0,20000, 2500)) +
  scale_x_date(breaks = "2 months", date_labels =  "%b 1") +
  labs(title = paste0("COVID-19 Week-Over-Week Growth"),
       subtitle = paste0("RNA copies per mL in North- and South-Shore wastewater samples at Deer Island Treatment Plant through ",
                         max_mwra_date, "."),    
       caption = 'Source: Massachusetts Water Resource Authority and Biobot Analytics. https://www.mwra.com/biobot/biobotdata.htm',
       x = "",
       y = "") +
  theme(plot.title=element_text(face="bold", size = 15),
        text = element_text(size = 10),
        plot.caption = element_text(hjust = 0),
        strip.text = element_text(size = 12, face = "bold"))

ww_ratio
ratio <- ggplotly(ww_ratio, width = 800)

# htmlwidgets::saveWidget(fig, "ww_fig.html")
# htmlwidgets::saveWidget(log_fig, "ww_fig_log.html")
# htmlwidgets::saveWidget(ratio, "ww_ratio.html")
# 
# 
# browseURL("ww_fig.html")
# browseURL("ww_fig_log.html")
# browseURL("ww_ratio.html")


updated <- (max(ww_final$date, na.rm = T))
last_checked <- as.character(now("America/New_York"))




