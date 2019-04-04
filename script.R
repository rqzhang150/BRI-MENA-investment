library(tidyverse)
library(readxl)
library(janitor)
library(plotly)
library(ggthemes)
library(scales)
library(gt)

investment <- read_excel("China-Global-Investment-Tracker-2018-1.xlsx", sheet = "Dataset 1+2", skip = 4)

glimpse(investment)

bri_investment <- investment %>% 
  clean_names() %>% 
  filter(bri == TRUE)

bri_investment

unique(bri_investment$year)
unique(bri_investment$region)

bri_investment %>% 
  filter(region == "Arab Middle East and North Africa" | country %in% c("Turkey", "Iran", "Syria")) %>% 
  group_by(year) %>% 
  summarize(total_investment = sum(quantity_in_millions))

# Middle East and Turkey's BRI investment trend by year
bri_investment %>% 
  filter(country %in% c("Turkey", "Iran", "Saudi Arabia", "UAE", "Egypt", "Oman", "Qatar", "Iraq")) %>% 
  group_by(year, country) %>% 
  summarize(total_investment = sum(quantity_in_millions)) %>% 
  arrange(desc(total_investment)) %>% 
  # plot_ly(x = ~year, y = ~total_investment, color = ~country, type = 'scatter', mode = 'line')
  ggplot(aes(x = year, y = total_investment, color = country)) +
    geom_line() +
    theme_fivethirtyeight() +
    scale_y_continuous(breaks = c(0, 2000, 4000, 6000, 8000),
                       labels = c("$0", "$2bn", "$4bn", "$6bn", "$8bn")) +
    labs(title = "Aggregate Value of Belt and Road Initiative Investments, in Billion USD",
         subtitle = "In Middle Eastern, North African, and West Ansian Countries, 2013 - 2018, by year",
         color = "Country",
         caption = "Source: China Global Investment Tracker/American Enterprise Institute")

# Middle East and Turkey's BRI investment trend by year
bri_investment %>% 
  filter(country %in% c("Turkey", "Saudi Arabia", "Iran")) %>% 
  group_by(year, country) %>% 
  summarize(total_investment = sum(quantity_in_millions)) %>% 
  arrange(desc(total_investment)) %>% 
  # plot_ly(x = ~year, y = ~total_investment, color = ~country, type = 'scatter', mode = 'line')
  ggplot(aes(x = year, y = total_investment, color = country)) +
  geom_line() +
  theme_fivethirtyeight() +
  scale_y_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000),
                      labels = c("$0", "$1bn", "$2bn", "$3bn", "$4bn", "$5bn")) +
  labs(title = "Aggregate Value of Belt and Road Initiative Investments, in Billion USD",
       subtitle = "In Iran, Saudi Arabia, and Turkey, 2013 - 2018, by year",
       color = "Country",
       caption = "Source: China Global Investment Tracker/American Enterprise Institute")


bri_investment %>% 
  filter(region == "Arab Middle East and North Africa" | country %in% c("Turkey", "Iran", "Syria")) %>%
  group_by(country, year) %>% 
  summarize(total_investment = sum(quantity_in_millions)) %>% 
  spread(key = year, value = total_investment) %>% 
  ungroup() %>% 
  mutate_all(list(~replace_na(.,0))) %>%
  mutate(total_investment = `2013` + `2014` + `2015` + `2016` + `2017` + `2018`) %>% 
  arrange(desc(total_investment)) %>% 
  gt() %>% 
  tab_header(
    title = 'Belt and Road Initiative Investments in Middle East',
    subtitle = "Aggregate investment in million dollars, 2013-2018"
  ) %>% 
  gt(rowname_col = "country") %>% 
  tab_stubhead_label(label = "Country") %>% 
  cols_label(total_investment = "Total Investment") %>% 
  tab_source_note(source_note = "Source: China Global Investment Tracker/American Enterprise Institute") %>% 
  fmt_currency(
    columns = vars(`2013`, `2014`, `2015`, `2016`, `2017`, `2018`, total_investment),
    currency = "USD",
    decimals = 0,
    pattern = "{x} M")
  

## BRI investment in middle eastern and north african countries (incl turkey and iran) by invested sectors
bri_investment %>% 
  filter(region == "Arab Middle East and North Africa" | country %in% c("Turkey", "Iran", "Syria"),
         year == 2018) %>%
  group_by(country, sector) %>% 
  summarize(sector_total_investment = sum(quantity_in_millions)) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  ggplot(aes(x = country, y = sector_total_investment, fill = sector)) +
    geom_col() +
    coord_flip()

bri_investment %>% 
  filter(country == "Saudi Arabia") %>% 
  View()
