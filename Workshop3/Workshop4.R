

# install required packages - needed only once! (comment with a # after first use)
install.packages('pacman')
# load required packages
library(pacman)
required_packages <- c("dplyr", "tidyr", "ggplot2", "paletteer",
                       "stringr", "readr","readxl", "glue",
                       "highcharter", "forcats", "scales",  
                       "plotly", "lubridate", "here", "ISOweek",
                       "countrycode")
p_load(char=required_packages)
p_load(dplyr, tidyr, ggplot2, readr,  paletteer, glue, scales, plotly, lubridate, patchwork, visdat)

"https://covid.ourworldindata.org/data/owid-covid-data.csv"
# read data from the 'data' folder
covid_data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv", 
                       col_types = paste(c("c", "f", "c", "D", rep("d", 29), 
                                           "c", rep("d", 26)), collapse = "")) #%>% mutate(date=ymd(date))
  

str(covid_data)
# summary of variables in my data
summary(covid_data)


# check which location have continent as NA
covid_data %>% filter(is.na(continent)) %>% count(location)
unique(covid_data$location)

# find extreme rows
covid_data %>% arrange(desc(total_cases))

ggplot(covid_data, aes(x=total_cases)) +
  geom_histogram(fill="lightskyblue") +
  theme_bw(16)

# find the 10 most affected countries (to date)
latest_data <- covid_data %>% filter(!is.na(continent)) %>% 
  group_by(location) %>% arrange(desc(date)) %>% slice(1) %>% ungroup() 
most_affected_countries <- latest_data  %>%  
  arrange(desc(total_deaths)) %>% slice(1:10) %>% 
  select(location)

# subset just the data from the 10 most affected countries and order them from the most affected to the least one
most_affected_data <- covid_data %>% 
  inner_join(most_affected_countries) %>% 
  mutate(Country=factor(location, levels = most_affected_countries$location))

# create a line plot the data of total cases
ggplot(most_affected_data, aes(x=date, y=total_cases, colour=Country)) +
  geom_line(size=1) + scale_y_continuous(labels=comma) + 
  scale_color_paletteer_d("rcartocolor::Bold") +
  labs(color="Country", y = "Total COVID-19 cases") +
  theme_bw(16)

# better formatting of date axis, log scale 
ggplot(most_affected_data, aes(x=date, y=total_cases, colour=Country)) +
  geom_line(size=1) + scale_y_log10(labels=comma) + 
  scale_x_date(NULL,
               breaks = breaks_width("1 months"), 
               labels = label_date_short()) + 
  scale_color_paletteer_d("rcartocolor::Bold") +
  labs(color="Country", y = "Total COVID-19 cases") +
  theme_bw(16)

# create a line plot the data of total deaths
ggplot(most_affected_data, aes(x=date, y=total_deaths, colour=Country)) +
  geom_line(size=1) + scale_y_continuous(labels=comma) + 
  scale_x_date(NULL,
               breaks = breaks_width("1 months"), 
               labels = label_date_short()) + 
  scale_color_paletteer_d("rcartocolor::Bold") +
  labs(color="Country", y = "Total deaths") +
  theme_bw(16) +  
  theme(panel.grid.minor = element_blank()) # remove minor grid lines


# vaccination rates
ggplot(most_affected_data, aes(x=date, y=people_vaccinated, colour=Country)) +
  geom_line(size=1) + scale_y_continuous(labels=comma) + 
  scale_color_paletteer_d("rcartocolor::Bold") +
  scale_x_date(NULL,
               breaks = breaks_width("1 months"), 
               labels = label_date_short()) + 
  labs(color="Country") +
  theme_bw(16) + 
  theme(panel.grid.minor = element_blank())


# look at vaccination and hospitalisation data
# visualise missingness
vis_dat(covid_data %>% filter(date>dmy("01-01-2021")) %>% 
          select(continent, location, total_cases, total_deaths, 
                 hosp_patients, people_vaccinated, people_fully_vaccinated))
                                                                 
# find which countries has the most number of observations (least missing data)
covid_data %>% filter(!is.na(continent), !is.na(people_vaccinated)) %>% # group_by(location) %>% 
  count(location) %>% arrange(desc(n)) %>% print(n=30)

covid_data %>% filter(!is.na(continent), !is.na(hosp_patients)) %>% # group_by(location) %>% 
  count(location) %>% arrange(desc(n)) %>% print(n=30)

countries_subset <- c("Italy", "United States", "Israel", "United Kingdom",  "France", "Czechia")
col_pal <- "ggsci::category10_d3"
hosp_data <- covid_data %>% filter(location %in% countries_subset)
# # new cases by vaccination
# ggplot(most_affected_data, aes(x=people_vaccinated_per_hundred, y=new_deaths , colour=Country)) +
#   geom_point(size=2.5, alpha = 0.65) +
#   scale_color_paletteer_d("rcartocolor::Bold")


# latest vaccination by population
# plot_data <- latest_data %>% inner_join(most_affected_countries) %>% 
#   mutate(Country = factor(location, levels = most_affected_countries$location))
# ggplot(plot_data, aes(x = population, y =people_vaccinated, colour=Country, size = gdp_per_capita)) +
#   geom_point() + scale_y_continuous(labels=comma) + 
#   scale_color_paletteer_d("rcartocolor::Bold") +
#   scale_x_continuous(labels=comma) + 
#   labs(color="Country") +
#   theme_bw(16) + 
#   theme(panel.grid.minor = element_blank())

# hospitalisation rates per vaccinated


col_pal <- "ggsci::category10_d3"
ggplot(hosp_data, aes(x=date, y = hosp_patients,colour=location)) +
  geom_line(size=1) + scale_y_continuous(labels=comma) + 
  scale_color_paletteer_d(col_pal) +
  scale_x_date(name = NULL,
               breaks = breaks_width("1 months"), 
               labels = label_date_short()) + 
  labs(color="Country", y = "Hospitalised patients") +
  theme_bw(16) + 
  theme(panel.grid.minor = element_blank())
# hosp per population size
p1 <- ggplot(hosp_data, 
       aes(x=date, y = hosp_patients_per_million,colour=location)) +
  geom_line(size=1) + 
  scale_y_continuous(labels=comma) + 
  scale_color_paletteer_d(col_pal) +
  scale_x_date(name = NULL,
               breaks = breaks_width("1 months"), 
               labels = label_date_short()) + 
  labs(color="Country", y = "Hospitalised patients (per million)") +
  theme_bw(16) + 
  theme(panel.grid.minor = element_blank())
p1
# total vaccination per population
p2 <- ggplot(hosp_data, 
             aes(x=date, y = people_fully_vaccinated_per_hundred ,colour=location)) +
  geom_line(size=1, linetype="dashed") + 
  scale_y_continuous(labels=comma) + 
  scale_color_paletteer_d(col_pal) +
  scale_x_date(name = NULL,
               breaks = breaks_width("1 months"), 
               labels = label_date_short()) + 
  labs(color="Country", y = "Fully vaccinated (per hundred)") +
  theme_bw(16) + 
  theme(panel.grid.minor = element_blank())
p2
p1 + p2 # show graphs side by side
p1 / p2 # show graphs on top of each other
# Trim off 2020
p3 <- ggplot(hosp_data, 
             aes(x=date, y = people_fully_vaccinated_per_hundred ,colour=location)) +
  geom_line(size=1, linetype="dashed") + 
  scale_y_continuous(labels=comma) + 
  scale_color_paletteer_d(col_pal) +
  scale_x_date(name = NULL,
               limits = c(dmy("01-01-2021"), NA),
               breaks = breaks_width("1 months"), 
               labels = label_date_short()) + 
  labs(color="Country", y = "Fully vaccinated (per hundred)") +
  theme_bw(16) + 
  theme(panel.grid.minor = element_blank())
p1 + p3 + plot_layout(widths = c(2, 1)) # maybe like this?

# show on the same graph
ggplot(hosp_data, 
       aes(x=date, colour=location)) +
  geom_line(aes(y = hosp_patients_per_million), size=1) + 
  geom_line(aes(y = people_fully_vaccinated_per_hundred*10), size=1, linetype="dashed") + 
  scale_y_continuous(labels=comma, name = "Hospitalised patients (per million)",
                     # Add a second axis and specify its features
                     sec.axis = sec_axis(trans=~./10,  name="Fully vaccinated (%)")) + 
  scale_color_paletteer_d(col_pal) +
  scale_x_date(NULL,
               breaks = breaks_width("2 months"), 
               labels = label_date_short()) + 
  labs(color="Country") +
  theme_bw(16) + 
  theme(panel.grid.minor = element_blank()) +
  facet_wrap(~location)
# create output folder
dir.create("./output", showWarnings = FALSE)
# save the plot to pdf file
ggsave("output/hospit_vacc_rates_facet_country.pdf", width=8, height = 6)
