# install required packages (do it only once)
# install.packages("tidyverse")
library(tidyverse)

# Read in data
gapminder <- read_csv("http://tinyurl.com/gapdata")
# explore the data
head(gapminder)
summary(gapminder) 

# population growth in the americas
america_gap <- gapminder %>% filter(continent=="Americas")
ggplot(america_gap, aes(x = year, y=pop, color=country )) +
  geom_line()
# gdpPercap growth in 8 America countries (Z-P)
# create a variable with the desired country names
z_p_countries <- unique(america_gap$country)[18:25]
# filter data and plot gdppercap in American countries
z_p_data <- gapminder %>% filter(country %in% z_p_countries)
ggplot(z_p_data, aes(x = year, y=gdpPercap, color=country )) +
  geom_line() + labs(x = "Year", y = "GDP per capita [$]", 
                     color = "Country") +
  scale_color_brewer(palette = "Dark2") + theme_bw(18)
# save the plot
ggsave("output/gdp_per_cap_americas_P-Z.pdf", width=10, 
       height = 7)
