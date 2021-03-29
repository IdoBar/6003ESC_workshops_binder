# Install required packages
install.packages(c("ggpubr", "paletteer", "tidyverse", "Rmisc"))
pacman::p_load(char = c("pastecs", "ggpubr", "paletteer", "tidyverse", "Rmisc", "plotly"))
# load required packages
library(tidyverse)
# read data straight form the web
gapminder <- read_csv("https://tinyurl.com/gapdata")

# explore and summarise the data frame
head(gapminder) # show first 10 rows of the data and typr of variables
summary(gapminder) # brief summary statistics


# manually calculate standard error

library(Rmisc)

summarySE(data=gapminder,
          "lifeExp",
          groupvars="continent",
          conf.interval = 0.95)

gapminder %>% arrange(lifeExp)

gapminder %>% arrange(gdpPercap)
# remove outliers (if we decide to)

gapminder$lifeExp[gapminder$lifeExp<30] <- NA
# Case of missing values - mean, max, sd, etc...

# Visualising data distribution ####

# violin plots
plot <- ggplot(gapminder, aes(x = continent, y = lifeExp)) +
  geom_violin(aes(fill = continent, colour = continent), alpha = 0.5) +
  geom_boxplot(aes(colour = continent), width = 0.15) +
  scale_fill_paletteer_d("wesanderson::FantasticFox1") +
  scale_color_paletteer_d("wesanderson::FantasticFox1")


# interactive plot
ggplotly(plot)
