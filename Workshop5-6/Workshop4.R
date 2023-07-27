# Workshop.

########################################################################################################
# load packages 
install.packages("R/SciDatAnalysis_0.1.0.tar.gz", repos = NULL, type="source")
pacman::p_load(learnr, agricolae, tidyverse, car, MASS, SciDatAnalysis)
# 1. Chi-Squared Test of Independence:

### Hair and Eye colour:

# In a table
x <- matrix(c(5, 12, 1, 25, 2, 8, 40, 6, 1), nc = 3, byrow = T)
dimnames(x) <- list(Eye = c("Blue", "Green", "Brown"), Hair = c("Brown", "Blonde", "Red"))
hair.eye <- chisq.test(x)
hair.eye$expect
hair.eye


# As two factors
eye <- factor(rep(c("Blue", "Green", "Brown"), c(18, 35, 47)))
hair.c <- c("Brown", "Blonde", "Red")
hair <- c(rep(hair.c, x[1, ]), rep(hair.c, x[2, ]), rep(hair.c, x[3, ]))
table(eye, hair)
chisq.test(hair, eye)

### Survey Data
library(MASS)
data("survey")

## TOI of smoking status versus gender: ##

# Make faceted bar charts

library(tidyverse)

surv.prop <- survey %>%
  filter(Sex != "NA", Smoke != "NA") %>%
  count(Sex, Smoke) %>%
  group_by(Sex) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

surv.prop

# Faceted Barplot - raw counts
ggplot(surv.prop, aes(x = Smoke, y = n)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), vjust = 1.5, color = "white", 
            size = 3.5) +
  labs(title = "Smoking Status by Sex",
       x = "Smoking Status",
       y = "Frequency") +
  facet_wrap(~Sex) +
  theme_bw() +
  theme(panel.grid = element_blank()) 

# Faceted Barplot - proportions
ggplot(surv.prop, aes(x = Smoke, y = proportion)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(proportion, 2)), vjust = 1.5, color = "white", 
            size = 3.5) +
  labs(title = "Smoking Status by Sex",
       x = "Smoking Status",
       y = "Proportion") +
  facet_wrap(~Sex) +
  theme_bw() +
  theme(panel.grid = element_blank()) 

# Do TOI:

names(survey)
tab <- table(survey$Sex, survey$Smoke)
tab
x <- chisq.test(tab)
x
x$expected

# Exercise status versus smoking status

tab <- table(survey$Exer, survey$Smoke)
tab
x <- chisq.test(tab)
x
x$expected

######################################################################################################

# 2. T-Test (2 sample t-test) ##

boxplot(Height ~ Sex, data = survey)

### By Hand:
x <- tapply(survey$Height, survey$Sex, 
       function(x) c(mean = mean(x, na.rm = T), 
                     sd = sd(x, na.rm = T), 
                     n = sum(!is.na(x))))

fem <- x$Female
mal <- x$Male

# Alt Hypothesis: mu_f - mu_m < -10

sp2 <- ((fem["n"] - 1) * fem["sd"]^2 + 
          (mal["n"] - 1) * mal["sd"]^2)/(fem["n"] + mal["n"] - 2)

tstat <- ((fem["mean"] - mal["mean"]) - (-10))/(sqrt(sp2) * sqrt(1/mal["n"] + 1/fem["n"]))

df <- fem["n"] + mal["n"] - 2

# 1-sided p-val:

p.val <- unname(1 - pt(abs(tstat), df = df))

cat("\n     Two Sample t-test \n\n", "T =", tstat, ", df =", df, ", p-value =", p.val)

### Using t.test()

t.test(Height ~ Sex, data = survey, mu = -10, 
       alternative = "less", var.equal = TRUE)

######################################################################################################

# 3. One-way ANOVA with MTC

bonf.val <- function(alpha = 0.05, k, df) qt(1-((alpha/2)/choose(k, 2)), df = df)

# Strontium levels in lakes:
w.body <- factor(rep(c("Joe Lake", "Wallaby Pond", "Teatree Swamp",
                       "Rock River", "Flo Dam", "Edna Bay"), rep(6, 6)))

strontium <- c(56.3, 54.1, 59.4, 62.7, 60.0, 57.3,
               39.6, 40.8, 37.9, 37.1, 43.6, 42.4,
               28.2, 33.2, 36.4, 34.6, 29.1, 31.0,
               32.5, 36.1, 38.1, 39.2, 34.2, 36.1,
               46.3, 42.1, 43.5, 48.8, 43.7, 40.1,
               41.0, 44.1, 46.4, 40.2, 38.6, 36.3)

stront.dat <- data.frame(w.body, strontium)

boxplot(strontium ~ w.body, data = stront.dat)

fit <- aov(strontium ~ w.body, data = stront.dat)
plot(fit)
summary(fit)

# install.packages("agricolae")

library(agricolae)

# Fisher's LSD:

lsd <- LSD.test(fit, "w.body", console = T)

# Bonferonni-Adjusted LSD:

bonfer <- LSD.test(fit, "w.body", p.adj = "bonferroni", console = T)

# Tukey's HSD:

tukey <- HSD.test(fit, "w.body", console = T)

# SNK:

snk <- SNK.test(fit, "w.body", console = T)

par(mfrow = c(2,2))

plot(lsd, main = "LSD")
plot(bonfer, main  = "Bonferroni")
plot(tukey, main = "Tukey")
plot(snk, main = "SNK")

par(mfrow = c(1, 1))

########################################################################################################

# 4. Multiple Regression 

car.prices <- read.table("car-prices.csv", header = TRUE, sep = ",", as.is = FALSE)

### Simplified Analysis

cars.sub <- data.frame(price = car.prices$price, 
                       wheelbase = car.prices$wheelbase, 
                       horsepower = car.prices$horsepower)
pairs(cars.sub)

fit1 <- lm(price ~ wheelbase + horsepower, data = cars.sub)

par(mfrow = c(2, 2))
plot(fit1)
par(mfrow = c(1, 1))

# log-transformed:
fit2 <- lm(log(price) ~ wheelbase + horsepower, data = cars.sub)

par(mfrow = c(2, 2))
plot(fit2)
par(mfrow = c(1, 1))

# View Results:
summary(fit2)

# Predict at some new data:
new.data <- data.frame(wheelbase = sample(seq(100, 110, length = 5), 5), 
                       horsepower = sample(seq(150, 200, length = 5), 5))
new.data

pred.c <- predict(fit2, new.data, interval = "confidence") # Predict mean values
pred.p <- predict(fit2, new.data, interval = "prediction") # Predict individual values

cat("\n\n Predicted Mean Values (log Scale) \n")
cbind(new.data, pred.c)

cat("\n\n Predicted Individual Values (log Scale) \n")
cbind(new.data, pred.p)


### Fuller Analysis

# Do a pairs plot of numeric variables only.
# Extract out numeric variables - 2 eg ways:
#1. 
n.vars <- unlist(lapply(car.prices, is.numeric))
n.vars; sum(n.vars)  # 14 numeric variables in data frame.
n.cars1 <- car.prices[, n.vars]
pairs(n.cars1)

# 2.
library(dplyr)
n.cars2 <- select_if(car.prices, is.numeric)
pairs(n.cars2)

# Indications that a few variables display fairly strong relationships with each other.
# Multicolinearity will probably be an issue. To check, load the car library:

library(car)

vif(lm(price ~ ., data = n.cars2)) # anything over say 5 might be problematic

vif(lm(price ~ . - carwidth - curbweight - citympg
       , data = n.cars2)) 

# Fit initial plausible model and check residual diagnostics
fit1 <- lm(price ~ fueltype + carbody + drivewheel + wheelbase +
             carheight + cylindernumber + horsepower, data = car.prices)

par(mfrow = c(2, 2))
plot(fit1)
par(mfrow = c(1, 1))

# Looks poor, see if transformation would help.
library(MASS)

boxcox(fit1)

fit2 <- lm(log(price) ~ fueltype + carbody + drivewheel + wheelbase +
             carheight + cylindernumber + horsepower, data = car.prices)

par(mfrow = c(2, 2))
plot(fit2)
par(mfrow = c(1, 1))

summary(fit2)

# Model Selection using AIC
prices.stp <- stepAIC(fit2, scope = list(upper = ~ .,
                                     lower = ~ 1))

# Look at the final selected model:
par(mfrow = c(2, 2))
plot(prices.stp)
par(mfrow = c(1, 1))

summary(prices.stp)

## Use this model to predict:
# Setup some new data:

attach(car.prices)
new.data <- expand.grid(fueltype = levels(fueltype), 
            carbody = levels(carbody), 
            drivewheel = levels(drivewheel))

new.data$wheelbase <- sample(wheelbase, nrow(new.data))
new.data$horsepower <- sample(horsepower, nrow(new.data))
detach()

pred.c <- predict(prices.stp, new.data, interval = "confidence") # Predict mean values
pred.p <- predict(prices.stp, new.data, interval = "prediction") # Predict individual values


