# linear regression ###########################################################
###############################################################################
# restart R session

# load data
library(gapminder)

# load other packages needed
library(ggplot2)
library(broom)
library(tidyverse)

# inspect data
head(gapminder)
unique(gapminder$country)
min(gapminder$year)

# plot data
ggplot(gapminder, aes(year, lifeExp)) +
  geom_point()  
  
# bivariate regression ########################################################
# fit model
reg1 <- lm(lifeExp ~ year, data = gapminder)

# inspect regression
summary(reg1)
tidt(reg1)
glance(reg1)

# plot regression1
ggplot(gapminder, aes(year, lifeExp)) +
  geom_point() + 
  geom_jitter(width = 1) +
  geom_smooth(se = FALSE, method = "lm") 

# bivariate regression 2 ######################################################
# fit model 
reg2 <- lm(lifeExp ~ gdpPercap, data = gapminder)

# inspect model
tidy(reg2)
glance(reg2)

ggplot(gapminder, aes(gdpPercap, lifeExp)) +
  geom_point() + 
  geom_smooth(se = FALSE, method = "lm") 

# bivariate regression with transformation ####################################
# fit model
reg3 <- lm(lifeExp ~ log10(gdpPercap), data = gapminder)

# inspect model
glance(reg3)

# plot model
ggplot(gapminder, aes(gdpPercap, lifeExp)) +
  geom_point() + 
  geom_smooth(se = FALSE, method = "lm") +
  scale_x_log10()


# mulitple regression with categorical variable and interaction ###############

# subset data
gapminder.2007 <- subset(gapminder, year == 2007)

# fit model
reg4 <- lm(lifeExp ~ log10(gdpPercap) * continent, data = gapminder.2007)

# inspect model
tidy(reg4)
glance(reg4)

# plot
ggplot(gapminder.2007, 
       aes(gdpPercap, lifeExp, color = continent)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm") +
  scale_x_log10()

# run multiple multiple regressions ###########################################
gapminder %>%
  nest(-year) %>% 
  mutate(fit = map(data, ~ lm(lifeExp ~ log10(gdpPercap) * continent, data = .x)),  
         tidied = map(fit, glance)) %>% 
  unnest(tidied)


# create interactive plot of multiple regressions #############################
# load packages
library(plotly)

# create plot object
gg <- ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent, frame = year)) +
  geom_point(aes(ids = country, size = pop)) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_x_log10()

# create interactive plot with hover highlights
ggplotly(gg) %>% 
  highlight("plotly_hover")

