# linear regression ###########################################################

# restart R session
# load data
library(gapminder)

# inspect data
head(gapminder)
unique(gapminder$country)
min(gapminder$year)

# plot data
plot(gapminder$year, gapminder$lifeExp)

# regress 
reg1 <- lm(lifeExp ~ year, data = gapminder)
summary(reg1)

# plot abline  reg1
plot(jitter(gapminder$year), gapminder$lifeExp)
abline(reg1$coefficients)

# regression 2 
reg2 <- lm(lifeExp ~ gdpPercap, data = gapminder)
summary(reg2)

plot(gapminder$gdpPercap, gapminder$lifeExp)
abline(reg2, col = "red", lwd = 3)

# regression 3



# multiple regression 

reg4 <- lm(lifeExp ~ log(gdpPercap) + year, data = gapminder)
summary(reg4)


# mulitple regression with categorical variable

# subset of the data
gapminder.2007 <- subset(gapminder, year == 2007)


reg5 <- lm(lifeExp ~ log10(gdpPercap)  + continent, data = gapminder.2007)
summary(reg5)


plot(log10(gapminder.2007$gdpPercap), gapminder.2007$lifeExp, 
     col =gapminder.2007$continent, pch = 19)
legend("topleft", legend = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
       col = 1:5, pch = 19)

coef <- reg5$coefficients

abline(coef[1], coef[2], col = "black")
abline(coef[1]+ coef[3], coef[2], col = "red")
abline(coef[1]+ coef[4], coef[2], col = "green3")
abline(coef[1]+ coef[5], coef[2], col = "blue")
abline(coef[1]+ coef[6], coef[2], col = "cyan")

# mulitple regression with categorical variable and interaction
reg6 <- lm(lifeExp ~ log10(gdpPercap) * continent, data = gapminder.2007)
summary(reg6)

plot(log10(gapminder.2007$gdpPercap), gapminder.2007$lifeExp, 
     col = gapminder.2007$continent, pch = 19)
legend("topleft", legend = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
       col = 1:5, pch = 19)

coef <- reg6$coefficients
abline(coef[1], coef[2])
abline(coef[1] + coef[3], coef[2] + coef[7], col = "green3")
abline(coef[1] + coef[4], coef[2] + coef[8], col = "red")
abline(coef[1] + coef[5], coef[2] + coef[9], col = "blue")
abline(coef[1] + coef[6], coef[2] + coef[10], col = "cyan")

# subset of the data
gapminder.1952 <- subset(gapminder, year == 1952)

reg7 <- lm(lifeExp ~ log10(gdpPercap) * continent, data = gapminder.1952)
summary(reg6)

plot(log10(gapminder.1952$gdpPercap), gapminder.1952$lifeExp, 
     col = gapminder.1952$continent, pch = 19)
legend("topleft", legend = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
       col = 1:5, pch = 19)

coef <- reg7$coefficients
abline(coef[1], coef[2])
abline(coef[1] + coef[3], coef[2] + coef[7], col = "green3")
abline(coef[1] + coef[4], coef[2] + coef[8], col = "red")
abline(coef[1] + coef[5], coef[2] + coef[9], col = "blue")
abline(coef[1] + coef[6], coef[2] + coef[10], col = "cyan")

# interactive 
library(ggplot2)
library(plotly)
library(tidyverse)

gg <- ggplot(gapminder, 
             aes(gdpPercap, lifeExp, color = continent, frame = year)) +
  geom_point(aes(size = pop)) +
  geom_smooth(se = FALSE, method = "lm") +
  scale_x_log10()

ggplotly(gg) %>% 
  highlight("plotly_hover")
