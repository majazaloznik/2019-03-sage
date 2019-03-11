# R basics ####################################################################
# calculation  #########

7+3

(4 + 5 +
    7 + 1) * 2 


2^5-1

# assigning variables #########################################################

height <- 11
width <- 5
height
width
area <- height * width
area 
height <- 7
area
area <- height * width
area

# data types ##################################################################
# numeric vectors
vec1 <- c(20, 33, 41, 22, 12)
vec1
vec2 <- 1:5
vec2
vec1 + vec2
vec1 * 10
vec2 + 1000
# string  vectors
vec3 <- c("Maja", "Michael", "Amy", "Nabila", "Taylor")
vec3
# logical vectors 
vec4 <- c(TRUE, FALSE, FALSE, TRUE, TRUE)
vec4
## mixing data types

vec5 <- c(1, 2, 3, "four", "five")
vec5  


## functions #########################

# calculate the average
(20 +33 + 41 + 22 + 12)/5
sum(vec1)/5
sum(vec1)/length(vec1)
mean(vec1)

# sample from normal distribution
rnorm(10)
rnorm(10, 5)
rnorm(10, 5, 100)

## user written function
# user defined funcitons
sample(1:6, 1)
dice <- function(){sample(1:6, 1)}
dice()

dice.new <- function(sides){sample(1:sides, 1)}
dice.new(3)
dice.new(100)
  
## data frames 

# create data frame
vec5 <- c(1981, 1974, 1993, 1957, 1971)

data.frame(vec1, vec2, vec3, vec4, vec5)
vec5 <- c(1981, 1974, 1993, 1957, 1971)

my.df <- data.frame(id = vec2, 
                    year.born = vec5, 
                    name = vec3, 
                    rps = vec1,  
                    like.wc = vec4)
my.df

## plotting 

plot(my.df$rps,
     type = "p",
     col = "red",
     pch = 19)

hist(my.df$rps)


plot(my.df$year.born, my.df$rps,
     col = "red",
     xlab = "Year born",
     ylab = "Rock-Paper-Scissors score")


# linear regression

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


reg5 <- lm(lifeExp ~ log(gdpPercap)  + continent, data = gapminder.2007)
summary(reg5)


plot(log(gapminder.2007$gdpPercap), gapminder.2007$lifeExp, 
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
reg6 <- lm(lifeExp ~ log(gdpPercap) * continent, data = gapminder.2007)
summary(reg6)

plot(log(gapminder.2007$gdpPercap), gapminder.2007$lifeExp, 
     col = gapminder.2007$continent, pch = 19)
legend("topleft", legend = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
       col = 1:5, pch = 19)

coef <- reg6$coefficients
abline(coef[1], coef[2])
abline(coef[1] + coef[3], coef[2] + coef[7], col = "green3")
abline(coef[1] + coef[4], coef[2] + coef[8], col = "red")
abline(coef[1] + coef[5], coef[2] + coef[9], col = "blue")
abline(coef[1] + coef[6], coef[2] + coef[10], col = "cyan")
