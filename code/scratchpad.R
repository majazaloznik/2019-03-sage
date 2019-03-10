# R basics ####################################################################

# calculation  #########

7+3

(4 + 5 +
    7 + 1) * 2 


2^5-1

# assigning variables #########################################################

a <- 12
b <- 4

(a * b)*2

# data types ##################################################################
## numeric vectors
vec <- c(1,2,3,4,8)
vec2 <- c(9,1,2)

vec + vec2

vec*10
## string vectors
string <- c("maja", "michael", "amy", "nabila", "taylor")

## logical vectors

vec3 <- c(TRUE, FALSE, FALSE, TRUE, TRUE)

## mixing data types

vec4 <- c(1, 2, 3, "four", "five")
vec4  

## functions #########################

c(1,2 4))

(1 + 2+ 3+ 4+ 8)/5)


sum(vec)/5

sum(vec)/length(vec)

mean(vec)

rnorm(100, 10)

rnor

## user written function
fair.dice <- function(x){sample(1:x, 1)}

fair.dice(100)
  
  
  