###########################################################################

################ MODULE 6 ORDINARY LEAST SQAURES REGRESSION ###############

###########################################################################


############### IMPORTING THE WORLD VALUES DATA ###############

# load 'WV6_Data_R_v_2016_01_01.rdata'

load("/Users/andreeamoldovan/Downloads/WV6_Data_R_v_2016_01_01.rdata")

# save it as a new object called 'values_data'

values_data <- WV6_Data_R

## Frequency distributions and other descriptive statistics
# Plus data management

# frequencies for the attitudes to work variable

table(values_data$V100)

# internal structure of the work variable

str(values_data$V100)

# load the library of functions for the core 'tidyverse' packages

library(tidyverse)

########## DEPENDENT VARIABLE: ATTITUDES TO WORK ########## 

# reverse code the work variable so that higher values indicate a more positive
# attitude; save it as 'work' in the 'values_data' data frame; when you leave
# out values (i.e. -1, -2, -3 in our case; see above), 'recode' treats them as
# missing - which is what we want here; we want to save it as numeric, so 
# we shall use 'as.numeric(recode())'

values_data$work <- as.numeric(recode(values_data$V100, '10' = 1, '9' = 2, 
                                      '8' = 3, '7' = 4,
                                      '6' = 5, '5' = 6, 
                                      '4' = 7, '3' = 8,
                                      '2' = 9, '1' = 10))

# frequencies for the new variable 

table(values_data$work)

########## INDEPENDENT VARIABLES ########## 

# frequencies for the main earner variable

table(values_data$V235)

# internal structure of the main earner variable

str(values_data$V235)

# frequencies for the sex variable

table(values_data$V240)

# internal structure of the sex variable

str(values_data$V240)

# frequencies for the independence variable

table(values_data$V233)

# internal structure of the independence variable

str(values_data$V233)

# recode the original sex variable to a factor with categories "male" and
# "female"; by leaving out -5 and -2 we are recoding them as missing (see 
# message below)

values_data$sex <- recode_factor(values_data$V240, '1' = "male", 
                                                   '2' = "female")

# frequencies 

table(values_data$sex)

############# BIVARIATE REGRESSIONS ############# 

# regress 'work' on 'earner', save the results in new object 'reg1'

reg1 <- lm(work ~ earner, weights = V258, data = values_data)

# use 'summary()' to display the regression results

summary(reg1)

# change reference level for factors in regressions
# choose those who are not the main earners as the reference category instead
# of main earners / change reference level for a factor 

values_data$earner <- relevel(values_data$earner, ref = "not main")

## USING LOGGED OR STANDARDIZED VARIABLES

# use the log of a variable in a linear regression

log_reg <- lm(log(work) ~ log(independence), data = values_data)

# show results of the new regression using logged variables

summary(log_reg)

# standardize 'work' and 'independence' and save them as new variables 
# 'work_sd' and 'ind_sd'

values_data$work_sd <- scale(values_data$work, scale = TRUE)

values_data$ind_sd <- scale(values_data$independence, scale = TRUE)

# use the new variables in the regression to get standardized coefficients

sd_reg <- lm(work_sd ~ ind_sd, data = values_data)

# show results of the new regression using standardized variables

summary(sd_reg)

############# MULTIVARIATE REGRESSIONS ############# 

# regress work on 'earner', 'sex', and 'independence'
# save the results in new object 'reg2'

reg2 <- lm(work ~ earner + sex + independence, 
           weights = V258, data = values_data)

# use 'summary()' to display the regression results

summary(reg2)

########## FITTING INTERACTION TERMS ########## 

# regress work on the 'sex' variable and an interaction between 'independence'
# and 'earner'
# save the results in new object 'reg3'

reg3 <- lm(work ~ sex + independence * earner, 
           weights = V258, data = values_data)

# use 'summary()' to display the regression results

summary(reg3)

## ROBUST STANDARD ERROS USING PACKAGES 'lmtest' AND 'sandwich'

# install packages 'sandwich' and 'lmtest'

install.packages("sandwich", repos = "https://cran.r-project.org/")

install.packages("lmtest", repos = "https://cran.r-project.org/")

# load the libraries of functions for package 'sandwich' and 'lmtest'

library(sandwich)
library(lmtest)

# run a multivariate OLS regression using 'lm'
# save the results in 'reg_robustSE'

reg_robustSE <- lm(work ~ sex + independence + earner, 
                   data = values_data)

# use 'coeftest' and 'vcov = vcovHC' to estimate robust standard erros of type
# 'H1' (to replicate the 'robust' command in Stata); 
# save the new results in object 'robust_SE'

robust_SE <- coeftest(reg_robustSE, vcov = vcovHC(reg_robustSE, "HC1"))

# display the new results

robust_SE

######### VISUALIZING INTERACTIONS USING 'ggplot2' AND 'effects' ##########

## USING 'ggplot2'

# create object 'plot_1' and save the ggplot graph in it
# 'lm' omits missing values by default, which we want to replicate,
# so I omitted missing values using 'na.omit(data_name)' 
# instead of just 'data_name'

plot_1 <- ggplot(data = na.omit(values_data), 
                 # define coordinates by variables
                 mapping = aes(x = independence, # goes on the X axis
                               y = work, # goes on the Y axis
                               color = earner)) + # moderator of the 
  # relationship between X and Y
  # LAYER 1 #
  
  geom_smooth(method = "lm") + # we want a regression line fitted
  
  # LAYER 2 #
  
  # change labels for the color variable, give the plot a title, change labels
  # for the variables on the X and Y axes
  
  labs(color = "Earner",
       title = "Independence by Earner Type on Attitudes to Work",
       x     = "Independence",
       y     = "Attitude to work") +
  
  # LAYER 3 #
  
  # place the title in the middle       
  theme (plot.title = element_text(hjust=0.5)) +
  
  # LAYER 4 #
  
  # change the colour of the lines (default is red and blue)
  scale_color_manual(values = c("yellow", "blue"))

# display the plot saved in object 'plot_1'

plot_1

## USING 'effects'

# install package 'effects'

install.packages("effects", repos = "https://cran.r-project.org/")

# load library of functions for 'effects'

library(effects)

# create object 'interaction' and save the interaction effect
# "independence * earner" from 'reg3 using 'effect'

interaction <- effect("independence * earner", reg3)

# create plot of the interaction effect using 'plot' and save it in object 
# 'plot_2'
# we want it to plot the interaction effect, so we need to pass that as the 
# first argument to get lines, define the 'style =' argument as such; 
# to get 'plot' to display multiple lines on the same panel,set
# 'multiline' to true; if you leave it out, it will display one line
# per panel per value of the moderator ('earner')

plot_2 <- plot(interaction, style = "line", multiline = "true",
               colors = c("yellow", "blue"), # change colors for the lines
               main   = "Independence by Earner Type on Attitudes to Work", # title
               xlab   = "Independence", # X axis label
               ylab   = "Attitude to work") # Y axis label

# display 'plot_2'

plot_2

# with 'multiline' left out or set to 'false'; save new plot
# in object 'plot_3'

plot_3 <- plot(interaction, style = "line", multiline = "false",
               colors = c("red"), # change colors for the lines
               main   = "Independence by Earner Type on Attitudes to Work", # title
               xlab   = "Independence", # X axis label
               ylab   = "Attitude to work") # Y axis label

# display 'plot_3'

plot_3

######### PUBLICATION QUALITY TABLES USING PACKAGE 'texreg' ##########

# install package 'texreg'

install.packages("texreg", repos = "https://cran.r-project.org/")

# load the library of functions for 'texreg'

library(texreg)

## EXPORT TO HTML USING THE FUNCTION 'htmlreg'

# I chose 'reg1' to be displayed as Model 1, and 'reg3' as Model 2
# 'inner.rule' and 'outer.rule' are set to "_" to get straight lines wrapping
# the table; 'column.spacing' specifies how large the space between columns
# should be (the default is 2; I set it to 1 to make it smaller)
# 'caption' is used to give the table a title; 'caption.above = TRUE' places
# it above the table (the default is below the table);
# 'fontsize' is set to "small"; I used 'star.symbol = "\\*"' and 
# 'doctype = FALSE' because I am working in R Markdown and I wouldn't be able
# to export it to html from Markdown otherwise (please see page 18 of 
# the vignette); 
# the second table is generated in an R script, not Markdown; see attached
# script

htmlreg(list("Model 1" = reg1, "Model 2" = reg3), inner.rule = "_",
        outer_rule = "_", column.spacing = 1, 
        caption = "OLS models predicting attitudes to work",
        caption.above = TRUE,
        fontsize = "small", center = TRUE, star.symbol = "\\*",
        doctype = FALSE)

# to customize the names of the coefficients, we can use 'custom.coef.names'; 
# we need to use the concatenate function 'c()' to create a list of the six 
# coefficient names for our six coefficients. (Remember to count the intercept!) 
# 'file =' is used to specify the file to be created that will contain the table; 
# you can change the path below to a valid one for your computer, or set a working 
# directory and input "table.htm"

htmlreg (list("Model 1" = reg1, "Model 2" = reg3), inner.rule = "_",
         outer_rule = "_", column.spacing = 1, 
         caption = "OLS models predicting attitudes to work",
         caption.above = TRUE,
         fontsize = "small", center = TRUE,
         custom.coef.names = c("Intercept", "Not Main Earner",
                               "Independence at work",
                               "Female", "Main earner",
                               "Independence X Main Earner"), 
         file = "table1.htm")

## PRINT TO THE CONSOLE USING THE FUNCTION 'screenreg' 
## (leave out the 'file = ' argument)


screenreg (list("Model 1" = reg1, "Model 2" = reg3), inner.rule = "_",
           outer_rule = "_", column.spacing = 1, 
           caption = "OLS models predicting attitudes to work",
           caption.above = TRUE,
           fontsize = "small", center = TRUE,
           custom.coef.names = c("Intercept", "Not Main Earner",
                                 "Independence at work",
                                 "Female", "Main earner",
                                 "Independence X Main Earner"))