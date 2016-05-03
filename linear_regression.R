#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
# setwd("~/Desktop/Rstatistics")
setwd("D:/zhivko/linear_regression")

##   You might also start by listing the files in your working directory

getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
# hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?







#I'll first remove all NA values so I can work with the data
library(dplyr)
Exercise1Data <- filter(states.data, !is.na(energy) & !is.na(metro))

#I'll check on whether the variables are correlated
cor(Exercise1Data$energy, Exercise1Data$metro)

#I see that they're not very correlated so I don't expect a good linear fit, but let's still plot it

plot(Exercise1Data$energy, Exercise1Data$metro)

#if there's any linear relationship it looks like a constant, so it doesn't sound like a good model.
# Let's see the distributions of each variable. It looks like energy is heavily skewed
hist(Exercise1Data$energy)
hist(Exercise1Data$metro)

#Indeed, it looks like the energy variable is heavily skewed to the right, so I will estimate its
# natural log instead of the direct value to account for biases by outlier points. Let me plt that:

plot(log(Exercise1Data$energy), Exercise1Data$metro)

#Still not massively helpful but at least some neagtive slope is starting to emerge to the linear relationship

EnergyModel1 <- lm(log(Exercise1Data$energy) ~ Exercise1Data$metro)
summary(EnergyModel1)


#Indeed, my regression has a very low R2 - the model only explains 15% of the movements in the dependent variable
# Even if both the constant and the metro variable appear as statistically significant causal facrots, while 
# The constant is a fairly substantial number, the Metro coefficient is negligible so my model really doesn't 
#suggest any significant impact of the metro variable on energy consumption
#Let me explore other potential candidates to add as independent variables. I will first see which ones are correlated to the dependent variable as a hint for pausible causation:
#First I need to remove all categorical columns and all remaining NA values
Exercise1Correlations <- Exercise1Data %>% select(which(sapply(., is.numeric))) %>% na.omit()

test <- cor(Exercise1Correlations)
plot(test[,"energy"])

#from the plot it looks like I should focus on the variables that have |cor| > 0.5
# Exploring those, I should focus on:
#- miles
#- toxic
#- green
#- house
#and I will also add income as reasonably correlated and a logical driver for energy consumption
#let me see what happens:

EnergyModel2 <- lm(log(Exercise1Data$energy) ~ Exercise1Data$metro + Exercise1Data$miles +Exercise1Data$toxic + Exercise1Data$green + Exercise1Data$house + Exercise1Data$income)
summary(EnergyModel2)

#I have a good R2 so it looks like a good model. Let's check for multicorrelarity

independentCorrelation <- Exercise1Correlations %>% select(matches('metro|miles|toxic|green|house|income'))
cor(independentCorrelation)

#There aren't any highly related variables, the highest correlation is between metro and income so let's remove income since it is the least significant any way

EnergyModel3 <- lm(log(Exercise1Data$energy) ~ Exercise1Data$metro + Exercise1Data$miles +Exercise1Data$toxic + Exercise1Data$green + Exercise1Data$house)
summary(EnergyModel3)

#The model is simpler and the R2 still good, so removal was justified. Let's remove the next most insignificant variable - metro has the largest Pr(>|t|)

EnergyModel4 <- lm(log(Exercise1Data$energy) ~ Exercise1Data$miles +Exercise1Data$toxic + Exercise1Data$green + Exercise1Data$house)
summary(EnergyModel4)

#The model is simpler and the R2 still good, so removal was justified. Let's remove the next insignificant variable

EnergyModel5 <- lm(log(Exercise1Data$energy) ~ Exercise1Data$miles +Exercise1Data$toxic + Exercise1Data$green)
summary(EnergyModel5)

#This looks like a good model with all variable significant at 10% mark. Let's see if we further remove the miles:

EnergyModel6 <- lm(log(Exercise1Data$energy) ~ Exercise1Data$toxic + Exercise1Data$green)
summary(EnergyModel6)

#Adjusted R2 goes below 0.7, but also the coefficient of the miles variable was the largest, so I would keep Model5 as the best model









## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?






# two of the variables in the model - toxic and miles, point to energy use being a facor
# of travel - likely through petrol used. Let's see if area/density as a measure of the distances
# likely to be travelled in each state has any impact on energy as well

InteractionModel <- lm(log(Exercise1Data$energy) ~ Exercise1Data$miles +Exercise1Data$toxic + Exercise1Data$green + Exercise1Data$area/Exercise1Data$density)
summary(InteractionModel)

#it looks like the area might have some relevance on its own, but not the interactive element


RegionModel <- lm(log(energy) ~ miles +toxic + green + C(region, contr.helmert), data=states.data)
summary(RegionModel)

# IT looks like one of the regions is significantly important so it is likely that geography matters
# when it comes to energy consumption - colder/hotter climates will burn energy to heat/cool
