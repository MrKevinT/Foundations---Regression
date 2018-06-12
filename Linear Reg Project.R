########
# Exercise 1
########

# read the states data
states.data <- readRDS("dataSets/states.rds") 
# get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])

######
# energy vs. metro
######
# summary of energy and metro columns, all rows
sts.en.metro <- subset(na.omit(states.data), select = c("energy", "metro"))
summary(sts.en.metro)
# correlation between energy and metro
cor(sts.en.metro)

# scatter plot of energy vs metro
plot(sts.en.metro)
# There does not appear to be a significant correlation between energy and percent of people living in a metropolitan area.  The slope of an imaginary trendline is probably non-existent (the line would be nearly vertical)

# Fit our regression model
sat.model <- lm(energy ~ metro, # regression formula
              data=na.omit(states.data)) # data set
# Summarize and print the results
summary(sat.model) # show regression coefficients table

# The Estimate for metro is -1.6526 and the R^2 is 0.09714 (lousy)

######
# energy vs. metro + pop
######
# summary of energy, metro and pop columns, all rows
sts.en.metro2 <- subset(na.omit(states.data), select = c("energy", "metro", "pop"))
summary(sts.en.metro2)
# correlation between energy and metro
cor(sts.en.metro2)

# scatter plot of energy vs metro
plot(sts.en.metro2)
# There does not appear to be a significant correlation between energy and percent of people living in a metropolitan area.  The slope of an imaginary trendline is probably non-existent (the line would be nearly vertical)

# Fit our regression model
sat.model2 <- lm(energy ~ metro + pop, # regression formula
                data=na.omit(states.data)) # data set
# Summarize and print the results
summary(sat.model2) # show regression coefficients table

# pop has a significance of nothing.  The R^2 is 0.0977 (Almost no change)


######
# energy vs. metro + pop + high
######
# summary of energy, metro, pop and high columns, all rows
sts.en.metro3 <- subset(na.omit(states.data), select = c("energy", "metro", "pop", "high"))
summary(sts.en.metro3)
cor(sts.en.metro3)

# scatter plot of energy vs metro
plot(sts.en.metro3)

# Fit our regression model
sat.model3 <- lm(energy ~ metro + pop + high, # regression formula
                 data=na.omit(states.data)) # data set
summary(sat.model3) # show regression coefficients table

# pop and high have a significance of nothing.  The R^2 is 0.1133 (Little change)

######
# energy vs. metro + pop + high + college
######
# summary of energy, metro, pop and high columns, all rows
sts.en.metro4 <- subset(na.omit(states.data), select = c("energy", "metro", "pop", "high", "college"))
summary(sts.en.metro4)
cor(sts.en.metro4)

# scatter plot of energy vs metro
plot(sts.en.metro4)

# Fit our regression model
sat.model4 <- lm(energy ~ metro + pop + high + college, # regression formula
                 data=na.omit(states.data)) # data set
summary(sat.model4) # show regression coefficients table

# College has a significance of '.' and all others are nothing.  The R^2 is 0.172 (A little change)


########
# Exercise 2
########
#Add the interaction to the model
energy.metro.by.college <- lm(csat ~ metro*college,
                             data=na.omit(states.data)) 
#Show the results
coef(summary(energy.metro.by.college)) # show regression coefficients table
## The coefficient for metro area and college is close to zero and not likely significant

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
energy.region <- lm(energy ~ region,
                 data=na.omit(states.data)) 
#Show the results
coef(summary(energy.region)) # show regression coefficients table
anova(energy.region) # show ANOVA table




