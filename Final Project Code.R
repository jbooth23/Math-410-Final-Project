#project code put together by Nhan Tran and James Booth. March 19, 2021

health.df=read.csv(file.choose()) #imports our project data

summary(health.df)

par(mfrow=c(2,4))

#the code below plots four of the predictor variables against the two response variables le_agg and le_raceadj

plot(health.df$hh_inc, xlab ="Mean Household Income(dollars/year) ", 
     health.df$le_agg, ylab ="Unadjusted Life Expectancy(years old)")

plot(health.df$hh_inc_age40, xlab ="Mean Household Income(dollars/year) at age 40 ", 
     health.df$le_agg, ylab ="Unadjusted Life Expectancy(years old)")

plot(health.df$count, xlab ="Count (Number of people per income percentile) ", 
     health.df$le_agg, ylab ="Unadjusted Life Expectancy(years old)")

plot(health.df$pctile, xlab ="Income Percentile", 
     health.df$le_agg, ylab ="Unadjusted Life Expectancy(years old)")

plot(health.df$hh_inc, xlab ="Mean Household Income(dollars/year) ", 
     health.df$le_raceadj, ylab ="Race Adjusted Life Expectancy(years old)")

plot(health.df$hh_inc_age40, xlab ="Mean Household Income(dollars/year) at age 40 ", 
     health.df$le_raceadj, ylab ="Race Adjusted Life Expectancy(years old)")

plot(health.df$count, xlab ="Count (Number of people per income percentile) ", 
     health.df$le_raceadj, ylab ="Race Adjusted Life Expectancy(years old)")

plot(health.df$pctile, xlab ="Income Percentile", 
     health.df$le_raceadj, ylab ="Race Adjusted Life Expectancy(years old)")

#the code below plots all of the predictor variables against each other

par(mfrow=c(2,3))

plot(health.df$pctile, xlab ="Household Income Percentile", 
     health.df$count, ylab ="Count")

plot(health.df$pctile, xlab ="Household Income Percentile", 
     health.df$hh_inc, ylab ="Mean household income")

plot(health.df$pctile, xlab ="Household Income Percentile", 
     health.df$hh_inc_age40, ylab ="Mean Household Income, Age 40")

plot(health.df$count, xlab ="Count", 
     health.df$hh_inc, ylab ="Mean household income")

plot(health.df$hh_inc, xlab ="Mean household income", 
     health.df$hh_inc_age40, ylab ="Mean Household Income, Age 40")

plot(health.df$count, xlab ="Mean household income", 
     health.df$hh_inc_age40, ylab ="Mean Household Income, Age 40")

#and this code plots the response variables against each other

par(mfrow=c(1,1))

plot(health.df$le_agg, xlab = "Life Expectancy",
     health.df$le_raceadj, ylab = "Race Adjusted Life Expectancy") #this plot demonstrates the strong collinearity between
#life expectancy and race adjusted life expectancy. They are almost perfectly related.


summary(health.df)

#below we plotted boxplots for all of our variables

par(mfrow=c(2,4))
boxplot(health.df$pctile, ylab= "Income Percentile")
boxplot(health.df$hh_inc, ylab = "Mean Household Income")
boxplot(health.df$hh_inc_age40, ylab= "Mean Household Income, Age 40")
boxplot(health.df$count, ylab= "Count (People per Income Percentile)")
boxplot(health.df$le_raceadj, ylab= "Race adjusted life expectancy(years")
boxplot(health.df$le_agg, ylab= "Unadjusted life expectancy(years)")
boxplot(health.df$sd_le_raceadj, ylab= "Standard error of race adjusted life expectancy(years)")
boxplot(health.df$sd_le_agg, ylab= "Standard error of unadjusted life expectancy(years)")

par(mfrow=c(2,2))

health.lm = lm(le_agg ~ hh_inc + pctile + hh_inc_age40 + gnd + count, data= health.df)
summary(health.lm)
plot(health.lm)

healthStep.bic=step(health.lm) #runs the BIC for two steps for our first linear model, health.lm
healthStep.bic #this process eliminated hh_inc variable from the model

car::vif(health.lm) #this shows extremely high VIF for hh_inc, hh_inc_age40, gnd, and count

health2.lm = lm(le_agg ~ hh_inc_age40 + pctile + gnd + count, data = health.df) #this was our second
#linear model as recommended by the BIC
summary(health2.lm)
plot(health2.lm)

car::vif(health2.lm) #here the VIF values are lower as we've accounted for high collinearity between hh_inc_age40 and hh_inc
#by removing hh_inc through the BIC. However we have very high VIF values for gnd and count

healthStep2.bic=step(health2.lm)
healthStep2.bic #BIC did not eliminate any variables here. So we chose to eliminate count because it had the highest VIF.

health3.lm = lm(le_agg ~ hh_inc_age40 + pctile + gnd, data = health.df) #this is our third and final linear model
summary(health3.lm) #gives additional details about the linear model which we used in our report and poster
plot(health3.lm)

car::vif(health3.lm) #this model has extremely low VIF values indicating no collinearity between the predictors. Could there be
#high collinearity between gnd and count?

health.df[c(100, 101, 200),] #provides information about observations 100, 101, and 200 (all flagged by Cook's distance chart
#for health2.lm)
