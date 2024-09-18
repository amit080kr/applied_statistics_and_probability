# Load required libraries
library(dplyr)
library(ggplot2)
library(lmtest)

#Q1.

#Read the Observation data from csv
observation_data <- read.csv("Kittiwake_Observation_20594315.csv")

# Display a summary of the dataset
summary(observation_data)
summary_stats <- apply(observation_data, 2, function(x) c(Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x)))
print(summary_stats)

#Visualizing the data - Histogram || Q-Q plot || Boxplot

# Get the column names of numeric variables
observation_data_col <- names(observation_data)[sapply(observation_data, is.numeric)]

# Loop through each numeric column
for (col in observation_data_col) {
  
  # Set up a layout for multiple plots
  par(mfrow = c(3, 3))
  # Histogram
  hist(observation_data[[col]], freq=FALSE,main = paste("Histogram of", col),xlab = col, col = "skyblue", border = "black")
  curve(dnorm(x,mean(observation_data[[col]]),sqrt(var(observation_data[[col]])/nrow(observation_data))),0,200,col=2,add=TRUE) 
  
  # Q-Q Plot
  qqnorm(observation_data[[col]], main = paste("Q-Q Plot of", col))
  qqline(observation_data[[col]])
  
  # Boxplot
  boxplot(observation_data[[col]], main = paste("Boxplot of", col), col = "lightgreen", border = "black", horizontal = FALSE, outline = TRUE)
  
  # Reset the layout to the default
  par(mfrow = c(1, 1))
  
}

# Calculate the mean and standard deviation of observations at dawn

mean_dawn <- mean(observation_data$dawn)
sd_dawn <- sd(observation_data$dawn)
min_dawn <- min(observation_data$dawn)
max_dawn <- max(observation_data$dawn)
cat("Mean number of kittiwakes observed at dawn:", mean_dawn, "\n")
cat("Standard deviation of observations at dawn:", sd_dawn, "\n")

#Confidence Interval

n<-10 # choose the sample size
samp<-sample(observation_data$dawn,n) # take a sample of size n from pop
conf_interval <- t.test(samp, conf.level = 0.90)$conf.int #Confidence level based on sample is 125 - 153

#To illustrate the randomness of the confidence interval we will rerun the experiment 100 times

m<-100 # number of times to rerun the experiment
plot(0,type="n",xlim=c(0,100),
     ylim=c(80,200),
     xlab="Sample number",ylab="Confidence interval") # create a blank set of axes on which to plot

for (i in 1:m){ 
  samp<-sample(observation_data$dawn,n) # take a sample of size n from obervation data
  lower<-t.test(samp)$conf.int[1] # calculate the lower limit of the confidence interval
  upper<-t.test(samp)$conf.int[2] # calculate the upper limit of the confidence interval
  lines(c(i,i),c(lower,upper)) # plot a vertical line at x=i, between y=lower and y=upper
}
abline(h=mean_dawn,col=2) # add a red horizontal line for the true population mean

#Interpretation of confidence level

samp<-sample(observation_data$dawn,n) # take a sample of size n from obervation
plot(0,type="n",xlim=c(0,100),
     ylim=c(80,200),
     xlab="Level of confidence",ylab="Confidence interval") # create a blank set of axes on which to plot
for (i in 1:m){ 
  lower<-t.test(samp,conf.level=i/m)$conf.int[1] # calculate the lower limit of the CI where the confidence level is i/m
  upper<-t.test(samp,conf.level=i/m)$conf.int[2] # calculate the upper limit of the CI where the confidence level is i/m
  lines(c(i,i),c(lower,upper)) # plot a vertical line at x=i, between y=lower and y=upper
}
abline(h=mean_dawn,col=2) # add a red horizontal line for the true population mean

#Confidence level based on observation data

conf_interval_tm <- t.test(observation_data$dawn, conf.level = 0.90)$conf.int # 122-138
conf_interval_tm

#Q2.

#Read the Historical data from csv
historical_data <- read.csv("Kittiwake_Historical_20594315.csv")

# Display a summary of the dataset
summary(historical_data)
summary_stats <- apply(historical_data, 2, function(x) c(Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x)))
print(summary_stats)

## Visualizing the historical data

ggplot(historical_data, aes(x = X, y = Site.A, color = "Site A")) +
  geom_line() +
  geom_line(aes(x = X, y = Site.B, color = "Site B")) +
  geom_line(aes(x = X, y = Site.C, color = "Site C")) +
  geom_line(aes(x = X, y = Site.D, color = "Site D")) +
  labs(title = "Breeding Pairs Over Time by Site") +
  scale_y_continuous(limits = c(25, 70), name = "Site")

#Boxplot for each plot type
boxplot(historical_data$Site.A,historical_data$Site.B,historical_data$Site.C,historical_data$Site.D,names=c("Site.A","Site.B","Site.C","Site.D"),ylab="Number of Sites")

# Perform Chi-squared test

matrix_data = as.matrix(historical_data[,-1],6,5)
rownames(matrix_data) = historical_data[,1]

chisq.test(matrix_data, correct = F)

# p value = 0.5408 > 0.05, insufficient evidence to reject null hypothesis
# thus, decline in number is independent on site

#No. of breeding pair in 2006 (linear interpolation)
approx(x=historical_data$X, y=historical_data$Site.A, xout = 2006)$y

#Q3

#Read the Measurement data from csv
measurement_data <- read.csv("Kittiwake_Measurement_20594315.csv")

summary(measurement_data)
# For Red-legged
summary(measurement_data[measurement_data$Sub.species=='Red-legged',])
summary_stats_red_legged <- lapply(measurement_data[measurement_data$Sub.species == 'Red-legged', ], function(x) {
  if(is.numeric(x)) {
    c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE))
  } else {
    NULL  # Return NULL for non-numeric columns
  }
})
summary_stats_red_legged
# For Black-legged
summary(measurement_data[measurement_data$Sub.species=='Black-legged',])
summary_stats_black_legged <- lapply(measurement_data[measurement_data$Sub.species == 'Black-legged', ], function(x) {
  if(is.numeric(x)) {
    c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE))
  } else {
    NULL  # Return NULL for non-numeric columns
  }
})
summary_stats_black_legged

#Part:a::::Visual Summary

# Boxplot
# Wingspan plot
ggplot(measurement_data, aes(x=Sub.species, y=Wingspan, fill=Sub.species)) +
  geom_boxplot() +  
  labs(title = 'Wingspan Summary', x='Sub Species', y='Wingspan')

# Culmen plot
ggplot(measurement_data, aes(x=Sub.species, y=Culmen, fill=Sub.species)) +
  geom_boxplot() +  
  labs(title = 'Culmen Summary', x='Sub Species', y='Culmen')

# Weight plot
ggplot(measurement_data, aes(x=Sub.species, y=Weight, fill=Sub.species)) +
  geom_boxplot() +  
  labs(title = 'Weight Summary', x='Sub Species', y='Weight')

#Histogram
hist(measurement_data$Weight, freq=FALSE,main="Weight Distribution", xlab="Weight (g)", col="lightblue")
curve(dnorm(x,mean(measurement_data$Weight),sqrt(var(measurement_data$Weight)/nrow(measurement_data))),300,500,col=2,add=TRUE) 

hist(measurement_data$Wingspan, freq=FALSE,main="Wing Span Distribution", xlab="Wing Span (cm)", col="lightgreen")
curve(dnorm(x,mean(measurement_data$Wingspan),sqrt(var(measurement_data$Wingspan)/nrow(measurement_data))),50,150,col=2,add=TRUE) 

hist(measurement_data$Culmen,freq=FALSE, main="Culmen Length Distribution", xlab="Culmen Length (mm)", col="lightcoral")
curve(dnorm(x,mean(measurement_data$Culmen),sqrt(var(measurement_data$Culmen)/nrow(measurement_data))),0,100,col=2,add=TRUE)

#Density Plot

par(mfrow=c(3,1))

plot(density(measurement_data[measurement_data$Sub.species=='Black-legged','Wingspan']),main='Wingspan Distribution of Black Legged',lwd=2, xlim=c(50,150), col='green')
lines(density(measurement_data[measurement_data$Sub.species=='Red-legged','Wingspan']),main='Wingspan Distribution of Black Legged', col='orange',lwd=2)
legend('right',c('Black-legged','Red-Legged'),lty=1,lwd=2,bty='n',col=c('green','orange'))

plot(density(measurement_data[measurement_data$Sub.species=='Black-legged','Culmen']),main='Culmen Distribution of Black Legged',lwd=2, xlim=c(0,50),col='green')
lines(density(measurement_data[measurement_data$Sub.species=='Red-legged','Culmen']),main='Culmen Distribution of Black Legged', col='orange',lwd=2)
legend('right',c('Black-legged','Red-Legged'),lty=1,lwd=2,bty='n',col=c('green','orange'))

plot(density(measurement_data[measurement_data$Sub.species=='Black-legged','Weight']),main='Weight Distribution of Black Legged',lwd=2, xlim=c(300,500),col='green')
lines(density(measurement_data[measurement_data$Sub.species=='Red-legged','Weight']),main='Weight Distribution of Black Legged', col='orange',lwd=2)
legend('right',c('Black-legged','Red-Legged'),lty=1,lwd=2,bty='n',col=c('green','orange'))

par(mfrow=c(1,1))

#Part:b::: Using chi-square tests for independence to check if wing span and culmen length are independent for each sub-species.

red_legged = measurement_data[measurement_data$Sub.species=='Red-legged',c('Culmen','Wingspan')]
black_legged = measurement_data[measurement_data$Sub.species=='Black-legged',c('Culmen','Wingspan')]

cor(red_legged) # cor = 0.302
cor.test(red_legged$Wingspan, red_legged$Culmen, method='pearson',alternative = 'greater')

# p value = 0.1366 > 0.05
# insufficient evidence to reject null hypothesis
# for red legged, wingspan and culmen are not correlated

cor(black_legged) # cor = 0.456
cor.test(black_legged$Wingspan, black_legged$Culmen, method='pearson',alternative = 'greater')
# p value = 0.05061 > 0.05
# insufficient evidence to reject null hypothesis
# for black legged, wingspan and culmen are not correlated

#Part-C :::: Difference in Weights

# Use a t-test to check if there is evidence that the weights of birds of the two sub-species are different.

t_test_weights <- t.test(Weight ~ Sub.species, data=measurement_data)
print("Welch Two Sample t-test for Weight:")
print(t_test_weights)
# p value = 0.1355 > 0.05 
# insufficient evidence to reject null hypothesis.
# thus we can conclude that mean weights are equal for red and black legged

#Part -D :::: Overall Difference between Sub-species

# Weight
black_weight <- measurement_data[measurement_data$Sub.species == 'Black-legged', 'Weight']
red_weight <- measurement_data[measurement_data$Sub.species == 'Red-legged', 'Weight']

#T-test
weight_t_test <- t.test(black_weight, red_weight, alternative = 'greater', var.equal = TRUE)
weight_t_test
if (weight_t_test$p.value > 0.05) {
  cat('pvalue = ', round(weight_t_test$p.value * 100, 2), '% > 5%. Thus we have insufficient evidence to reject null hypothesis. Weight is equal for both sub-species\n')
} else {
  cat('pvalue = ', round(weight_t_test$p.value * 100, 2), '% < 5%. Thus we have sufficient evidence to reject null hypothesis. Weight is not equal for both sub-species\n')
}
#Variance Test
weight_test <- var.test(black_weight, red_weight, alternative = 'greater')
weight_test
if (weight_test$p.value > 0.05) {
  cat('pvalue = ', round(weight_test$p.value * 100, 2), '% > 5%. Thus we have insufficient evidence to reject null hypothesis. Variance Weight is equal for both sub-species\n')
} else {
  cat('pvalue = ', round(weight_test$p.value * 100, 2), '% < 5%. Thus we have sufficient evidence to reject null hypothesis. Variance Weight is not equal for both sub-species\n')
}

# Culmen
black_culmen <- measurement_data[measurement_data$Sub.species == 'Black-legged', 'Culmen']
red_culmen <- measurement_data[measurement_data$Sub.species == 'Red-legged', 'Culmen']

#T-test
culmen_t_test <- t.test(black_culmen, red_culmen, alternative = 'greater', var.equal = TRUE)
culmen_t_test
if (culmen_t_test$p.value > 0.05) {
  cat('pvalue = ', round(culmen_t_test$p.value * 100, 2), '% > 5%. Thus we have insufficient evidence to reject H0 at 5% LS. The average Culmen is equal for both sub-species\n')
} else {
  cat('pvalue = ', round(culmen_t_test$p.value * 100, 2), '% < 5%. Thus we have sufficient evidence to reject H0 at 5% LS. The average Culmen is not equal for both sub-species\n')
}

#Variance Test
culmen_test <- var.test(black_culmen, red_culmen, alternative = 'greater')
culmen_test
if (culmen_test$p.value > 0.05) {
  cat('pvalue = ', round(culmen_test$p.value * 100, 2), '% > 5%. Thus we have insufficient evidence to reject H0 at 5% LS. The variance of Culmen is equal for both sub-species\n')
} else {
  cat('pvalue = ', round(culmen_test$p.value * 100, 2), '% < 5%. Thus we have sufficient evidence to reject H0 at 5% LS. The variance of Culmen is not equal for both sub-species\n')
}

# Wingspan
black_wingspan <- measurement_data[measurement_data$Sub.species == 'Black-legged', 'Wingspan']
red_wingspan <- measurement_data[measurement_data$Sub.species == 'Red-legged', 'Wingspan']

#T-test
wingspan_t_test <- t.test(black_wingspan, red_wingspan, alternative = 'greater', var.equal = TRUE)
wingspan_t_test
if (wingspan_t_test$p.value > 0.05) {
  cat('pvalue = ', round(wingspan_t_test$p.value * 100, 2), '% > 5%. Thus we have insufficient evidence to reject H0 at 5% LS. The average Wingspan is equal for both sub-species\n')
} else {
  cat('pvalue = ', round(wingspan_t_test$p.value * 100, 2), '% < 5%. Thus we have sufficient evidence to reject H0 at 5% LS. The average Wingspan is not equal for both sub-species\n')
}

#Variance Test
wingspan_test <- var.test(black_wingspan, red_wingspan, alternative = 'greater')
wingspan_test
if (wingspan_test$p.value > 0.05) {
  cat('pvalue = ', round(wingspan_test$p.value * 100, 2), '% > 5%. Thus we have insufficient evidence to reject H0 at 5% LS. The variance of Wingspan is equal for both sub-species\n')
} else {
  cat('pvalue = ', round(wingspan_test$p.value * 100, 2), '% < 5%. Thus we have sufficient evidence to reject H0 at 5% LS. The variance of Wingspan is not equal for both sub-species\n')
}

# Q4.

#Read the location data from csv
location <- read.csv("Kittiwake_Location_20594315.csv")

# Fit a linear model
linear_model <- lm(Breeding.pairs ~ Coast.direction + sandeel + Summer.temp + cliff.height, data=location)

# Print model summary
summary(linear_model) 
#Adjusted R-squared:  0.8896 
# Breeding.pairs = -79.95 + 3.54*north_coast +8.95*south_coast +8.69*West_coast + 11.12*sandeel -4.07*summer.temp + 51.26*cliff.height

AIC(linear_model) 
#AIC = 219.9343

# Dropping insignificant variables - Coast direction and summer temperature
linear_model_a = lm(Breeding.pairs ~ sandeel  + cliff.height, data = location)
summary(linear_model_a) 
# Adjusted R-square = 0.8111 
# Breeding.pairs = -145.96 + 11.1*sandeel +50.09*cliff.height

AIC(linear_model_a) 
#AIC = 232.8507

### Part -b :::: Fit a linear model to the logarithm of the number of breeding pairs.

# Fit a linear model to the logarithm of BreedingPairs
log_linear_model <- lm(log(Breeding.pairs) ~ Coast.direction + sandeel + Summer.temp + cliff.height, data=location)

# Print model summary
summary(log_linear_model) 
#Adjusted R-square = 0.9386 
# Breeding.pairs = 0.93 + 0.04*north_coast + 0.093*south_coast + 0.03*West_coast + 0.194*sandeel -0.06*summer.temp + 1.04*cliff.height

AIC(log_linear_model)
#AIC = -32.67992(lower AIC is better)

# Dropping insignificant variables - Coast direction and summer temperature
log_linear_model_a = lm(log(Breeding.pairs) ~ sandeel  + cliff.height, data = location)
summary(log_linear_model_a)
# Adj R squared = 0.883 
# Breeding.pairs = -0.24 + 0.21*sandeel + 1.04*cliff.height

AIC(log_linear_model_a) 
#AIC = -16.52

### Part - c :::::: Choose the most appropriate linear model for the data

# Residual analysis for linear_model
plot(linear_model, which = 1, main = "Linear Model-Fit-1")
plot(linear_model_a, which = 1,  main = "Linear Model-Fit 2(Dropping Two Columns)")

# Residual analysis for log_linear_model
plot(log_linear_model, which = 1, main = "Log Linear Model-Fit 1")
plot(log_linear_model_a, which = 1,  main = "Log Linear Model-Fit 2(Dropping Two Columns)")

# choice of appropriate model
# AIC (lower the better) = log-linear model is better
# adjusted R square (higher the better) -> log-linear model is better

# part (d) 

# Breeding.pairs = 0.93 + 0.04*north_coast + 0.093*south_coast + 0.03*West_coast + 0.194*sandeel -0.06*summer.temp + 1.04*cliff.height

# considering log-linear model - we have a high Adj R square > 80% thus we can say the fit is Good
# we have six covariates : north_coast, south_coast, West_coast, sandeel, and cliff height with positive beta coefficients while summer temperature with negative beta coefficient
# this means that as beta increases, the log of breeding pairs increases except for summer temp which leads to decrease in breeding pair
# For every 1-unit increase in the beta variable for north_coast, south_coast, West_coast, sandeel, and cliff height there is increase of 0.04, 0.093,0.03, 0.194, 1.04 units respectively and decrease of 0.06 units for a unit increase in summer temperature.

### Part e ::: Provide a 90% confidence interval for the number of breeding pairs at a specific set of covariate values.

# New data for prediction
new_data <- data.frame(
  Coast.direction = "West",
  sandeel = 2.1,
  Summer.temp = 14.7,
  cliff.height = 3.63
)

# Predict using the selected model
prediction <- predict(log_linear_model, newdata=new_data, interval="confidence", level=0.9)

# Print the prediction and confidence interval
print("Predicted Breeding Pairs:")
print(exp(prediction[1]))  
# 67.95026 
print("90% Confidence Interval:")
print(exp(prediction[2:3]))  
# 59.01277 - 78.24133 