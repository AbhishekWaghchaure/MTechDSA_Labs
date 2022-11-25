library(readxl)
Lab4 <- read_excel("Lab4.xlsx")

#create scatterplot of Age VS Resting Heart Rate
plot(Lab4$Age, Lab4$Resting_Heart_Rate, pch=13, col='steelblue', 
     main='Age VS Resting Heart Rate', xlab='Age', ylab='Resting Heart Rate')

#calculate correlation between Age VS Resting Heart Rate
corr <- cor(Lab4$Age, Lab4$Resting_Heart_Rate)
message("Correlation between Age and Resting Heart Rate in given data is: ", corr)
#fit simple linear regression model
fit <- lm(Age ~ Resting_Heart_Rate, data=Lab4)

#view summary model 
summary(fit)

message("The correlation coefficient turns out to be: ", corr," This value is close to 1,
        which indicates a strong positive correlation between Age and Resting Heart Rate")

