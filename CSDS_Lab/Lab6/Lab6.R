#Air Quality Dataset  
aq <- data.frame(airquality)
View(aq)

#Replace Null values with 0
aq[is.na(aq)] = 0

# Model1 Ozone Vs Solar Radiation
model1 <- lm(aq$Ozone~aq$Solar.R)
summary(model1)
plot(aq$Ozone ~ aq$Solar.R , pch=21, col='steelblue', main='Ozone VS Solar Radiation', xlab='Solar Radiation', ylab='Ozone')
abline(model1)
print('Solar Radiation is moderately correlated with ozone concentration.')

#model2 Ozon Vs Temp 
model2 <- lm(aq$Ozone~aq$Temp)
summary(model2)
plot( aq$Temp, aq$Ozone, pch=21, col='steelblue', main='Ozone VS Temperature', xlab='Temp', ylab='Ozone')
abline(model2)
print('Temperature and ozone are strongly correlated..')


#model3 Ozon Vs Wind 
model3 <- lm(aq$Ozone~aq$Wind)
summary(model3)
plot( aq$Wind, aq$Ozone, pch=21,col='steelblue', main='Ozone VS Wind', xlab='Wind', ylab='Ozone')
abline(model3)
print('Wind and ozone seem strongly correlated.')

#model4  Wind Vs Solar
model4 <- lm(aq$Solar.R~aq$Wind)
summary(model4)
plot( aq$Wind,aq$Solar.R, pch=21,col='steelblue', main='Wind VS Solar Radiation', xlab='Wind', ylab='Solar Radiation')
abline(model4)
print('Very weak correlation between wind and solar radiation.')


#model5  Temp Vs Solar
model5 <- lm(aq$Solar.R~aq$Temp)
summary(model5)
plot( aq$Temp,aq$Solar.R, pch=21,col='steelblue', main='Temp VS Solar Radiation', xlab='Temp', ylab='Solar Radiation')
abline(model5)
print('Temperature and solar radition are weakly correlated.')

#model6  Temp Vs Wind
model6 <- lm(aq$Wind~aq$Temp)
summary(model6)
plot( aq$Temp,aq$Wind, pch=21,col='steelblue', main='Temp VS Solar Radiation', xlab='Temp', ylab='Wind')
abline(model6)
print('Temperature and wind are moderately correlated.')


