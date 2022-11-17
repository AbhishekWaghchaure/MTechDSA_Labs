empid=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
empid

age=c(30,37,45,32,50,60,35,32,34,43,32,30,43,50,60)
age 

sex=c(0,1,0,1,1,1,0,0,1,0,0,1,1,0,0)
sex

status=c(1,1,2,2,1,1,1,2,2,1,2,1,2,1,2)
status

#Creating a dataframe
empinfo = data.frame(empid,age,sex,status)
empinfo

empinfo$sex=factor(empinfo$sex,labels=c("male","female"))
empinfo$status=factor(empinfo$status,labels=c("staff","faculty"))
empinfo

#Male data only
sexm = subset(empinfo,empinfo$sex=='male')
sexm

#Female data only
sexf = subset(empinfo,empinfo$sex=='female')
sexf

summary(empinfo)

summary(sexf)
summary(sexm)

#summary statistics for age 
summary(empinfo$age)

#creating one way table
#1. For sex
table1 = table(empinfo$sex)
table1

#2. For status
table2 = table(empinfo$status)

#creating 2 way table
table3 = table(empinfo$sex, empinfo$status)


#graphical Representation in R 
plot(empinfo$age, type='l',main= 'age of subjects',xlab = 'empid',ylab= 'age in years', col='blue')


#Pie chart
table4<-table(empinfo$sex)
pie(table4)

table5 = table(empinfo$sex, empinfo$status)
barplot(table5,beside=T,xlim=c(1,15),ylim=c(0,5),col=c('blue','red'))

legend('topright',legend = rownames(table5),fill = c('blue','red'),bty='n')

#boxPlot
boxplot(empinfo$age~empinfo$status,col=c('red','blue'))





