library(readxl)
2+3

100 + 200 + 300

c(100,200,300)

5 # type 5 at the prompt

3 + 4

5^3

pi

1+2*3

log(10)
log10(10)

sin(1)
4*atan(1)

1:15

5:-5

#sqrt(-1)
1+sin(NA)

#Assignment
x = 18 
x<- 18
18 -> x
x

x = 22
x = x+ 25
x

x = c(1,2,3,4)
x

x=as.character(4.58)
x

paste('First','Second','Third')
paste('First','Second','Third',sep = ':')

fname = 'Sri'; lname = "Ram"
paste(fname)
paste(fname,lname)


a = c(1,3,5,7)
b = c(1,2,3,4)

a + b

5*a

a-b

a*b

a/b

a = c(1,2,3,4)

2*a+1

u = c(10,20,30)
v = c(1,2,3,4,5,6,7,8,9)
u+v

#data Frame
n = c(2,3,5)
s = c('aa','bb','cc')
b = c(TRUE,FALSE,TRUE)
df = data.frame(n,s,b)
df

mtcars[1,2]
mtcars['Mazda RX4','cyl']

nrow(mtcars)
ncol(mtcars)
head(mtcars)

#Data Import
mydata = read_excel("Lab1.xlsx")

