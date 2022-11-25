# Question 1

library(readxl)
Lab5 <- read_excel("Lab5.xlsx")
View(Lab5)
x <- sum(Lab5$X)
x
y <- sum(Lab5$Y)
y
xy <- sum(Lab5$X*Lab5$Y)
xy
xq <- sum(Lab5$`X^2`)
xq
yq<- sum(Lab5$`Y^2`)
yq
n = 10


A = matrix( 
c(n,x,x,xq),
nrow = 2,
ncol = 2 ,
byrow = TRUE
)
print(A)

B = matrix( 
  c(y,xy),
  nrow = 2,
  ncol = 1 ,
  byrow = TRUE
)
print(B)

x <-solve(A, B)
a = x[1]
a
b = x[2]
b

#The equation of least square line is given by Y = a + bX


d <- Lab5$Y-(a + b*Lab5$X)
d
fit <- lm( d~Lab5$Y)
plot(d~Lab5$Y, main ='Y = a + bX',xlab = 'X', ylab = 'Y' )
abline(fit)








#Question 2
#Fit the second order polynomial
#x:1, 2, 3,4
#y:6,11,18,27
n=4
x=c(1.0,2.0,3.0,4.0)
x
y=c(6.0,11.0,18.0,27.0)
y
x2=x^2
x2
x3=x^3
x3
x4=x^4
x4
xy=x*y
xy
x2y=x2*y
x2y
table= data.frame(x,y,x2,x3,x4,xy,x2y)
table

sum_x <- sum(x)
sum_y <- sum(y)
sum_x2 <-sum(x2)
sum_x3<-sum(x3)
sum_x4<-sum(x4)
sum_xy<-sum(xy)
sum_x2y<-sum(x2y)

sum_x
sum_y
sum_x2 
sum_x3
sum_x4
sum_xy
sum_x2y

A = matrix( 
  c(n,sum_x,sum_x2,
    sum_x,sum_x2,sum_x3,
    sum_x2,sum_x3,sum_x4),
  nrow = 3,
  ncol = 3,
  byrow = TRUE
)

A 
B = matrix( 
  c(sum_y,sum_xy,sum_x2y),
  nrow = 3,
  ncol = 1,
  byrow = TRUE
)
B

R1=c(n,sum_x,sum_x2)
R2=c(sum_x,sum_x2,sum_x3)
R3=c( sum_x2,sum_x3,sum_x4)

R1_sum=c(sum_y)
R2_sum=c(sum_xy)
R3_sum=c(sum_x2y)

R1
R2
R3
R1_sum
R2_sum
R3_sum

R1<-R1/4
R1_sum<-R1_sum/4
R1
R1_sum




R2<-R2-10*R1
R2_sum<-R2_sum-10*R1_sum
R2
R2_sum

R3<-R3-30*R1
R3_sum<-R3_sum-30*R1_sum
R3
R3_sum

R2<-R2/5
R2_sum<-R2_sum/5
R2
R2_sum

R3<-R3-25*R2
R3_sum<-R3_sum-25*R2_sum
R3
R3_sum

#Now by using back substitution we can find the values of a1, a2, and a3.
#Here, 4a3 = 4 , a3 = 1
#And, a2 + 5a3 = 7 , a2 = 2
#Also, a1 + 2.5a2 + 7.5a3 = 15.5 , a1 = 3
#Therefore, a1 = 3, a2 = 2, a3 = 1.

#after normalizing

a3=(sum(R3))/R3_sum
a3

a2=(R2_sum)-(5*a3)
a2

a1=15.5-((2.5*a2)+(7.5*a3))
a1

#The Quadratic Equation Becomes

Quad_eq <- substitute(expression(Y = c(X^2) + bX+ a), list(a = a1, b = a2,c=a3))
Quad_eq




