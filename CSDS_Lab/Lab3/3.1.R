#Q1)The first four moments about the working mean 5 of a distribution are -4, 22, -117 and 560. Calculate the first four moments about mean. Comment on the skewness & Kurtosis of the distribution.

mean = 5

m1 = readline(prompt ="Enter the first moment: ")
m1 = as.numeric(m1)
m2 = readline(prompt ="Enter the Second moment: ")
m2 = as.numeric(m2)
m3 = readline(prompt ="Enter the Third moment: ")
m3 = as.numeric(m3)
m4 = readline(prompt ="Enter the fourth moment: ")
m4 = as.numeric(m4)

n1 = 0
n2 = m2-m1^2
n3 = m3-(3*m1*m2)+(2*m1^3)
n4 = m4-(4*m3*m1)+(6*m2*m1^2)-(3*m1^4)

message("First Moment about mean : ",n1)
message("Second Moment about mean : ",n2 )
message("Third Moment about mean : ",n3 )
message("Fourth Moment about mean : ",n4 )

sk= (n3^2)/(n2^3)
kr = n4/n2^2

skw <- c(n1,n2,n3,n4)

if (sk < 0) {
  print("The graph is said to be negatively skewed with the majority of data values greater than mean.")
  message("skewness: ", sk)
} else if (sk == 0) {
  print("The graph is said to be symmetric and data is normally distributed.")
  message("skewness: ", sk)
} else {
  print("The graph is said to be positively skewed with the majority of data values less than mean.")
  message("skewness: ",sk)
}

hist(skw)

if (kr > 3) {
  print("The curve is leptokurtic")
  message("Kurtosis: ", kr)
} else if (kr == 3) {
  print("The curve is Mesokurtic")
  message("Kurtosis: ", kr)
} else {
  print("The curve is Platykurtic")
  message("Kurtosis: ", kr)
}
