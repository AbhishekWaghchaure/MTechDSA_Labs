#Q2) Find central moments and coefficient of skewness and kurtosis for the given data points
library(readxl)
data_q2 <- read_excel("Lab3.xlsx")
data_q2

add_f = sum(data_q2$f)

x_bar = sum(data_q2$f*data_q2$x)/add_f
message("x_bar: ", x_bar)

for ( r in 1:4) {
  moment = sum(data_q2$f*(data_q2$x - x_bar)^r)/ add_f
  message("r: ", r)
  message("moment: ", moment, "\n")
}

