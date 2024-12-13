##Case study 1 9/3/24
#loading
data("iris")

install.packages("ggplot2")
library(ggplot2)
#summary
summary(iris)

#store mean as named object
petal_length_mean <- mean(iris$Petal.Length)

#histogram petal length
hist(iris$Petal.Length)

ggplot() +
geom_histogram(aes(iris$Petal.Length))
