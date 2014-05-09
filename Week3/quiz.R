library(sqldf)
library(datasets)
data(iris)
#Mean of Sepal.Length for the spices virginica
#select avg(sepal.length) from iris where Species=="virginica"
mean(iris[iris$Species=="virginica","Sepal.Length"])
#returns a vector of the means of the variables 'Sepal_Length', 'Sepal_Width', 'Petal.Length', and 'Petal.Width'?
apply(iris[, 1:4], 2, mean) # 1: Hang, 2:Cot

library(datasets)
data(mtcars)
#calculate the average miles per gallon (mpg) by number of cylinders in the car (cyl)?
sapply(split(mtcars$mpg, mtcars$cyl), mean)
#the absolute difference between the average horsepower of 4-cylinder cars and the average horsepower of 8-cylinder cars
avgHpGroupByCyl=sqldf("select cyl,avg(hp) from mtcars group by cyl")
abs(avgHpGroupByCyl[3,2]-avgHpGroupByCyl[1,2])

y = rnorm(100)
x1 = rpois(100,0.2)
x2 = rbinom(100,10,0.2)
Rprof()
fit <- lm(y ~ x1 + x2)
Rprof(NULL)