#question 1
#a
vector_a <- c(1,2,3,4,5)

#b
"mindy" <-c(12)

#c
matrix_c <- matrix  (1:6, nrow=2, byrow=TRUE)

#d
matrix_d <- matrix(1:6, nrow=2)

#e
matrix_e <- matrix(1, nrow=10, ncol=10)

#f
vector_f <- c("THIS", "IS", "A", "VECTOR")

#g
add <- function (x,y,z) {x+y+z}

#h
h <- function(x) {if (x<=10) {print("yes")} else {print("no")} }

#i
g <- rnorm(1000, 10, 1)

#j
y <- rnorm (1000,5,.5)

#k
x = NULL
for(i in 1:1000)
{x[i]<- mean(sample(x=g, size=10, replace=TRUE))}
x

#l
regression <- lm(y~x)
summary (regression)
#This shows an intercept of 5.49652 and a B1 hat of -.04820, a negative relationship

#Question 2
#a 
library(readr)
pums_chicago <- read.csv ("Downloads/pums_chicago.csv")

#b
#there are 50000 observations of 204 variables

#c
avgincome <- mean(pums_chicago$PINCP[!is.na(pums_chicago$PINCP)])
#avg income is $38247.62

#d
pums_chicago$PINCP_log <- log(pums_chicago$PINCP)
#NaNs were produced because log (0) is undefined 

#e
GRAD.DUMMY <- ifelse(pums_chicago$SCHL>12,
                     "grad",
                     "no grad")

#f 
pums_chicago$SERIALNO <- NULL

#g
write.csv(pums_chicago, file = "pums_chicago.csv")

#h
under_16 <- pums_chicago[is.na(pums_chicago$ESR), ]
employed <-  pums_chicago[pums_chicago$ESR == 1 | pums_chicago$ESR == 2, ]
unemployed <- pums_chicago[pums_chicago$ESR == 3, ]
armed_forces <- pums_chicago[pums_chicago$ESR == 4 | pums_chicago$ESR == 5, ]
not_in_the_labor_force <- pums_chicago[pums_chicago$ESR == 6, ]

#i
employed_af <- rbind(employed, armed_forces)

#j
data.frame(employed_af$AGEP,employed_af$RAC1P,employed_af$PINCP_LOG)

#ki
mean_travel_time <- mean(pums_chicago$JWMNP[!is.na(pums_chicago$JWMNP)])
#The mean travel time is 34.8388 minutes
median_travel_time <- median(pums_chicago$JWMNP[!is.na(pums_chicago$JWMNP)])
#The median travel time is 30
eightieth_percentile <- quantile(pums_chicago$JWMNP[!is.na(pums_chicago$JWMNP)], 0.80)
#The 80th percentile is 45 minutes

#kii
notna <- pums_chicago[!is.na(pums_chicago$JWMNP) & !is.na(pums_chicago$WAGP), ]
correlation <- cor(notna$JWMNP, notna$WAGP)
#the correlation is equal to -0.04205232

#kiii
plot(pums_chicago$AGEP, pums_chicago$PINCP_LOG)

#kiv
#exported the scatter plot to my 311_2 folder

#kv
crosstab <- table(pums_chicago$ESR, pums_chicago$RAC1P)

#kvi
Wage_and_Hours <- lm(pums_chicago$WAGP~pums_chicago$WKHP)

#kvii
residuals <- residuals(Wage_and_Hours)
estimation <- fitted.values(Wage_and_Hours)
comparison <- plot(residuals,estimation)
#This plot shows heteroskedasticity

#l
data(mtcars)

#li
regression_cars <-lm(mtcars$mpg~mtcars$wt)
summary (regression_cars)

#lii
automatic <- mtcars[mtcars$am == 0, ]
manual <- mtcars[mtcars$am == 1, ]
regression_automatic <- lm(automatic$mpg ~ automatic$wt)
regression_manual <- lm(manual$mpg ~ manual$wt)

#liii
loghorsepower <- log(mtcars$hp)
regression_horsepower <- lm(mtcars$mpg~loghorsepower)

#m
install.packages("ggplot2")
library("ggplot2")

#mi
ggplot(mtcars, aes(x=wt, y=mpg)+geom_point(

#mii
color = ifelse(mtcars$am==1, "blue", "orange"),

#miii
shape = mtcars$gear) +

#miv
labs(x = "Weight", y = "Miles Per Gallon")  

#v
theme_minimal()


















