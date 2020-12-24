death_rates <- read.csv("death_rates.csv")
View(death_rates)
# 1)Destriptive analysis of the dataset
# let x be age,y be Deathrate and z be the sex
x<-death_rates$Age
x
y<-death_rates$DeathRate
y
z<-death_rates$Sex
# a) sex
length(z)
z
maledata<-death_rates[1:31,]
maledata
femaledata<-death_rates[32:62,]
femaledata
#b)Age 
n<-length(x)
n
x_bar<-sum(x)/n
x_bar
var_x<-sum((x-x_bar)^2)/(n-1)
var_x
sd_x<-(var_x)^0.5
sd_x
max(x)
min(x)
median(x)
summary(x)
hist(Age)
boxplot(x)

#c)DeathRate 
n<-length(y)
n
y_bar<-sum(y)/n
y_bar
var_y<-sum((y-y_bar)^2)/(n-1)
var_y
sd_y<-(var_y)^0.5
sd_y
max(y)
min(y)
median(y)
summary(y)
hist(y )
boxplot(y)

# we can find the covariance and correlation
cov_x_y<-sum((x-x_bar)*(y-y_bar))/(n-1)
cov_x_y
cor_x_y<-cov_x_y/(var_x*var_y)^0.5
cor_x_y
plot(x,y)
#2)splitting the dataset into males and females

maledata<-death_rates[1:31,]
maledata
femaledata<-death_rates[32:62,]
femaledata
# males only
#1. age
x_1<-maledata$Age
x_1
y_1<-maledata$DeathRate
y_1



m<-length(x_1)
m
x_1_bar<-sum(x_1/m)
x_1_bar
var_x_1<-sum(((x_1-x_1_bar)^2))/(m-1)
var_x_1
sd_x_1<-(var_x_1)^0.5
sd_x_1
max(x_1)
min(x_1)
median(x_1)
summary(x_1)
hist(y_1)
boxplot(y_1)



#2.DeathRate


m<-length(y_1)
m
y_1_bar<-sum(y_1/m)
y_1_bar
var_y_1<-sum(((y_1-y_1_bar)^2))/(m-1)
var_y_1
sd_y_1<-(var_y_1)^0.5
sd_y_1
max(y_1)
min(y_1)
median(y_1)
summary(y_1)
hist(y_1)
boxplot(y_1)


# we can find the covariance and correlation
cov_x_1_y_1<-sum((x_1-x_1_bar)*(y_1-y_1_bar))/(m-1)
cov_x_1_y_1
cor_x_1_y_1<-cov_x_1_y_1/(var_x_1*var_y_1)^0.5
cor_x_1_y_1
plot(x_1,y_1)
cor(x_1,y_1)



# females only
#1. age
x_2<-femaledata$Age
x_2
y_2<-femaledata$DeathRate
y_2



p<-length(x_2)
p
x_2_bar<-sum(x_2)/p
x_2_bar
var_x_2<-sum(((x_2-x_2_bar)^2))/(p-1)
var_x_2
sd_x_2<-(var_x_2)^0.5
sd_x_2
max(x_2)
min(x_2)
median(x_2)
summary(x_2)
hist(y_2)
boxplot(y_2)



#2.DeathRate


p<-length(y_2)
p
y_2_bar<-sum(y_2/m)
y_2_bar
var_y_2<-sum(((y_2-y_2_bar)^2))/(p-1)
var_y_2
sd_y_2<-(var_y_2)^0.5
sd_y_2
max(y_2)
min(y_2)
median(y_2)
summary(y_2)
hist(y_2)
boxplot(y_2)


# we can find the covariance and correlation
cov_x_2_y_2<-sum((x_2-x_2_bar)*(y_2-y_2_bar))/(p-1)
cov_x_2_y_2
cor_x_2_y_2<-cov_x_2_y_2/(var_x_2*var_y_2)^0.5
cor_x_2_y_2
plot(x_2,y_2)

#3)simple linear regression model to explain the death rates of females
x_22<-femaledata$Age
x_22
y_22<-femaledata$DeathRate
y_22



p<-length(x_22)
p
x_22_bar<-sum(x_22)/p
y_22_bar<-sum(y_22)/p

beta1_hat<-sum((x_22-x_22_bar)*y_22)/sum((x_22-x_22_bar)^2)
beta1_hat

beta0_hat<-y_22_bar-beta1_hat*x_22_bar
beta0_hat

 y_hat<-beta0_hat+beta1_hat*x_22
 y_hat
#4) to predict the death rate of females aged 51.
 x_22<-51
 y_hat<-beta0_hat+beta1_hat*x_22
 y_hat
 

