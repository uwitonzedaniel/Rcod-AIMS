data_purchase_behaviour <- read.csv("data_purchase_behaviour.csv")
View(data_purchase_behaviour)
 purchase<-data_purchase_behaviour$Purchase
 user_number<-data_purchase_behaviour$User_ID
 gender<-data_purchase_behaviour$Gender
 city_category<-data_purchase_behaviour$City_Category
 stay_city<-data_purchase_behaviour$Stay_In_Current_City_Years
 marital_status<-data_purchase_behaviour$Marital_Status
 age<-data_purchase_behaviour$Age_num
 #1)Descriptive analysis of the variables in this dataset.
 #analyzing Purchase
summary(purchase)
var(purchase)
sd(purchase)
#a)analyzing Gender
length(gender)
summary(gender)
table(gender)
#b)analyzing City_Category
summary(city_category)
table(city_category)
#c)analyzing Stay_In_Current_City_Years
summary(stay_city)
var(stay_city)
sd(stay_city)
#d)analyzing Marital_Status
table(marital_status)
#e)analyzing Age_num
summary(age)
var(age)
sd(age)
boxplot(age)
summary(data_purchase_behaviour)
hist(data_purchase_behaviour$Purchase)
#2. building the model
#independent variable is purchase
model1<-lm(purchase~gender+city_category+stay_city+marital_status+age,data =data_purchase_behaviour) 
model1
summary(model1) 
model2<-lm(purchase~gender+city_category+stay_city+age,data =data_purchase_behaviour)
model2
summary(model2) 

model3<-lm(purchase~gender+city_category+age,data =data_purchase_behaviour)
model3
summary(model3)

#3) Interpret your final model
#In latex

#4) checking if all the assumptions about the model are ok
#linearly association
plot(model3,which=1)
#check assumption(homoskedastic  )
plot(model3,which=3)
#check assumption5(normality)
plot(model3,which=2)
#check assumption(potential ouliers)
plot(model3,which=4)
#The drawings
par(mfrow=c(2,2))
plot(model3,which=1,col=5)
plot(model3,which=3)
plot(model3,which=2)
plot(model3,which=4,col=6)

#5)ways use final model to help the company improve theirsales
#In latex
# 6)  possible ways of improving your model.
#In latex
#7) Compare your final model to the model built on Gender and Age only.
model4<-lm(purchase~gender+age,data =data_purchase_behaviour)
summary(model4)
anova(model3,model4)


#anova(model1,model2)
