data<-HIV_AIDS_Data_set_STA_471_2_0_2_
library(MASS)
library(ggplot2)
library(tidyverse)
library(scales) 
library(broom)


#converting to factor
data$Y1<-as.factor(data$Y1)
data$Residence<-as.factor(data$Residence)
data$Region<-as.factor(data$Region)
data$Religion<-as.factor(data$Religion)
data$Ethnicity<-as.factor(data$Ethnicity)
data$`Age group`<-as.factor(data$`Age group`)
data$`Marital state`<-as.factor(data$`Marital state`)
data$`Current marital state`<-as.factor(data$`Current marital state`)
data$`Highest education qualification`<-as.factor(data$`Highest education qualification`)
data$`Frequency of reading newspapper`<-as.factor(data$`Frequency of reading newspapper`)
data$`Frequency of watching televisio`<-as.factor(data$`Frequency of watching televisio`)
data$`Frequency of all media combined`<-as.factor(data$`Frequency of all media combined`)
data$`Frequency of listening to radio`<-as.factor(data$`Frequency of listening to radio`)
data$`Have you ever given birth`<-as.factor(data$`Have you ever given birth`)
data$`Current pregnancy status`<-as.factor(data$`Current pregnancy status`)
data$`Working status`<-as.factor(data$`Working status`)
data$`Welth index`<-as.factor(data$`Welth index`)


set.seed(123)
index <- sample(1:nrow(data),size=0.8*nrow(data))
train<-data[index,]
test<-data[-index,]

class(train$Region)

full<-glm(Y1~.,family=binomial,data=train)
summary(full)
null<-glm(Y1~1,family=binomial,data=train)
null


backward<-stepAIC(full,direction="backward",trace=FALSE)
backward$anova
summary(backward)

backwards = step(full) # Backwards selection is the default
summary(backwards)

model <- glm(Y1 ~ Region + Ethnicity + `Age group` + `Current marital state` + 
              `Highest education qualification` + `Frequency of reading newspapper` + 
              `Frequency of listening to radio` + `Welth index`, family = binomial, 
            data = train)
summary(model)

library(car)
vif(model)
influenceIndexPlot(model)
influencePlot(model)
model1 <- update(model,subset=c(-8300))
compareCoefs(model,model1)
plot(cooks.distance(model))

#predictions

library(InformationValue)


#find optimal cutoff probability to use to maximize accuracy
predicted <- predict(model, test, type="response")
optimal <- optimalCutoff(test$Y1, predicted)
optimal

#residual analysis


p<-misClassError(test$Y1, predicted,threshold = optimal)
Accuracy<- 1-p
Accuracy


#infmodel
#find optimal cutoff probability to use to maximize accuracy
predicted <- predict(model1, test, type="response")
optimal1 <- optimalCutoff(test$Y1, predicted)
optimal1

p1<-misClassError(test$Y1, predicted,threshold = optimal1)
Accuracy<- 1-p1
Accuracy


res<- resid(model)
plot(fitted(model),res)
abline(0,0)

?misClassError
#GOF
library(ResourceSelection)
fitted_values<-fitted(model)
hoslem.test(train$Y1,fitted_values)


#influential cases
library(car)
plot(j, type = "h", lwd = 2, col = "blue")
cookd(best)
?cookd

cooks.distance(best, plot.it = FALSE, coefs, identify)
j<- cooks.distance(model, plot.it=TRUE, col="red", lty=1, lwd=1, col.lab="blue",
               col.axis="blue", col.main="black", cex=0.8)
plot((j))



library(ggplot2)
ggplot(train, aes(fill=as.factor(Y1), x=Region)) + 
  geom_bar(position="dodge")+scale_fill_discrete(name = "People can get HIV virus from mosquito
                                                bites(Y1)")
ggplot(train, aes(x=Highest education qualification)) +
  geom_bar( fill=("light blue") )+labs(x="Highest education qualification")

chisq.test(data$Y1,data$Religion,correct = FALSE)




  model.data<-augment(model)
  ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = as.factor(Y1)), alpha = 0.8) +
  theme_bw()
