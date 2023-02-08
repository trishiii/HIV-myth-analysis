data<-HIV_AIDS_Data_set_STA_471_2_0_1_
library(ggplot2)
library(tidyverse)
library(scales) 

set.seed(123)
index <- sample(1:nrow(data),size=0.8*nrow(data))
train<-data[index,]
test<-data[-index,]


#one qualitative
ggplot(train, aes(x=(Residence)) )+
  geom_bar( fill=rgb(0.1,0.4,0.5) )


ggplot(train, aes(x=Religion)) +
  geom_bar( fill=("light blue") )+theme(axis.text.x = element_text(angle = 90))

ggplot(train, aes(x=as.factor(Y1)))+
  geom_bar( fill=("skyblue") )+labs(x="People can get HIV virus from mosquito
bites(Y1)")


ggplot(train, aes(x=(Ethnicity ))) +
  geom_bar( fill=("light green") )



ggplot(train, aes(x=Age_group)) +
  geom_bar( fill=("light blue") )+theme(axis.text.x = element_text(angle = 90))+labs(x="Age group")

ggplot(train, aes(x=Highest_education_qualification)) +
  geom_bar( fill=("light blue") )+labs(x="Highest education qualification")+theme(axis.text.x = element_text(angle = 90))


ggplot(train, aes(x=Current_marital_state)) +
  geom_bar( fill=("light blue") )+labs(x="Current Marital state")



ggplot(train, aes(x=Fequency_of_listening_to_radio)) +
  geom_bar( fill=("light blue") )+labs(x="Frequency of watching television")


ggplot(train, aes(x=Frequency_of_all_media_combined)) +
  geom_bar( fill=("light blue"))+labs(x="Frequency of all media combined")

ggplot(train, aes(x=Working_status)) +
  geom_bar( fill=("light blue"))+labs(x="working status")


ggplot(train, aes(x=Wealth_index)) +
  geom_bar( fill=("light blue"))+labs(x="Wealth index")


#two qualitatives

ggplot(train, aes(fill=as.factor(Y1), x=Region)) + 
  geom_bar(position="dodge")+scale_fill_discrete(name = "Y1")+theme(axis.text.x = element_text(angle = 90))


ggplot(train, aes(fill=as.factor(Y1), x=Religion)) + 
  geom_bar(position="dodge")+scale_fill_discrete(name = "Y1")


ggplot(train, aes(fill=as.factor(Y1), x=Ethnicity)) + 
  geom_bar(position="dodge")+scale_fill_discrete(name = "Y1")

ggplot(train, aes(fill=as.factor(Y1), x=Age_group)) + 
  geom_bar(position="dodge")+scale_fill_discrete(name = "Y1")

ggplot(train, aes(fill=as.factor(Y1), x=Ethnicity)) + 
  geom_bar(position="fill")+scale_fill_discrete(name = "People can get HIV virus from mosquito
bites(Y1)")

ggplot(train, aes(fill=as.factor(Y1), x=Highest_education_qualification)) + 
  geom_bar(position="dodge")+scale_fill_discrete(name = "Y1")+theme(axis.text.x = element_text(angle = 90))

ggplot(train, aes(fill=as.factor(Y1), x=Marital_state)) + 
  geom_bar(position="dodge")+scale_fill_discrete(name = "Y1")

ggplot(train, aes(fill=as.factor(Y1), x=Current_marital_state)) + 
  geom_bar(position="dodge")+scale_fill_discrete(name = "People can get HIV virus from mosquito
bites(Y1)")

ggplot(train, aes(fill=as.factor(Y1), x=Frequency_of_reading_newspapper)) + 
  geom_bar(position="dodge")+scale_fill_discrete(name = "Y1)")

ggplot(train, aes(fill=as.factor(Y1), x=Frequency_of_reading_newspapper)) + 
  geom_bar(position="dodge")+scale_fill_discrete(name = "Y1")+labs(x="Frequency of reading newspaper")

ggplot(train, aes(fill=as.factor(Y1), x=Frequency_of_all_media_combined) )+ 
  geom_bar(position="dodge")+scale_fill_discrete(name = "Y1")

ggplot(train, aes(fill=as.factor(Y1), x=Wealth_index)) + 
  geom_bar(position="dodge")+scale_fill_discrete(name = "Y1")

ggplot(train, aes(fill=as.factor(Y1), x=Working_status)) + 
  geom_bar(position="dodge")+scale_fill_discrete(name = "Y1")
#chisq test

chisq.test(data$Y1,data$Frequency_of_reading_newspapper,correct = FALSE)

