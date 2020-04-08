library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

#data<- read.csv("wbl.csv",header = T)
data<- read.csv("wbl.csv")
str(data)
xtabs(~gdp+wbl_index, data= data)
data$wbl_index<- as.factor(data$wbl_index)
data$gdp<-as.factor(data$gdp)

#visualization
paris.panels(data[-1])
data %>%
         ggplot(aes(x=gdp, y=literacy, fill= gdp))+
         geom_boxplot()+
         ggtitle("Box Plot")

data%>% ggplot(aes(x=literacy, fill= gdp))+
        geom_density(alpha=0.8, color='black')+
        ggtitle("Density Plot")

# data partition
set.seed(1234)
ind<-sample(2, nrow(data), replace = T, prob = c(0.8,0.2))
train<-data[ind==1,]
test<- data[ind==2,]

#naive bayes model
model<-naive_bayes(gdp~., data=train, usekernel=T)
model

train %>%
         filter(gdp =="1")%>%
         summarise(mean(business),sd(business))

plot(model)

#predict
p<- predict(model, train, type='prob')
head(cbind(p, train))

#confusion matrix -train data
p1<- predict(model, train)
(tab1<-table(p1, train$admit))
1 - sum(diag(tab1))/sum(tab1)

#confusion matrix-test data
p2<-predict(model, test)
(tab2<-table(p2, test$admit))
1- sum(diag(tab2))/sum(tab2)
