library(ggplot2)
library(gganimate)
library(caTools)
library(ISLR)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
COVID<-read.csv("country_wise_latest.csv")
COVID

#Interactive plot
p <- ggplot(
  COVID, 
  aes(x = COVID$Deaths, y=COVID$Active, size = COVID$Deaths, colour = COVID$Country.Region)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "Deaths", y = "Active")

p+transition_time(COVID$Recovered)+labs(title = "Recoveries: {frame_time}")

#Corrgram and Corrplot
c.cols<-sapply(COVID,is.numeric)
cordata<-cor(COVID[,c.cols])
print(corrplot(cordata,method="color"))
corrgram(COVID)

final<-COVID[,-c(9,10,11,14)]

c.cols<-sapply(final,is.numeric)
cordata<-cor(final[,c.cols])
print(corrplot(cordata,method="color"))
corrgram(final)


# Linear Regression / Model Training
is.na(final)<-sapply(final,is.infinite)
na.omit(final)
set.seed(101)
p1<-sample.split(final$Deaths,SplitRatio = 0.7)
train<-subset(final,p1==TRUE)
test<-subset(final,p1==FALSE)
pmodel<-lm(Deaths~Confirmed+Active+Recovered,data=train)

summary(pmodel)
layout(matrix(c(1,2,3,4),2,2))
plot(pmodel)


#Residual Fitting
res<-residuals(pmodel)
res<-as.data.frame(res)
ggplot(res,aes(res))+geom_histogram(fill="red")

#Prediction
lp<-predict(pmodel,test)
print(lp)
lres<-cbind(lp,test$Deaths)
colnames(lres)<-c("Predicted","Actual")
print(lres)


