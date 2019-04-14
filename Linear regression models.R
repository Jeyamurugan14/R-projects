
                                    #OR/SYST-568 Applied Predictive Analytics (Spring 2019)

                                          #Assignment 2 - Linear Regression Models

print(getwd())

setwd('C:/Users/murug/Desktop')

data = read.csv('AirfaresData.csv')

# removing '*' from the dataset and converting to data frame
data1 <- sapply(data, gsub, pattern="\\*", replacement="")
data1 <- as.data.frame(data1)

datanew <- data[ ,c(-1:-4,-7,-8,-14,-15)]
datacor <- datanew[,-10]
datacor <- as.data.frame(datacor)


library(ggplot2)
library(mlbench)

View(data1)
str(data)
str(data1)
str(datacor)
str(datanew)
summary(data)
is.null(datanew)
library(MASS)

                                        ########### Part A ###############

####Correlation plot
library("PerformanceAnalytics")
chart.Correlation(datacor, histogram=TRUE, pch=19)

library(corrplot)

M <- cor(datacor)
corrplot(M, method="number",type = 'lower')

###Scatter plot
ggplot(datanew, aes(x = datanew$COUPON,y= datanew$FARE)) + 
  geom_point(fill = "orange", width = 0.7)
ggplot(datanew, aes(x = datanew$NEW,y= datanew$FARE)) + 
  geom_point(fill = "orange", width = 0.7)
ggplot(datanew, aes(x = datanew$HI,y= datanew$FARE)) + 
  geom_point(fill = "orange", width = 0.7)
ggplot(datanew, aes(x = datanew$S_INCOME,y= datanew$FARE)) + 
  geom_point(fill = "orange", width = 0.7)

pairs(datanew, main="Scatterplot Matrix")

#"distance" seems to be the best single predictor of FARE

library(miscset) # install from CRAN if required
ggplotGrid(ncol = 3,
           lapply(c("COUPON", "NEW", "HI", "S_INCOME","E_INCOME","S_POP","E_POP","DISTANCE","PAX","FARE"),
                  function(col) {
                    ggplot(datanew, aes(x = datanew$col,y= datanew$FARE)) + 
                      geom_point(fill = "orange", width = 0.7) + coord_flip()
                  }))

                                          ########### Part B ###############


datanew1 <- data[,(-1:-4)]
str(datanew1)


summaryslot <-aggregate(datanew1$FARE ~ datanew1$SLOT, data = datanew, FUN = mean)
plot(summaryslot)

summaryVacation <-aggregate(datanew1$FARE ~ datanew1$VACATION, data = datanew, FUN = mean)
plot(summaryVacation)

summarygate <-aggregate(datanew1$FARE ~ datanew1$GATE, data = datanew, FUN = mean)
plot(summarygate)

summarySW <-aggregate(datanew1$FARE ~ datanew1$SW, data = datanew, FUN = mean)
plot(summarySW)


  
#SW seems to have the largest difference in the mean values.


                                          ########### Part C ###############

datanew0=datanew1[,c(4,12,14)]
set.seed(12345)

sampledata <- floor(0.6*(nrow(datanew0)))

trainindex <- sample(seq_len(nrow(datanew0)), size = sampledata)

training <- datanew0[trainindex, ]
validation <- datanew0[-trainindex, ]

head(training)
summary(datanew1)
plot(datanew1)

dim(training)


trainfitz<-lm(FARE ~ DISTANCE + SW, data=training)

predict1<-predict(trainfitz,validation)


summary(trainfitz)
AIC(trainfitz)
BIC(trainfitz)
fitsummary = summary(trainfitz)
fitsummary$r.squared
fitsummary$adj.r.squared


residual<-residuals(trainfitz)
hist(residual,breaks=20)

qqnorm(residual, ylab="Standardized Residuals", xlab="Normal Scores", main="Residual") 
qqline(residual)

install.packages("metrics")
library(Metrics)

mse(validation$FARE,predict1)

plot(fitted(trainfitz), residuals(trainfitz),lines(smooth.spline(fitted(trainfitz), residuals(trainfitz))))

#mean((datanew1$FARE - predict.lm(trainval, datanew1)) ^ 2)
#mean((datanew1$FARE-predict(trainval, datanew1))[-sampledata]^2)

library(olsrr)
ols_plot_resid_qq(trainfitz)
ols_test_normality(trainfitz)
ols_plot_resid_fit(trainfitz)

par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(trainfitz)

                                              ########### Part D ###############

row<-nrow(datanew1)

sampledata <- floor(0.6*(nrow(datanew1)))

set.seed(12345)

trainindex <- sample(seq_len(nrow(datanew1)), size = sampledata)

training3 <- datanew1[trainindex, ]
validation3 <- datanew1[-trainindex, ]

trainfit2<-lm(FARE ~ ., data=training3)

backward<-step(trainfit2, direction='backward')
summary(backward)
BIC(backward)
fitsummary = summary(backward)
fitsummary$r.squared
fitsummary$adj.r.squared


predict2 <- predict(backward,validation3)

library(Metrics)

mse(validation3$FARE,predict2)

plot(fitted(backward), residuals(backward),lines(smooth.spline(fitted(backward), residuals(backward))))


                                              ########### Part E ###############


#Explained in PDF#




