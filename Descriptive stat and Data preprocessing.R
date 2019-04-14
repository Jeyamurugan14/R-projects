getwd()

setwd('C:/Users/murug/Desktop')

                              ############################ Part 1 ############################

########  Q1  ########

a) 27*(38-17)

b) log(147)

c) sqrt((436/12))

########  Q2  ########

a <- seq(5 , 160, by=5)
b <- c(87 : 56 )
d <- a * b

a) # What are the 19th, 20th, and 21st elements of d? 

d[19]
d[20]
d[21]

b) # What are all of the elements of d which are less than 2000?
d[d < 2000]

c) #How many elements of d are greater than 6000?
length(d[d>6000])


########  Q3  ########

(a) sum(d)
(b) mean(d)
    median(d)
(c) standard deviation
    sd(d)
    
    
########  Q4  ########

str(cars)

is.na(cars)  

data1 <- cars
    
head(data1)   

view(cars)


a) hist(data1$dist, ylim = c(0,20),main= "Simple histogram of Cars data",
        xlab ="Distance", ylab = "Frequency",col="steelblue")

b) boxplot(data1$speed, data = data1,
           main = "Boxplot represented of Speed", ylab = "Speed")

c) plot(data1$speed ~ data1$dist, main = "Plot between Speed and distance", 
        xlab ="Distance", ylab = "Speed", pch = 10, col = 'red')
    
    
                          ############################ Part 2 ############################


library(mlbench)
library(ggplot2)
library(corrplot)

data(Glass)
str(Glass)
View(Glass)
unique(Glass$Type)
summary(Glass)

hist(Glass$Na,col="steelblue")

Glass[,1:9]
nrow(Glass[])
ncol(Glass[])
ncol(Glass[,10])

Glass[c(1,3),4:5]
Glass[,1]
(Glass[,-1])
(Glass[-1])[2]

names(Glass[1])
colnames(Glass[,-1])


                                              ########## Part 2-a ############


unique(Glass$Type)
ggplot(Glass,aes(x=Glass$Type))+
  geom_histogram(fill = 'blue',stat="count")

####plot 1

par(mfrow = c(3, 3))
for (i in 1:ncol(Glass[-1])) {
  hist((Glass)[ ,i],
       xlab = names(Glass[i]), 
       main = paste(names((Glass[1:9])[i]), "Histogram"), 
       col="red",
       border = par("fg"))  
}

#####plot 2

pairs(Glass[, -1],pch = 21,
      lower.panel=panel.smooth,
      col="lightseagreen")

#####plot 3

library("PerformanceAnalytics")
chart.Correlation(Glass[1:9], histogram=TRUE, pch=19)


#####plot 4

M <- cor(Glass[1:9])
corrplot(M, method="number",type = 'lower')


                                            ########## Part 2-b ############

install.packages("outliers")
library(outliers)

outlier(Glass$Na)
scores(Glass$Na,prob=0.9)

boxplot(Glass[1])

plot(density(Glass[,2]))

Glassnew <- Glass[,1:9]

####plot 5

par(mfrow = c(3,3))
for (i in 1:ncol(Glassnew)){
  boxplot(Glassnew[,i],
          ylab = names(X[i]), 
          horizontal=T,
          #main = colnames(Glassnew[i]),"Boxplot",
          main = paste(names(Glassnew[i]), "Boxplot"),
          outline = TRUE,
          col="tomato2")
}
          
####plot 6

par(mfrow = c(3,3))
for ( i in 1:ncol(Glass[-1])){
  plot(density(Glassnew[,i]),col="tomato2",
       main = paste(names(Glassnew[i]), "Density"))
}


                                            ########## Part 2-c ############

library(MASS) 
library(caret)

  
install.packages("AppliedPredictiveModeling")

library(AppliedPredictiveModeling)
library(e1071)


summary(Glass)
trans_data <- preProcess(Glass[,1:9],method=c("BoxCox","center","scale"))
print(trans_data)
trans_glass <-predict(trans_data,Glass[,1:9])
summary(trans_glass)


trans_glass$bc$VarIntenCh3
{par(mfrow=c(1, 2))
  plot(Glass$Ca, main="Original", xlab="dist",panel=panel.smooth)
  plot(trans_glass$Ca, main="Centered and Scaled", xlab="dist")}

sample <- BoxCoxTrans(Glass$RI)
sample
mytrans <- predict(sample, Glass$RI)
skewness(mytrans)

  

                            ############################ Part 3 ############################


install.packages('plyr')
library(plyr)
library(dplyr)
library(mlbench)

data("Soybean")
View(Soybean)
str(Soybean)


colnames(Soybean)

unique(Soybean$Class)


nrow(Soybean)
ncol(Soybean)

sum(table(factor(Soybean$Class)))
barplot(table(factor(Soybean$Class)), names.arg = c(unique(Soybean$Class)))
unique(table(factor(Soybean$Class)))
head(subset(Soybean, select = Soybean$leaves))
count(Soybean, 'Soybean$Class')

                                            ########## Part 3-a ############

junk <- Soybean %>%
  group_by(Soybean$Class) %>%
  summarise(count=n())


####plot 7

gg <- ggplot(Soybean, aes(Soybean$Class)) 

gg + geom_bar(fill = 'tomato3')+
  labs(title="Ordered Bar Chart", 
       x = "Class",
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6,size=12))


install.packages("freqdist")
library(freqdist)

freqdist(Soybean$Class)

summary(Soybean[,-1])
sum(is.na(Soybean))
sum(is.na(Soybean$precip))

nearZeroVar(Soybean, names = TRUE, saveMetrics=T)


                                          ########## Part 3-b ############


install.packages("mice")
install.packages("VIM")
install.packages("Amelia")

library(mice)
library(VIM)


####plot 8

aggr(Soybean, prop = c(T, T), bars=T,
     numbers=TRUE, sortVars=T,gap=3)


####plot 9

abc <- md.pattern(Soybean)



                                        ########## Part 3-c ############

 
#using MICE and amelia imputation methods

library(VIM)
library(mice)


tempData2 <- mice(Soybean,m=50,seed=245435)

imputed_Data <- mice(Soybean, m=5, maxit = 50, method = 'pmm', seed = 500,pred=new.pred)

summary(imputed_Data)
  
library(Amelia)
data("Soybean")
amelia_fit <- amelia(Soybean, m=5, parallel = "multicore", noms = "Species")

amelia_fit$imputations[[7]]


#by removing na values and rows
Soybean2 <- na.omit(Soybean)

gg <- ggplot(Soybean2, aes(Soybean2$Class)) 
gg + geom_bar(fill = 'tomato3')+
  labs(title="Cleaned and Ordered Bar Chart", 
       x = "Class",
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6,size=12))

str(Soybean2)
sum(is.na(Soybean2))


