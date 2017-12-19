#########################
#Variable Exploration
#########################
library(haven)
Society_Past_Cruisers <- read_sav("C:/Users/ttlus/Desktop/Society Past Cruisers.sav")
Past_Cruisers<- read_sav("C:/Users/ttlus/Desktop/Society Past Cruisers_file_D02172017.sav")

state<-as.data.frame(table(Past_Cruisers$STATE))
state$prop<-state$Freq/sum(state$Freq)
#12583 viable obs
#CA--30% 
#FL--20%

#Added year pattern
addyear<-as.data.frame(table(Society_Past_Cruisers$addyear))
sum(addyear$Freq)
plot(addyear,xlab = 'Added Year')
#12583 viable obs
#Time series

#last purchase year
Society_Past_Cruisers$lastyear<-substr(Society_Past_Cruisers$LT_DT,1,4)
plot(table(Society_Past_Cruisers$lastyear),xlab = 'Last Year of purchasing',ylab = 'Frequency')

#age pattern & KBM age prediction accuracy
C<-c(6,66:71)
age<-as.data.frame(table(Society_Past_Cruisers$AGE))
validage<-Society_Past_Cruisers[is.na(Society_Past_Cruisers$AGE)==F,C]
sum(age$Freq)
plot(age)
#only 1068 viable obs
# increasing trend:41-64
# (slightly) decreasing trend:65-80

#Accuracy of estimated age
validage$group<-cut(validage$AGE,c(18,34,44,54,64,74,120),
                    labels=c("age1","age2","age3","age4","age5","age6"))
AGE1<-validage[validage$group=="age1",]
nrow(AGE1[AGE1$age1==1,])/nrow(AGE1)

AGE2<-validage[validage$group=="age2",]
nrow(AGE2[AGE2$age2==1,])/nrow(AGE2)

AGE3<-validage[validage$group=="age3",]
nrow(AGE3[AGE3$age3==1,])/nrow(AGE3)

AGE4<-validage[validage$group=="age4",]
nrow(AGE4[AGE4$age4==1,])/nrow(AGE4)

AGE5<-validage[validage$group=="age5",]
nrow(AGE5[AGE5$age5==1,])/nrow(AGE5)

AGE6<-validage[validage$group=="age6",]
nrow(AGE6[AGE6$age6==1,])/nrow(AGE6)

#Time interval
Society_Past_Cruisers$interval<-
  Society_Past_Cruisers$LT_DT - Society_Past_Cruisers$ADD_DA
Past_Cruisers$interval<-
  Past_Cruisers$LT_DT -Past_Cruisers$ADD_DA

#One-time customer with LT_DT before 2015
oldcustomer<-Society_Past_Cruisers[Society_Past_Cruisers$addyear<2015,]
onetimeoldcustomer<-oldcustomer[oldcustomer$interval==0,]
nrow(onetimeoldcustomer)/nrow(oldcustomer)
#0.6143665

#One-time customer with LT_DT after 2015
newcustomer<-Society_Past_Cruisers[Society_Past_Cruisers$addyear>=2015,]
onetimenewcustomer<-newcustomer[newcustomer$interval==0,]
nrow(onetimenewcustomer)/nrow(newcustomer)
#0.9080347

#What's the proportion of returning customers each year? changes?
#This is the idea I think of this morning and I don't have a chance to take a closer look at it
#But this may be an interesting point of view to look at it.

interval<- as.data.frame(Society_Past_Cruisers$LT_DT - Society_Past_Cruisers$ADD_DA)
interval$adddate<-Society_Past_Cruisers$ADD_DA
interval$LT_DT<-Society_Past_Cruisers$LT_DT
colnames(interval)<-c("interval",'add date','last date')

quantile(interval$interval,na.rm = T)
return_interval<-interval[interval$interval!=0,]
quantile(as.numeric(return_interval$interval),na.rm = T)

########################################################
# Subset & look at Customers from 2015-2017 (responding)
########################################################
Past_Cruisers<- read_sav("C:/Users/ttlus/Desktop/Society Past Cruisers_file_D02172017.sav")
Past_Cruisers$addyear<-substr(Past_Cruisers$ADD_DA,1,4)
col_select<-c(3,6,9, 12, 13, 14:22, 26:54)
new<-Past_Cruisers[Past_Cruisers$addyear>2014,col_select]
all<-Past_Cruisers[,col_select]
#15234 new customers among 123583 customers

#newvip<- as.data.frame(table(new$VIP))
#newpocean<- as.data.frame(table(new$POCEAN))
#???

new1516<- as.data.frame(table(new$R_1516))
all1516<- as.data.frame(table(all$R_1516))
#newTTL<-as.data.frame(table(new$R_TTL))

#new_rev_1516<-as.numeric(as.character(new1516$Var1))
#all_rev_1516<-as.numeric(as.character(all1516$Var1))
#hist(new_rev_1516,breaks = 100, 
#main = "Distribution of Rev_1516", xlab = "Revenue 15 & 16")
#median(new_rev_1516)
#8437

#new_rev_ttl<-as.numeric(as.character(newTTL$Var1))
#hist(new_rev_ttl,breaks = 100, 
#     main = "Distribution of Rev_Total", xlab = "Revenue Total")
#median(new_rev_ttl)
#7595

#Maybe using rev_1516 makes more sense
#
#quantile(new_rev_1516,c(1/3, 2/3, 1),na.rm = T)
#33.33333% 66.66667%      100% 
#  6200     11071        109268

#quantile(all_rev_1516,c(1/3, 2/3, 1),na.rm = T)
#33.33333%  66.66667%       100% 
# 7869.333  15394.333 450834.000 

#quantile(new_rev_1516,na.rm = T)
#8437 - MEDIUM

#quantile(all_rev_1516,na.rm = T)
#10821 -medium

#Group them into 3 levels
new$rev_1516_group<-cut(new$R_1516,c(100,6200,11071,109268),
                        labels=c("L","M","H"))
all$rev_1516_group<-cut(all$R_1516,c(100,7869,15394,450834),
                        labels=c("L","M","H"))

#Also try: Group them into 2 levels
new$rev_1516_group_2<-cut(new$R_1516,c(100,8437,109268),
                          labels=c("L","H"))
all$rev_1516_group_2<-cut(all$R_1516,c(1,10822,500000),
                          labels=c("L","H"))
nrow(all[all$rev_1516_group_2=='H',])

##########MB on revenue 1516########
#library(ggplot2)
#ggplot(new, aes(MB2G, MB2S, color = rev_1516_group),na.rm=T) + geom_point()
#ggplot(new, aes(MB2G, MB2S, color = rev_1516_group_2)) + geom_point()
#library(psych)
#hist(new$MB2G,breaks = 10)
#hist(new$MB2S,breaks = 25)

#Compare l/m/h groups of revenue
mb2g_6<-all[which(all$MB2G == 6 ),]
mb2g_7<-all[which(all$MB2G == 7 ),]
tab<- cbind(as.data.frame(prop.table(table(mb2g_6$rev_1516_group_2))),
            as.data.frame(prop.table(table(mb2g_7$rev_1516_group_2)))) 
prop.table(table(all$rev_1516_group_2,all$MB2G),2)
prop.test(table(all$MB2G,all$rev_1516_group_2), correct=FALSE) 


#Proportion of revenue in each MB segment
MB<-all[is.na(all$MB2G)==F,]
MBrev<-MB[is.na(MB$R_1516)==F,]
contrib <- NA
for (i in 1:8){
  contrib[i]<- sum(MBrev[which(MBrev$MB2G==i),]$R_1516, na.rm = T)/sum(MBrev$R_1516, na.rm = T)
  print(contrib[i])
}
table(MBrev$MB2G)
prop.table(table(MBrev$MB2G))
prop.table(table(MBrev$MB2G,MBrev$rev_1516_group_2))

#Proportion of revenue in each MB segment --new cruisers
MB<-new[is.na(new$MB2G)==F,]
MBrev<-MB[is.na(MB$R_1516)==F,]
table(MBrev$MB2G)
prop.table(table(MBrev$MB2G))
contrib <- NA
for (i in 1:8){
  contrib[i]<- sum(MBrev[which(MBrev$MB2G==i),]$R_1516, na.rm = T)/sum(MBrev$R_1516, na.rm = T)
  print(contrib[i])
}

prop.table(table(MBrev$MB2G,MBrev$rev_1516_group_2))
#Try to use randomforest to predict rev_1516
#Before we build our model, let's separate our data into testing and training sets so that we can test the prediction accuracy later.
#change new to all here




###########################################
# Random Forest: MotiveMix and revenue
###########################################

selectedcol<-c(44:45,32:34,36:41)
data<- as.matrix(na.omit(all[,selectedcol])) 

samp <- sample(nrow(data), 0.8 * nrow(data))
train <- data[samp, ]
test <- data[-samp, ]


set.seed(321)
library(randomForest)
#Model set up
model <- randomForest(rev_1516_group ~ DIMFINO + DIMINF+DIMTECH+DIMCUL+DIMFAM+DIMNOV+
                        DIMREL+DIMCEXP+DIMLEARN,data = train,importance=TRUE)
model_2 <- randomForest(rev_1516_group_2 ~ DIMFINO + DIMINF+DIMTECH+DIMCUL+DIMFAM+DIMNOV+
                          DIMREL+DIMCEXP+DIMLEARN,data = train,importance=TRUE)
#prediction accuracy
pred <- predict(model, newdata = test)
pred_2<- predict(model_2, newdata = test)
table(pred, test[,1])
table(pred_2, test[,2])
accuracy <- sum(pred == test[,1])/length(test[,1])
accuracy_2 <- sum(pred_2 == test[,2])/length(test[,2])
#variable importance
model$importance
varImpPlot(model,sort=TRUE, type = 1)
varImpPlot(model_2,sort=TRUE, type = 1)

#logistic regression
ltrain<-as.data.frame(train)
ltest<-as.data.frame(test)
lmodel <- glm(rev_1516_group_2 ~DIMFINO + DIMINF+DIMTECH+DIMCUL+DIMFAM+DIMNOV+
                DIMREL+DIMCEXP+DIMLEARN,family=binomial(link='logit'),data=ltrain)
summary(lmodel)


library(GoodmanKruskal)
GKtau(new$STATE, new$rev_1516_group)
GKmatrix1 <- GKtauDataframe(data)
plot(GKmatrix1)

GKmatrix1 <- GKtauDataframe(new)
plot(GKmatrix1)


####################################Cruisers RFM##########################
# from cruisers data, the added time and last purchase time
# are the most complete. We can do a time-based analysis based on the time 
##########################################################################
library(haven)
library(gplots)#for visualization
Past_Cruisers<- read_sav("C:/Users/ttlus/Desktop/Society Past Cruisers_file_D02172017.sav")

#Most recent cruiser in this dataset is "2017-01-30"
#max(as.Date(Past_Cruisers$LT_DT),na.rm = T)

####Recency

Past_Cruisers$days_since_LTDT<- as.Date("2017-01-30")- as.Date(Past_Cruisers$LT_DT)
sum(is.na(Past_Cruisers$days_since_LTDT))
hist(as.numeric(Past_Cruisers$days_since_LTDT),w=100)
#quantile(Past_Cruisers$days_since_LTDT,na.rm = T)
#Time differences in days
#0%  25%  50%  75% 100% 
#0  897 2619 4532 8059 

###Frequency, in this case, interval on board
#Cannot get assess to exact frequence of purchasing 
#But typically, customers with long time interval btw add date and last purchase date
#have a higher chance to have higher frequency
#Time interval
Past_Cruisers$interval<-
  Past_Cruisers$LT_DT -Past_Cruisers$ADD_DA
sum(is.na(Past_Cruisers$interval))
sum(Past_Cruisers$interval==0,na.rm = T)
#80398
80398/123582
#Over 65% of customers are one-time customer or 1st time customer

####Monetary
#R_TTL, directly
sum(is.na(Past_Cruisers$R_TTL))
quantile(Past_Cruisers$R_TTL,na.rm = T)
# 0%        25%        50%        75%       100% 
#1.00    5203.80    9158.91   18327.77 4142974.27 

#R_1516

###
RFM<-matrix(0,123583,1)
RFM<-as.data.frame(RFM)

RFM$R_segment <- findInterval(Past_Cruisers$days_since_LTDT, 
                              quantile(Past_Cruisers$days_since_LTDT, c(0.0, 0.25, 0.50, 0.75, 0.9999),na.rm = T))
#make some transformation of recency and get higher scores --> better recency
RFM$Rsegment <- ifelse(RFM$R_segment == 5, 1,
                       ifelse(RFM$R_segment == 4, 2,
                              ifelse(RFM$R_segment == 2, 4, 
                                     ifelse(RFM$R_segment == 1, 5,3))))
RFM$Recency <- ordered(ifelse(RFM$Rsegment == 1, "7332-8059",
                              ifelse(RFM$Rsegment == 2, "4533-7331", 
                                     ifelse(RFM$Rsegment == 3, "2620-4532",
                                            ifelse(RFM$Rsegment == 4, "898-2619",
                                                   "0-897")))),
                       levels = c("0-897","898-2619","2620-4532","4533-7331","7332-8059"))
RFM$Fsegment <- findInterval(Past_Cruisers$interval, 
                             quantile(Past_Cruisers$interval, c(0.0, 0.6506, 0.767, 0.8835, 0.9999),na.rm = T))
RFM$Interval <- ordered(ifelse(RFM$Fsegment == 1, "0",
                               ifelse(RFM$Fsegment == 2, "1-986", 
                                      ifelse(RFM$Fsegment == 3, "987-2687",
                                             ifelse(RFM$Fsegment == 4, "2688-7298",
                                                    "7299-42708")))),
                        levels = c("0","1-986","987-2687","2688-7298","7299-42708"))

RFM$Msegment <- findInterval(Past_Cruisers$R_TTL, 
                             quantile(Past_Cruisers$R_TTL, c(0.0, 0.25, 0.5, 0.75, 0.9999),na.rm = T))
RFM$Monetary <- ordered(ifelse(RFM$Msegment <= 1, "1-5203",
                               ifelse(RFM$Msegment <= 2, "5204-9158", 
                                      ifelse(RFM$Msegment <= 3, "9159-18327",
                                             ifelse(RFM$Msegment <= 4, "18328-1843748",
                                                    "1843748-4142974")))),
                        levels = c("1-5203","5204-9158","9159-18327","18328-1843748", "1843748-4142974"))
RFM<-RFM[,c(3:8)]

# Recency by Interval - Counts
RxF <- as.data.frame(table(RFM$Recency, RFM$Interval,
                           dnn = c("Recency", "Interval")),
                     responseName = "Number_Customers")
with(RxF, balloonplot(Recency, Interval, Number_Customers,
                      zlab = "# Total Customers"))
RxM <- as.data.frame(table(RFM$Recency, RFM$Monetary,
                           dnn = c("Recency", "Monetary")),
                     responseName = "Number_Customers")
with(RxM, balloonplot(Recency, Monetary, Number_Customers,
                      zlab = "# Total Customers"))
FxM <- as.data.frame(table(RFM$Interval, RFM$Monetary,
                           dnn = c("Interval", "Monetary")),
                     responseName = "Number_Customers")
with(FxM, balloonplot(Interval, Monetary, Number_Customers,
                      zlab = "# Total Customers"))