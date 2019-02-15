#########################PACKAGES############################################

#install packages to grab market data from Yahoo Finance and Quandl.
install.packages("Quandl")  
library("Quandl")
Quandl.api_key("s_6EugCDNsEPDyeXe3dM")
install.packages("quantmod")  
library("quantmod")
#install packages to apply svm method.
install.packages("kernlab")
library("kernlab")
install.packages("prettyR")
library("prettyR")
#########################FUNCTIONS############################################

#function to get stock market data from yahoo finance.
getdata<-function(x){
  hprice<-getSymbols(x,from = "2010-04-15",to = "2017-09-12",src = "yahoo",auto.assign=FALSE)
  hreturn<-(hprice[,c(1:6)])
  return(hreturn)
}

getdata3<-function(x){
  hprice<-get.hist.quote(x,start = "2008-04-15",end = "2017-09-12")#, provider =c ("yahoo"),retclass = c("zoo", "ts"))
  hreturn<-(hprice[,c(1:6)])
  return(hreturn)
}
#function to get stock market data from Quandl.
getdata1<-function(x){
  R<-Quandl(x,start_date = "2008-04-15",end_date = "2017-09-12",type = "xts")
  t<-cbind(lag(R),R)
  return(t)
}

#function to calculate market movement in different time range.
difference<-function(x){
  returns<-c()
  diff<-c()
  for(i in 1:7){
    diff<-c[,i*2]-lag(c[,(i*2-1)],k=x)
    returns<-cbind(returns,diff)
  }
  colnames(returns)<-c("DAX","FTSE","HENGSENG","Nikkei","OIL","GOLD","USD/EUR")
  return(na.omit(returns))
}

#function to calculate return in different time range.
returncalculation<-function(x){
  returns<-c()
  diff<-c()
  for(i in 1:8){
    diff<-(b[,i*2]-lag(b[,(i*2-1)],k=x))/lag(b[,(i*2-1)],k=x)
    returns<-cbind(returns,diff)
  }
  colnames(returns)<-c("SP500","DAX","FTSE","HENGSENG","Nikkei","OIL","GOLD","USD/EUR")
  return(na.omit(returns))
}

#function to calculate return in different time range.
returncalculation2<-function(x){
  returns<-c()
  diff<-c()
  for(i in 1:7){
    diff<-(c[,i*2]-lag(c[,(i*2-1)],k=x))/lag(c[,(i*2-1)],k=x)
    returns<-cbind(returns,diff)
  }
  colnames(returns)<-c("DAX","FTSE","HENGSENG","Nikkei","OIL","GOLD","USD/EUR")
  return(na.omit(returns))
}

# function to normalize data
normal<-function(x){
  y<-sign(x)
  y<-y[-c(which(y[,1]==0)),]
  return(y)
}

#function to calculate correlation
corcalculation<-function(x){
  maxlag<-50
  crossCorr<-matrix(0,(maxlag*2 + 1), 9);
  for (i in 1:9){
    crossCorr[,i] = ccf(as.numeric(x[,1]),as.numeric(x[,i]), lag.max=maxlag)$acf
  }
}
#function to divide data 
multi<-function(y,indecator1,indecator2){
  y[which(y>indecator1 & y < indecator2)]=0
  y[which(y<indecator1 & y!=0)]=-1
  y[which(y>indecator2)]=1
  return(y)
}
#function to balance dataset
balance<-function(x,y,z){
  n<-abs(length(which(x==y))-length(which(x==z)))
  if(length(which(x==y))-length(which(x==z))>0){
    s<-sample(1:length(which(x==y)),size=n)
    ss<-which(x==y)[s]
    x<-x[-ss]
  }else{
    s<-sample(1:length(which(x==z)),size=n)
    ss<-which(x==z)[s]
    x<-x[-ss]
  }
  return(x)
}

#function to implement SVM method.
svm<-function(x,y,s){
  #apply svm method.
  model<-c()
  model <- ksvm(as.matrix(x)[s,],as.matrix(y)[s,],type = "C-bsvc",kernel = "rbfdot", C = 10, prob.model = TRUE,cross=3)  
  return(model)
}

#function to implement SVM method to run prediction(calculate accuracy rate).
prediction1<-function(model,x,target){
  testdata<-c()
  testtarget<-c()
  #get test set.
  testdata<-as.matrix(tail(x,n=252))
  testtarget<-as.matrix(tail(target,n=252))
  modeltest<-c()
  modeltest <- predict(model,testdata,type="response")
  AR<-length(which(modeltest==testtarget))/length(testtarget)
  return(AR)
}

#function to implement SVM method to run prediction(return prediction).
prediction2<-function(model){
  testdata<-c()
  modeltest<-c()
  #get test set.
  testdata<-as.matrix(tail(dataused,n=252))
  modeltest <- predict(model,testdata,type="response")
  #AR<-length(which(modeltest==testtarget))/length(testtarget)
  return(modeltest)
}

#function to run strategy simulation in the time range 2014-2015.
simulation<-function(x,y,simtarget){
  v<-c()
  v[1]=10000
  for(i in 1:length(x)){
    if(x[i]==0){
      v[i+1]=v[i]
    }else if (x[i]==y[i]){
      v[i+1]=v[i]*(1+abs(as.numeric(simtarget[i])))
    }else{
      v[i+1]=v[i]*(1-abs(as.numeric(simtarget[i])))
    }
  }
  return (v)
}

#########################INPUT-DATA############################################

#SP500<-getdata("SPY")
#DJIA<-getdata("^DJI")
#DAX<-getdata("EWG")
#FTSE<-getdata("VGK")
#HENGSENG<-getdata("EWH")
#Nasdaq<-getdata("JPXN")
#NIKKEI<-getdata("NKY")
#GOLD<-getdata("GLD")
#OIL<-getdata("USO")
#USDEUR<-getdata("FXE")


SP500 <- read.csv(file="/Users/huhenry/Documents/study/career/SVM/data/SPY.csv", header=TRUE, sep=",")
DAX <- read.csv(file="/Users/huhenry/Documents/study/career/SVM/data/EWG.csv", header=TRUE, sep=",")
FTSE <- read.csv(file="/Users/huhenry/Documents/study/career/SVM/data/VGK.csv", header=TRUE, sep=",")
HENGSENG <- read.csv(file="/Users/huhenry/Documents/study/career/SVM/data/EWH.csv", header=TRUE, sep=",")
NIKKEI <- read.csv(file="/Users/huhenry/Documents/study/career/SVM/data/JPXN.csv", header=TRUE, sep=",")
GOLD <- read.csv(file="/Users/huhenry/Documents/study/career/SVM/data/GLD.csv", header=TRUE, sep=",")
OIL <- read.csv(file="/Users/huhenry/Documents/study/career/SVM/data/USO.csv", header=TRUE, sep=",")
USDEUR <- read.csv(file="/Users/huhenry/Documents/study/career/SVM/data/FXE.csv", header=TRUE, sep=",")

######################### DATA PROCESS ############################################

output<-c()
a<-c()
b<-c()
c<-c()
corr<-c()
output<-cbind(SP500[,c(2,5)],DAX[,c(2,5)],FTSE[,c(2,5)],HENGSENG[,c(2,5)],NIKKEI[,c(2,5)],OIL[,c(2,5)],GOLD[,c(2,5)],USDEUR[,c(2,5)])
a<-na.approx(output[,-c(1,2)])
b<-na.omit(cbind(output[,c(1,2)],a)) 
c<-b[,-c(1:2)]
#u<-na.omit(cbind(lag(b[,c(1:6)]),c))
diff<-c()
for(i in 0:30){
  diff<-cbind(diff,difference(0))
}
diff_day<-SP500[,5]-SP500[,2]
return_day<-returncalculation(0)
return_week<-returncalculation(5)
return_2week<-returncalculation(10)
return_month<-returncalculation(30)
returns<-c()
for(i in 0:30){
  returns<-cbind(returns,returncalculation2(i))
}
tail(b,n=1)
y1<-sign(returns)
sreturns<-y1[-c(which(y1[,1]==0)),]
sreturn_day<-normal(return_day)
sreturn_week<-normal(return_week)
sreturn_2week<-normal(return_2week)
sreturn_month<-normal(return_month)

#########################Correlation Analysis############################################

corr_day<-cor(return_day)
corr_week<-cor(return_week)
corr_2week<-cor(return_2week)
corr_month<-cor(return_month)
colors<-c("blue","yellow","black","orange","gray","red","pink","purple","green","brown")
cf1=ccf(as.ts(return_day[,1]),as.ts(return_day[,1]),plot="F",type="correlation")
plot(cf1,cf1$lag,type="l",col="blue",xlim=c(-5,5),ylim=c(0,1),
     xlab="Lag",ylab="Percentage",main="Cross-correlation with respect to Nasdaq")
for(i in 2:8){
  cf=ccf(as.ts(return_day[,1]),as.ts(return_day[,i]),plot="F",type="correlation")
  lines(cf$lag,cf$acf,type="l",col=colors[i])
}
legend("topright",legend=colnames(return_day),col=colors,lty=1,x.intersp=0.1,
       y.intersp=0.5,text.width=1)

####################Prediction of daily movement by SVM ###################

#features<-c(1,8,9,15)
features<-c(2)
ar_day<-c()

dataused<-na.omit(sreturns)[,features]
target<-na.omit(cbind(sign(return_day[,1]),dataused))[,1]
target0<-balance(target,1,-1)
dataused0<-na.omit(cbind(target0,dataused))[,-1]
#for (p in 10:500){
p = 500
s<-sample(1:round((length(target0)-252)*0.001*p),size=(length(target0)-252)*0.001*p) 
model<-svm(dataused0,target0,s)
ar_day<-prediction1(model,dataused0,target0)
#ar_day[p]<-prediction1(model,dataused0,target0)
#}

################## Prediction of daily movement by SVM (Improved) #######################
prediction<-c()
ignorepart<-c(-6,6)  
y<-multi(diff_day,ignorepart[1],ignorepart[2])
mfeatures<-c(1,8,9,15)
testtarget<-as.matrix(tail(y,n=252))
ytemp<-na.omit(y[which(y==1|y==0)])
target_temp<-balance(ytemp,1,0)
dataused1<-na.omit(cbind(sreturns,target_temp))[,mfeatures]
target1<-na.omit(cbind(target_temp,dataused1))[,1]

ytemp<-na.omit(y[which(y==-1|y==0)])
target_temp<-balance(ytemp,-1,0)
dataused2<-na.omit(cbind(sreturns,target_temp))[,mfeatures]
target2<-na.omit(cbind(target_temp,dataused2))[,1]

ytemp<-na.omit(y[which(y==1|y==-1)])
target_temp<-balance(ytemp,1,-1)
dataused3<-na.omit(cbind(sreturns,target_temp))[,mfeatures]
target3<-na.omit(cbind(target_temp,dataused3))[,1]

model1<-svm(dataused1,target1,s)
model2<-svm(dataused2,target2,s)
model3<-svm(dataused3,target3,s)
pre1<-prediction2(model1)
pre2<-prediction2(model3)
pre3<-prediction2(model2)
pre<-cbind(pre1,pre2,pre3)


for(j in 1:nrow(pre)){
  if(sum(pre[j,])!=0){
    prediction[j]<-as.numeric(Mode(pre[j,]))
  }else{
    prediction[j]<-0
  }
}
tp<-prediction[which(prediction != 0 & prediction==testtarget)]
fp<-prediction[which(prediction != 0 & prediction!=testtarget)]
fn<-prediction[which(prediction == 0 & prediction!=testtarget)]
precision<-length(tp)/(length(tp)+length(fp))
recall<-length(tp)/(length(tp)+length(fn))
F1<-2*precision*recall/(precision+recall)

######################### Model Trading Simulation ####################################

simtarget<-tail(na.omit(cbind(return_day[,1],dataused))[,1],n=252)
expectvalue<-simulation(prediction,tail(target,n=252),simtarget)
market<-c()
market[1]=10000
for(i in 1:252){
  market[i+1]=market[i]*(1+simtarget[i])
}

######################### Simulation Analysis ###############################

expectreturn<-(expectvalue[length(expectvalue)]-10000)/10000
r<-(expectvalue[-1]-expectvalue[-length(expectvalue)])/expectvalue[-length(expectvalue)]
volatility<-sd(r)*sqrt(252)
loss_days<-length(which(r<0))
max_loss<-r[which.min(r)]
plot(c(1:253),expectvalue,type="l",main="Strategy Performace",ylab="Expected Value",xlab="Trading days in 2014-2015",col="red")
lines(c(1:253),market,col="black")
legend("topleft",legend=c("SVM Strategy","Simple Strategy"),col=c("red","black"),lty=1,x.intersp=0.1,
       y.intersp=1,text.width=2.0,box.col="white")
x<-na.omit(cbind(diff_day,diff))
write.csv(x,file="/Users/huhenry/Documents/study/career/SVM/data/datalist.csv",row.names = TRUE)
x$row.names 


