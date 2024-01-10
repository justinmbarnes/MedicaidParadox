
##################################################################
####################################################################
### BRFSS read-in
library(foreign)
d16 <- read.xport("~/Downloads/LLCP2016.XPT ")
d17 <- d18 <- d19 <- NULL
dat <- data.frame(mam=c(d16$X_MAM5021,d18$X_MAM5022),pap=c(d16$X_RFPAP33,d18$X_RFPAP34),hpvtest=c(d16$HPVTEST,d18$HPVTEST,d19$HPVTEST),hpvttime=c(d16$HPLSTTST,d18$HPLSTTST,d19$HPLSTTST),crc=c(d16$X_CRCREC,d18$X_CRCREC),marital=c(d16$MARITAL,d17$MARITAL,d18$MARITAL,d19$MARITAL),
                  residence=c(d16$MSCODE,d17$MSCODE,d18$MSCODE,d19$MSCODE),race=c(d16$X_RACEGR3,d17$X_RACEGR3,d18$X_RACEGR3,d19$X_RACEGR3),age=c(d16$X_AGE80,d17$X_AGE80,d18$X_AGE80,d19$X_AGE80),
                  income=c(d16$INCOME2,d17$INCOME2,d18$INCOME2,d19$INCOME2),education=c(d16$X_EDUCAG,d17$X_EDUCAG,d18$X_EDUCAG,d19$X_EDUCAG),insured=c(d16$HLTHPLN1,d17$HLTHPLN1,d18$HLTHPLN1,d19$HLTHPLN1),insurance.type=c(d16$HLTHCVR1,d17$HLTHCVR1,d18$HLTHCVR1,d19$HLTHCVR1),nocov.12mos=c(d16$NOCOV121,d17$NOCOV121,d18$NOCOV121),cost.doctor=c(d16$MEDCOST,d17$MEDCOST,d18$MEDCOST,d19$MEDCOST),cost.meds=c(d16$MEDSCOST,d17$MEDSCOS1,d18$MEDSCOS1),delay.care=c(d16$DELAYMED,d17$DELAYMED,d18$DELAYME1),
                  state=c(d16$X_STATE,d17$X_STATE,d18$X_STATE,d19$X_STATE),year=rep(2016,nrow(d16)),weights=c(d16$X_LLCPWT,d17$X_LLCPWT,d18$X_LLCPWT,d19$X_LLCPWT),sex=c(d16$SEX,d17$SEX,d18$SEX,d19$X_SEX),cancerhx = c(d16$CHCOCNCR,d17$CHCOCNCR,d18$CHCOCNCR,d19$CHCOCNCR),skincancerhx=c(d16$CHCSCNCR,d17$CHCSCNCR,d18$CHCSCNCR,d19$CHCSCNCR),on.treatment=c(d16$CSRVTRT1,d17$CSRVTRT2,d18$CSRVTRT2,d19$CSRVTRT3),insurance.cancer=c(d16$CSRVINSR,d17$CSRVINSR,d18$CSRVINSR,d19$CSRVINSR),
                  kids=c(d16$CHILDREN,d17$CHILDREN,d18$CHILDREN,d19$CHILDREN),adults=c(d16$NUMADULT,d17$NUMADULT,d18$NUMADULT,d19$NUMADULT),strata=c(d16$X_STSTR,d17$X_STSTR,d18$X_STSTR,d19$X_STSTR),psu=c(d16$X_PSU,d17$X_PSU,d18$X_PSU,d19$X_PSU),cancertype=c(d16$CNCRTYP1,d17$CNCRTYP1,d18$CNCRTYP1,d19$CNCRTYP1),month=c(d16$IMONTH,d17$IMONTH,d18$IMONTH,d19$IMONTH),ownhome=d16$RENTHOM1,
                  hadmam=c(d16$HADMAM,d18$HADMAM,d19$HADMAM),mamtime=c(d16$HOWLONG,d18$HOWLONG,d19$HOWLONG),hadpap=c(d16$HADPAP2,d18$HADPAP2,d19$HADPAP2),paptime=c(d16$LASTPAP2,d18$LASTPAP2,d19$LASTPAP2),hysterectomy=c(d16$HADHYST2,d18$HADHYST2,d19$HADHYST2),hadbldstool=c(d16$BLDSTOOL,d18$BLDSTOOL,d19$BLDSTOOL),bldstooltime=c(d16$LSTBLDS3,d18$LSTBLDS3,d19$LSTBLDS3),hadsigco=c(d16$HADSIGM3,d18$HADSIGM3,d19$HADSIGM3),sigcotime=c(d16$LASTSIG3,d18$LASTSIG3,d19$LASTSIG3),employment=c(d16$EMPLOY1,d17$EMPLOY1,d18$EMPLOY1,d19$EMPLOY1))



dat$agegroups <- ifelse(dat$age>29,1,0) + ifelse(dat$age>49,1,0) + ifelse(dat$age>64,1,0)
dat$ages <- ifelse(dat$age>24,1,0) + ifelse(dat$age>29,1,0) +  ifelse(dat$age>34,1,0) +  ifelse(dat$age>39,1,0) +  ifelse(dat$age>44,1,0) + ifelse(dat$age>49,1,0) +  ifelse(dat$age>54,1,0) + ifelse(dat$age>59,1,0) + ifelse(dat$age>64,1,0)
dat$ages <- as.factor(dat$ages)
dat$agegroups2 <- ifelse(dat$age>39,1,0) + ifelse(dat$age>64,1,0)
dat$kids[which(dat$kids==88)] <- 0
dat$kids[which(dat$kids==99)] <- NA
dat$household <- dat$kids+dat$adults
dat$residence <- ifelse(dat$residence<=4,1,0) # metro (or in same county) vs not metro
dat$race[which(dat$race==4)] <- 3
dat$race[which(dat$race==5)] <- 4
dat$race[which(dat$race==9)] <- NA
dat$race <- dat$race-1 # 0=white, 1=black, 2=other, 3=hispanic
dat$income[which(dat$income==77 | dat$income==99)] <- NA
income2 <- dat$income
income2[which(dat$income<5)] <- 1 # <25k
income2[which(dat$income<7 & dat$income>=5)] <- 2 #25-50k
income2[which(dat$income==7)] <- 3 # 50-75k
income2[which(dat$income==8)] <- 4 # >75k
dat$countyincome <- income2
dat$countyeducation <- dat$education
dat$countyeducation[which(dat$countyeducation==9)] <- NA
dat$education[which(dat$education==9)] <- NA
dat$marital[which(dat$marital==9)] <- NA
dat$marital[which(dat$marital>1)] <- 0
dat$marital <- as.factor(dat$marital)
dat$countyeducation <- as.factor(dat$countyeducation-1)
dat$countyincome <- as.factor(dat$countyincome-1)
dat$residence <- as.factor(dat$residence)
dat$race <- as.factor(dat$race)
dat$agegroups <- as.factor(dat$agegroups)
dat$insured[which(dat$insured>2)] <- NA
dat$insured <- 2-dat$insured
dat$insured[which(dat$insured==0 & dat$insurance.type %in% c(1:7))] <- 1
dat$insured[which(dat$insured==1 & dat$insurance.type ==8)] <- 0
#dat$intervention.group <- ifelse(dat$state==5,1,0) # 5=arkansas
#dat$intervention.group[-which(dat$state %in% c(5,21,22,48))] <- NA
dat$expansion <- ifelse(dat$state %in% c(30,22,51,23),1,0) + ifelse(dat$state %in% c(1,12,13,20,28,37,45,46,47,48,55,56,16,49,31,40,29),2,0)  #1 = 2016-19 adopt, # 2=not adopt (or adopt after 2019)
dat$expansion[which(dat$state %in% c(66,72) | dat$state > 60)] <- NA
dat$insurance.groups <- ifelse(dat$insurance.type==1,1,0) + ifelse(dat$insurance.type==2,2,0) + ifelse(dat$insurance.type==4,3,0) + ifelse(dat$insurance.type %in% c(3,5,6,7),4,0)
dat$insurance.groups[which(dat$insurance.groups==0)] <- NA # 1=private, 2=got own, 3=medicaid, 4 = other, NA = unknown or no insurance (too many NAs of people who are insured to count all people without insurance..)
dat$insurance.private <- ifelse(dat$insurance.groups==1,1,0)
dat$insurance.own <- ifelse(dat$insurance.groups==2,1,0)
dat$insurance.medicaid <- ifelse(dat$insurance.groups==3,1,0)
dat$insurance.other <- ifelse(dat$insurance.groups==4,1,0)
dat$insurance.groups2 <- dat$insurance.groups
dat$insurance.groups2[which(dat$insurance.groups==1)] <- 2
dat$insurance.groups2 <- dat$insurance.groups2-1
dat$ownhome[which(dat$ownhome>2)] <- NA
dat$ownhome <- 2-dat$ownhome
#dat$insurance.groups[which(dat$insured==0 | dat$insurance.type==8)] <- 0 # 0 = uninsured, 
dat$hpvUTD <- ifelse(dat$hpvtest==1 & dat$hpvtest<=4,1,0)
dat$cervixrec <- 2-dat$pap
dat$cervixrec[which(dat$pap==2)] <- dat$hpvUTD[which(dat$pap==2)]
dat$mam <- 2-dat$mam
dat$crc <- 2-dat$crc
dat$on.treatment[which(dat$on.treatment %in% c(7,9))] <- NA
dat$on.treatment <- ifelse(dat$on.treatment %in% c(1),1,0) # I could include #4, which is people whose treatment hasn't started yet.. but I'd rather
dat$insurance.cancer[which(dat$insurance.cancer %in% c(7,9))] <- NA
dat$insurance.cancer <- ifelse(dat$insurance.cancer==1,1,0)
dat$cancerhx[which(dat$cancerhx %in% c(7,9))] <- NA
dat$cancerhx <- ifelse(dat$cancerhx==1,1,0)
dat$cancerhx[which(dat$cancerhx==0)] <- ifelse(dat$skincancerhx[which(dat$cancerhx==0)]==1 & dat$cancertype[which(dat$cancerhx==0)]==21,1,0)
dat$cancerhx[which(dat$cancertype==22)] <- 0 # I've included all melanomas, excluded non-melanoma skin cancer
dat$cost.doctor[which(dat$cost.doctor %in% c(7,9))] <- NA
dat$cost.doctor <- ifelse(dat$cost.doctor==1,1,0)
dat$cost.meds[which(dat$cost.meds %in% c(3,7,9))] <- NA # also excluding people who weren't prescribed anything
dat$cost.meds <- ifelse(dat$cost.meds==1,1,0)
dat$delay.care[which(dat$delay.care %in% c(7,9))] <- NA
dat$delay.care <- ifelse(dat$delay.care %in% c(1,2,3,4,5,6),1,0)
dat$post.period2 <- ifelse((dat$year==2018 & dat$month %in% c(10,11,12)) | (dat$year==2019 & dat$month %in% c(1,2,3)),1,0) + ifelse(dat$year==2019 & dat$month %in% c(7,8,9,10,11,12),2,0)
dat$post.period <- ifelse(dat$year %in% c(2019),1,0)
#dat$post.period[which(dat$year==2018)] <- NA
dat$quarter <- ifelse(dat$month %in% c(1,2,3),1,0) + ifelse(dat$month %in% c(4,5,6),1,0) + ifelse(dat$month %in% c(7,8,9),1,0) + ifelse(dat$month %in% c(10,11,12),1,0)
dat$time <- dat$year + ((dat$quarter-1)/4)
dat$state2 <- dat$state
dat$state2[which(dat$state==6)] <- 0
dat$state2 <- as.factor(dat$state2)
dat$sex[which(dat$sex==9)] <- NA
dat$sex <- dat$sex - 1 #sex: 0=male, 1=female

dat$cost.doctor2 <- dat$cost.doctor
dat$insured2 <- dat$insured

poverty.estimator <- function(data,est="mid",ret="pov"){
  data$income2 <- data$income
  
  if(est=="bottom"){
    # these values are the bottom of the range
    data$income2[which(data$income2==1)] <- 0 # these income values are the middle of the range
    data$income2[which(data$income2==2)] <- 10000
    data$income2[which(data$income2==3)] <- 15000
    data$income2[which(data$income2==4)] <- 20000
    data$income2[which(data$income2==5)] <- 25000
    data$income2[which(data$income2==6)] <- 35000
    data$income2[which(data$income2==7)] <- 50000
    data$income2[which(data$income2==8)] <- 75000
  }
  else{
    data$income2[which(data$income2==1)] <- 5000 # these income values are the middle of the range
    data$income2[which(data$income2==2)] <- 12500
    data$income2[which(data$income2==3)] <- 17500
    data$income2[which(data$income2==4)] <- 22500
    data$income2[which(data$income2==5)] <- 30000
    data$income2[which(data$income2==6)] <- 42500
    data$income2[which(data$income2==7)] <- 67500
    data$income2[which(data$income2==8)] <- 85000
  }
  
  # estimates based on 2018 poverty guidelines
  ### https://aspe.hhs.gov/2018-poverty-guidelines
  
  baseline <- ifelse((data$state %in% c(2,15))==F,12140,0) + ifelse((data$state %in% c(2)),15180,0) + ifelse((data$state %in% c(15)),13960,0)
  amount <- ifelse((data$state %in% c(2,15))==F,4320,0) + ifelse((data$state %in% c(2)),5400,0) + ifelse((data$state %in% c(15)),4810,0)
  level <- (baseline+(amount*data$household))
  #level2 <- 4*(baseline+(amount*data$household))
  fpl <- data$income2/level
  return(fpl)
}
dat$fpl <- poverty.estimator(dat)





dat$rowID <- seq(1:nrow(dat))
set.seed(1234567)
ksamples <- sample(1:nrow(dat),replace=F)
modu <- modi <- list(NULL)

require(survey)
options(survey.lonely.psu = "adjust")
brffs <- svydesign(id=~psu, strata=~strata,nest = TRUE, weights=~weights,data=dat)
brffs <- subset(brffs,(ages %in% seq(0,8) & expansion %in% c(0,1,2) & year==2016))



o1 <- svyhist(~fpl,brffs,probability=T)
p1 <- svyhist(~fpl,subset(brffs,insurance.groups2==1),probability=T)
m1 <- svyhist(~fpl,subset(brffs,insurance.groups2==2),probability=T)
u1 <- svyhist(~fpl,subset(brffs,insured2==0),probability=T)


dat2 <- dat[which(dat$ages %in% seq(0,8) & dat$expansion %in% c(0,1,2) & dat$year==2016),]
dat2 <- dat2[-which(rowMeans(is.na(dat2[,which(colnames(dat2) %in% c("ages","race","sex","marital","residence","state2","fpl"))]))>0),]
datu <- dat2[-which(is.na(dat2$insured2)),]
datI <- dat2[-which(is.na(dat2$insurance.groups)),]

#datI states: Delaware, DC, Georgia, Kentucky, Louisiana, Minnesota, New Mexico, Pennsylvania

#modu.fpl$residuals[which(datu$insured2==0)]


modu.fpl <- svyglm(fpl~insured2+ages+race+sex+marital+residence+state2,design=brffs)
modu.fpl$coefficients
modI.fpl <- svyglm(fpl~as.factor(insurance.groups2)+ages+race+sex+marital+residence+state2,design=brffs)

modu.fpl$fitted.values[which(modu.fpl$fitted.values<0)] <- 0
modI.fpl$fitted.values[which(modI.fpl$fitted.values<0)] <- 0



#####
for(i in 1:5){
  if(i==1) these <- ksamples[1:97260]
  if(i==2) these <- ksamples[(97260+1):(97260*2)]
  if(i==3) these <- ksamples[((97260*2)+1):(97260*3)]
  if(i==4) these <- ksamples[((97260*3)+1):(97260*4)]
  if(i==5) these <- ksamples[((97260*4)+1):(nrow(dat))]
  datk <- dat[-these,]
  brffs <- svydesign(id=~psu, strata=~strata,nest = TRUE, weights=~weights,data=datk)
  brffs <- subset(brffs,(ages %in% seq(0,8) & expansion %in% c(0,1,2) & year==2016))
  
  modu[[i]] <- svyglm(fpl~insured2+ages+race+sex+marital+residence+state2,design=brffs)
  modi[[i]] <- svyglm(fpl~as.factor(insurance.groups2)+ages+race+sex+marital+residence+state2,design=brffs)
}

predsu <- predsi <- preds <- numeric(nrow(dat))
dat$insurance <- ifelse(dat$insurance.groups2==2,1,0)
dat$insurance[which(is.na(dat$insurance))] <- ifelse(dat$insured2[which(is.na(dat$insurance))]==0,2,0)
X <- model.matrix(~rowID+fpl+as.factor(insurance)+ages+race+sex+marital+residence+state2,data=dat)

rowID <- X[,2]
thefpl <- X[,3]
X <- X[,-c(2,3)]

cols <- which(colnames(X[,-c(1,2,3)]) %in% names(modu[[1]]$coefficients))

X <- X[,unique(c(1,2,3,(cols+3)))]

dF1 <- modu[[1]]$coefficients
dF1 <- c(modi[[1]]$coefficients[1],dF1)
dF1[2] <- modi[[1]]$coefficients[2]
dF1[3] <- c(modu[[1]]$coefficients[1]-dF1[1])

dF2 <- modu[[2]]$coefficients
dF2 <- c(modi[[2]]$coefficients[1],dF2)
dF2[2] <- modi[[2]]$coefficients[2]
dF2[3] <- c(modu[[2]]$coefficients[1]-dF2[1])

dF3 <- modu[[3]]$coefficients
dF3 <- c(modi[[3]]$coefficients[1],dF3)
dF3[2] <- modi[[3]]$coefficients[2]
dF3[3] <- c(modu[[3]]$coefficients[1]-dF3[1])

dF4 <- modu[[4]]$coefficients
dF4 <- c(modi[[4]]$coefficients[1],dF4)
dF4[2] <- modi[[4]]$coefficients[2]
dF4[3] <- c(modu[[4]]$coefficients[1]-dF4[1])

dF5 <- modu[[5]]$coefficients
dF5 <- c(modi[[5]]$coefficients[1],dF5)
dF5[2] <- modi[[5]]$coefficients[2]
dF5[3] <- c(modu[[5]]$coefficients[1]-dF5[1])

preds <- numeric(nrow(X))

preds[which(rowID %in% ksamples[1:97260])] <- X[which(rowID %in% ksamples[1:97260]),] %*% dF1
preds[which(rowID %in% ksamples[(97260+1):(97260*2)])] <- X[which(rowID %in% ksamples[(97260+1):(97260*2)]),] %*% dF2
preds[which(rowID %in% ksamples[((97260*2)+1):(97260*3)])] <- X[which(rowID %in% ksamples[((97260*2)+1):(97260*3)]),] %*% dF3
preds[which(rowID %in% ksamples[((97260*3)+1):(97260*4)])] <- X[which(rowID %in% ksamples[((97260*3)+1):(97260*4)]),] %*% dF4
preds[which(rowID %in% ksamples[((97260*4)+1):(nrow(dat))])] <- X[which(rowID %in% ksamples[((97260*4)+1):(nrow(dat))]),] %*% dF5

predsQ <- ifelse(preds>qua)

hist(datu$fpl[which(datu$insured2==0)],ylim=c(0,3000),main="",xlab="% Federal Poverty Level",xaxt='n')
hist(modu.fpl$fitted.values[which(datu$insured2==0)],add=T,col="red",border="red",density=3,breaks=5)
axis(1,at=c(0,1,2,3,4,5),labels=c(0,100,200,300,400,500),tick = T)
legend("topright",c("Actual","Predicted"),col=c("black","red"
),lty=1,lwd=2)

hist(datI$fpl[which(datI$insurance.groups2==1)],ylim=c(0,5000),main="",xlab="% Federal Poverty Level",xaxt='n')
hist(modI.fpl$fitted.values[which(datI$insurance.groups2==1)],add=T,col="red",border="red",density=3,breaks=5)
axis(1,at=c(0,1,2,3,4,5),labels=c(0,100,200,300,400,500),tick = T)
legend("topright",c("Actual","Predicted"),col=c("black","red"
),lty=1,lwd=2)

hist(datI$fpl[which(datI$insurance.groups2==2)],ylim=c(0,800),main="",xlab="% Federal Poverty Level",xaxt='n')
hist(modI.fpl$fitted.values[which(datI$insurance.groups2==2)],add=T,col="red",border="red",density=3,breaks=5)
axis(1,at=c(0,1,2,3,4,5),labels=c(0,100,200,300,400,500),tick = T)
legend("topright",c("Actual","Predicted"),col=c("black","red"
),lty=1,lwd=2)


hist(modu.fpl$fitted.values[which(datu$insured2==0)],ylim=c(0,1.2),xlim=c(0,5),main="",xlab="Estimated % Federal Poverty Level",xaxt='n',border="orange3",col="orange3",density=6,freq=F)
hist(modI.fpl$fitted.values[which(datI$insurance.groups2==1)],ylim=c(0,5000),add=T,col="black",border="black",density=3,freq=F)
hist(modI.fpl$fitted.values[which(datI$insurance.groups2==2)],ylim=c(0,800),add=T,col="lightblue3",border="lightblue3",density=15,freq=F)
axis(1,at=c(0,1,2,3,4,5),labels=c(0,100,200,300,400,500),tick = T)
legend("topright",c("Private","Medicaid","Uninsured"),col=c("black","lightblue3","orange3"
),lty=1,lwd=2,ncol=1)


plot(o1,border="gainsboro",col="gainsboro",ylab="Density",xlab="% Federal Poverty Level",main="",ylim=c(0,0.8),xaxt='n')
plot(p1,add=T,border="black",col="black",density=3)
plot(m1,add=T,border="lightblue3",col="lightblue3",density=15)
plot(u1,add=T,border="orange3",col="orange3",density=6)
axis(1,at=c(0,1,2,3,4,5),labels=c(0,100,200,300,400,500),tick = T)
legend("topright",c("Overall","Private","Medicaid","Uninsured"),col=c("gray","black","lightblue3","orange3"
),lty=1,lwd=2,ncol=2)


write.csv(cbind(modu.fpl$coefficients,sqrt(diag(modu.fpl$cov.unscaled))),"MedicaidParadox-brfss-Uninsured-FPL.csv")
write.csv(cbind(modI.fpl$coefficients,sqrt(diag(modI.fpl$cov.unscaled))),"MedicaidParadox-brfss-insurancetype-FPL.csv")



mi <- svymean(~as.factor(insured2),design=brffs,na.rm=T)
mi2 <- svymean(~as.factor(insurance.groups2),design=brffs,na.rm=T)
ma <- svymean(~as.factor(ages),design=brffs,na.rm=T)
mr <- svymean(~as.factor(race),design=brffs,na.rm=T)
ms <- svymean(~as.factor(sex),design=brffs,na.rm=T)
mm <- svymean(~as.factor(marital),design=brffs,na.rm=T)
mre <- svymean(~as.factor(residence),design=brffs,na.rm=T)
mst <- svymean(~as.factor(state2),design=brffs,na.rm=T)

summat <- rbind(
  cbind(paste(round(as.matrix(mi)[,1]*100,1)," (",round(mi[,2]*100,1),")",sep="")),
  cbind(paste(round(mi2[,1]*100,1)," (",round(mi2[,2]*100,1),")",sep="")),
  cbind(paste(round(ma[,1]*100,1)," (",round(ma[,2]*100,1),")",sep="")),
  cbind(paste(round(mr[,1]*100,1)," (",round(mr[,2]*100,1),")",sep="")),
  cbind(paste(round(ms[,1]*100,1)," (",round(ms[,2]*100,1),")",sep="")),
  cbind(paste(round(mm[,1]*100,1)," (",round(mm[,2]*100,1),")",sep="")),
  cbind(paste(round(mre[,1]*100,1)," (",round(mre[,2]*100,1),")",sep="")),
  cbind(paste(round(mst[,1]*100,1)," (",round(mst[,2]*100,1),")",sep="")))

summat <- rbind(as.matrix(mi),as.matrix(mi2),as.matrix(ma),as.matrix(ma),as.matrix(mr),as.matrix(ms),as.matrix(mm),as.matrix(mre),as.matrix(mst))









######################
### Prepare SEER Data

## Read in data
data <- read.table("~/export-SEERdata.txt",sep=";",header=T,quote = "")

## make R objects for a couple objects (for defining a couple more exclusions not specified in the SEER*STAT application)
seer.diagnoseyear <- data$X.Year.of.diagnosis.
seer.diagnosemonth <- data$X.Month.of.diagnosis.
seer.cancertype <- data$X.Site.recode.ICD.O.3.WHO.2008.
seer.insurance <- data$X.Insurance.Recode..2007...
seer.state <- data$X.State.

## Define additional exclusions
removes.year <- which((seer.diagnoseyear == 2013 & seer.diagnosemonth %in% c("October","November","December","Blank(s)")) | (seer.diagnoseyear == 2014 & (seer.diagnosemonth %in% c("January","February","March","Blank(s)")))) # 3 months before, 3 months after

## remove the observations that met the exclusion criteria
data <- data[-unique(c(removes.year)),]

## define even more exlusions (to be removed later--need to make a larger set of data for insurance analyses)
seer.age <- data$X.Age.at.diagnosis. # equivalent to the data$X.Age.recode.with.single.ages.and.85.. variable
seer.cancertype <- data$X.Primary.Site...labeled.
seer.stage <- data$X.Derived.AJCC.Stage.Group..7th.ed..2010.2015.. #data$X.Derived.AJCC.Stage.Group..7th.ed..2010...
removes.cancertypes <- NULL # taking out CNS tumors, Leukemia
removes.ages <- NULL
#for(i in 1:nrow(data)){
#  chopped <- strsplit(as.character(seer.cancertype[i]),split=NULL)[[1]]
#  if((chopped[1]=="1" | chopped[1]=="3") & chopped[2]==".") removes.cancertypes <- c(removes.cancertypes,i)
#}
removes.stage <- c(which(is.na(seer.stage)))
#removes <- unique(c(removes.ages, removes.cancertypes,removes.stage))


### make R objects for the covariates of interest using the variable names from the dataset
seer.age <- data$X.Age.at.diagnosis.
seer.diagnoseyear <- data$X.Year.of.diagnosis.
seer.diagnosemonth <- data$X.Month.of.diagnosis.
seer.cancertype <- data$X.Site.recode.ICD.O.3.WHO.2008.
seer.stage <- data$X.Derived.AJCC.Stage.Group..7th.ed..2010.2015..
seer.stage2 <- data$X.Summary.stage.2000..1998...
seer.alive <- data$X.Vital.status.recode..study.cutoff.used..
seer.alive2 <- data$X.COD.to.site.recode.
seer.survmonths <- data$X.Survival.months.
seer.insurance <- data$X.Insurance.Recode..2007...
seer.marital <- data$X.Marital.status.at.diagnosis.
seer.race <- data$X.Race.recode..W..B..AI..API..
seer.hispanic <- data$X.Origin.recode.NHIA..Hispanic..Non.Hisp..
seer.sex <- data$X.Sex.
seer.statecounty <- data$X.State.
library(stringr)
est <- do.call("rbind",strsplit(as.character(seer.statecounty),":"))
seer.state <- est[,1]
seer.residence <- data$X.Rural.Urban.Continuum.Code.2013. 
seer.countyeducation <- data$X.....High.school.education.ACS.2011.15.
seer.countyincome <- data$X.Median.household.income..in.tens..ACS.2011.15.
seer.site <- data$X.Primary.Site...labeled.

### Modify the classes within the variables and apply desired formatting
marital <- (ifelse(seer.marital=="Married (including common law)",1,0)+ifelse(seer.marital=="Unknown",2,0)) # married=1, unmarried=0, unk=2
marital[which(marital==2)] <- NA
race <- (ifelse(seer.race=="Black",1,0) + ifelse(seer.race=="Asian or Pacific Islander",2,0) + ifelse(seer.race=="American Indian/Alaska Native",3,0) + ifelse(seer.race=="Unknown",4,0)) # white=0, black=1, Asian =2, American Indian=3, unk  =4
race[which(race==4)] <- NA
hispanic <- ifelse(seer.hispanic=="Non-Spanish-Hispanic-Latino",0,1) # hispanic=1
sex <- ifelse(seer.sex=="Female",1,0) # female=1
age <- seer.age
diagnoseyear <- seer.diagnoseyear
diagnosemonth <- seer.diagnosemonth
diagnoseyear2 <- diagnoseyear + ifelse(diagnosemonth %in% c("April","May","June"),0.25,0) + ifelse(diagnosemonth %in% c("July","August","September"),0.5,0) + ifelse(diagnosemonth %in% c("October","November","December"),0.75,0)
diagnoseyear3 <- diagnoseyear + ifelse(diagnosemonth %in% c("February"),1/12,0) + ifelse(diagnosemonth %in% c("March"),2/12,0) + ifelse(diagnosemonth %in% c("April"),3/12,0) + ifelse(diagnosemonth %in% c("May"),4/12,0) + ifelse(diagnosemonth %in% c("June"),5/12,0) + ifelse(diagnosemonth %in% c("July"),6/12,0) + ifelse(diagnosemonth %in% c("August"),7/12,0) + ifelse(diagnosemonth %in% c("September"),8/12,0) + ifelse(diagnosemonth %in% c("October"),9/12,0) + ifelse(diagnosemonth %in% c("November"),10/12,0) + ifelse(diagnosemonth %in% c("December"),11/12,0)
state <- seer.state
cancertype <- as.character(seer.cancertype)
cancertype[which(cancertype=="Cervix Uteri")] <- "Cervical"
cancertype[which(cancertype %in% c("Nasopharynx", "Nose, Nasal Cavity and Middle Ear", "Oropharynx", "Other Oral Cavity and Pharynx", "Salivary Gland", "Thyroid", "Tongue", "Tonsil", "Floor of Mouth", "Gum and Other Mouth", "Hypopharynx", "Larynx", "Lip"))] <- "Head and Neck" ## not eye and orbit
cancertype[which(cancertype %in% c("Rectosigmoid Junction", "Rectum", "Ascending Colon", "Cecum", "Descending Colon"," Hepatic Flexure", "Large Intestine, NOS", "Sigmoid Colon", "Splenic Flexure", "Transverse Colon"))] <- "Colorectal"
cancertype[unique(c(grep("leuke",cancertype),grep("Leuke",cancertype)))] <- "Leukemia"
cancertype[which(cancertype %in% c("Corpus Uteri","Uterus, NOS"))] <- "Uterus"
cancertype[grep("Hodgkin",cancertype)] <- "Hodgkin Lymphoma"
cancertype[grep("NHL",cancertype)] <- "Non-Hodgkin Lymphoma"
cancertype[which(cancertype %in% c("Brain","Cranial Nerves Other Nervous System"))] <- "Nervous system"
cancertype[which(cancertype %in% c("Melanoma of the Skin","Other Non-Epithelial Skin"))] <- "Skin"
cancertype[which(cancertype!="Prostate" & cancertype!="Lung and Bronchus" & cancertype!="Breast" & cancertype!="Cervical" & cancertype!= "Colorectal" & cancertype!="Pancreas" & cancertype!="Stomach" & cancertype!="Urinary Bladder" & cancertype!="Testis" & cancertype!="Ovary" & cancertype !="Nervous System" & cancertype !="Skin" & cancertype!="Non-Hodgkin Lymphoma" & cancertype!="Hodgkin Lymphoma" & cancertype != "Uterus" & cancertype!="Leukemia" & cancertype!="Head and Neck" & cancertype!="Liver" & cancertype!="Myeloma" & cancertype !="Thyroid" & cancertype !="Kidney and Renal Pelvis")] <- "Other"
cancertype[which(cancertype=="Breast" & sex==0)] <- "Other"
alive <- ifelse(seer.alive=="Alive",1,0) # 1=alive
alive2 <- ifelse(seer.alive2=="Alive or dead of other cause",1,0)
alive2[which(seer.alive2=="N/A not first tumor")] <- NA
suicide <- ifelse(seer.alive2=="Suicide and Self-Inflicted Injury",1,0)
insurance <- (ifelse(seer.insurance=="Any Medicaid",1,0) + ifelse(seer.insurance=="Insured" | seer.insurance=="Insured/No specifics",2,0) + ifelse(seer.insurance=="Insurance status unknown",9,0)) # 0=no insurance, 1 = medicaid, 2 = other insurance, 9=unk
insurance[which(insurance==9)] <- NA
#stage[which(stage==0)] <- 1 ## I'm going to combine 0 and 1.. I think it makes more sense since there are only 200/27000 (<1%) and they're only for 1 type of cancer
#stage2[which(stage2==0)] <- 1
stage2 <- seer.stage2
stage2[which(stage2=="Blank(s)")] <- "Unknown/unstaged"
survmonths <- as.numeric(as.character(seer.survmonths))
residence  <- (ifelse(seer.residence=="Counties in metropolitan areas ge 1 million pop" | seer.residence=="Counties in metropolitan areas of 250,000 to 1 million pop" | seer.residence=="Counties in metropolitan areas of lt 250 thousand pop",1,0) +
                 ifelse(seer.residence=="Unknown/missing/no match" | seer.residence=="Unknown/missing/no match (Alaska or Hawaii - Entire State)",2,0))
residence[which(residence==2)] <- NA
residence3 <- 1-residence
residence3[grep("rural", seer.residence)] <- 2
residence2  <- (ifelse(seer.residence2=="Counties in metropolitan areas ge 1 million pop" | seer.residence2=="Counties in metropolitan areas of 250,000 to 1 million pop" | seer.residence2=="Counties in metropolitan areas of lt 250 thousand pop",1,0) +
                  ifelse(seer.residence2=="Unknown/missing/no match" | seer.residence2=="Unknown/missing/no match (Alaska or Hawaii - Entire State)",2,0))
residence2[which(residence2==2)] <- NA
residence2[which(diagnoseyear>2010)] <- residence[which(diagnoseyear>2010)]
raceethnicity.cat <- 1+ifelse(race==0 & hispanic==0,-1,0) + ifelse(race==1 & hispanic==0,1,0) + ifelse(hispanic==1,2,0) # 0=non-hispanic white, 1=non-hispanic other,  2=non-hispanic black. 3=hispanic
raceethnicity.cat[which(raceethnicity.cat==1)] <- 5
raceethnicity.cat[which(raceethnicity.cat==2)] <- 1
raceethnicity.cat[which(raceethnicity.cat==5)] <- 2
raceethnicity <- ifelse(race==0 & hispanic==0,1,0) + ifelse(race==1 & hispanic==0,2,0) + ifelse(hispanic==0 & race==4,3,0) + ifelse(hispanic==1,4,0)  # 0=non-hispanic other, 1=non-hispanic white, 2=non-hispanic black. 3=non-hispanic unk, 4=hispanic
countyeducation <- as.numeric(as.character(seer.countyeducation))
countyeducation.even <- ifelse(countyeducation>=quantile(countyeducation,0.25,na.rm=T),1,0) + ifelse(countyeducation>=quantile(countyeducation,0.5,na.rm=T),1,0) + ifelse(countyeducation>=quantile(countyeducation,0.75,na.rm=T),1,0)
intervention.group3 <- ifelse(state %in% c("CA","CT","NJ","WA"),1,0) + ifelse(state %in% c("HI","IA","KY","MI","NM"),2,0) + ifelse(state %in% c("AK","LA"),3,0) ## AK in 9/1/15, LA in 7/1/16
intervention.group2 <- ifelse(state %in% c("CA","CT","NJ","WA"),1,0) + ifelse(state %in% c("HI","IA","KY","MI","NM"),2,0) ## AK in 9/1/15, LA in 7/1/16
intervention.group <- ifelse(intervention.group2>0,1,0)
intervention.group[which(intervention.group3==3)] <- NA
intervention.group.age <- ifelse(age>=65,0,1)
post.aca <- ifelse(diagnoseyear>=2014,1,0)
countyincome <- as.numeric(as.character(seer.countyincome))
countyincome.even <- ifelse(countyincome>=quantile(countyincome,0.25,na.rm=T),1,0) + ifelse(countyincome>=quantile(countyincome,0.5,na.rm=T),1,0) + ifelse(countyincome>=quantile(countyincome,0.75,na.rm=T),1,0) # 5125 5754 6935
age2 <- ifelse(age>=35,1,0) + ifelse(age>=45,1,0) + ifelse(age>=55,1,0)
age3 <- ifelse(age>=40,1,0) + ifelse(age>=65,1,0)
age4 <- ifelse(age>=25,1,0) + ifelse(age>=45,1,0) + ifelse(age>=65,1,0)
ages <- ifelse(age>24,1,0) + ifelse(age>29,1,0) +  ifelse(age>34,1,0) +  ifelse(age>39,1,0) +  ifelse(age>44,1,0) + ifelse(age>49,1,0) +  ifelse(age>54,1,0) + ifelse(age>59,1,0) + ifelse(age>64,1,0)
ages <- as.factor(ages)

# I'm switching the income groups--I think it is easier to justify even groups rather than the other numbers...
data.insurance <- data.frame(insurance=as.factor(insurance),post.aca, intervention.group,
                             marital=as.factor(marital), race=as.factor(raceethnicity.cat), countyeducation=as.factor(countyeducation.even), education=countyeducation,
                             countyincome=as.factor(countyincome.even), income=countyincome, residence=as.factor(residence3), residence2=as.factor(residence), age=as.factor(age2), sex, diagnoseyear, diagnoseyear2, diagnoseyear3,
                             alive, alive2, suicide, survmonths, stage=as.factor(stage2),statecounty=as.factor(seer.statecounty),state=as.factor(state),cancertype =as.factor(cancertype),age3=as.factor(age3),age4=as.factor(age4),ages)
library(survival)

dat <- data.insurance[-which(rowMeans(is.na(data.insurance[,which(colnames(data.insurance) %in% c("age","sex","marital","race","residence","countyincome","countyeducation","survmonths","insurance"))]))>0),]
dat <- droplevels(dat[-which(dat$age3==2),])


dat$cens <- 1-dat$alive
dat$insurance <- as.factor(2-as.numeric(as.character(dat$insurance)))
dat$earlystage <- ifelse(dat$stage %in% c("In situ","Localized"),1,0)
dat$latestage <- ifelse(dat$stage %in% c("Distant"),1,0)

data.insurance <- data <- ests <- aaa <- age <- age2 <- age3 <- age4 <- ages <- alive <- alive2 <- cancertype <- countyeducation <- countyincome <- countyincome.even <- diagnosemonth <- diagnoseyear <- diagnoseyear2 <- diagnoseyear3 <- hispanic <- insurance <- intervention.group <- intervention.group.age <- intervention.group2 <- intervention.group3 <- marital <- post.aca <- race <- raceethnicity <- raceethnicity.cat <- removes.stage <- removes.year <- residence <- residence3 <- seer.age <- seer.alive <- seer.alive2 <- seer.cancertype <- seer.countyeducation <- seer.countyincome <- seer.diagnoseyear <- seer.diagnosemonth <- seer.hispanic <- seer.insurance <- seer.marital <- seer.race <- seer.residence <- seer.sex <- seer.site <- seer.stage <- seer.stage2 <- seer.state <- seer.statecounty <- seer.survmonths <- sex <- stage2 <- state <- suicide <- survmonths <- thesestates <- NULL

library(pseudo)
library(sandwich)
library(lmtest)
library(prodlim)


data.insurance$deathdate <- data.insurance$survmonths/12 + data.insurance$diagnoseyear3 + 0.01
data.insurance$deathmonth <- as.numeric(substr(data.insurance$deathdate,5,7))


################################
################################
########### Data analysis

f <- prodlim(Hist(survmonths,cens)~1,data=dat) 
abc <- jackknife(f,times=c(12,24,36,48,60))
dat$survrate <- abc[,1]
dat$survrate2  <- abc[,2]


# these two files contain the model estimates from the BRFSS-trained models
duF <- read.csv("~/MedicaidParadox-brfss-Uninsured-FPL.csv") 
diF <- read.csv("~/MedicaidParadox-brfss-insurancetype-FPL.csv")


thesestates <- which(duF[,1] %in% c("state29","state213","state215","state219","state221","state222","state226","state234","state235","state249","state253"))

dF <- duF[c(1:16,thesestates),]
rownames(dF) <- dF[,1]
dF <- dF[,-1]
dF <- rbind(diF[1,2:3],dF)
dF[2,] <- diF[2,2:3]
dF[3,] <- c(duF[1,2]-dF[1,1],sqrt(duF[1,3]^2+dF[1,2]^2))  ##duF1 = intercept = uninsured.   ## DF1 = intercept diF = private. 
rownames(dF)[1:3] <- c("(intercept)","insurance1","insurance2")

estmat <- model.matrix(~insurance+ages+race+sex+marital+residence2+state,data=dat)
estmat2 <- estmat[which(dat$post.aca==0),]
estmat <- estmat[-which(dat$post.aca==0),]
dat2 <- droplevels(dat[which(dat$post.aca==0),])
dat <- droplevels(dat[-which(dat$post.aca==0),])



dat$fplP <- estmat %*% cbind(dF[,1])
dat$fplP[which(dat$fplP<0)] <- 0
dat2$fplP <- estmat2 %*% cbind(dF[,1])
dat2$fplP[which(dat2$fplP<0)] <- 0



#dat$inP <- estmat %*% cbind(dI[,1])
#dat$edP <- estmat %*% cbind(dE[,1])
#dat$ownP <- estmat %*% cbind(dO[,1])





xmat <- model.matrix(~insurance+ages+race+sex+marital+residence2+state,data=dat)




## Stage analyses

#X <- model.matrix(~insurance+age3+race+sex+residence+income+education+cancertype+stage+state,dat=dats2)
#estmatX <- model.matrix(~ages+race+sex+marital+residence2+state,data=dats2)

early.unadjust <- glm(earlystage~insurance+age3+race+sex+residence+marital+income+education+cancertype+state,data=dat,family=binomial(link="logit"))
late.unadjust <- glm(latestage~insurance+age3+race+sex+residence+marital+income+education+cancertype+state,data=dat,family=binomial(link="logit"))

early.adjust <- glm(earlystage~insurance+age3+race+sex+residence+marital+income+education+cancertype+state+fplP,data=dat,family=binomial(link="logit"))
late.adjust <- glm(latestage~insurance+age3+race+sex+residence+marital+income+education+cancertype+state+fplP,data=dat,family=binomial(link="logit"))

early.adjust.fplB <- glm(earlystage~insurance+age3+race+sex+residence+marital+income+education+cancertype+state,data=dat[which(dat$fplP > 1 & dat$fplP < 1.5),],family=binomial(link="logit"))
late.adjust.fplB <- glm(latestage~insurance+age3+race+sex+residence+marital+income+education+cancertype+state,data=dat[which(dat$fplP > 1 & dat$fplP < 1.5),],family=binomial(link="logit"))

summarize.glm <- function(obj){
  abc <- summary(obj)$coefficients
  vv <- vcov(obj)
  dif <- abc[2,1]-abc[3,1]
  difv <- sqrt(vv[2,2] + vv[3,3] - (2*vv[2,3]))
  ppd <- pnorm(-1*abs(dif/difv))*2
  mat <- matrix(NA,ncol=4,nrow=3)
  mat[,1] <- c(abc[2:3,1],dif)
  mat[1,2:3] <- abc[2,1] + c(-1,1)*qnorm(0.975)*abc[2,2]
  mat[2,2:3] <- abc[3,1] + c(-1,1)*qnorm(0.975)*abc[3,2]
  mat[3,2:3] <- dif + c(-1,1)*qnorm(0.975)*difv
  mat[,4] <- c(abc[2:3,4],ppd)
  mat[,1:3] <- exp(mat[,1:3])
  rownames(mat) <- c("Medicaid","Uninsured","Med-Un")
  return(round(mat,3))
}

summarize.lm <- function(obj){
  abc <- summary(obj)$coefficients
  vv <- vcov(obj)
  dif <- abc[2,1]-abc[3,1]
  difv <- sqrt(vv[2,2] + vv[3,3] - (2*vv[2,3]))
  ppd <- pnorm(-1*abs(dif/difv))*2
  mat <- matrix(NA,ncol=4,nrow=3)
  mat[,1] <- c(abc[2:3,1],dif)
  mat[1,2:3] <- abc[2,1] + c(-1,1)*qnorm(0.975)*abc[2,2]
  mat[2,2:3] <- abc[3,1] + c(-1,1)*qnorm(0.975)*abc[3,2]
  mat[3,2:3] <- dif + c(-1,1)*qnorm(0.975)*difv
  mat[,4] <- c(abc[2:3,4],ppd)
  rownames(mat) <- c("Medicaid","Uninsured","Med-Un")
  return(round(mat,3))
}

write.csv(summarize.glm(early.unadjust),"medicaidparadox-earlystage-unadjusted.csv")
write.csv(summarize.glm(late.unadjust),"medicaidparadox-latestage-unadjusted.csv")
write.csv(summarize.glm(early.adjust),"medicaidparadox-earlystage-FPLadjusted.csv")
write.csv(summarize.glm(late.adjust),"medicaidparadox-latestage-FPLadjusted.csv")

write.csv(summarize.glm(early.adjust.fplB),"medicaidparadox-earlystage-adjusted-FPL100-150.csv")
write.csv(summarize.glm(late.adjust.fplB),"medicaidparadox-latestage-adjusted-FPL100-150.csv")


survrate.fplB <- lm(survrate~insurance+age3+race+sex+residence+marital+income+education+cancertype+state,data=dat[which(dat$fplP > 1 & dat$fplP < 1.5),])
survrate.stage.fplB <- lm(survrate~insurance+age3+race+sex+residence+marital+income+education+cancertype+state+stage,data=dat[which(dat$fplP > 1 & dat$fplP < 1.5),])

write.csv(summarize.lm(survrate.fplB),"medicaidparadox-survrate-adjusted-nostage-FPL100-150.csv")
write.csv(summarize.lm(survrate.stage.fplB),"medicaidparadox-survrate-adjusted-withstage-FPL100-150.csv")



############### BAYESIAN ANALYSES

blprsum <- function(ob){
  library(HDInterval)
  ob -> mat
  pp <- colMeans(mat>0)
  pp1 <- colMeans(mat>0)
  pp2 <- colMeans(mat<0)
  if(sum(pp>0.5)>0) pp[which(pp>0.5)] <- 1-pp[which(pp>0.5)]
  p <- pp
  mat2 <- cbind(colMeans(mat),t(apply(mat,2,quantile,c(0.025,0.975))),t(apply(mat,2,hdi)),p,pp1,pp2)
  colnames(mat2) <- c("Est","qLB","qUB","HPD LB","HPD UB","p-value","P(B>0)","P(B<0)")
  return(mat2)
}



os.bayes.fpl <- function(X,Y,insurance,estmat,muI,vcovI,K=3,draws=90000){ # note this function automatically takes out the intercept matrix column
  thep <- which(insurance==0)
  them <- which(insurance==1)
  theu <- which(insurance==2)
  X <- X[c(thep,them,theu),]
  Y <- Y[c(thep,them,theu)]
  Xp <- estmat[thep,]
  Xm <- estmat[them,]
  Xu <- estmat[theu,]
  Y <- as.numeric(Y)
  #if(sum(Y<=0)>1) Y[which(Y<=0)] <- 0.00001
  #if(sum(Y>=1)>1) Y[which(Y>=1)] <- 0.99999
  library(R2jags)
  mdl <- '
  model {
  for(i in 1:numn.p){
  Zp[i] ~ dnorm(mup[i],1/1.2) T(0,)
  mup[i] <- inprod(bi,Xp[i ,]) + 3
  Z[i] <- Zp[i]
  }
  for(i in 1:numn.m){
  Zm[i] ~ ddexp(mum[i],1/0.7)  T(0,)
  mum[i] <- inprod(bi,Xm[i ,]) + 0.7
  Z[i+numn.p] <- Zm[i]
  }
  for(i in 1:numn.u){
  Zu[i] ~ ddexp(muu[i],1/1.2)  T(0,)
  muu[i] <- inprod(bi,Xu[i ,]) + 1.2
  Z[i+numn.p+numn.m] <- Zu[i]
  }
  #for(j in 1:nump.i){bi[j] ~ dnorm(mI[j],vI[j]) }
  bi ~ dmnorm(mI,vI) 
for (i in 1:numn){
  elinpred[i] <- inprod(beta[],X[i ,])+omega*Z[i] + mu
  #Y[i] ~ dnorm(elinpred[i],sdi) T(0,1)
  Y[i] ~ dnorm(elinpred[i],sdi)
}
# Prior distributions
mu ~ dunif(0,1)
for(l in 1:nump){ beta[l]  ~ dnorm(0,1) }
#for(l in 1:nump){ bm[l]  ~ dnorm(0,0.001) }
omega ~ dnorm(0,1)
sdi ~ dt(0, pow(25,-2), 1) T(0.000001,)
}
  '
writeLines(mdl,'mdl.txt')
X <- as.matrix(X)[,-1]
numn <- nrow(X)
nump <- ncol(X)
nump.i <- length(muI)
mI <- muI
#vI <- 1/vcovI
vI <- solve(vcovI)
numn.p <- nrow(Xp)
numn.m <- nrow(Xm)
numn.u <- nrow(Xu)
data.jags <- c('Y','X','numn','nump','mI','vI','nump.i','numn.p','numn.m','numn.u','Xp','Xm','Xu')
parms <- c('beta','mu','omega','sdi')
poi1.sim <- jags(data=data.jags,parameters.to.save=parms,
                 model.file='mdl.txt',n.iter=draws,n.burnin=2000,n.chains=1,n.thin=1)
#return(poi1.sim)
sims <- as.mcmc(poi1.sim)
chains <- as.matrix(sims)
#betas <- chains[,-ncol(chains)]
#betas <- betas[,-ncol(betas)]
return(chains)
}


os.bayes <- function(X,Y,insurance,estmat,muI,vcovI,K=3,draws=20000){ # note this function automatically takes out the intercept matrix column
  thep <- which(insurance==0)
  them <- which(insurance==1)
  theu <- which(insurance==2)
  X <- X[c(thep,them,theu),]
  Y <- Y[c(thep,them,theu)]
  Xp <- estmat[thep,]
  Xm <- estmat[them,]
  Xu <- estmat[theu,]
  Y <- as.numeric(Y)
  #if(sum(Y<=0)>1) Y[which(Y<=0)] <- 0.00001
  #if(sum(Y>=1)>1) Y[which(Y>=1)] <- 0.99999
  library(R2jags)
  mdl <- '
  model {
  for (i in 1:numn){
  elinpred[i] <- inprod(beta[],X[i ,]) + mu
  #Y[i] ~ dnorm(elinpred[i],sdi) T(0,1)
  Y[i] ~ dnorm(elinpred[i],sdi)
}
# Prior distributions
mu ~ dunif(0,1)
#for(l in 1:nump){ beta[l]  ~ dnorm(0,1) T(-1,1) }
for(l in 1:nump){ beta[l]  ~ dnorm(0,1) }
#for(l in 1:nump){ bm[l]  ~ dnorm(0,0.001) }
sdi ~ dt(0, pow(25,-2), 1) T(0.000001,)
}
  '
writeLines(mdl,'mdl.txt')
X <- as.matrix(X)[,-1]
numn <- nrow(X)
nump <- ncol(X)
nump.i <- length(muI)
mI <- muI
vI <- 1/vcovI
numn.p <- nrow(Xp)
numn.m <- nrow(Xm)
numn.u <- nrow(Xu)
data.jags <- c('Y','X','numn','nump')
parms <- c('beta','mu','sdi')
poi1.sim <- jags(data=data.jags,parameters.to.save=parms,
                 model.file='mdl.txt',n.iter=draws,n.burnin=2000,n.chains=1,n.thin=1)
#return(poi1.sim)
sims <- as.mcmc(poi1.sim)
chains <- as.matrix(sims)
#betas <- chains[,-ncol(chains)]
#betas <- betas[,-ncol(betas)]
return(chains)
}


f <- prodlim(Hist(survmonths,cens)~1,data=dat) 
abc <- jackknife(f,times=c(12,24,36,48,60))
dat$survrate <- abc[,1]
dat$survrate2  <- abc[,2]

set.seed(123456)

dats <- dat[sample(1:nrow(dat),20000),]
rmsts <- pseudomean(dats$survmonths,dats$cens,24) # calculate pseudoobservations of the restricted mean survival at 12 months
dats$rmst <- rmsts
X <- model.matrix(~insurance+age3+race+sex+residence+countyincome+countyeducation+cancertype+stage+state,dat=dats)
estmatX <- model.matrix(~ages+race+sex+marital+residence2+state,data=dats)
estmatX <- estmatX[,-1]
coefX <- read.csv("~/MedicaidParadox-brfss-baselinenoinsurance-FPL.csv")
vcovX <- read.csv("~/MedicaidParadox-brfss-baselinenoinsurance-FPL-covmatrix.csv")
thesestates <- which(coefX[,1] %in% c("state29","state213","state215","state219","state221","state222","state226","state234","state235","state249","state253"))
vcovX <- vcovX[,-1]
vcovX <- vcovX[c(2:15,thesestates),c(2:15,thesestates)]
coefX <- coefX[c(2:15,thesestates),2]
os.a <- os.bayes(X,dats$survrate,insurance = dats$insurance,estmat = estmatX,muI = coefX,vcovI = vcovX)
Sys.time()
os.a.fpl <- os.bayes.fpl(X,dats$survrate,insurance = dats$insurance,estmat = estmatX,muI = coefX,vcovI = vcovX)
Sys.time()






duF <- read.csv("~/MedicaidParadox-brfss-Uninsured-FPL.csv")
diF <- read.csv("~/MedicaidParadox-brfss-insurancetype-FPL.csv")


thesestates <- which(duF[,1] %in% c("state29","state213","state215","state219","state221","state222","state226","state234","state235","state249","state253"))

dF <- duF[c(1:16,thesestates),]
rownames(dF) <- dF[,1]
dF <- dF[,-1]
dF <- rbind(diF[1,2:3],dF)
dF[2,] <- diF[2,2:3]
dF[3,] <- c(duF[1,2]-dF[1,1],sqrt(duF[1,3]^2+dF[1,2]^2)) 
rownames(dF)[1:3] <- c("(intercept)","insurance1","insurance2")


estmat <- model.matrix(~insurance+ages+race+sex+marital+residence2+state,data=dat)
estmat2 <- estmat[which(dat$post.aca==0),]
estmat <- estmat[-which(dat$post.aca==0),]
dat2 <- droplevels(dat[which(dat$post.aca==0),])
dat <- droplevels(dat[-which(dat$post.aca==0),])



dat$fplP <- estmat %*% cbind(dF[,1])
dat$fplP[which(dat$fplP<0)] <- 0
dat2$fplP <- estmat2 %*% cbind(dF[,1])
dat2$fplP[which(dat2$fplP<0)] <- 0

dat$fplP2 <- ifelse(dat$fplP>0.5,1,0) + ifelse(dat$fplP>1.0,1,0) + ifelse(dat$fplP>1.38,1,0) + ifelse(dat$fplP>2.0,1,0) + ifelse(dat$fplP>2.5,1,0) + ifelse(dat$fplP>3,1,0)  + ifelse(dat$fplP>3.5,1,0)



###########################################
#### BOOTSTRAPPING


ecdf_fun <- function(x,perc) ecdf(x)(perc)

pairwise.boot <- function(obj,dems=c("insurance","age3","race","sex","residence","income","education","cancertype","stage","state")){
  obj$boots
  obj$estimate
  new.p <- numeric(nrow(obj$output))
  for(i in 1:nrow(obj$output)) new.p[i] <- 2*(.5-abs(0.5-ecdf_fun(obj$boots[,i]-obj$estimate[i],obj$estimate[i])))
  obj$output <- cbind(obj$output,new.p)
  for(i in dems){
    demlist <- grep(i,names(obj$estimate))
    takeouts <- grep(i,dems)
    if(length(takeouts)>1){
      thisone <- dems[takeouts[which(dems[takeouts]!=i)]]
      ddlist <- grep(thisone,names(obj$estimate))
      demlist <- demlist[-which(demlist %in% ddlist)]
    }
    if(length(demlist)>1){
      mat <- matrix(NA,nrow=length(demlist),ncol=length(demlist))
      for(j in 1:(length(demlist)-1)){
        j1 <- j+1
        for(k in j1:length(demlist)){
          reals <- obj$estimate[demlist[j]]-obj$estimate[demlist[k]]
          bootys <- obj$boots[,demlist[j]]-obj$boots[,demlist[k]]
          mat[j,k] <- 2*(.5-abs(0.5-ecdf_fun(bootys-reals,reals)))
        }
      }
      obj[[i]] <- mat
    }
  }
  return(obj)
}







boot.survrate <- function(form=survrate~insurance+age3+race+sex+residence+income+education+cancertype+stage+state,data,b=500,timemo=c(12,24),fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat,boot.coefs=T){
  mat <- mat2 <- NULL
  library(sn)
  library(ExtDist)
  data.ins <- data
  if(fpl.exclude) data.ins <- data[which(data$fplP > 1 & data$fplP<1.5),]
  f <- prodlim(Hist(survmonths,cens)~1,data=data.ins) 
  abc <- jackknife(f,times=timemo)
  data.ins$survrate <- abc[,1]
  fgfit1 <- lm(form, data=data.ins)
  reals <- fgfit1$coefficients
  data.ins$survrate <- abc[,2]
  fgfit2 <- lm(form, data=data.ins)
  reals2 <- fgfit2$coefficients
  for(i in 1:b){
    bootsamples <- sample(1:nrow(data),replace=T)
    data.ins <- data[bootsamples,]
    if(fpl.boot){
      if(boot.coefs){
        newcoefs <- matrix(NA,ncol=1,nrow=nrow(fpl.calcs))
        for(j in 1:nrow(newcoefs)) newcoefs[j,1] <- rnorm(1,fpl.calcs[j,1],fpl.calcs[j,2])
        #cat(dim(newcoefs))
        #cat(dim(fpl.calcmat[bootsamples,]))
        data.ins$fplP <- fpl.calcmat[bootsamples,] %*% newcoefs
        #data.ins$fplP[which(data.ins$insurance==0)] <- data.ins$fplP[which(data.ins$insurance==0)] + rnorm(sum(data.ins$insurance==0),0.2,1.12) # adding this is what makes this account for the variance of the "prediction" (like comparing prediction interval to confidence interval). SD(residual) = 1.20165 for the uninsured model, 1.08373 for the insurance type model
        #data.ins$fplP[which(data.ins$insurance==1)] <- data.ins$fplP[which(data.ins$insurance==1)] + rsn(sum(data.ins$insurance==1),xi=-1.5,omega=0.9,alpha=5,tau=0.5,dp=NULL)+1 
        #data.ins$fplP[which(data.ins$insurance==2)] <- data.ins$fplP[which(data.ins$insurance==2)] + 2-log(abs(rnorm(sum(data.ins$insurance==2),5,12))) 
      }
      else{
        data.ins$fplP[which(data.ins$insurance==0)] <- rnorm(sum(data.ins$insurance==0),3,1.2)
        data.ins$fplP[which(data.ins$insurance==1)] <- rLaplace(sum(data.ins$insurance==1),0.7,0.5)
        data.ins$fplP[which(data.ins$insurance==2)] <- abs(rLaplace(sum(data.ins$insurance==2),1.2,0.8))
      }
      data.ins$fplP[which(data.ins$fplP<0)] <- 0
    }
    if(fpl.exclude) data.ins <- data.ins[which(data.ins$fplP > 1 & data.ins$fplP<1.5),]
    f <- prodlim(Hist(survmonths,cens)~1,data=data.ins) 
    abc <- jackknife(f,times=timemo)
    data.ins$survrate <- abc[,1]
    fgfit1 <- lm(form, data=data.ins)
    mat <- rbind(mat,fgfit1$coefficients)
    data.ins$survrate <- abc[,2]
    fgfit2 <- lm(form, data=data.ins)
    mat2 <- rbind(mat2,fgfit2$coefficients)
    cat(i)
  }
  p.actual <- numeric(ncol(mat))
  ci.actual <- matrix(NA,nrow=ncol(mat),ncol=2)
  for(j in 1:ncol(mat)){
    ci.actual[j,] <- reals[j]-quantile(mat[,j]-reals[j],c(0.975,0.025))
    p.actual[j] <- 2*(.5-abs(0.5-mean(mat[,j]<0)))
  }
  ret.mat <- cbind((reals),(ci.actual),p.actual)
  ret.mat <- round(ret.mat,3)
  
  p.actual2 <- numeric(ncol(mat2))
  ci.actual2 <- matrix(NA,nrow=ncol(mat2),ncol=2)
  for(j in 1:ncol(mat2)){
    ci.actual2[j,] <- reals2[j]-quantile(mat2[,j]-reals2[j],c(0.975,0.025))
    p.actual2[j] <- 2*(.5-abs(0.5-mean(mat2[,j]<0)))
  }
  ret.mat2 <- cbind((reals2),(ci.actual2),p.actual2)
  ret.mat2 <- round(ret.mat2,3)
  return(list(output=ret.mat,boots=mat,estimate=reals,output2=ret.mat2,boots2=mat2,estimate2=reals2))
}


ests.u <- boot.survrate(form=survrate~insurance,data=dat,b=500,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat)
#write.csv(rbind(ests.u$estimate,ests.u$boots),"boots-medicaidparadox-estsu2014.csv")
#ests.u.fplex <- boot.survrate(form=survrate~insurance,data=dat,b=2,fpl.boot=F,fpl.exclude=T,fpl.calcs=dF,fpl.calcmat=estmat)
ests.u.fplexB <- boot.survrate(form=survrate~insurance,data=dat,b=500,fpl.boot=T,fpl.exclude=T,fpl.calcs=dF,fpl.calcmat=estmat)
#write.csv(rbind(ests.u.fplexB$estimate,ests.u.fplexB$boots),"boots-medicaidparadox-estsu-fplexB2014.csv")
#ests.u.fpl <- boot.survrate(form=survrate~insurance+fplP,data=dat,b=2,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat)
#ests.u.fplB <- boot.survrate(form=survrate~insurance+fplP,data=dat,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat)
ests.a <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+cancertype+stage+state,data=dat,b=500,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat)
#write.csv(rbind(ests.a$estimate,ests.a$boots),"boots-medicaidparadox-estsa2014.csv")
#ests.a.fplex <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+cancertype+stage+state,data=dat,b=2,fpl.boot=F,fpl.exclude=T,fpl.calcs=dF,fpl.calcmat=estmat)
ests.a.fplexB <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+cancertype+stage+state,data=dat,b=500,fpl.boot=T,fpl.exclude=T,fpl.calcs=dF,fpl.calcmat=estmat)
#write.csv(rbind(ests.a.fplexB$estimate,ests.a.fplexB$boots),"boots-medicaidparadox-estsa-fplexB2014.csv")
#ests.a.fpl <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+cancertype+stage+state+fplP,data=dat,b=2,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat)
ests.a.fplB <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+cancertype+stage+state+fplP,data=dat,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat)
#write.csv(rbind(ests.a.fplB$estimate,ests.a.fplB$boots),"boots-medicaidparadox-estsu-fplB2014.csv")




ests.u2 <- boot.survrate(form=survrate~insurance,data=dat2,b=500,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2,timemo=c(48,60))
#write.csv(rbind(ests.u2$estimate,ests.u2$boots),"boots-medicaidparadox-estsu2011.csv")
#ests.u.fplex <- boot.survrate(form=survrate~insurance,data=dat2,b=2,fpl.boot=F,fpl.exclude=T,fpl.calcs=dF,fpl.calcmat=estmat2)
ests.u2.fplexB <- boot.survrate(form=survrate~insurance,data=dat2,b=500,fpl.boot=T,fpl.exclude=T,fpl.calcs=dF,fpl.calcmat=estmat2,timemo=c(48,60))
#write.csv(rbind(ests.u2.fplexB$estimate,ests.u2.fplexB$boots),"boots-medicaidparadox-estsu-fplexB2011.csv")
#ests.u.fpl <- boot.survrate(form=survrate~insurance+fplP,data=dat2,b=2,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2)
#ests.u.fplB <- boot.survrate(form=survrate~insurance+fplP,data=dat2,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2)
ests.a2 <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+cancertype+stage+state,data=dat2,b=500,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2,timemo=c(48,60))
#write.csv(rbind(ests.a2$estimate,ests.a2$boots),"boots-medicaidparadox-estsa2011.csv")
#ests.a.fplex <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+cancertype+stage+state,data=dat2,b=2,fpl.boot=F,fpl.exclude=T,fpl.calcs=dF,fpl.calcmat=estmat2)
ests.a2.fplexB <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+cancertype+stage+state,data=dat2,b=500,fpl.boot=T,fpl.exclude=T,fpl.calcs=dF,fpl.calcmat=estmat2,timemo=c(48,60))
#write.csv(rbind(ests.a2.fplexB$estimate,ests.a2.fplexB$boots),"boots-medicaidparadox-estsa-fplexB2011.csv")
#ests.a.fpl <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+cancertype+stage+state+fplP,data=dat2,b=2,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2)
ests.a2.fplB <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+cancertype+stage+state+fplP,data=dat2,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2,timemo=c(48,60))
#write.csv(rbind(ests.a2.fplB$estimate,ests.a2.fplB$boots),"boots-medicaidparadox-estsu-fplB2011.csv")

ests.u.fplB <- boot.survrate(form=survrate~insurance+fplP,data=dat,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat)
ests.u2.fplB <- boot.survrate(form=survrate~insurance+fplP,data=dat2,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2,timemo=c(48,60))
meds.u <- boot.survrate(form=fplP~insurance,data=dat,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat)
meds.u2 <- boot.survrate(form=fplP~insurance,data=dat2,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2)


meds.a <- boot.survrate(form=fplP~insurance+age3+race+sex+residence+income+education+cancertype+stage+state,data=dat,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat)
meds.a2 <- boot.survrate(form=fplP~insurance+age3+race+sex+residence+income+education+cancertype+stage+state,data=dat2,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2)




#ests.aM <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+cancertype+stage+(1|statecounty),data=dat,b=500,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat)
#ests.aM.fplex <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+cancertype+stage+(1|statecounty),data=dat,b=2,fpl.boot=F,fpl.exclude=T,fpl.calcs=dF,fpl.calcmat=estmat)
#ests.aM.fplexB <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+cancertype+stage+(1|statecounty),data=dat,b=500,fpl.boot=T,fpl.exclude=T,fpl.calcs=dF,fpl.calcmat=estmat)
#ests.aM.fpl <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+cancertype+stage+(1|statecounty)+fplP,data=dat,b=2,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat)
#ests.aM.fplB <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+cancertype+stage+(1|statecounty +fplP,data=dat,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat)


ret.u <- pairwise.boot(ests.u,dems="insurance")
ret.u.fplexB <- pairwise.boot(ests.u.fplexB,dems="insurance")
ret.a <- pairwise.boot(ests.a)
ret.a.fplexB <- pairwise.boot(ests.a.fplexB)
ret.a.fplB <- pairwise.boot(ests.a.fplB,fpldem=T)

ret.u2 <- pairwise.boot(ests.u2,dems="insurance")
ret.u2.fplexB <- pairwise.boot(ests.u2.fplexB,dems="insurance")
ret.a2 <- pairwise.boot(ests.a2)
ret.a2.fplexB <- pairwise.boot(ests.a2.fplexB)
ret.a2.fplB <- pairwise.boot(ests.a2.fplB,fpldem=T)

ret.u. <- pairwise.boot(ests.u,dems="insurance",time2=T)
ret.u.fplexB. <- pairwise.boot(ests.u.fplexB,dems="insurance",time2=T)
ret.a. <- pairwise.boot(ests.a,time2=T)
ret.a.fplexB. <- pairwise.boot(ests.a.fplexB,time2=T)
ret.a.fplB. <- pairwise.boot(ests.a.fplB,fpldem=T,time2=T)

ret.u2. <- pairwise.boot(ests.u2,dems="insurance",time2=T)
ret.u2.fplexB. <- pairwise.boot(ests.u2.fplexB,dems="insurance",time2=T)
ret.a2. <- pairwise.boot(ests.a2,time2=T)
ret.a2.fplexB. <- pairwise.boot(ests.a2.fplexB,time2=T)
ret.a2.fplB. <- pairwise.boot(ests.a2.fplB,fpldem=T,time2=T)

retmed.a <- pairwise.boot(meds.a)
retmed.a2 <- pairwise.boot(meds.a2)

retmed.u <- pairwise.boot(meds.u)
retmed.u2 <- pairwise.boot(meds.u2)

ret.u.fplB <- pairwise.boot(ests.u.fplB,dems="insurance",fpldem=T)
ret.u2.fplB <- pairwise.boot(ests.u2.fplB,dems="insurance",fpldem=T)
ret.u.fplB. <- pairwise.boot(ests.u.fplB,dems="insurance",fpldem=T,time2=T)
ret.u2.fplB. <- pairwise.boot(ests.u2.fplB,dems="insurance",fpldem=T,time2=T)

outthis <- function(ob,tt=c(2,3,4,1,seq(5,nrow(ob)))) {
  if(nrow(ob)<=4) tt=c(2,3,4,1) 
  return(cbind(paste(round(ob[tt,1],2)," (",round(ob[tt,2],2),", ",round(ob[tt,3],2),")",sep=""),round(ob[tt,5],ifelse(ob[tt,5]<.05,3,2)),paste(round(ob[tt,1]*100,1)," (",round(ob[tt,2]*100,1),", ",round(ob[tt,3]*100,1),")",sep=""),round(ob[tt,5],ifelse(ob[tt,5]<.05,3,2))))
}

write.csv(outthis(ret.u$output),"medicaidparadox-out-unadjust-14-1yr.csv")
write.csv(outthis(ret.u.fplexB$output),"medicaidparadox-out-unadjust-fplex-14-1yr.csv")
write.csv(outthis(ret.a$output),"medicaidparadox-out-adjust-14-1yr.csv")
write.csv(outthis(ret.a.fplexB$output),"medicaidparadox-out-adjust-fplex-14-1yr.csv")
write.csv(outthis(ret.a.fplB$output),"medicaidparadox-out-adjust-fpl-14-1yr.csv")

write.csv(outthis(ret.u2$output),"medicaidparadox-out-unadjust-11-4yr.csv")
write.csv(outthis(ret.u2.fplexB$output),"medicaidparadox-out-unadjust-fplex-11-4yr.csv")
write.csv(outthis(ret.a2$output),"medicaidparadox-out-adjust-11-4yr.csv")
write.csv(outthis(ret.a2.fplexB$output),"medicaidparadox-out-adjust-fplex-11-4yr.csv")
write.csv(outthis(ret.a2.fplB$output),"medicaidparadox-out-adjust-fpl-11-4yr.csv")

write.csv(outthis(ret.u.$output),"medicaidparadox-out-unadjust-14-2yr.csv")
write.csv(outthis(ret.u.fplexB.$output),"medicaidparadox-out-unadjust-fplex-14-2yr.csv")
write.csv(outthis(ret.a.$output),"medicaidparadox-out-adjust-14-2yr.csv")
write.csv(outthis(ret.a.fplexB.$output),"medicaidparadox-out-adjust-fplex-14-2yr.csv")
write.csv(outthis(ret.a.fplB.$output),"medicaidparadox-out-adjust-fpl-14-2yr.csv")

write.csv(outthis(ret.u2.$output),"medicaidparadox-out-unadjust-11-5yr.csv")
write.csv(outthis(ret.u2.fplexB.$output),"medicaidparadox-out-unadjust-fplex-11-5yr.csv")
write.csv(outthis(ret.a2.$output),"medicaidparadox-out-adjust-11-5yr.csv")
write.csv(outthis(ret.a2.fplexB.$output),"medicaidparadox-out-adjust-fplex-11-5yr.csv")
write.csv(outthis(ret.a2.fplB.$output),"medicaidparadox-out-adjust-fpl-11-5yr.csv")

write.csv((ret.u$boots),"medicaidparadox-boot-unadjust-14-1yr.csv")
write.csv((ret.u.fplexB$boots),"medicaidparadox-boot-unadjust-fplex-14-1yr.csv")
write.csv((ret.a$boots),"medicaidparadox-boot-adjust-14-1yr.csv")
write.csv((ret.a.fplexB$boots),"medicaidparadox-boot-adjust-fplex-14-1yr.csv")
write.csv((ret.a.fplB$boots),"medicaidparadox-boot-adjust-fpl-14-1yr.csv")

write.csv((ret.u2$boots),"medicaidparadox-boot-unadjust-11-4yr.csv")
write.csv((ret.u2.fplexB$boots),"medicaidparadox-boot-unadjust-fplex-11-4yr.csv")
write.csv((ret.a2$boots),"medicaidparadox-boot-adjust-11-4yr.csv")
write.csv((ret.a2.fplexB$boots),"medicaidparadox-boot-adjust-fplex-11-4yr.csv")
write.csv((ret.a2.fplB$boots),"medicaidparadox-boot-adjust-fpl-11-4yr.csv")

write.csv((ret.u.$boots2),"medicaidparadox-boot-unadjust-14-2yr.csv")
write.csv((ret.u.fplexB.$boots2),"medicaidparadox-boot-unadjust-fplex-14-2yr.csv")
write.csv((ret.a.$boots2),"medicaidparadox-boot-adjust-14-2yr.csv")
write.csv((ret.a.fplexB.$boots2),"medicaidparadox-boot-adjust-fplex-14-2yr.csv")
write.csv((ret.a.fplB.$boots2),"medicaidparadox-boot-adjust-fpl-14-2yr.csv")

write.csv((ret.u2.$boots2),"medicaidparadox-boot-unadjust-11-5yr.csv")
write.csv((ret.u2.fplexB.$boots2),"medicaidparadox-boot-unadjust-fplex-11-5yr.csv")
write.csv((ret.a2.$boots2),"medicaidparadox-boot-adjust-11-5yr.csv")
write.csv((ret.a2.fplexB.$boots2),"medicaidparadox-boot-adjust-fplex-11-5yr.csv")
write.csv((ret.a2.fplB.$boots2),"medicaidparadox-boot-adjust-fpl-11-5yr.csv")


write.csv(outthis(ret.u.fplB$output),"medicaidparadox-out-unadjust-fpl-14-1yr.csv")
write.csv(outthis(ret.u.fplB.$output),"medicaidparadox-out-unadjust-fpl-14-2yr.csv")
write.csv(outthis(ret.u2.fplB$output),"medicaidparadox-out-unadjust-fpl-11-4yr.csv")
write.csv(outthis(ret.u2.fplB.$output),"medicaidparadox-out-unadjust-fpl-11-5yr.csv")


write.csv((ret.u.fplB$boots),"medicaidparadox-boot-unadjust-fpl-14-1yr.csv")
write.csv((ret.u.fplB.$boots),"medicaidparadox-boot-unadjust-fpl-14-2yr.csv")
write.csv((ret.u2.fplB$boots),"medicaidparadox-boot-unadjust-fpl-11-4yr.csv")
write.csv((ret.u2.fplB.$boots),"medicaidparadox-boot-unadjust-fpl-11-5yr.csv")









#### CANCER TYPE ANALYSES

cancertype.paradox <- function(dat,dat2,cancertype="Breast",dF=dF.){
  these1 <- which(dat$cancertype==cancertype)
  these2 <- which(dat2$cancertype==cancertype)
  dat <- droplevels(dat[these1,])
  estmat1 <- model.matrix(~insurance+ages+race+sex+marital+residence2+state,data=dat)
  #if(sum(abs(0.5-colMeans(estmat1))==0.5)>1) {
  #		rm1 <- which(abs(0.5-colMeans(estmat1))==0.5)[-1]
  #		estmat1 <- estmat1[,-rm1]
  #		dF <- dF[-rm1,]
  #	}
  dat2 <- droplevels(dat2[these2,])
  estmat2 <- model.matrix(~insurance+ages+race+sex+marital+residence2+state,data=dat2)
  #		if(sum(abs(0.5-colMeans(estmat2))==0.5)>1) {
  #		rm2 <- which(abs(0.5-colMeans(estmat2))==0.5)[-1]
  #		estmat2 <- estmat[,-rm2]
  #	}
  if(cancertype %in% c("Breast","Cervical","Ovary","Uterus","Prostate","Testis")){
    estmat1 <- model.matrix(~insurance+ages+race+marital+residence2+state,data=dat)
    estmat2 <- model.matrix(~insurance+ages+race+marital+residence2+state,data=dat2)
    dF <- dF[-15,]
    if(cancertype=="Prostate"){
      dF2 <- dF
      dF <- dF[-4,]
    } 
    ests.a <- boot.survrate(form=survrate~insurance+age3+race+residence+income+education+stage+state,data=dat,b=500,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat1)
    ests.u <- boot.survrate(form=survrate~insurance,data=dat,b=500,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat1)
    ests.a.fplB <- boot.survrate(form=survrate~insurance+age3+race+residence+income+education+stage+state+fplP,data=dat,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat1)
    meds.u <- boot.survrate(form=fplP~insurance,data=dat,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat1)
    meds.a <- boot.survrate(form=fplP~insurance+age3+race+residence+income+education+stage+state,data=dat,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat1)
    ests.u.fplB <- boot.survrate(form=survrate~insurance+fplP,data=dat,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat1)
    if(cancertype=="Prostate"){
      dF <- dF2
    } 
    ests.u2 <- boot.survrate(form=survrate~insurance,data=dat2,b=500,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2,timemo=c(48,60))
    ests.a2 <- boot.survrate(form=survrate~insurance+age3+race+residence+income+education+stage+state,data=dat2,b=500,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2,timemo=c(48,60))
    ests.a2.fplB <- boot.survrate(form=survrate~insurance+age3+race+residence+income+education+stage+state+fplP,data=dat2,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2,timemo=c(48,60))
    ests.u2.fplB <- boot.survrate(form=survrate~insurance+fplP,data=dat2,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2,timemo=c(48,60))
    meds.u2 <- boot.survrate(form=fplP~insurance,data=dat2,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2)
    meds.a2 <- boot.survrate(form=fplP~insurance+age3+race+residence+income+education+stage+state,data=dat2,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2)
  }
  else if(cancertype == "Leukemia"){
    ests.a <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+state,data=dat,b=500,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat1)
    ests.u <- boot.survrate(form=survrate~insurance,data=dat,b=500,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat1)
    ests.a.fplB <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+state+fplP,data=dat,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat1)
    ests.u2 <- boot.survrate(form=survrate~insurance,data=dat2,b=500,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2,timemo=c(48,60))
    ests.a2 <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+state,data=dat2,b=500,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2,timemo=c(48,60))
    ests.a2.fplB <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+state+fplP,data=dat2,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2,timemo=c(48,60))
    ests.u.fplB <- boot.survrate(form=survrate~insurance+fplP,data=dat,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat1)
    ests.u2.fplB <- boot.survrate(form=survrate~insurance+fplP,data=dat2,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2,timemo=c(48,60))
    meds.u <- boot.survrate(form=fplP~insurance,data=dat,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat1)
    meds.u2 <- boot.survrate(form=fplP~insurance,data=dat2,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2)
    meds.a <- boot.survrate(form=fplP~insurance+age3+race+sex+residence+income+education+state,data=dat,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat1)
    meds.a2 <- boot.survrate(form=fplP~insurance+age3+race+sex+residence+income+education+state,data=dat2,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2)
  }
  
  else{
    ests.a <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+stage+state,data=dat,b=500,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat1)
    ests.u <- boot.survrate(form=survrate~insurance,data=dat,b=500,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat1)
    ests.a.fplB <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+stage+state+fplP,data=dat,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat1)
    ests.u2 <- boot.survrate(form=survrate~insurance,data=dat2,b=500,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2,timemo=c(48,60))
    ests.a2 <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+stage+state,data=dat2,b=500,fpl.boot=F,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2,timemo=c(48,60))
    ests.a2.fplB <- boot.survrate(form=survrate~insurance+age3+race+sex+residence+income+education+stage+state+fplP,data=dat2,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2,timemo=c(48,60))
    ests.u.fplB <- boot.survrate(form=survrate~insurance+fplP,data=dat,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat1)
    ests.u2.fplB <- boot.survrate(form=survrate~insurance+fplP,data=dat2,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2,timemo=c(48,60))
    meds.u <- boot.survrate(form=fplP~insurance,data=dat,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat1)
    meds.u2 <- boot.survrate(form=fplP~insurance,data=dat2,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2)
    meds.a <- boot.survrate(form=fplP~insurance+age3+race+sex+residence+income+education+stage+state,data=dat,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat1)
    meds.a2 <- boot.survrate(form=fplP~insurance+age3+race+sex+residence+income+education+stage+state,data=dat2,b=500,fpl.boot=T,fpl.exclude=F,fpl.calcs=dF,fpl.calcmat=estmat2)
  }
  
  ret.u <- pairwise.boot(ests.u,dems="insurance")
  ret.a <- pairwise.boot(ests.a)
  ret.a.fplB <- pairwise.boot(ests.a.fplB,fpldem=T)
  ret.u2 <- pairwise.boot(ests.u2,dems="insurance")
  ret.a2 <- pairwise.boot(ests.a2)
  ret.a2.fplB <- pairwise.boot(ests.a2.fplB,fpldem=T)
  ret.u. <- pairwise.boot(ests.u,dems="insurance",time2=T)
  ret.a. <- pairwise.boot(ests.a,time2=T)
  ret.a.fplB. <- pairwise.boot(ests.a.fplB,fpldem=T,time2=T)
  ret.u2. <- pairwise.boot(ests.u2,dems="insurance",time2=T)
  ret.a2. <- pairwise.boot(ests.a2,time2=T)
  ret.a2.fplB. <- pairwise.boot(ests.a2.fplB,fpldem=T,time2=T)
  ret.u.fplB <- pairwise.boot(ests.u.fplB,dems="insurance",fpldem=T)
  ret.u2.fplB <- pairwise.boot(ests.u2.fplB,dems="insurance",fpldem=T)
  ret.u.fplB. <- pairwise.boot(ests.u.fplB,dems="insurance",fpldem=T,time2=T)
  ret.u2.fplB. <- pairwise.boot(ests.u2.fplB,dems="insurance",fpldem=T,time2=T)
  
  write.csv(outthis(ret.u$output),paste(cancertype,"medicaidparadox-out-unadjust-14-1yr.csv",sep=""))
  write.csv(outthis(ret.a$output),paste(cancertype,"medicaidparadox-out-adjust-14-1yr.csv",sep=""))
  write.csv(outthis(ret.a.fplB$output),paste(cancertype,"medicaidparadox-out-adjust-fpl-14-1yr.csv",sep=""))
  
  write.csv(outthis(ret.u2$output),paste(cancertype,"medicaidparadox-out-unadjust-11-4yr.csv",sep=""))
  write.csv(outthis(ret.a2$output),paste(cancertype,"medicaidparadox-out-adjust-11-4yr.csv",sep=""))
  write.csv(outthis(ret.a2.fplB$output),paste(cancertype,"medicaidparadox-out-adjust-fpl-11-4yr.csv",sep=""))
  
  write.csv(outthis(ret.u.$output),paste(cancertype,"medicaidparadox-out-unadjust-14-2yr.csv",sep=""))
  write.csv(outthis(ret.a.$output),paste(cancertype,"medicaidparadox-out-adjust-14-2yr.csv",sep=""))
  write.csv(outthis(ret.a.fplB.$output),paste(cancertype,"medicaidparadox-out-adjust-fpl-14-2yr.csv",sep=""))
  
  write.csv(outthis(ret.u2.$output),paste(cancertype,"medicaidparadox-out-unadjust-11-5yr.csv",sep=""))
  write.csv(outthis(ret.a2.$output),paste(cancertype,"medicaidparadox-out-adjust-11-5yr.csv",sep=""))
  write.csv(outthis(ret.a2.fplB.$output),paste(cancertype,"medicaidparadox-out-adjust-fpl-11-5yr.csv",sep=""))

  
  
  write.csv((ret.u$boots),paste(cancertype,"medicaidparadox-boot-unadjust-14-1yr.csv",sep=""))
  write.csv((ret.a$boots),paste(cancertype,"medicaidparadox-boot-adjust-14-1yr.csv",sep=""))
  write.csv((ret.a.fplB$boots),paste(cancertype,"medicaidparadox-boot-adjust-fpl-14-1yr.csv",sep=""))
  
  write.csv((ret.u2$boots),paste(cancertype,"medicaidparadox-boot-unadjust-11-4yr.csv",sep=""))
  write.csv((ret.a2$boots),paste(cancertype,"medicaidparadox-boot-adjust-11-4yr.csv",sep=""))
  write.csv((ret.a2.fplB$boots),paste(cancertype,"medicaidparadox-boot-adjust-fpl-11-4yr.csv",sep=""))
  
  write.csv((ret.u.$boots2),paste(cancertype,"medicaidparadox-boot-unadjust-14-2yr.csv",sep=""))
  write.csv((ret.a.$boots2),paste(cancertype,"medicaidparadox-boot-adjust-14-2yr.csv",sep=""))
  write.csv((ret.a.fplB.$boots2),paste(cancertype,"medicaidparadox-boot-adjust-fpl-14-2yr.csv",sep=""))
  
  write.csv((ret.u2.$boots2),paste(cancertype,"medicaidparadox-boot-unadjust-11-5yr.csv",sep=""))
  write.csv((ret.a2.$boots2),paste(cancertype,"medicaidparadox-boot-adjust-11-5yr.csv",sep=""))
  write.csv((ret.a2.fplB.$boots2),paste(cancertype,"medicaidparadox-boot-adjust-fpl-11-5yr.csv",sep=""))
  

  write.csv(outthis(ret.u.fplB$output),paste(cancertype,"medicaidparadox-out-unadjust-fpl-14-1yr.csv",sep=""))
  write.csv(outthis(ret.u.fplB.$output),paste(cancertype,"medicaidparadox-out-unadjust-fpl-14-2yr.csv",sep=""))
  write.csv(outthis(ret.u2.fplB$output),paste(cancertype,"medicaidparadox-out-unadjust-fpl-11-4yr.csv",sep=""))
  write.csv(outthis(ret.u2.fplB.$output),paste(cancertype,"medicaidparadox-out-unadjust-fpl-11-5yr.csv",sep=""))

  write.csv((ret.u.fplB$boots),paste(cancertype,"medicaidparadox-boot-unadjust-fpl-14-1yr.csv",sep=""))
  write.csv((ret.u.fplB.$boots),paste(cancertype,"medicaidparadox-boot-unadjust-fpl-14-2yr.csv",sep=""))
  write.csv((ret.u2.fplB$boots),paste(cancertype,"medicaidparadox-boot-unadjust-fpl-11-4yr.csv",sep=""))
  write.csv((ret.u2.fplB.$boots),paste(cancertype,"medicaidparadox-boot-unadjust-fpl-11-5yr.csv",sep=""))
  

  return(list(ret.u=ret.u,ret.a=ret.a,ret.a.fplB=ret.a.fplB,ret.u2=ret.u2,ret.a2=ret.a2,ret.a2.fplB=ret.a2.fplB,ret.u.=ret.u.,ret.a.=ret.a.,ret.a.fplB.=ret.a.fplB.,ret.u2.=ret.u2.,ret.a2.=ret.a2.,ret.a2.fplB.=ret.a2.fplB.,ret.u.fplB=ret.u.fplB,ret.u.fplB.=ret.u.fplB.,ret.u2.fplB=ret.u2.fplB,ret.u2.fplB.=ret.u2.fplB.))
}
#abc <- #list(ret.u=ret.u,ret.a=ret.a,ret.a.fplB=ret.a.fplB,ret.u2=ret.u2,ret.a2=ret.a2,ret.a2.fplB=ret.a2.fplB,ret.u.=ret.u.,ret.a.=ret.a.,ret.a.fplB.=ret.a.fplB.,ret.u2.=ret.u2.,ret.a2.=ret.a2.,ret.a2.fplB.=ret.a#2.fplB.,retmed.a=retmed.a,retmed.a2=retmed.a2,ret.u.fplB=ret.u.fplB,ret.u.fplB.=ret.u.fplB.,ret.u2.fplB=ret.u2.fplB,ret.u2.fplB.=ret.u2.fplB.,retmed.u=retmed.u,retmed.u2=retmed.u2)
dF. <- dF

breasts <- cancertype.paradox(dat,dat2,"Breast")
cervixs <- cancertype.paradox(dat,dat2,"Cervical")
colons <- cancertype.paradox(dat,dat2,"Colorectal")
hnc <- cancertype.paradox(dat,dat2,"Head and Neck")
hlymph <- cancertype.paradox(dat,dat2,"Hodgkin Lymphoma")
kidneys <- cancertype.paradox(dat,dat2,"Kidney and Renal Pelvis")
leukemias <- cancertype.paradox(dat,dat2,"Leukemia") 
livers <- cancertype.paradox(dat,dat2,"Liver")
lungs <- cancertype.paradox(dat,dat2,"Lung and Bronchus")
myelomas <- cancertype.paradox(dat,dat2,"Myeloma")
nonhlymph <- cancertype.paradox(dat,dat2,"Non-Hodgkin Lymphoma")
others <- cancertype.paradox(dat,dat2,"Other")
ovarys <- cancertype.paradox(dat,dat2,"Ovary")
pancreas <- cancertype.paradox(dat,dat2,"Pancreas")
prostates <- cancertype.paradox(dat,dat2,"Prostate") 
skins <- cancertype.paradox(dat,dat2,"Skin")
stomachs <- cancertype.paradox(dat,dat2,"Stomach")
testis <- cancertype.paradox(dat,dat2,"Testis") 
bladders <- cancertype.paradox(dat,dat2,"Urinary Bladder")
uterus <- cancertype.paradox(dat,dat2,"Uterus")
myelomas.fixed <- boot.fixer(myelomas)
overalls <- list(ret.u=ret.u,ret.a=ret.a,ret.a.fplB=ret.a.fplB,ret.u2=ret.u2,ret.a2=ret.a2,ret.a2.fplB=ret.a2.fplB,ret.u.=ret.u.,ret.a.=ret.a.,ret.a.fplB.=ret.a.fplB.,ret.u2.=ret.u2.,ret.a2.=ret.a2.,ret.a2.fplB.=ret.a2.fplB.,ret.u.fplB=ret.u.fplB,ret.u.fplB.=ret.u.fplB.,ret.u2.fplB=ret.u2.fplB,ret.u2.fplB.=ret.u2.fplB.,ret.u.fplexB=ret.u.fplexB,ret.a.fplexB=ret.a.fplexB,ret.u2.fplexB=ret.u2.fplexB,ret.a2.fplexB=ret.a2.fplexB,ret.u.fplexB.=ret.u.fplexB.,ret.a.fplexB.=ret.a.fplexB.,ret.u2.fplexB.=ret.u2.fplexB.,ret.a2.fplexB.=ret.a2.fplexB.)


boot.fixer <- function(obj){
  for(i in 1:length(obj)){
    mat <- obj[[i]]$boots
    reals <- obj[[i]]$estimate
    unis <- numeric(nrow(mat))
    for(j in 1:nrow(mat)) unis[j] <- length(unique(mat[j,]))
    keeps <- which(unis==max(unis))
    mat <- mat[keeps,]
    p.actual <- numeric(ncol(mat))
    ci.actual <- matrix(NA,nrow=ncol(mat),ncol=2)
    for(j in 1:ncol(mat)){
      ci.actual[j,] <- reals[j]-quantile(mat[,j]-reals[j],c(0.975,0.025))
      p.actual[j] <- 2*(.5-abs(0.5-mean(mat[,j]<0)))
    }
    ret.mat <- cbind((reals),(ci.actual),p.actual)
    ret.mat <- round(ret.mat,3)
    new.p <- numeric(nrow(ret.mat))
    for(j in 1:nrow(ret.mat)) new.p[j] <- 2*(.5-abs(0.5-ecdf_fun(mat[,j]-reals[j],reals[j])))
    insdiff <- reals[2]-reals[3]
    bootdiff <- mat[,2] - mat[,3]
    ret.mat <- cbind(ret.mat,new.p)
    insdiffci <- insdiff-quantile(bootdiff-insdiff,c(0.975,0.025))
    insdiffp <- 2*(.5-abs(0.5-ecdf_fun(bootdiff-insdiff,insdiff)))
    medicaidMINUSuninsured <- c(insdiff,insdiffci,insdiffp,insdiffp)
    obj[[i]]$output <- rbind(medicaidMINUSuninsured,ret.mat)
  }
  return(obj)
}


ins.extract <- function(objlist){
  mat <- NULL
  for(i in 1:length(objlist)){
    mat <- cbind(mat,objlist[[i]]$output[c(3,4,1),c(1,2,3,5)])
  }
  colnames(mat) <- paste(rep(c("Est","CIL","CIU","P"),length(objlist)),rep(names(objlist),each=4))
  rownames(mat) <- c("Medicaid","Uninsured","Med-Un")
  return(mat)
}


fpl.extract <- function(objlist){
  mat <- NULL
  these <- grep("fplB",names(objlist))
  for(i in these){
    mat <- c(mat,objlist[[i]]$output[nrow(objlist[[i]]$output),c(1,2,3,5)])
  }
  mat <- rbind(mat)
  colnames(mat) <- paste(rep(c("Est","CIL","CIU","P"),length(these)),rep(names(objlist)[these],each=4))
  return(mat)
}

ins.breast <- ins.extract(breasts)
ins.cervix <- ins.extract(cervixs)
ins.colon <- ins.extract(colons)
ins.hnc <- ins.extract(hnc)
ins.hlymph <- ins.extract(hlymph)
ins.kidney <- ins.extract(kidneys)
ins.leukemia <- ins.extract(leukemias)
ins.liver <- ins.extract(livers)
ins.lung <- ins.extract(lungs)
ins.myeloma <- ins.extract(myelomas.fixed)
ins.nonhlymph <- ins.extract(nonhlymph)
ins.other <- ins.extract(others)
ins.ovary <- ins.extract(ovarys)
ins.pancreas <- ins.extract(pancreas)
ins.prostate <- ins.extract(prostates)
ins.skin <- ins.extract(skins)
ins.stomach <- ins.extract(stomachs)
ins.testis <- ins.extract(testis)
ins.bladder <- ins.extract(bladders)
ins.uterus <- ins.extract(uterus)
ins.overall <- ins.extract(overalls)

fpl.breast <- fpl.extract(breasts)
fpl.cervix <- fpl.extract(cervixs)
fpl.colon <- fpl.extract(colons)
fpl.hnc <- fpl.extract(hnc)
fpl.hlymph <- fpl.extract(hlymph)
fpl.kidney <- fpl.extract(kidneys)
fpl.leukemia <- fpl.extract(leukemias)
fpl.liver <- fpl.extract(livers)
fpl.lung <- fpl.extract(lungs)
fpl.myeloma <- fpl.extract(myelomas.fixed)
fpl.nonhlymph <- fpl.extract(nonhlymph)
fpl.other <- fpl.extract(others)
fpl.ovary <- fpl.extract(ovarys)
fpl.pancreas <- fpl.extract(pancreas)
fpl.prostate <- fpl.extract(prostates)
fpl.skin <- fpl.extract(skins)
fpl.stomach <- fpl.extract(stomachs)
fpl.testis <- fpl.extract(testis)
fpl.bladder <- fpl.extract(bladders)
fpl.uterus <- fpl.extract(uterus)
fpl.overall <- fpl.extract(overalls)

ins.all <- rbind(ins.overall[,1:80],ins.breast,ins.cervix,ins.colon,ins.hnc,ins.hlymph,ins.kidney,ins.leukemia, ins.liver,ins.lung,ins.myeloma,ins.nonhlymph,ins.other,ins.ovary,ins.pancreas,ins.prostate,ins.skin,ins.stomach, ins.testis,ins.bladder,ins.uterus)

fpl.all <- rbind(fpl.overall,fpl.breast,fpl.cervix,fpl.colon,fpl.hnc,fpl.hlymph,fpl.kidney,fpl.leukemia, fpl.liver,fpl.lung,fpl.myeloma,fpl.nonhlymph,fpl.other,fpl.ovary,fpl.pancreas,fpl.prostate,fpl.skin,fpl.stomach, fpl.testis,fpl.bladder,fpl.uterus)


nameys <- c("overall","breast","cervix","colon","hnc","hlymph","kidney","leukemia", "liver","lung","myeloma","nonhlymph","other","ovary","pancreas","prostate","skin","stomach", "testis","bladder","uterus")
rownames(fpl.all) <- nameys
rownames(ins.all) <- paste(rep(nameys,each=3),rownames(ins.all))
rownames(meds.all) <- paste(rep(nameys,each=8),rownames(meds.all))
write.csv(fpl.all,"medicaidparadox-fpl-estimates.csv")
write.csv(ins.all,"medicaidparadox-insurance-estimates.csv")



total.maker <- function(mat){
  mat <- cbind(rowSums(mat),mat)
  colnames(mat)[1] <- "Total"
  return(mat)
}
percent.adder <- function(mat){
  coly <- colSums(mat)
  cols <- matrix(rep(coly,nrow(mat)),byrow=T,ncol=ncol(mat))
  mat2 <- mat/cols
  newmat <- paste(as.character(mat)," (",as.character(round(mat2,3)*100),")",sep="")
  newmat <- matrix(newmat,ncol=ncol(mat),nrow=nrow(mat))
  colnames(newmat) <- colnames(mat)
  rownames(newmat) <- rownames(mat)
  return(newmat)
}
doit <- function(mat){
  mat <- total.maker(mat)
  mat <- percent.adder(mat)
  return(mat)
}

meanse <- function(x,group){
  this <- paste(round(mean(x),1)," (",round(sd(x),1),")",sep="")
  for(i in as.numeric(names(table(group)))) this <- c(this,paste(round(mean(x[which(group==i)]),1)," (",round(sd(x[which(group==i)]),1),")",sep=""))
  this <- rbind(this)
  colnames(this) <- c("Total",names(table(group)))
  return(this)
}

groups <- dat$insurance


summarymat <- rbind(doit(rbind(table(groups))),
                    doit(rbind(table(dat$age3,groups))),
                    doit(rbind(table(dat$race,groups))),
                    doit(rbind(table(dat$sex,groups))),
                    doit(rbind(table(dat$residence,groups))),
                    doit(rbind(table(dat$stage,groups))),
                    doit(rbind(table(dat$state,groups))),
                    doit(rbind(table(dat$cancertype,groups))),
                    meanse(dat$income*10,groups),
                    meanse(dat$education/100,groups),
                    meanse(dat$fplP*100,groups))

groups <- dat2$insurance
summarymat2 <- rbind(doit(rbind(table(groups))),
                     doit(rbind(table(dat2$age3,groups))),
                     doit(rbind(table(dat2$race,groups))),
                     doit(rbind(table(dat2$sex,groups))),
                     doit(rbind(table(dat2$residence,groups))),
                     doit(rbind(table(dat2$stage,groups))),
                     doit(rbind(table(dat2$state,groups))),                    
                     doit(rbind(table(dat2$cancertype,groups))),
                     meanse(dat2$income*10,groups),
                     meanse(dat2$education/100,groups),
                     meanse(dat2$fplP*100,groups))

dat3 <- dat[which(dat$fplP>1 & dat$fplP<1.5),]
groups <- dat3$insurance
summarymat.fpl <- rbind(doit(rbind(table(groups))),
                        doit(rbind(table(dat3$age3,groups))),
                        doit(rbind(table(dat3$race,groups))),
                        doit(rbind(table(dat3$sex,groups))),
                        doit(rbind(table(dat3$residence,groups))),
                        doit(rbind(table(dat3$stage,groups))),
                        doit(rbind(table(dat3$state,groups))),
                        doit(rbind(table(dat3$cancertype,groups))),
                        meanse(dat3$income*10,groups),
                        meanse(dat3$education/100,groups),
                        meanse(dat3$fplP*100,groups))


dat3 <- dat2[which(dat2$fplP>1 & dat2$fplP<1.5),]
groups <- dat3$insurance
summarymat2.fpl <- rbind(doit(rbind(table(groups))),
                         doit(rbind(table(dat3$age3,groups))),
                         doit(rbind(table(dat3$race,groups))),
                         doit(rbind(table(dat3$sex,groups))),
                         doit(rbind(table(dat3$residence,groups))),
                         doit(rbind(table(dat3$stage,groups))),
                         doit(rbind(table(dat3$state,groups))),
                         doit(rbind(table(dat3$cancertype,groups))),
                         meanse(dat3$income*10,groups),
                         meanse(dat3$education/100,groups),
                         meanse(dat3$fplP*100,groups))


write.csv(summarymat,"Medicaidparadox-Table1-fullsample2014.csv")
write.csv(summarymat2,"Medicaidparadox-Table1-fullsample2011.csv")
write.csv(summarymat.fpl,"Medicaidparadox-Table1-fpl100-150-2014.csv")
write.csv(summarymat2.fpl,"Medicaidparadox-Table1-fpl100-150-2011.csv")








### Adjusted ests for plots

f <- prodlim(Hist(survmonths,cens)~1,data=dat) 
abc <- jackknife(f,times=seq(1,33))
coefs1 <- NULL
for(i in 1:ncol(abc)) coefs1 <- rbind(coefs1,lm(abc[,i]~insurance+age3+race+sex+residence+income+education+cancertype+stage+state+fplP,dat=dat)$coefficients)

f <- prodlim(Hist(survmonths,cens)~1,data=dat2) 
abc <- jackknife(f,times=seq(1,71))
coefs2<- NULL
for(i in 1:ncol(abc)) coefs2 <- rbind(coefs2,lm(abc[,i]~insurance+age3+race+sex+residence+income+education+cancertype+stage+state+fplP,dat=dat2)$coefficients)

estmat1 <- cbind(colMeans(model.matrix(~insurance+age3+race+sex+residence+income+education+cancertype+stage+state+fplP,data=dat)))
estmat2 <- cbind(colMeans(model.matrix(~insurance+age3+race+sex+residence+income+education+cancertype+stage+state+fplP,data=dat2)))
estmat1[nrow(estmat1)] <- estmat2[nrow(estmat2)] <- 1.5
estmat1 <- estmat1[-c(1:3),]
estmat2 <- estmat2[-c(1:3),]

asurvs1.private <- coefs1 %*% cbind(c(1,0,0,estmat1))
asurvs1.medicaid <- coefs1 %*% cbind(c(1,1,0,estmat1))
asurvs1.uninsured <- coefs1 %*% cbind(c(1,0,1,estmat1))
asurvs2.private <- coefs2 %*% cbind(c(1,0,0,estmat2))
asurvs2.medicaid <- coefs2 %*% cbind(c(1,1,0,estmat2))
asurvs2.uninsured <- coefs2 %*% cbind(c(1,0,1,estmat2))

asurvs1.private <- c(1,asurvs1.private)
asurvs1.medicaid <- c(1,asurvs1.medicaid)
asurvs1.uninsured <- c(1,asurvs1.uninsured)
asurvs2.private <- c(1,asurvs2.private)
asurvs2.medicaid <- c(1,asurvs2.medicaid)
asurvs2.uninsured <- c(1,asurvs2.uninsured)

plot(NULL,xlim=c(0,36),ylim=c(0,1),ylab="Overall Survival",
     xlab="Time (Months)")
lines(seq(0,33),asurvs1.private,
      type="s",col="black",lty=1,lwd=2)
lines(seq(0,33),asurvs1.medicaid,
      type="s",col="lightblue",lty=1,lwd=2)
lines(seq(0,33),asurvs1.uninsured,
      type="s",col="orange3",lty=1,lwd=2)
legend("topright",col=c("black","lightblue","orange3"),c("Private","Medicaid","Uninsured"),lty=1,lwd=2)

plot(NULL,xlim=c(0,72),ylim=c(0,1),ylab="Overall Survival",
     xlab="Time (Months)")
lines(seq(0,71),asurvs2.private,
      type="s",col="black",lty=1,lwd=2)
lines(seq(0,71),asurvs2.medicaid,
      type="s",col="lightblue",lty=1,lwd=2)
lines(seq(0,71),asurvs2.uninsured,
      type="s",col="orange3",lty=1,lwd=2)
legend("topright",col=c("black","lightblue","orange3"),c("Private","Medicaid","Uninsured"),lty=1,lwd=2)


f <- prodlim(Hist(survmonths,cens)~1,data=dat[which(dat$fplP>1 & dat$fplP<1.5),]) 
abc <- jackknife(f,times=seq(1,33))
coefs1 <- NULL
for(i in 1:ncol(abc)) coefs1 <- rbind(coefs1,lm(abc[,i]~insurance+age3+race+sex+residence+income+education+cancertype+stage+state,dat=dat[which(dat$fplP>1 & dat$fplP<1.5),])$coefficients)

f <- prodlim(Hist(survmonths,cens)~1,data=dat2[which(dat2$fplP>1 & dat2$fplP<1.5),]) 
abc <- jackknife(f,times=seq(1,71))
coefs2<- NULL
for(i in 1:ncol(abc)) coefs2 <- rbind(coefs2,lm(abc[,i]~insurance+age3+race+sex+residence+income+education+cancertype+stage+state,dat=dat2[which(dat2$fplP>1 & dat2$fplP<1.5),])$coefficients)

estmat1 <- cbind(colMeans(model.matrix(~insurance+age3+race+sex+residence+income+education+cancertype+stage+state,data=dat[which(dat$fplP>1 & dat$fplP<1.5),])))
estmat2 <- cbind(colMeans(model.matrix(~insurance+age3+race+sex+residence+income+education+cancertype+stage+state,data=dat2[which(dat2$fplP>1 & dat2$fplP<1.5),])))
estmat1 <- estmat1[-c(1:3),]
estmat2 <- estmat2[-c(1:3),]

asurvs1.privatef <- coefs1 %*% cbind(c(1,0,0,estmat1))
asurvs1.medicaidf <- coefs1 %*% cbind(c(1,1,0,estmat1))
asurvs1.uninsuredf <- coefs1 %*% cbind(c(1,0,1,estmat1))
asurvs2.privatef <- coefs2 %*% cbind(c(1,0,0,estmat2))
asurvs2.medicaidf <- coefs2 %*% cbind(c(1,1,0,estmat2))
asurvs2.uninsuredf <- coefs2 %*% cbind(c(1,0,1,estmat2))

asurvs1.privatef <- c(1,asurvs1.privatef)
asurvs1.medicaidf <- c(1,asurvs1.medicaidf)
asurvs1.uninsuredf <- c(1,asurvs1.uninsuredf)
asurvs2.privatef <- c(1,asurvs2.privatef)
asurvs2.medicaidf <- c(1,asurvs2.medicaidf)
asurvs2.uninsuredf <- c(1,asurvs2.uninsuredf)
for(i in 1:(length(asurvs1.privatef)-1)) if(asurvs1.privatef[i] < asurvs1.privatef[i+1]) asurvs1.privatef[i+1] <- asurvs1.privatef[i] 
for(i in 1:(length(asurvs2.privatef)-1)) if(asurvs2.privatef[i] < asurvs2.privatef[i+1]) asurvs2.privatef[i+1] <- asurvs2.privatef[i] 

plot(NULL,xlim=c(0,36),ylim=c(0,1),ylab="Overall Survival",
     xlab="Time (Months)")
lines(seq(0,33),asurvs1.privatef,
      type="s",col="black",lty=1,lwd=2)
lines(seq(0,33),asurvs1.medicaidf,
      type="s",col="lightblue",lty=1,lwd=2)
lines(seq(0,33),asurvs1.uninsuredf,
      type="s",col="orange3",lty=1,lwd=2)
legend("topright",col=c("black","lightblue","orange3"),c("Private","Medicaid","Uninsured"),lty=1,lwd=2)

plot(NULL,xlim=c(0,72),ylim=c(0,1),ylab="Overall Survival",
     xlab="Time (Months)")
lines(seq(0,71),asurvs2.privatef,
      type="s",col="black",lty=1,lwd=2)
lines(seq(0,71),asurvs2.medicaidf,
      type="s",col="lightblue",lty=1,lwd=2)
lines(seq(0,71),asurvs2.uninsuredf,
      type="s",col="orange3",lty=1,lwd=2)
legend("bottomleft",col=c("black","lightblue","orange3"),c("Private","Medicaid","Uninsured"),lty=1,lwd=2)


g1 <- survfit(Surv(survmonths,cens)~insurance,data=dat,type="kaplan-meier")
g2 <- survfit(Surv(survmonths,cens)~insurance,data=dat2,type="kaplan-meier")
g1f <- survfit(Surv(survmonths,cens)~insurance,data=dat[which(dat$fplP>1 & dat$fplP<1.5),],type="kaplan-meier")
g2f <- survfit(Surv(survmonths,cens)~insurance,data=dat2[which(dat2$fplP>1 & dat2$fplP<1.5),],type="kaplan-meier")

dat$fpl2 <- ifelse(dat$fplP>1.5,1,0) + ifelse(dat$fplP>2.5,1,0)

gf <- survfit(Surv(survmonths,cens)~fpl2,data=dat,type="kaplan-meier")


dat2$fpl2 <- ifelse(dat2$fplP>1.5,1,0) + ifelse(dat2$fplP>2.5,1,0)

gf2 <- survfit(Surv(survmonths,cens)~fpl2,data=dat2,type="kaplan-meier")


ggkmTable(g1,ystratalabs=c("Private","Medicaid","Uninsured"), main="",timeby=6,ylabs="Overall Survival",xlabs="Time (months)",maxtime=36,ystrataname="Insurance",pval=F,cols=c("black","lightblue","orange3"))
ggkmTable(g1f,ystratalabs=c("Private","Medicaid","Uninsured"), main="",timeby=6,ylabs="Overall Survival",xlabs="Time (months)",maxtime=36,ystrataname="Insurance",pval=F,cols=c("black","lightblue","orange3"))
ggkmTable(g2,ystratalabs=c("Private","Medicaid","Uninsured"), main="",timeby=12,ylabs="Overall Survival",xlabs="Time (months)",maxtime=72,ystrataname="Insurance",pval=F,cols=c("black","lightblue","orange3"))
ggkmTable(g2f,ystratalabs=c("Private","Medicaid","Uninsured"), main="",timeby=12,ylabs="Overall Survival",xlabs="Time (months)",maxtime=72,ystrataname="Insurance",pval=F,cols=c("black","lightblue","orange3"))
ggkmTable(list(time=seq(0,33),surv=c(asurvs1.private,asurvs1.medicaid,asurvs1.uninsured),strata=as.factor(c(rep(c(0,1,2),each=length(asurvs1.private))))),ystratalabs=c("Private","Medicaid","Uninsured"), main="",timeby=6,ylabs="Overall Survival",xlabs="Time (months)",maxtime=36,ystrataname="Insurance",pval=F,table=F,cols=c("black","lightblue","orange3"),objb=T)
ggkmTable(list(time=seq(0,33),surv=c(asurvs1.privatef,asurvs1.medicaidf,asurvs1.uninsuredf),strata=as.factor(c(rep(c(0,1,2),each=length(asurvs1.privatef))))),ystratalabs=c("Private","Medicaid","Uninsured"), main="",timeby=6,ylabs="Overall Survival",xlabs="Time (months)",maxtime=36,ystrataname="Insurance",pval=F,table=F,cols=c("black","lightblue","orange3"),objb=T)
ggkmTable(list(time=seq(0,71),surv=c(asurvs2.private,asurvs2.medicaid,asurvs2.uninsured),strata=as.factor(c(rep(c(0,1,2),each=length(asurvs2.private))))),ystratalabs=c("Private","Medicaid","Uninsured"), main="",timeby=6,ylabs="Overall Survival",xlabs="Time (months)",maxtime=72,ystrataname="Insurance",pval=F,table=F,cols=c("black","lightblue","orange3"),objb=T)
ggkmTable(list(time=seq(0,71),surv=c(asurvs2.privatef,asurvs2.medicaidf,asurvs2.uninsuredf),strata=as.factor(c(rep(c(0,1,2),each=length(asurvs2.privatef))))),ystratalabs=c("Private","Medicaid","Uninsured"), main="",timeby=6,ylabs="Overall Survival",xlabs="Time (months)",maxtime=72,ystrataname="Insurance",pval=F,table=F,cols=c("black","lightblue","orange3"),objb=T)


ggkmTable(gf,ystratalabs=c("<150% FPL","150-250% FPL",">250% FPL"), main="",timeby=6,ylabs="Overall Survival",xlabs="Time (months)",maxtime=36,ystrataname="Insurance",pval=F,cols=c("seagreen", "navyblue", "palevioletred1"))

hist(dat$fplP*100,xlab="Estimated % Federal Poverty Level",main="",freq=F,border="gray",ylim=c(0,0.01),)
hist(dat$fplP[which(dat$insurance==0)]*100,add=T,freq=F,border="black",col="black",density=3)
hist(dat$fplP[which(dat$insurance==1)]*100,add=T,freq=F,border="lightblue3",col="lightblue3",density=15,breaks=10)
hist(dat$fplP[which(dat$insurance==2)]*100,add=T,freq=F,border="orange3",col="orange3",density=6,breaks=10)
legend("top",c("Overall","Private/other","Medicaid","Uninsured"),col=c("gray","black","lightblue3","orange3"),lty=1,ncol=2,lwd=2)







##############
### Forest plots

forestplot.fpl <- function(obj,cols,type="insurance",os="1-year",xlims=c(-0.3,0.3),med.adjust=T){
  test <- data.frame(obj[,cols])
  library(stringr)
  library(plyr)
  library(ggplot2)
  library(ggstance)
  if(type=="insurance"){
    namers <- do.call("rbind",strsplit(rownames(test)," "))
    test$Subgroup <- namers[,1]
    test$Insurance <- namers[,2]
    test$Insurance[which(test$Insurance=="Medicaid")] <- "Medicaid vs. Private"
    test$Insurance[which(test$Insurance=="Uninsured")] <- "Uninsured vs. Private"
    test$Insurance[which(test$Insurance=="Med-Un")] <- "Medicaid vs. Uninsured"
  }
  else if(type=="fpl"){
    test$Subgroup <- rownames(test)
  }
  else{
    namers <- do.call("rbind",strsplit(rownames(test)," "))
    test$Subgroup <- namers[,1]
    test$Analysis <- namers[,2]
    test$Analysis[which(test$Analysis %in% c("mb.ao1","mb.uo1"))] <- "2014-16, 1-yr OS"
    test$Analysis[which(test$Analysis %in% c("mb.aa1","mb.ua1"))] <- "2014-16, 2-yr OS"
    test$Analysis[which(test$Analysis %in% c("mb.ao2","mb.uo2"))] <- "2011-13, 4-yr OS"
    test$Analysis[which(test$Analysis %in% c("mb.aa2","mb.ua2"))] <- "2011-13, 5-yr OS"
    if(med.adjust) test <-  test[seq(1,nrow(test),by=2),]
    else test <- test[seq(2,nrow(test),by=2),]
  }
  
  test$Subgroup <- str_to_title(test$Subgroup)
  test$Subgroup[which(test$Subgroup=="Hnc")] <- "Head and Neck"
  test$Subgroup[which(test$Subgroup=="Hlymph")] <- "Hodgkin Lymphoma"
  test$Subgroup[which(test$Subgroup=="Nonhlymph")] <- "Non-Hodgkin Lymphoma"
  
  test$y <- test[,1]
  test$ylo <- as.numeric(test[,2])
  test$yhi <- as.numeric(test[,3])
  
  xvals <- sort(unique(test$Subgroup),decreasing=T)[c(9,1:6,8,10:21,7)]
  #xvals <- test$Group[13:1]
  test$xseery <- factor(test$Subgroup, levels = xvals)
  
  if(type=="insurance"){
    f<-ggplot(test, aes(x=y, y=xseery, color=Insurance))+ 
      geom_pointrangeh(aes(xmin=ylo, xmax=yhi),
                       position = position_dodgev(height=0.5), alpha=.9, fatten=3) + 
      scale_color_manual(values=c("blue4","green4","goldenrod")) + geom_vline(xintercept = 0, linetype=2)+
      xlab(paste(os," overall survival difference",sep="")) +
      ylab("") + 
      coord_cartesian(xlim=xlims) +
      theme_minimal() +
      theme(legend.position="top",legend.direction="vertical",legend.title=element_blank(),axis.text.y=element_text(face=ifelse(levels(test$xseery)=="Overall","bold","plain"),
                                                                                                                    color = ifelse(levels(test$xseery)=="Overall","#2b83ba","black")))+ #,plot.margin      = grid::unit(c(0.1,0.1,2,0.1), 'lines'))  +
      guides(fill=guide_legend(reverse=FALSE), 
             colour=guide_legend(reverse=FALSE))
  }
  
  else if (type=="fpl"){
    test$y <- test$y*100
    test$ylo <- test$ylo*100
    test$yhi <- test$yhi*100
    f<-ggplot(test, aes(x=y, y=xseery))+ 
      geom_pointrangeh(aes(xmin=ylo, xmax=yhi),
                       position = position_dodgev(height=0.5), alpha=.9, fatten=3) + 
      scale_color_manual(values=c("black")) + geom_vline(xintercept = 0, linetype=2)+
      xlab(paste(os," overall survival difference",sep="")) +
      ylab("") + 
      coord_cartesian(xlim=xlims) +
      theme_minimal() +
      theme(axis.text.y=element_text(face=ifelse(levels(test$xseery)=="Overall","bold","plain"),
                                     color = ifelse(levels(test$xseery)=="Overall","#2b83ba","black"))) #,plot.margin      = grid::unit(c(0.1,0.1,2,0.1), 'lines'))  +
    
  }
  else{
    f<-ggplot(test, aes(x=y, y=xseery, color=Analysis))+ 
      geom_pointrangeh(aes(xmin=ylo, xmax=yhi),
                       position = position_dodgev(height=0.5), alpha=.9, fatten=3) + 
      scale_color_manual(values=c("blue4","green4","goldenrod","violetred")) + geom_vline(xintercept = 0, linetype=2)+
      xlab(paste(os,"",sep="")) +
      ylab("") + 
      coord_cartesian(xlim=xlims) +
      theme_minimal() +
      theme(legend.position="top",legend.direction="vertical",legend.title=element_blank(),axis.text.y=element_text(face=ifelse(levels(test$xseery)=="Overall","bold","plain"),
                                                                                                                    color = ifelse(levels(test$xseery)=="Overall","#2b83ba","black")))+ #,plot.margin      = grid::unit(c(0.1,0.1,2,0.1), 'lines'))  +
      guides(fill=guide_legend(reverse=FALSE), 
             colour=guide_legend(reverse=FALSE))
    
  }
  
  return(f)
  
}


ins.u <- forestplot.fpl(ins.all,c(1:4),os="1-year",xlims=c(-.3,.3))
ins.a <- forestplot.fpl(ins.all,c(5:8),os="1-year",xlims=c(-.3,.3))
ins.afpl <- forestplot.fpl(ins.all,c(9:12),os="1-year",xlims=c(-.3,.3))

ins.u2 <- forestplot.fpl(ins.all,c(13:16),os="4-year",xlims=c(-.3,.3))
ins.a2 <- forestplot.fpl(ins.all,c(17:20),os="4-year",xlims=c(-.3,.3))
ins.afpl2 <- forestplot.fpl(ins.all,c(21:24),os="4-year",xlims=c(-.3,.3))

ins.u. <- forestplot.fpl(ins.all,c(25:28),os="2-year",xlims=c(-.3,.3))
ins.a. <- forestplot.fpl(ins.all,c(29:32),os="2-year",xlims=c(-.3,.3))
ins.afpl. <- forestplot.fpl(ins.all,c(33:36),os="2-year",xlims=c(-.3,.3))

ins.u2. <- forestplot.fpl(ins.all,c(37:40),os="5-year",xlims=c(-.3,.3))
ins.a2. <- forestplot.fpl(ins.all,c(41:44),os="5-year",xlims=c(-.3,.3))
ins.afpl2. <- forestplot.fpl(ins.all,c(45:48),os="5-year",xlims=c(-.3,.3))





fpl.afpl <- forestplot.fpl(fpl.all,c(1:4),os="1-year",xlims=c(-5,15),type="fpl")
fpl.afpl2 <- forestplot.fpl(fpl.all,c(5:8),os="4-year",xlims=c(-5,15),type="fpl")
fpl.afpl. <- forestplot.fpl(fpl.all,c(9:12),os="2-year",xlims=c(-5,15),type="fpl")
fpl.afpl2. <- forestplot.fpl(fpl.all,c(13:16),os="5-year",xlims=c(-5,15),type="fpl")
fpl.ufpl <- forestplot.fpl(fpl.all,c(17:20),os="1-year",xlims=c(-10,10),type="fpl")
fpl.ufpl2 <- forestplot.fpl(fpl.all,c(21:24),os="4-year",xlims=c(-10,10),type="fpl")
fpl.ufpl. <- forestplot.fpl(fpl.all,c(25:28),os="2-year",xlims=c(-10,10),type="fpl")
fpl.ufpl2. <- forestplot.fpl(fpl.all,c(29:32),os="5-year",xlims=c(-10,10),type="fpl")


pdf("medicaidparadox-forestplot-unadjust2014-1yr.pdf")
ins.u
dev.off()

pdf("medicaidparadox-forestplot-adjust2014-1yr.pdf")
ins.a
dev.off()

pdf("medicaidparadox-forestplot-adjustFPL2014-1yr.pdf")
ins.afpl
dev.off()

pdf("medicaidparadox-forestplot-unadjust2011-4yr.pdf")
ins.u2
dev.off()

pdf("medicaidparadox-forestplot-adjust2011-4yr.pdf")
ins.a2
dev.off()

pdf("medicaidparadox-forestplot-adjustFPL2011-4yr.pdf")
ins.afpl2
dev.off()

pdf("medicaidparadox-forestplot-unadjust2014-2yr.pdf")
ins.u.
dev.off()

pdf("medicaidparadox-forestplot-adjust2014-2yr.pdf")
ins.a.
dev.off()

pdf("medicaidparadox-forestplot-adjustFPL2014-2yr.pdf")
ins.afpl.
dev.off()

pdf("medicaidparadox-forestplot-unadjust2011-5yr.pdf")
ins.u2.
dev.off()

pdf("medicaidparadox-forestplot-adjust2011-5yr.pdf")
ins.a2.
dev.off()

pdf("medicaidparadox-forestplot-adjustFPL2011-5yr.pdf")
ins.afpl2.
dev.off()




pdf("medicaidparadox-forestplot-fpladjustFPL2011-5yr.pdf")
fpl.afpl2.
dev.off()

pdf("medicaidparadox-forestplot-fpladjustFPL2011-4yr.pdf")
fpl.afpl2
dev.off()

pdf("medicaidparadox-forestplot-fplunadjustFPL2011-5yr.pdf")
fpl.ufpl2.
dev.off()

pdf("medicaidparadox-forestplot-fplunadjustFPL2011-4yr.pdf")
fpl.ufpl2
dev.off()

pdf("medicaidparadox-forestplot-fpladjustFPL2014-2yr.pdf")
fpl.afpl.
dev.off()

pdf("medicaidparadox-forestplot-fpladjustFPL2014-1yr.pdf")
fpl.afpl
dev.off()

pdf("medicaidparadox-forestplot-fplunadjustFPL2014-2yr.pdf")
fpl.ufpl.
dev.off()

pdf("medicaidparadox-forestplot-fplunadjustFPL2014-1yr.pdf")
fpl.ufpl
dev.off()




