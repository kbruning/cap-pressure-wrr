require(mgcv)
require(R.matlab)

S1<-read.csv("SP-random.csv",head=TRUE,sep=" ")
S2<-read.csv("SP-dynamic.csv",head=TRUE,sep=" ")

data<-rbind(S1[,c("sw","pwn","awn","ans","Euler")],S2[,c("sw","pwn","awn","ans","Euler")])

D32=83.663
radius = D32/2
porosity = 0.369

sw <- data$sw
epn<-(1-data$sw)*porosity
epwn<-data$awn
epns<-data$ans
Jpwn<-data$pwn
Jpns<-2*D32/radius
epwn.epns <- epns+epwn
Jpwnepwn<-data$awn*Jpwn
Jpnsepns<-data$ans*Jpns
Jpwnepwn.Jpnsepns<-Jpnsepns+Jpwnepwn
Xpn<-data$Euler

##GAM 1 of 16
GAM<-gam(epn~te(epwn.epns,Jpwnepwn.Jpnsepns,Xpn))
rsq_1<-summary(GAM)$r.sq
gcv_1<-GAM$gcv.ubre

##GAM 2 of 16
GAM<-gam(Jpwnepwn.Jpnsepns~te(epn,epwn.epns,Xpn))
rsq_2<-summary(GAM)$r.sq
gcv_2<-GAM$gcv.ubre

##GAM 3 of 16
GAM<-gam(epn~te(epwn,Jpwnepwn,Xpn))
rsq_3<-summary(GAM)$r.sq
gcv_3<-GAM$gcv.ubre

##GAM 4 of 16
GAM<-gam(Jpwnepwn~te(epn,epwn,Xpn))
rsq_4<-summary(GAM)$r.sq
gcv_4<-GAM$gcv.ubre

##GAM 5 of 16
GAM<-gam(epn~te(epwn.epns,Jpwnepwn.Jpnsepns))
rsq_5<-summary(GAM)$r.sq
gcv_5<-GAM$gcv.ubre

##GAM 6 of 16
GAM<-gam(Jpwnepwn.Jpnsepns~te(epn,epwn.epns))
rsq_6<-summary(GAM)$r.sq
gcv_6<-GAM$gcv.ubre

##GAM 7 of 16
GAM<-gam(epn~te(epwn,Jpwnepwn))
rsq_7<-summary(GAM)$r.sq
gcv_7<-GAM$gcv.ubre

##GAM 8 of 16
GAM<-gam(Jpwnepwn~te(epn,epwn))
rsq_8<-summary(GAM)$r.sq
gcv_8<-GAM$gcv.ubre

##GAM 9 of 16
GAM<-gam(epn~te(Jpwnepwn.Jpnsepns,Xpn))
rsq_9<-summary(GAM)$r.sq
gcv_9<-GAM$gcv.ubre

##GAM 10 of 16
GAM<-gam(Jpwnepwn.Jpnsepns~te(epn,Xpn))
rsq_10<-summary(GAM)$r.sq
gcv_10<-GAM$gcv.ubre

##GAM 11 of 16
GAM<-gam(epn~te(Jpwnepwn,Xpn))
rsq_11<-summary(GAM)$r.sq
gcv_11<-GAM$gcv.ubre

##GAM 12 of 16
GAM<-gam(Jpwnepwn~te(epn,Xpn))
rsq_12<-summary(GAM)$r.sq
gcv_12<-GAM$gcv.ubre

##GAM 13 of 16
GAM<-gam(epn~te(Jpwnepwn.Jpnsepns))
rsq_13<-summary(GAM)$r.sq
gcv_13<-GAM$gcv.ubre

##GAM 14 of 16
GAM<-gam(Jpwnepwn.Jpnsepns~te(epn))
rsq_14<-summary(GAM)$r.sq
gcv_14<-GAM$gcv.ubre

##GAM 15 of 16
GAM<-gam(epn~te(Jpwnepwn))
rsq_15<-summary(GAM)$r.sq
gcv_15<-GAM$gcv.ubre

##GAM 16 of 16
GAM<-gam(Jpwnepwn~te(epn))
rsq_16<-summary(GAM)$r.sq
gcv_16<-GAM$gcv.ubre

##GAM Extra 1 of 3
GAM<-gam(Jpwn~te(sw))
rsq_ex_1<-summary(GAM)$r.sq
gcv_ex_1<-GAM$gcv.ubre

##GAM Extra 2 of 3
GAM<-gam(Jpwn~te(sw,epwn))
rsq_ex_2<-summary(GAM)$r.sq
gcv_ex_2<-GAM$gcv.ubre

##GAM Extra 3 of 3
GAM<-gam(Jpwn~te(sw,epwn,Xpn))
rsq_ex_3<-summary(GAM)$r.sq
gcv_ex_3<-GAM$gcv.ubre

rsq <- c(rsq_1, rsq_2, rsq_3, rsq_4, rsq_5, rsq_6, rsq_7, rsq_8, rsq_9, rsq_10, 
         rsq_11, rsq_12, rsq_13, rsq_14,rsq_15, rsq_16, rsq_ex_1, rsq_ex_2, rsq_ex_3)

gcv <- c(gcv_1, gcv_2, gcv_3, gcv_4, gcv_5, gcv_6, gcv_7, gcv_8, gcv_9, gcv_10,
         gcv_11, gcv_12, gcv_13, gcv_14, gcv_15, gcv_16, gcv_ex_1, gcv_ex_2, gcv_ex_3)

data2 <- data.frame(rsq,gcv)

head(data2,n=19)


