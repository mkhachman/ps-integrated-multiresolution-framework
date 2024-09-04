rm(list=ls())

###Packages
library(data.table)
library(sf)
library(dplyr)
library(stringr)
library(gsubfn)

###Import the hierarchical sample
PUMF<-fread("Input/Census/PUMF/Hierarchical PUMF.csv", header=TRUE)
###HHCOUNT
PUMF$HHCOUNT<-1
###PPCOUNT
agg<-aggregate(data=PUMF, PP_ID~HH_ID, length)
names(agg)[2]<-"n"
PUMF<-merge(PUMF,agg,by='HH_ID')
colnames(PUMF)[colnames(PUMF)=='n']<-'PPCOUNT'
###HHSIZE
PUMF$HHSIZE[PUMF$PPCOUNT<5]<-PUMF$PPCOUNT[PUMF$PPCOUNT<5]
PUMF$HHSIZE[PUMF$PPCOUNT>=5]<-5
###HHTYPE
agg1<-aggregate(data=PUMF, CF_ID~HH_ID, function(x) length(unique(x)))
names(agg1)[2]<-"NCF"
agg2<-aggregate(data=PUMF[PUMF$CFSTAT==4|PUMF$CFSTAT==5,], PP_ID~HH_ID, length)
names(agg2)[2]<-"NCHILDREN"
PUMF<-merge(PUMF,agg1,by="HH_ID",all.x=TRUE)
PUMF<-merge(PUMF,agg2,by="HH_ID",all.x=TRUE)
PUMF$NCHILDREN[is.na(PUMF$NCHILDREN)]<-0
PUMF$HHTYPE[PUMF$HHSIZE==1]<-1
PUMF$HHTYPE[PUMF$HHSIZE>1 & PUMF$PPCOUNT==PUMF$NCF]<-1
PUMF$HHTYPE[PUMF$HHSIZE>1 & PUMF$NCF==1 & PUMF$NCHILDREN==0]<-2
PUMF$HHTYPE[PUMF$HHSIZE>1 & PUMF$NCF==1 & PUMF$NCHILDREN>0]<-3
PUMF$HHTYPE[PUMF$NCF>1]<-4
###HHNETINC
PUMF$TOTINC_AT[PUMF$TOTINC_AT==99999999]<-0
agg<-aggregate(data=PUMF, TOTINC_AT~HH_ID, sum)
names(agg)[2]<-"HHNETINC"
PUMF<-merge(PUMF,agg,by='HH_ID',all.x=TRUE)
PUMF$HHNETINC[PUMF$HHNETINC<30000]<-1
PUMF$HHNETINC[PUMF$HHNETINC>=30000 & PUMF$HHNETINC<60000]<-2
PUMF$HHNETINC[PUMF$HHNETINC>=60000 & PUMF$HHNETINC<100000]<-3
PUMF$HHNETINC[PUMF$HHNETINC>=100000]<-4
###HHDTYPE
PUMF$HHDTYPE<-PUMF$DTYPE
PUMF$HHDTYPE[PUMF$HHDTYPE==8]<-88
###HHDCONDO
PUMF$HHDCONDO<-PUMF$CONDO
PUMF$HHDCONDO[PUMF$HHDCONDO==0]<-2
PUMF$HHDCONDO[PUMF$HHDCONDO==8]<-88
###HHDNBEDRM
PUMF$HHDNBEDRM<-PUMF$BEDRM
PUMF$HHDNBEDRM[PUMF$HHDNBEDRM%in%c(8)]<-88
PUMF$HHDNBEDRM[PUMF$HHDNBEDRM%in%c(4,5)]<-5
PUMF$HHDNBEDRM[PUMF$HHDNBEDRM%in%c(3)]<-4
PUMF$HHDNBEDRM[PUMF$HHDNBEDRM%in%c(2)]<-3
PUMF$HHDNBEDRM[PUMF$HHDNBEDRM%in%c(1)]<-2
PUMF$HHDNBEDRM[PUMF$HHDNBEDRM%in%c(0)]<-1
###HHDBUILT
PUMF$HHDBUILT<-PUMF$BUILT
PUMF$HHDBUILT[PUMF$HHDBUILT%in%c(1,2,3)]<-1
PUMF$HHDBUILT[PUMF$HHDBUILT%in%c(4,5)]<-2
PUMF$HHDBUILT[PUMF$HHDBUILT%in%c(6,7,8)]<-3
PUMF$HHDBUILT[PUMF$HHDBUILT%in%c(9,10,11)]<-4
###PPSEX
PUMF$PPSEX<-PUMF$SEX
PUMF$PPSEX[PUMF$PPSEX==1]<-0
PUMF$PPSEX[PUMF$PPSEX==2]<-1
PUMF$PPSEX[PUMF$PPSEX==0]<-2
PUMF$PPSEX[PUMF$PPSEX==8]<-88
###PPAGEGRP
PUMF$PPAGEGRP<-PUMF$AGEGRP
PUMF$PPAGEGRP[PUMF$PPAGEGRP%in%c(1,2)]<-1
PUMF$PPAGEGRP[PUMF$PPAGEGRP%in%c(3,4,5,6,7,8,9,10,11)]<-2
PUMF$PPAGEGRP[PUMF$PPAGEGRP%in%c(12,13)]<-3
#PPMARST
PUMF$PPMARST<-PUMF$MarStH
PUMF$PPMARST[PUMF$PPMARST%in%c(8)]<-88
###PPLFST
PUMF$PPLFST<-PUMF$LFTAG
PUMF$PPLFST[PUMF$PPLFST%in%c(1,2)]<-1
PUMF$PPLFST[PUMF$PPLFST%in%c(3,4,5,6,7,8,9,10)]<-2
PUMF$PPLFST[PUMF$PPLFST%in%c(11,12,13,14,99)]<-3
###PPIND
PUMF$PPIND<-PUMF$NAICS
#Impute using NOCS
for(i in 1:10){
  PUMF$PPIND[PUMF$PPIND==88 & PUMF$NOCS==i]=as.numeric(names(which.max(table(PUMF$PPIND[PUMF$NOCS==i]))))
}
PUMF$PPIND[PUMF$PPIND%in%c(4,5)]<-4
PUMF$PPIND[PUMF$PPIND%in%c(6)]<-5
PUMF$PPIND[PUMF$PPIND%in%c(7)]<-6
PUMF$PPIND[PUMF$PPIND%in%c(8)]<-7
PUMF$PPIND[PUMF$PPIND%in%c(9)]<-8
PUMF$PPIND[PUMF$PPIND%in%c(10)]<-9
PUMF$PPIND[PUMF$PPIND%in%c(11)]<-10
###PPTJTWMODE
PUMF$PPJTWMODE<-PUMF$MODE
PUMF$PPJTWMODE[PUMF$PPJTWMODE%in%c(3,4)]<-4
PUMF$PPJTWMODE[PUMF$PPJTWMODE%in%c(1,7)]<-3
PUMF$PPJTWMODE[PUMF$PPJTWMODE%in%c(2,5)]<-1
PUMF$PPJTWMODE[PUMF$PPJTWMODE%in%c(6)]<-2
PUMF$PPJTWMODE[PUMF$PPJTWMODE%in%c(9)]<-99
###PPJTWDUR
PUMF$PPJTWDUR<-PUMF$DUR
PUMF$PPJTWDUR[PUMF$PPJTWDUR%in%c(8)]<-88
PUMF$PPJTWDUR[PUMF$PPJTWDUR%in%c(9)]<-99
###PPJTWLEAVE
PUMF$PPJTWLEAVE<-PUMF$LEAVE
PUMF$PPJTWLEAVE[PUMF$PPJTWLEAVE%in%c(9)]<-99
PUMF$PPJTWLEAVE[PUMF$PPJTWLEAVE%in%c(5,6)]<-5

###PUMF
PUMF<-data.table(hhid=PUMF$HH_ID
                ,hhcount=PUMF$HHCOUNT
                ,hhsize=PUMF$HHSIZE
                ,hhtype=PUMF$HHTYPE
                ,hhnetinc=PUMF$HHNETINC
                # ,hhdtype=PUMF$HHDTYPE
                ,hhdcondo=PUMF$HHDCONDO
                # ,hhdnbedrm=PUMF$HHDNBEDRM
                ,hhdbuilt=PUMF$HHDBUILT
                
                ,ppid=PUMF$PP_ID
                ,ppcount=PUMF$PPCOUNT
                ,ppsex=PUMF$PPSEX
                ,ppage=PUMF$PPAGEGRP
                ,ppmarst=PUMF$PPMARST
                ,pplfst=PUMF$PPLFST
                # ,ppind=PUMF$PPIND
                # ,ppjtwmode=PUMF$PPJTWMODE
                # ,ppjtwdur=PUMF$PPJTWDUR
                # ,ppjtwleave=PUMF$PPJTWLEAVE
)

#Indicators
nvarhh1=length(names(PUMF)[grepl('hh',names(PUMF))==TRUE])
nvarpp1=length(names(PUMF)[grepl('pp',names(PUMF))==TRUE])
nvar1=nvarhh1+nvarpp1
npp1=length(PUMF$ppid)
nhh1=length(unique(PUMF$hhid))
indicators1<-data.frame("number of household variables"=nvarhh1,
                          "number of person variables"=nvarpp1,
                          "number of households"=nhh1,
                          "number of persons"=npp1)




###Filter NAs from all variables
var<-names(PUMF)
PUMF<-data.frame(PUMF)
#Write the condition iterating over the variables checking ==88
varna<-paste(append(paste("PUMF[var[",1:(length(var)-1),"]]==88 | ",sep=""), paste("PUMF[var[",length(var),"]]==88",sep="")),collapse="")
varna<-eval(parse(text=varna))
#Drop households that have NAs
hhdrops<-unique(PUMF[varna,]$hhid)
PUMF<-PUMF[!(PUMF$hhid %in% hhdrops) ,]

write.csv(PUMF,"Output/hPUMF.csv")



#Indicators
nvarhh2=length(names(PUMF)[grepl('hh',names(PUMF))==TRUE])
nvarpp2=length(names(PUMF)[grepl('pp',names(PUMF))==TRUE])
nvar2=nvarhh2+nvarpp2
npp2=length(PUMF$ppid)
nhh2=length(unique(PUMF$hhid))
indicators2<-data.frame("number of household variables"=nvarhh2,
                        "number of person variables"=nvarpp2,
                        "number of households"=nhh2,
                        "number of persons"=npp2)