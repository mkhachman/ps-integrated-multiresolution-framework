library(tidyverse)
library(data.table)
rm(list=ls())
gc()
options(scipen = 999)


###Combine weighted and synthetic populations generated for DAs
# WP_3RI<-list.files(path="Output/Weighted population", full.names=TRUE) %>% map_dfr(readRDS)
# saveRDS(WP_3RI,"Output/WP_3RI")
# rm(list=ls())
# gc()
# SP_3RI<-list.files(path="Output/Synthetic population", full.names=TRUE) %>% map_dfr(readRDS)
# saveRDS(SP_3RI,"Output/SP_3RI")
# rm(list=ls())
# gc()
# WP_1RS<-list.files(path="Output/1RS_Weighted population", full.names=TRUE) %>% map_dfr(readRDS)
# saveRDS(WP_1RS,"Output/WP_1RS")
# rm(list=ls())
# gc()
# SP_1RS<-list.files(path="Output/1RS_Synthetic population", full.names=TRUE) %>% map_dfr(readRDS)
# saveRDS(SP_1RS,"Output/SP_1RS")
# rm(list=ls())
# gc()
# SSP_1RS<-list.files(path="Output/1RS_Spatialized synthetic population", full.names=TRUE) %>% map_dfr(readRDS)
# saveRDS(SSP_1RS,"Output/SSP_1RS")
# rm(list=ls())
# gc()


###Inputs
##Census totals
CDT<-fread("Output/hCDT.csv")
CDT<-CDT[,-1]
DAT<-fread("Output/hDAT.csv")
DAT<-DAT[,-1]
DBT<-fread("Output/hDBT.csv")
DBT<-DBT[,-1]
BT<-fread("Output/hBT.csv")
BT<-BT[,-1]
PUMF<-fread("Output/hPUMF.csv")
PUMF<-PUMF[,-1]
GIS<-fread("Output/hGIS.csv")
GIS<-GIS[,-1]
#Filter targets to variables kept for the PUMF
CDT<-CDT[,gsub("\\..*","",names(CDT))%in%c("geo",names(PUMF)),with=FALSE]
DAT<-DAT[,gsub("\\..*","",names(DAT))%in%c("geo",names(PUMF)),with=FALSE]
DBT<-DBT[,gsub("\\..*","",names(DBT))%in%c("geo",names(PUMF)),with=FALSE]
BT<-BT[,gsub("\\..*","",names(BT))%in%c("geo",names(PUMF)),with=FALSE]
# #Convert to data frames
CDT<-as.data.frame(CDT)
DAT<-as.data.frame(DAT)
DAT<-arrange(DAT,geo)
DBT<-as.data.frame(DBT)
DBT<-arrange(DBT,geo)
BT<-as.data.frame(BT)
# BT<-arrange(BT,geo)
#Each resolution variables
cdcat<-names(CDT)[names(CDT)!="geo"]
dacat<-names(DAT)[!(names(DAT)%in%names(DBT) | names(DAT)%in%names(BT))]
dbcat<-names(DBT)[!names(DBT)%in%names(BT)]
bcat<-names(BT)[names(BT)!="geo"]


###Import weighted and synthetic populations
# WP_3RI<-data.frame(readRDS("Output/WP_3RI"))
SP_3RI<-data.frame(readRDS("Output/SP_3RI"))
# WP_1RS<-data.frame(readRDS("Output/WP_1RS"))
# SP_1RS<-data.frame(readRDS("Output/SP_1RS"))
SSP_1RS<-data.frame(readRDS("Output/SSP_1RS"))
# #To be assessed
CFSP<-SSP_1RS
CFSP$cd_id<-2466
CFSP$db_id<-as.numeric(CFSP$db_id)
IFSP<-SP_3RI
IFSP$cd_id<-2466
IFSP$db_id<-as.numeric(IFSP$db_id)


###Local indicators
CFSP_RMSE<-data.frame(DA=unique(DAT$geo),CFSP_DAcdcat=c(0),CFSP_DAdacat_DB_B=c(0),CFSP_DAcdcat_DAdacat_DB_B=c(0), CFSP_B=c(0))
CFSP_NRMSE<-data.frame(DA=unique(DAT$geo),CFSP_DAcdcat=c(0),CFSP_DAdacat_DB_B=c(0),CFSP_DAcdcat_DAdacat_DB_B=c(0), CFSP_B=c(0))
IFSP_RMSE<-data.frame(DA=unique(DAT$geo),IFSP_DAcdcat=c(0),IFSP_DAdacat_DB_B=c(0),IFSP_DAcdcat_DAdacat_DB_B=c(0), IFSP_B=c(0))
IFSP_NRMSE<-data.frame(DA=unique(DAT$geo),IFSP_DAcdcat=c(0),IFSP_DAdacat_DB_B=c(0),IFSP_DAcdcat_DAdacat_DB_B=c(0), IFSP_B=c(0))

for (i in unique(GIS$da_id)){
  # i=unique(GIS$da_id)[1]
  print(i)
  
  ##Census
  DAT_i<-DAT[DAT$geo==i,]
  GIS$db_id<-as.numeric(GIS$db_id)
  DBT$geo<-as.numeric(DBT$geo)
  DBT_i<-DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i],]
  DBT_i<-arrange(DBT_i,geo)
  BT_i<-BT[BT$geo%in%GIS$bld_id[GIS$da_id==i],]
  BT_i<-arrange(BT_i,geo)
  
  ##CFSP
  CFSP_i<-CFSP[CFSP$da_id==i,]
  #CFSP_DAcdcat
  CFSP_DAcdcat<-aggregate(data=CFSP_i,eval(parse(text=paste("cbind(",paste(paste(cdcat,"*weight",sep=""),collapse=","),")",sep="")))~da_id,sum) 
  names(CFSP_DAcdcat)<-c("da_id",cdcat)
  CFSP_DAcdcat<-data.table(CFSP_DAcdcat)
  #CFSP_DAdacat
  CFSP_DAdacat<-aggregate(data=CFSP_i,eval(parse(text=paste("cbind(",paste(paste(dacat,"*weight",sep=""),collapse=","),")",sep="")))~da_id,sum) 
  names(CFSP_DAdacat)<-c("da_id",dacat)
  CFSP_DAdacat<-data.table(CFSP_DAdacat)
  #CFSP_DB
  CFSP_DB<-aggregate(data=CFSP_i,eval(parse(text=paste("cbind(",paste(paste(dbcat,"*weight",sep=""),collapse=","),")",sep="")))~db_id,sum) 
  names(CFSP_DB)<-c("db_id",dbcat)
  CFSP_DB<-data.frame(merge(data.table(DBT_i)[,"geo"],CFSP_DB,by.x="geo",by.y="db_id",all.x=TRUE))
  CFSP_DB[is.na(CFSP_DB)]<-0
  CFSP_DB<-data.table(CFSP_DB)
  CFSP_DB<-arrange(CFSP_DB,geo)
  #CFSP_B
  CFSP_B<-aggregate(data=CFSP_i,eval(parse(text=paste("cbind(",paste(paste(bcat,"*weight",sep=""),collapse=","),")",sep="")))~bld_id,sum)   
  names(CFSP_B)<-c("bld_id",bcat)
  CFSP_B<-data.frame(merge(data.table(BT_i)[,"geo"],CFSP_B,by.x="geo",by.y="bld_id",all.x=TRUE))
  CFSP_B[is.na(CFSP_B)]<-0
  CFSP_B<-data.table(CFSP_B)
  CFSP_B<-arrange(CFSP_B,geo)
  #Indicators
  CFSP_DAcdcat<-as.data.frame(CFSP_DAcdcat)
  CFSP_DAdacat<-as.data.frame(CFSP_DAdacat)
  CFSP_DB<-as.data.frame(CFSP_DB)
  CFSP_B<-as.data.frame(CFSP_B)
  #RMSE DAcdcat
  RMSE_DAcdcat<-sqrt(sum((unlist(CFSP_DAcdcat[,cdcat])-unlist(DAT_i[,cdcat]))^2)/(length(unlist(DAT_i[,cdcat]))))
  NRMSE_DAcdcat<-sqrt(sum((unlist(CFSP_DAcdcat[,cdcat])-unlist(DAT_i[,cdcat]))^2)/(length(unlist(DAT_i[,cdcat]))))/mean(unlist(DAT_i[,cdcat]))
  #RMSE DAdacat_DB_BLD
  RMSE_DAdacat_DB_B<-sqrt(sum((unlist(CFSP_DAdacat[,dacat])-unlist(DAT_i[,dacat]))^2,(unlist(CFSP_DB[,dbcat])-unlist(DBT_i[,dbcat]))^2,(unlist(CFSP_B[,bcat])-unlist(BT_i[,bcat]))^2)/length(c(unlist(DAT_i[,dacat]),unlist(DBT_i[,dbcat]),unlist(BT_i[,bcat]))))
  NRMSE_DAdacat_DB_B<-sqrt(sum((unlist(CFSP_DAdacat[,dacat])-unlist(DAT_i[,dacat]))^2,(unlist(CFSP_DB[,dbcat])-unlist(DBT_i[,dbcat]))^2,(unlist(CFSP_B[,bcat])-unlist(BT_i[,bcat]))^2)/length(c(unlist(DAT_i[,dacat]),unlist(DBT_i[,dbcat]),unlist(BT_i[,bcat]))))/mean(c(unlist(DAT_i[,dacat]),unlist(DBT_i[,dbcat]),unlist(BT_i[,bcat])))
  #RMSE DAcdcat_DAdacat_DB_B
  RMSE_DAcdcat_DAdacat_DB_B<-sqrt(sum((unlist(CFSP_DAcdcat[,cdcat])-unlist(DAT_i[,cdcat]))^2,(unlist(CFSP_DAdacat[,dacat])-unlist(DAT_i[,dacat]))^2,(unlist(CFSP_DB[,dbcat])-unlist(DBT_i[,dbcat]))^2,(unlist(CFSP_B[,bcat])-unlist(BT_i[,bcat]))^2)/length(c(unlist(DAT_i[,cdcat]),unlist(DAT_i[,dacat]),unlist(DBT_i[,dbcat]),unlist(BT_i[,bcat]))))
  NRMSE_DAcdcat_DAdacat_DB_B<-sqrt(sum((unlist(CFSP_DAcdcat[,cdcat])-unlist(DAT_i[,cdcat]))^2,(unlist(CFSP_DAdacat[,dacat])-unlist(DAT_i[,dacat]))^2,(unlist(CFSP_DB[,dbcat])-unlist(DBT_i[,dbcat]))^2,(unlist(CFSP_B[,bcat])-unlist(BT_i[,bcat]))^2)/length(c(unlist(DAT_i[,cdcat]),unlist(DAT_i[,dacat]),unlist(DBT_i[,dbcat]),unlist(BT_i[,bcat]))))/mean(c(unlist(DAT_i[,cdcat]),unlist(DAT_i[,dacat]),unlist(DBT_i[,dbcat]),unlist(BT_i[,bcat])))
  #RMSE B
  RMSE_B<-sqrt(sum((unlist(CFSP_B[,bcat])-unlist(BT_i[,bcat]))^2)/(length(unlist(BT_i[,bcat]))))
  NRMSE_B<-sqrt(sum((unlist(CFSP_B[,bcat])-unlist(BT_i[,bcat]))^2)/(length(unlist(BT_i[,bcat]))))/mean(unlist(BT_i[,bcat]))
  #CFSP_RMSE
  CFSP_RMSE[CFSP_NRMSE$DA==i,-1]<-c(RMSE_DAcdcat,RMSE_DAdacat_DB_B,RMSE_DAcdcat_DAdacat_DB_B,RMSE_B)
  #CFSP_NRMSE
  CFSP_NRMSE[CFSP_NRMSE$DA==i,-1]<-c(NRMSE_DAcdcat,NRMSE_DAdacat_DB_B,NRMSE_DAcdcat_DAdacat_DB_B,NRMSE_B)
  
  ##IFSP
  IFSP_i<-IFSP[IFSP$da_id==i,]
  #IFSP_DAcdcat
  IFSP_DAcdcat<-aggregate(data=IFSP_i,eval(parse(text=paste("cbind(",paste(paste(cdcat,"*weight",sep=""),collapse=","),")",sep="")))~da_id,sum) 
  names(IFSP_DAcdcat)<-c("da_id",cdcat)
  IFSP_DAcdcat<-data.table(IFSP_DAcdcat)
  #IFSP_DAdacat
  IFSP_DAdacat<-aggregate(data=IFSP_i,eval(parse(text=paste("cbind(",paste(paste(dacat,"*weight",sep=""),collapse=","),")",sep="")))~da_id,sum) 
  names(IFSP_DAdacat)<-c("da_id",dacat)
  IFSP_DAdacat<-data.table(IFSP_DAdacat)
  #IFSP_DB
  IFSP_DB<-aggregate(data=IFSP_i,eval(parse(text=paste("cbind(",paste(paste(dbcat,"*weight",sep=""),collapse=","),")",sep="")))~db_id,sum) 
  names(IFSP_DB)<-c("db_id",dbcat)
  IFSP_DB<-data.frame(merge(data.table(DBT_i)[,"geo"],IFSP_DB,by.x="geo",by.y="db_id",all.x=TRUE))
  IFSP_DB[is.na(IFSP_DB)]<-0
  IFSP_DB<-data.table(IFSP_DB)
  IFSP_DB<-arrange(IFSP_DB,geo)
  #IFSP_B
  IFSP_B<-aggregate(data=IFSP_i,eval(parse(text=paste("cbind(",paste(paste(bcat,"*weight",sep=""),collapse=","),")",sep="")))~bld_id,sum)   
  names(IFSP_B)<-c("bld_id",bcat)
  IFSP_B<-data.frame(merge(data.table(BT_i)[,"geo"],IFSP_B,by.x="geo",by.y="bld_id",all.x=TRUE))
  IFSP_B[is.na(IFSP_B)]<-0
  IFSP_B<-data.table(IFSP_B)
  IFSP_B<-arrange(IFSP_B,geo)
  #Indicators
  IFSP_DAcdcat<-as.data.frame(IFSP_DAcdcat)
  IFSP_DAdacat<-as.data.frame(IFSP_DAdacat)
  IFSP_DB<-as.data.frame(IFSP_DB)
  IFSP_B<-as.data.frame(IFSP_B)
  #RMSE DAcdcat
  RMSE_DAcdcat<-sqrt(sum((unlist(IFSP_DAcdcat[,cdcat])-unlist(DAT_i[,cdcat]))^2)/(length(unlist(DAT_i[,cdcat]))))
  NRMSE_DAcdcat<-sqrt(sum((unlist(IFSP_DAcdcat[,cdcat])-unlist(DAT_i[,cdcat]))^2)/(length(unlist(DAT_i[,cdcat]))))/mean(unlist(DAT_i[,cdcat]))
  #RMSE DAdacat_DB_BLD
  RMSE_DAdacat_DB_B<-sqrt(sum((unlist(IFSP_DAdacat[,dacat])-unlist(DAT_i[,dacat]))^2,(unlist(IFSP_DB[,dbcat])-unlist(DBT_i[,dbcat]))^2,(unlist(IFSP_B[,bcat])-unlist(BT_i[,bcat]))^2)/length(c(unlist(DAT_i[,dacat]),unlist(DBT_i[,dbcat]),unlist(BT_i[,bcat]))))
  NRMSE_DAdacat_DB_B<-sqrt(sum((unlist(IFSP_DAdacat[,dacat])-unlist(DAT_i[,dacat]))^2,(unlist(IFSP_DB[,dbcat])-unlist(DBT_i[,dbcat]))^2,(unlist(IFSP_B[,bcat])-unlist(BT_i[,bcat]))^2)/length(c(unlist(DAT_i[,dacat]),unlist(DBT_i[,dbcat]),unlist(BT_i[,bcat]))))/mean(c(unlist(DAT_i[,dacat]),unlist(DBT_i[,dbcat]),unlist(BT_i[,bcat])))
  #RMSE DAcdcat_DAdacat_DB_B
  RMSE_DAcdcat_DAdacat_DB_B<-sqrt(sum((unlist(IFSP_DAcdcat[,cdcat])-unlist(DAT_i[,cdcat]))^2,(unlist(IFSP_DAdacat[,dacat])-unlist(DAT_i[,dacat]))^2,(unlist(IFSP_DB[,dbcat])-unlist(DBT_i[,dbcat]))^2,(unlist(IFSP_B[,bcat])-unlist(BT_i[,bcat]))^2)/length(c(unlist(DAT_i[,cdcat]),unlist(DAT_i[,dacat]),unlist(DBT_i[,dbcat]),unlist(BT_i[,bcat]))))
  NRMSE_DAcdcat_DAdacat_DB_B<-sqrt(sum((unlist(IFSP_DAcdcat[,cdcat])-unlist(DAT_i[,cdcat]))^2,(unlist(IFSP_DAdacat[,dacat])-unlist(DAT_i[,dacat]))^2,(unlist(IFSP_DB[,dbcat])-unlist(DBT_i[,dbcat]))^2,(unlist(IFSP_B[,bcat])-unlist(BT_i[,bcat]))^2)/length(c(unlist(DAT_i[,cdcat]),unlist(DAT_i[,dacat]),unlist(DBT_i[,dbcat]),unlist(BT_i[,bcat]))))/mean(c(unlist(DAT_i[,cdcat]),unlist(DAT_i[,dacat]),unlist(DBT_i[,dbcat]),unlist(BT_i[,bcat])))
  #RMSE B
  RMSE_B<-sqrt(sum((unlist(IFSP_B[,bcat])-unlist(BT_i[,bcat]))^2)/(length(unlist(BT_i[,bcat]))))
  NRMSE_B<-sqrt(sum((unlist(IFSP_B[,bcat])-unlist(BT_i[,bcat]))^2)/(length(unlist(BT_i[,bcat]))))/mean(unlist(BT_i[,bcat]))
  #IFSP_RMSE
  IFSP_RMSE[IFSP_NRMSE$DA==i,-1]<-c(RMSE_DAcdcat,RMSE_DAdacat_DB_B,RMSE_DAcdcat_DAdacat_DB_B,RMSE_B)
  #IFSP_NRMSE
  IFSP_NRMSE[IFSP_NRMSE$DA==i,-1]<-c(NRMSE_DAcdcat,NRMSE_DAdacat_DB_B,NRMSE_DAcdcat_DAdacat_DB_B,NRMSE_B)
}
L_RMSE<-cbind(CFSP_RMSE,IFSP_RMSE[,-1])
L_NRMSE<-cbind(CFSP_NRMSE,IFSP_NRMSE[,-1])
write.csv(L_RMSE,"Output/Results/L_RMSE.csv")
write.csv(L_NRMSE,"Output/Results/L_NRMSE.csv")


###Global indicators
##CD
#CFSP
CFSP_CD<-aggregate(data=CFSP,eval(parse(text=paste("cbind(",paste(paste(cdcat,"*weight",sep=""),collapse=","),")",sep="")))~cd_id,sum)
names(CFSP_CD)<-c("cd_id",cdcat)
#IFSP
IFSP_CD<-aggregate(data=IFSP,eval(parse(text=paste("cbind(",paste(paste(cdcat,"*weight",sep=""),collapse=","),")",sep="")))~cd_id,sum)
names(IFSP_CD)<-c("cd_id",cdcat)
#Results
G_CD<-data.frame(CDVAR=unlist(names(CDT[,cdcat])),
                 CDT=unlist(CDT[,cdcat]),
                 CFSP=unlist(CFSP_CD[,cdcat]),
                 IFSP=unlist(IFSP_CD[,cdcat])
)
write.csv(G_CD,"Output/Results/G_CD.csv")
#Indicators
CFSP_G_RMSE_CD<-sqrt(sum((unlist(CFSP_CD[,cdcat])-unlist(CDT[,cdcat]))^2)/length(unlist(CDT[,cdcat])))
CFSP_G_NRMSE_CD<-sqrt(sum((unlist(CFSP_CD[,cdcat])-unlist(CDT[,cdcat]))^2)/length(unlist(CDT[,cdcat])))/mean(unlist(CDT[,cdcat]))
IFSP_G_RMSE_CD<-sqrt(sum((unlist(IFSP_CD[,cdcat])-unlist(CDT[,cdcat]))^2)/length(unlist(CDT[,cdcat])))
IFSP_G_NRMSE_CD<-sqrt(sum((unlist(IFSP_CD[,cdcat])-unlist(CDT[,cdcat]))^2)/length(unlist(CDT[,cdcat])))/mean(unlist(CDT[,cdcat]))
G_CD_Indicators<-data.frame(Indicator=c("CFSP_RMSE_CD","CFSP_NRMSE_CD","IFSP_RMSE_CD","IFSP_NRMSE_CD"),
                            Value=c(CFSP_G_RMSE_CD,CFSP_G_NRMSE_CD,IFSP_G_RMSE_CD,IFSP_G_NRMSE_CD))
write.csv(G_CD_Indicators,"Output/Results/G_CD_Indicators.csv")

##DAcdcat
#CFSP
CFSP_DAcdcat<-aggregate(data=CFSP,eval(parse(text=paste("cbind(",paste(paste(cdcat,"*weight",sep=""),collapse=","),")",sep="")))~da_id,sum)
names(CFSP_DAcdcat)<-c("da_id",cdcat)
#IFSP
IFSP_DAcdcat<-aggregate(data=IFSP,eval(parse(text=paste("cbind(",paste(paste(cdcat,"*weight",sep=""),collapse=","),")",sep="")))~da_id,sum)
names(IFSP_DAcdcat)<-c("da_id",cdcat)
#Results
G_DAcdcat<-data.frame(DAcdcatVAR=unlist(names(DAT[,cdcat])),
                 DAT=unlist(DAT[,cdcat]),
                 CFSP=unlist(CFSP_DAcdcat[,cdcat]),
                 IFSP=unlist(IFSP_DAcdcat[,cdcat])
)
write.csv(G_DAcdcat,"Output/Results/G_DAcdcat.csv")
#Indicators
CFSP_G_RMSE_DAcdcat<-sqrt(sum((unlist(CFSP_DAcdcat[,cdcat])-unlist(DAT[,cdcat]))^2)/length(unlist(DAT[,cdcat])))
CFSP_G_NRMSE_DAcdcat<-sqrt(sum((unlist(CFSP_DAcdcat[,cdcat])-unlist(DAT[,cdcat]))^2)/length(unlist(DAT[,cdcat])))/mean(unlist(DAT[,cdcat]))
IFSP_G_RMSE_DAcdcat<-sqrt(sum((unlist(IFSP_DAcdcat[,cdcat])-unlist(DAT[,cdcat]))^2)/length(unlist(DAT[,cdcat])))
IFSP_G_NRMSE_DAcdcat<-sqrt(sum((unlist(IFSP_DAcdcat[,cdcat])-unlist(DAT[,cdcat]))^2)/length(unlist(DAT[,cdcat])))/mean(unlist(DAT[,cdcat]))
G_DAcdcat_Indicators<-data.frame(Indicator=c("CFSP_RMSE_DAcdcat","CFSP_NRMSE_DAcdcat","IFSP_RMSE_DAcdcat","IFSP_NRMSE_DAcdcat"),
                            Value=c(CFSP_G_RMSE_DAcdcat,CFSP_G_NRMSE_DAcdcat,IFSP_G_RMSE_DAcdcat,IFSP_G_NRMSE_DAcdcat))
write.csv(G_DAcdcat_Indicators,"Output/Results/G_DAcdcat_Indicators.csv")

##DAdacat_DB_B
#CFSP
CFSP_DAdacat<-aggregate(data=CFSP,eval(parse(text=paste("cbind(",paste(paste(dacat,"*weight",sep=""),collapse=","),")",sep="")))~da_id,sum)
names(CFSP_DAdacat)<-c("da_id",dacat)
CFSP_DAdacat<-arrange(CFSP_DAdacat,da_id)
CFSP_DB<-aggregate(data=CFSP,eval(parse(text=paste("cbind(",paste(paste(dbcat,"*weight",sep=""),collapse=","),")",sep="")))~db_id,sum)
names(CFSP_DB)<-c("db_id",dbcat)
CFSP_DB<-as.data.frame(merge(unique(GIS[,"db_id"]),CFSP_DB,by="db_id",all.x=TRUE))
CFSP_DB[is.na(CFSP_DB)]<-0
CFSP_DB<-arrange(CFSP_DB,db_id)
CFSP_B<-aggregate(data=CFSP,eval(parse(text=paste("cbind(",paste(paste(bcat,"*weight",sep=""),collapse=","),")",sep="")))~bld_id,sum)
names(CFSP_B)<-c("bld_id",bcat)
CFSP_B<-as.data.frame(merge(GIS[,"bld_id"],CFSP_B,by="bld_id",all.x=TRUE))
CFSP_B[is.na(CFSP_B)]<-0
CFSP_B<-arrange(CFSP_B,bld_id)
#IFSP
IFSP_DAdacat<-aggregate(data=IFSP,eval(parse(text=paste("cbind(",paste(paste(dacat,"*weight",sep=""),collapse=","),")",sep="")))~da_id,sum)
names(IFSP_DAdacat)<-c("da_id",dacat)
IFSP_DAdacat<-arrange(IFSP_DAdacat,da_id)
IFSP_DB<-aggregate(data=IFSP,eval(parse(text=paste("cbind(",paste(paste(dbcat,"*weight",sep=""),collapse=","),")",sep="")))~db_id,sum)
names(IFSP_DB)<-c("db_id",dbcat)
IFSP_DB<-as.data.frame(merge(unique(GIS[,"db_id"]),IFSP_DB,by="db_id",all.x=TRUE))
IFSP_DB[is.na(IFSP_DB)]<-0
IFSP_DB<-arrange(IFSP_DB,db_id)
IFSP_B<-aggregate(data=IFSP,eval(parse(text=paste("cbind(",paste(paste(bcat,"*weight",sep=""),collapse=","),")",sep="")))~bld_id,sum)
names(IFSP_B)<-c("bld_id",bcat)
IFSP_B<-as.data.frame(merge(GIS[,"bld_id"],IFSP_B,by="bld_id",all.x=TRUE))
IFSP_B[is.na(IFSP_B)]<-0
IFSP_B<-arrange(IFSP_B,bld_id)
#Results
G_DAdacat_DB_B<-data.frame(DAdacat_DB_BVAR=c(names(unlist(DAT[,dacat])),names(unlist(data.table(DBT)[,"ppcount"])),names(unlist(BT[,bcat]))),
                        DAT_DBT_BT=c(unlist(DAT[,dacat]),unlist(DBT[,dbcat]),unlist(BT[,bcat])),
                        CFSP=c(unlist(CFSP_DAdacat[,dacat]),unlist(CFSP_DB[,dbcat]),unlist(CFSP_B[,bcat])),
                        IFSP=c(unlist(IFSP_DAdacat[,dacat]),unlist(IFSP_DB[,dbcat]),unlist(IFSP_B[,bcat]))
)
G_DAdacat_DB_B_1<-G_DAdacat_DB_B[1:1e6,]
G_DAdacat_DB_B_2<-G_DAdacat_DB_B[(1e6+1):2e6,]
G_DAdacat_DB_B_3<-G_DAdacat_DB_B[(2e6+1):nrow(G_DAdacat_DB_B),]
write.csv(G_DAdacat_DB_B_1,"Output/Results/G_DAdacat_DB_B_1.csv")
write.csv(G_DAdacat_DB_B_2,"Output/Results/G_DAdacat_DB_B_2.csv")
write.csv(G_DAdacat_DB_B_3,"Output/Results/G_DAdacat_DB_B_3.csv")
#Indicators
CFSP_G_RMSE_DAdacat_DB_B<-sqrt(sum((unlist(CFSP_DAdacat[,dacat])-unlist(DAT[,dacat]))^2,(unlist(CFSP_DB[,dbcat])-unlist(DBT[,dbcat]))^2,(unlist(CFSP_B[,bcat])-unlist(BT[,bcat]))^2)/length(c(unlist(DAT[,dacat]),unlist(DBT[,dbcat]),unlist(BT[,bcat]))))
CFSP_G_NRMSE_DAdacat_DB_B<-sqrt(sum((unlist(CFSP_DAdacat[,dacat])-unlist(DAT[,dacat]))^2,(unlist(CFSP_DB[,dbcat])-unlist(DBT[,dbcat]))^2,(unlist(CFSP_B[,bcat])-unlist(BT[,bcat]))^2)/length(c(unlist(DAT[,dacat]),unlist(DBT[,dbcat]),unlist(BT[,bcat]))))/mean(c(unlist(DAT[,dacat]),unlist(DBT[,dbcat]),unlist(BT[,bcat])))
IFSP_G_RMSE_DAdacat_DB_B<-sqrt(sum((unlist(IFSP_DAdacat[,dacat])-unlist(DAT[,dacat]))^2,(unlist(IFSP_DB[,dbcat])-unlist(DBT[,dbcat]))^2,(unlist(IFSP_B[,bcat])-unlist(BT[,bcat]))^2)/length(c(unlist(DAT[,dacat]),unlist(DBT[,dbcat]),unlist(BT[,bcat]))))
IFSP_G_NRMSE_DAdacat_DB_B<-sqrt(sum((unlist(IFSP_DAdacat[,dacat])-unlist(DAT[,dacat]))^2,(unlist(IFSP_DB[,dbcat])-unlist(DBT[,dbcat]))^2,(unlist(IFSP_B[,bcat])-unlist(BT[,bcat]))^2)/length(c(unlist(DAT[,dacat]),unlist(DBT[,dbcat]),unlist(BT[,bcat]))))/mean(c(unlist(DAT[,dacat]),unlist(DBT[,dbcat]),unlist(BT[,bcat])))
G_DAdacat_DB_B_Indicators<-data.frame(Indicator=c("CFSP_RMSE_DAdacat_DB_B","CFSP_NRMSE_DAdacat_DB_B","IFSP_RMSE_DAdacat_DB_B","IFSP_NRMSE_DAdacat_DB_B"),
                            Value=c(CFSP_G_RMSE_DAdacat_DB_B,CFSP_G_NRMSE_DAdacat_DB_B,IFSP_G_RMSE_DAdacat_DB_B,IFSP_G_NRMSE_DAdacat_DB_B))
write.csv(G_DAdacat_DB_B_Indicators,"Output/Results/G_DAdacat_DB_B_Indicators.csv")

##DAcdcat_DAdacat_DB_B
#CFSP
CFSP_DAcdcat<-aggregate(data=CFSP,eval(parse(text=paste("cbind(",paste(paste(cdcat,"*weight",sep=""),collapse=","),")",sep="")))~da_id,sum)
names(CFSP_DAcdcat)<-c("da_id",cdcat)
CFSP_DAcdcat<-arrange(CFSP_DAcdcat,da_id)
CFSP_DAdacat<-aggregate(data=CFSP,eval(parse(text=paste("cbind(",paste(paste(dacat,"*weight",sep=""),collapse=","),")",sep="")))~da_id,sum)
names(CFSP_DAdacat)<-c("da_id",dacat)
CFSP_DAdacat<-arrange(CFSP_DAdacat,da_id)
CFSP_DB<-aggregate(data=CFSP,eval(parse(text=paste("cbind(",paste(paste(dbcat,"*weight",sep=""),collapse=","),")",sep="")))~db_id,sum)
names(CFSP_DB)<-c("db_id",dbcat)
CFSP_DB<-as.data.frame(merge(unique(GIS[,"db_id"]),CFSP_DB,by="db_id",all.x=TRUE))
CFSP_DB[is.na(CFSP_DB)]<-0
CFSP_DB<-arrange(CFSP_DB,db_id)
CFSP_B<-aggregate(data=CFSP,eval(parse(text=paste("cbind(",paste(paste(bcat,"*weight",sep=""),collapse=","),")",sep="")))~bld_id,sum)
names(CFSP_B)<-c("bld_id",bcat)
CFSP_B<-as.data.frame(merge(GIS[,"bld_id"],CFSP_B,by="bld_id",all.x=TRUE))
CFSP_B[is.na(CFSP_B)]<-0
CFSP_B<-arrange(CFSP_B,bld_id)
#IFSP
IFSP_DAcdcat<-aggregate(data=IFSP,eval(parse(text=paste("cbind(",paste(paste(cdcat,"*weight",sep=""),collapse=","),")",sep="")))~da_id,sum)
names(IFSP_DAcdcat)<-c("da_id",cdcat)
IFSP_DAcdcat<-arrange(IFSP_DAcdcat,da_id)
IFSP_DAdacat<-aggregate(data=IFSP,eval(parse(text=paste("cbind(",paste(paste(dacat,"*weight",sep=""),collapse=","),")",sep="")))~da_id,sum)
names(IFSP_DAdacat)<-c("da_id",dacat)
IFSP_DAdacat<-arrange(IFSP_DAdacat,da_id)
IFSP_DB<-aggregate(data=IFSP,eval(parse(text=paste("cbind(",paste(paste(dbcat,"*weight",sep=""),collapse=","),")",sep="")))~db_id,sum)
names(IFSP_DB)<-c("db_id",dbcat)
IFSP_DB<-as.data.frame(merge(unique(GIS[,"db_id"]),IFSP_DB,by="db_id",all.x=TRUE))
IFSP_DB[is.na(IFSP_DB)]<-0
IFSP_DB<-arrange(IFSP_DB,db_id)
IFSP_B<-aggregate(data=IFSP,eval(parse(text=paste("cbind(",paste(paste(bcat,"*weight",sep=""),collapse=","),")",sep="")))~bld_id,sum)
names(IFSP_B)<-c("bld_id",bcat)
IFSP_B<-as.data.frame(merge(GIS[,"bld_id"],IFSP_B,by="bld_id",all.x=TRUE))
IFSP_B[is.na(IFSP_B)]<-0
IFSP_B<-arrange(IFSP_B,bld_id)
#Results
G_DAcdcat_DAdacat_DB_B<-data.frame(DAcdcat_DAdacat_DB_BVAR=c(names(unlist(DAT[,cdcat])),names(unlist(DAT[,dacat])),names(unlist(data.table(DBT)[,"ppcount"])),names(unlist(BT[,bcat]))),
                           DAT_DBT_BT=c(unlist(DAT[,cdcat]),unlist(DAT[,dacat]),unlist(DBT[,dbcat]),unlist(BT[,bcat])),
                           CFSP=c(unlist(CFSP_DAcdcat[,cdcat]),unlist(CFSP_DAdacat[,dacat]),unlist(CFSP_DB[,dbcat]),unlist(CFSP_B[,bcat])),
                           IFSP=c(unlist(IFSP_DAcdcat[,cdcat]),unlist(IFSP_DAdacat[,dacat]),unlist(IFSP_DB[,dbcat]),unlist(IFSP_B[,bcat]))
)
G_DAcdcat_DAdacat_DB_B_1<-G_DAcdcat_DAdacat_DB_B[1:1e6,]
G_DAcdcat_DAdacat_DB_B_2<-G_DAcdcat_DAdacat_DB_B[(1e6+1):2e6,]
G_DAcdcat_DAdacat_DB_B_3<-G_DAcdcat_DAdacat_DB_B[(2e6+1):nrow(G_DAcdcat_DAdacat_DB_B),]
write.csv(G_DAcdcat_DAdacat_DB_B_1,"Output/Results/G_DAcdcat_DAdacat_DB_B_1.csv")
write.csv(G_DAcdcat_DAdacat_DB_B_2,"Output/Results/G_DAcdcat_DAdacat_DB_B_2.csv")
write.csv(G_DAcdcat_DAdacat_DB_B_3,"Output/Results/G_DAcdcat_DAdacat_DB_B_3.csv")
#Indicators
CFSP_G_RMSE_DAcdcat_DAdacat_DB_B<-sqrt(sum((unlist(CFSP_DAcdcat[,cdcat])-unlist(DAT[,cdcat]))^2,(unlist(CFSP_DAdacat[,dacat])-unlist(DAT[,dacat]))^2,(unlist(CFSP_DB[,dbcat])-unlist(DBT[,dbcat]))^2,(unlist(CFSP_B[,bcat])-unlist(BT[,bcat]))^2)/length(c(unlist(DAT[,cdcat]),unlist(DAT[,dacat]),unlist(DBT[,dbcat]),unlist(BT[,bcat]))))
CFSP_G_NRMSE_DAcdcat_DAdacat_DB_B<-sqrt(sum((unlist(CFSP_DAcdcat[,cdcat])-unlist(DAT[,cdcat]))^2,(unlist(CFSP_DAdacat[,dacat])-unlist(DAT[,dacat]))^2,(unlist(CFSP_DB[,dbcat])-unlist(DBT[,dbcat]))^2,(unlist(CFSP_B[,bcat])-unlist(BT[,bcat]))^2)/length(c(unlist(DAT[,cdcat]),unlist(DAT[,dacat]),unlist(DBT[,dbcat]),unlist(BT[,bcat]))))/mean(c(unlist(DAT[,cdcat]),unlist(DAT[,dacat]),unlist(DBT[,dbcat]),unlist(BT[,bcat])))
IFSP_G_RMSE_DAcdcat_DAdacat_DB_B<-sqrt(sum((unlist(IFSP_DAcdcat[,cdcat])-unlist(DAT[,cdcat]))^2,(unlist(IFSP_DAdacat[,dacat])-unlist(DAT[,dacat]))^2,(unlist(IFSP_DB[,dbcat])-unlist(DBT[,dbcat]))^2,(unlist(IFSP_B[,bcat])-unlist(BT[,bcat]))^2)/length(c(unlist(DAT[,cdcat]),unlist(DAT[,dacat]),unlist(DBT[,dbcat]),unlist(BT[,bcat]))))
IFSP_G_NRMSE_DAcdcat_DAdacat_DB_B<-sqrt(sum((unlist(IFSP_DAcdcat[,cdcat])-unlist(DAT[,cdcat]))^2,(unlist(IFSP_DAdacat[,dacat])-unlist(DAT[,dacat]))^2,(unlist(IFSP_DB[,dbcat])-unlist(DBT[,dbcat]))^2,(unlist(IFSP_B[,bcat])-unlist(BT[,bcat]))^2)/length(c(unlist(DAT[,cdcat]),unlist(DAT[,dacat]),unlist(DBT[,dbcat]),unlist(BT[,bcat]))))/mean(c(unlist(DAT[,cdcat]),unlist(DAT[,dacat]),unlist(DBT[,dbcat]),unlist(BT[,bcat])))
G_DAcdcat_DAdacat_DB_B_Indicators<-data.frame(Indicator=c("CFSP_RMSE_DAcdcat_DAdacat_DB_B","CFSP_NRMSE_DAcdcat_DAdacat_DB_B","IFSP_RMSE_DAcdcat_DAdacat_DB_B","IFSP_NRMSE_DAcdcat_DAdacat_DB_B"),
                                      Value=c(CFSP_G_RMSE_DAcdcat_DAdacat_DB_B,CFSP_G_NRMSE_DAcdcat_DAdacat_DB_B,IFSP_G_RMSE_DAcdcat_DAdacat_DB_B,IFSP_G_NRMSE_DAcdcat_DAdacat_DB_B))
write.csv(G_DAcdcat_DAdacat_DB_B_Indicators,"Output/Results/G_DAcdcat_DAdacat_DB_B_Indicators.csv")

##B
#CFSP
CFSP_B<-aggregate(data=CFSP,eval(parse(text=paste("cbind(",paste(paste(bcat,"*weight",sep=""),collapse=","),")",sep="")))~bld_id,sum)
names(CFSP_B)<-c("bld_id",bcat)
CFSP_B<-as.data.frame(merge(GIS[,"bld_id"],CFSP_B,by="bld_id",all.x=TRUE))
CFSP_B[is.na(CFSP_B)]<-0
CFSP_B<-arrange(CFSP_B,bld_id)
#IFSP
IFSP_B<-aggregate(data=IFSP,eval(parse(text=paste("cbind(",paste(paste(bcat,"*weight",sep=""),collapse=","),")",sep="")))~bld_id,sum)
names(IFSP_B)<-c("bld_id",bcat)
IFSP_B<-as.data.frame(merge(GIS[,"bld_id"],IFSP_B,by="bld_id",all.x=TRUE))
IFSP_B[is.na(IFSP_B)]<-0
IFSP_B<-arrange(IFSP_B,bld_id)
#Results
G_B<-data.frame(BVAR=names(unlist(BT[,bcat])),
                BT=unlist(BT[,bcat]),
                CFSP=unlist(CFSP_B[,bcat]),
                IFSP=unlist(IFSP_B[,bcat])
)
G_B_1<-G_B[1:1e6,]
G_B_2<-G_B[(1e6+1):2e6,]
G_B_3<-G_B[(2e6+1):nrow(G_B),]
write.csv(G_B_1,"Output/Results/G_B_1.csv")
write.csv(G_B_2,"Output/Results/G_B_2.csv")
write.csv(G_B_3,"Output/Results/G_B_3.csv")
#Indicators
CFSP_G_RMSE_B<-sqrt(sum((unlist(CFSP_B[,bcat])-unlist(BT[,bcat]))^2)/length(unlist(BT[,bcat])))
CFSP_G_NRMSE_B<-sqrt(sum((unlist(CFSP_B[,bcat])-unlist(BT[,bcat]))^2)/length(unlist(BT[,bcat])))/mean(unlist(BT[,bcat]))
IFSP_G_RMSE_B<-sqrt(sum((unlist(IFSP_B[,bcat])-unlist(BT[,bcat]))^2)/length(unlist(BT[,bcat])))
IFSP_G_NRMSE_B<-sqrt(sum((unlist(IFSP_B[,bcat])-unlist(BT[,bcat]))^2)/length(unlist(BT[,bcat])))/mean(unlist(BT[,bcat]))
G_B_Indicators<-data.frame(Indicator=c("CFSP_RMSE_B","CFSP_NRMSE_B","IFSP_RMSE_B","IFSP_NRMSE_B"),
                            Value=c(CFSP_G_RMSE_B,CFSP_G_NRMSE_B,IFSP_G_RMSE_B,IFSP_G_NRMSE_B))
write.csv(G_B_Indicators,"Output/Results/G_B_Indicators.csv")