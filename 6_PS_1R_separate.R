rm(list=ls())
gc()



###Packages
library(data.table)
library(dplyr)
library(glmnet)



###Non-harmonized BT (OR at DB level and intra-stratum harmonization only)
for (nonhBT in 1){
  # ###Non-harmonized BT
  # ###Import totals
  # #GIS structure (standard geographic areas correspondence)
  # GIS<-fread("Input/Census/DB/2016_92-151_XBB.csv")
  # BT<-fread("Output/BT_nearestDB.csv")
  # BT<-BT[,2:8]
  # names(BT)[names(BT)=="DBUID"]<-"db_id"
  # GIS<-merge(GIS,BT,by.x="DBuid/IDidu",by.y="db_id",all.x=TRUE) #Include buildings in GIS
  # names(GIS)[names(GIS)=="DBuid/IDidu"]<-"db_id"
  # names(GIS)[names(GIS)=="DAuid/ADidu"]<-"da_id"
  # names(GIS)[names(GIS)=="CDuid/DRidu"]<-"cd_id"
  # GIS<-GIS[,c("bld_id","db_id","da_id","cd_id")]
  # 
  # 
  # 
  # ###Finalize GIS
  # #Census divisions totals (CDT)
  # CDT<-fread("Input/Census/CD/98-401-X2016060_English_CSV_data.csv") #Census divisons
  # CDT<-data.table(cd_id=CDT$`GEO_CODE (POR)`, cd_var=CDT$`DIM: Profile of Census Divisions (2247)`, cd_varid= CDT$`Notes: Profile of Census Divisions (2247)`, cd_catid=CDT$`Member ID: Profile of Census Divisions (2247)`, cd_total=CDT$`Dim: Sex (3): Member ID: [1]: Total - Sex`, cd_men=CDT$`Dim: Sex (3): Member ID: [2]: Male`, cd_women=CDT$`Dim: Sex (3): Member ID: [3]: Female`)
  # CDT<-CDT[CDT$cd_id==2466,] #Filter to Montreal
  # CDT$cd_total<-as.numeric(CDT$cd_total)
  # CDT$cd_men<-as.numeric(CDT$cd_men)
  # CDT$cd_women<-as.numeric(CDT$cd_women)
  # CDT[is.na(CDT)]<-0
  # GIS<-GIS[GIS$cd_id%in%CDT$cd_id,] #Update GIS
  # #Dissemination areas totals (DAT)
  # #DAT<-fread("Input/Census/DA/98-401-X2016044_English_CSV_data.csv") #Dissemination areas
  # # DAT_MTL<-DAT[DAT$`GEO_CODE (POR)`%in%unique(GIS$`DAuid/ADidu`[GIS$`CDname/DRnom`=="Montr?al"]),c("GEO_CODE (POR)","DIM: Profile of Dissemination Areas (2247)","Member ID: Profile of Dissemination Areas (2247)","Notes: Profile of Dissemination Areas (2247)","Dim: Sex (3): Member ID: [1]: Total - Sex","Dim: Sex (3): Member ID: [2]: Male","Dim: Sex (3): Member ID: [3]: Female")] #Census targets for dissemination areas in Montreal island
  # # write.csv(DAT_MTL,"Input/Census/DA/MTL_98-401-X2016044_English_CSV_data.csv")
  # DAT<-fread("Input/Census/DA/MTL_98-401-X2016044_English_CSV_data.csv")
  # DAT<-data.table(da_id=DAT$`GEO_CODE (POR)`, da_var=DAT$`DIM: Profile of Dissemination Areas (2247)`, da_varid= DAT$`Notes: Profile of Dissemination Areas (2247)`, da_catid=DAT$`Member ID: Profile of Dissemination Areas (2247)`, da_total=DAT$`Dim: Sex (3): Member ID: [1]: Total - Sex`, da_men=DAT$`Dim: Sex (3): Member ID: [2]: Male`, da_women=DAT$`Dim: Sex (3): Member ID: [3]: Female`)
  # DAT$da_total<-as.numeric(DAT$da_total)
  # DAT$da_men<-as.numeric(DAT$da_men)
  # DAT$da_women<-as.numeric(DAT$da_women)
  # DAT[is.na(DAT)]<-0
  # DAT<-DAT[!DAT$da_id%in%DAT$da_id[(DAT$da_catid==1 & DAT$da_total==0)|(DAT$da_catid==5 & DAT$da_total==0)]] #only keep DAs with a population and private dwellings occupied by usual residents
  # setorder(DAT,da_id)
  # GIS<-GIS[GIS$da_id%in%DAT$da_id] #update GIS
  # #DBT
  # DBT<-fread("Input/Census/DB/2016_92-151_XBB.csv") #Dissemination blocks
  # DBT<-data.table(db_id=DBT$`DBuid/IDidu`, db_pop=DBT$`DBpop2016/IDpop2016`, db_dwell=DBT$`DBtdwell2016/IDtlog2016`, db_urdwell=DBT$`DBurdwell2016/IDrh2016`)
  # DBT<-DBT[DBT$db_id%in%GIS$db_id,]
  # DBT<-DBT[!(DBT$db_pop==0 | DBT$db_urdwell==0)] #only keep DAs with a population and private dwellings occupied by usual residents
  # GIS<-GIS[GIS$db_id%in%DBT$db_id] #update GIS
  # #Update CDT and DAT
  # CDT<-CDT[CDT$cd_id%in%GIS$cd_id]
  # DAT<-DAT[DAT$da_id%in%GIS$da_id]
  # 
  # #Building totals (BT)
  # #hhdbuilt.9999 distribution
  # BT<-fread("Output/BT_nearestDB.csv")
  # DBlatlon<-fread("Input/Census/DB/DB_MTL.csv")
  # DBlatlon<-DBlatlon[,c(1,ncol(DBlatlon)-1,ncol(DBlatlon)-2),with=FALSE]
  # names(DBlatlon)<-c("db_id","db_lat","db_lon")
  # BT<-BT[,2:8]
  # names(BT)[names(BT)=="DBUID"]<-"db_id"
  # BT<-BT[BT$bld_id%in%GIS$bld_id,]
  # DBlatlon<-DBlatlon[DBlatlon$db_id%in%GIS$db_id]
  # BT<-rbind(BT,data.table(bld_id=DBT$db_id[!DBT$db_id%in%BT$db_id] ,bld_dwellings=DBT$db_dwell[!DBT$db_id%in%BT$db_id] ,bld_consyr=9999 ,bld_type="Unknown" ,bld_lat=DBlatlon$db_lat[!DBlatlon$db_id%in%BT$db_id] ,bld_lon=DBlatlon$db_lon[!DBlatlon$db_id%in%BT$db_id] ,db_id=DBT$db_id[!DBT$db_id%in%BT$db_id]))
  # BT<-merge(BT,unique(GIS[,c("db_id","da_id")]),by="db_id")
  # BT<-merge(BT,data.table(da_id=unique(DAT$da_id)
  #                         ,da_hhdbuilt.1=DAT$da_total[DAT$da_catid==1644]  #<=1960
  #                         ,da_hhdbuilt.2=DAT$da_total[DAT$da_catid==1645] #[1961,1980]
  #                         ,da_hhdbuilt.3=DAT$da_total[DAT$da_catid==1646]+DAT$da_total[DAT$da_catid==1647] #[1981,2000]
  #                         ,da_hhdbuilt.4=DAT$da_total[DAT$da_catid==1648]+DAT$da_total[DAT$da_catid==1649]+DAT$da_total[DAT$da_catid==1650] #[2001,2016]
  # )
  # ,by="da_id")
  # DBT$db_or<-DBT$db_urdwell/DBT$db_dwell
  # BT<-merge(BT,DBT[,c("db_id","db_or")],by="db_id")
  # BT$bld_hh<-BT$bld_dwellings*BT$db_or
  # BT$bld_consyr[BT$bld_consyr<=1960]<-1
  # BT$bld_consyr[BT$bld_consyr>=1961 & BT$bld_consyr<=1980]<-2
  # BT$bld_consyr[BT$bld_consyr>=1981 & BT$bld_consyr<=2000]<-3
  # BT$bld_consyr[BT$bld_consyr>=2001 & BT$bld_consyr<=2016]<-4
  # BT<-merge(BT,aggregate(data=BT[BT$bld_consyr==1],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
  # names(BT)[names(BT)=="bld_hh.x"]<-"bld_hh"
  # names(BT)[ncol(BT)]<-"sum_hhdbuilt.1"
  # BT<-merge(BT,aggregate(data=BT[BT$bld_consyr==2],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
  # names(BT)[names(BT)=="bld_hh.x"]<-"bld_hh"
  # names(BT)[ncol(BT)]<-"sum_hhdbuilt.2"
  # BT<-merge(BT,aggregate(data=BT[BT$bld_consyr==3],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
  # names(BT)[names(BT)=="bld_hh.x"]<-"bld_hh"
  # names(BT)[ncol(BT)]<-"sum_hhdbuilt.3"
  # BT<-merge(BT,aggregate(data=BT[BT$bld_consyr==4],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
  # names(BT)[names(BT)=="bld_hh.x"]<-"bld_hh"
  # names(BT)[ncol(BT)]<-"sum_hhdbuilt.4"
  # BT<-merge(BT,aggregate(data=BT[BT$bld_consyr==9999],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
  # names(BT)[names(BT)=="bld_hh.x"]<-"bld_hh"
  # names(BT)[ncol(BT)]<-"sum_hhdbuilt.9999"
  # BT<-BT[BT$da_id%in%BT$da_id[BT$sum_hhdbuilt.9999!=0]]
  # hhdbuilt_diff<-BT[,grepl("da_hhdbuilt",names(BT)),with=FALSE]-BT[,grepl("sum_hhdbuilt",names(BT)),with=FALSE][,1:4]
  # temp<-apply(hhdbuilt_diff,1,rank)
  # temp<-transpose(data.table(temp))
  # names(temp)<-paste("diff_hhdbuilt.",1:4,sep="")
  # BT<-cbind(BT,temp)
  # for (i in unique(BT$da_id)){
  #   print(i)
  #   temp<-BT[BT$da_id==i]
  #   temp_9999<-as.numeric(temp$bld_id[temp$bld_consyr==9999])
  # 
  #   for (j in temp_9999){
  #     temp$bld_consyr[temp$bld_id==j]<-as.numeric(substring(names(temp[temp$bld_id==j,grepl("diff",names(temp)) & temp[temp$bld_id==j,]==4,with=FALSE]),nchar(names(temp[temp$bld_id==j,grepl("diff",names(temp)) & temp[temp$bld_id==j,]==4,with=FALSE]))))
  #     #Update temp
  # 
  #     temp<-temp[,c(1:14)]
  # 
  #     if(nrow(temp[temp$bld_consyr==1])>0){
  #       temp<-merge(temp,aggregate(data=temp[temp$bld_consyr==1],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
  #       names(temp)[names(temp)=="bld_hh.x"]<-"bld_hh"
  #       names(temp)[ncol(temp)]<-"sum_hhdbuilt.1"
  #     }else{
  #       temp$sum_hhdbuilt.1<-NA
  #     }
  # 
  #     if(nrow(temp[temp$bld_consyr==2])>0){
  #       temp<-merge(temp,aggregate(data=temp[temp$bld_consyr==2],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
  #       names(temp)[names(temp)=="bld_hh.x"]<-"bld_hh"
  #       names(temp)[ncol(temp)]<-"sum_hhdbuilt.2"
  #     }else{
  #       temp$sum_hhdbuilt.2<-NA
  #     }
  # 
  #     if(nrow(temp[temp$bld_consyr==3])>0){
  #       temp<-merge(temp,aggregate(data=temp[temp$bld_consyr==3],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
  #       names(temp)[names(temp)=="bld_hh.x"]<-"bld_hh"
  #       names(temp)[ncol(temp)]<-"sum_hhdbuilt.3"
  #     }else{
  #       temp$sum_hhdbuilt.3<-NA
  #     }
  # 
  #     if(nrow(temp[temp$bld_consyr==4])>0){
  #       temp<-merge(temp,aggregate(data=temp[temp$bld_consyr==4],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
  #       names(temp)[names(temp)=="bld_hh.x"]<-"bld_hh"
  #       names(temp)[ncol(temp)]<-"sum_hhdbuilt.4"
  #     }else{
  #       temp$sum_hhdbuilt.4<-NA
  #     }
  # 
  #     if(nrow(temp[temp$bld_consyr==9999])>0){
  #       temp<-merge(temp,aggregate(data=temp[temp$bld_consyr==9999],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
  #       names(temp)[names(temp)=="bld_hh.x"]<-"bld_hh"
  #       names(temp)[ncol(temp)]<-"sum_hhdbuilt.9999"
  #     }else{
  #       temp$sum_hhdbuilt.9999<-NA
  #     }
  # 
  #     hhdbuilt_diff<-temp[,grepl("da_hhdbuilt",names(temp)),with=FALSE]-temp[,grepl("sum_hhdbuilt",names(temp)),with=FALSE][,1:4]
  #     hhdbuilt_diff[is.na(hhdbuilt_diff)]<-c(-999999)
  #     temp1<-apply(hhdbuilt_diff,1,function(x) rank(x,ties.method="first"))
  #     temp1<-transpose(data.table(temp1))
  #     names(temp1)<-paste("diff_hhdbuilt.",1:4,sep="")
  #     temp<-cbind(temp,temp1)
  # 
  #   }
  # 
  #   BT[BT$da_id==i]<-temp
  # 
  # }
  # temp_hhdbuilt<-BT[,c("bld_id","bld_consyr")]
  # #hhdtype.9999 distribution
  # BT<-fread("Output/BT_nearestDB.csv")
  # DBlatlon<-fread("Input/Census/DB/DB_MTL.csv")
  # DBlatlon<-DBlatlon[,c(1,ncol(DBlatlon)-1,ncol(DBlatlon)-2),with=FALSE]
  # names(DBlatlon)<-c("db_id","db_lat","db_lon")
  # BT<-BT[,2:8]
  # names(BT)[names(BT)=="DBUID"]<-"db_id"
  # BT<-BT[BT$bld_id%in%GIS$bld_id,]
  # DBlatlon<-DBlatlon[DBlatlon$db_id%in%GIS$db_id]
  # BT<-rbind(BT,data.table(bld_id=DBT$db_id[!DBT$db_id%in%BT$db_id] ,bld_dwellings=DBT$db_dwell[!DBT$db_id%in%BT$db_id] ,bld_consyr=9999 ,bld_type="Unknown" ,bld_lat=DBlatlon$db_lat[!DBlatlon$db_id%in%BT$db_id] ,bld_lon=DBlatlon$db_lon[!DBlatlon$db_id%in%BT$db_id] ,db_id=DBT$db_id[!DBT$db_id%in%BT$db_id]))
  # BT<-merge(BT,unique(GIS[,c("db_id","da_id")]),by="db_id")
  # BT<-merge(BT,data.table(da_id=unique(DAT$da_id)
  #                         ,da_hhdtype.1=DAT$da_total[DAT$da_catid==1622]  #Condo
  #                         ,da_hhdtype.2=DAT$da_total[DAT$da_catid==1623] #Not condo
  # )
  # ,by="da_id")
  # DBT$db_or<-DBT$db_urdwell/DBT$db_dwell
  # BT<-merge(BT,DBT[,c("db_id","db_or")],by="db_id")
  # BT$bld_hh<-BT$bld_dwellings*BT$db_or
  # BT$bld_type[BT$bld_type=="Condominium"]<-1
  # BT$bld_type[BT$bld_type=="Regular"]<-2
  # BT$bld_type[BT$bld_type=="Unknown"]<-9999
  # BT<-merge(BT,aggregate(data=BT[BT$bld_type==1],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
  # names(BT)[names(BT)=="bld_hh.x"]<-"bld_hh"
  # names(BT)[ncol(BT)]<-"sum_hhdtype.1"
  # BT<-merge(BT,aggregate(data=BT[BT$bld_type==2],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
  # names(BT)[names(BT)=="bld_hh.x"]<-"bld_hh"
  # names(BT)[ncol(BT)]<-"sum_hhdtype.2"
  # BT<-merge(BT,aggregate(data=BT[BT$bld_type==9999],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
  # names(BT)[names(BT)=="bld_hh.x"]<-"bld_hh"
  # names(BT)[ncol(BT)]<-"sum_hhdtype.9999"
  # BT<-BT[BT$da_id%in%BT$da_id[BT$sum_hhdtype.9999!=0]]
  # hhdbuilt_diff<-BT[,grepl("da_hhdtype",names(BT)),with=FALSE]-BT[,grepl("sum_hhdtype",names(BT)),with=FALSE][,1:2]
  # temp<-apply(hhdbuilt_diff,1,rank)
  # temp<-transpose(data.table(temp))
  # names(temp)<-paste("diff_hhdtype.",1:2,sep="")
  # BT<-cbind(BT,temp)
  # BT$bld_type<-as.numeric(BT$bld_type)
  # for (i in unique(BT$da_id)){
  #   print(i)
  #   temp<-BT[BT$da_id==i]
  #   temp_9999<-as.numeric(temp$bld_id[temp$bld_type==9999])
  # 
  #   for (j in temp_9999){
  #     temp$bld_type[temp$bld_id==j]<-as.numeric(substring(names(temp[temp$bld_id==j,grepl("diff",names(temp)) & temp[temp$bld_id==j,]==2,with=FALSE]),nchar(names(temp[temp$bld_id==j,grepl("diff",names(temp)) & temp[temp$bld_id==j,]==2,with=FALSE]))))
  # 
  #     #Update temp
  #     temp<-temp[,c(1:12)]
  # 
  #     if(nrow(temp[temp$bld_type==1])>0){
  #       temp<-merge(temp,aggregate(data=temp[temp$bld_type==1],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
  #       names(temp)[names(temp)=="bld_hh.x"]<-"bld_hh"
  #       names(temp)[ncol(temp)]<-"sum_hhdtype.1"
  #     }else{
  #       temp$sum_hhdtype.1<-NA
  #     }
  # 
  #     if(nrow(temp[temp$bld_type==2])>0){
  #       temp<-merge(temp,aggregate(data=temp[temp$bld_type==2],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
  #       names(temp)[names(temp)=="bld_hh.x"]<-"bld_hh"
  #       names(temp)[ncol(temp)]<-"sum_hhdtype.2"
  #     }else{
  #       temp$sum_hhdtype.2<-NA
  #     }
  # 
  #     if(nrow(temp[temp$bld_type==9999])>0){
  #       temp<-merge(temp,aggregate(data=temp[temp$bld_type==9999],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
  #       names(temp)[names(temp)=="bld_hh.x"]<-"bld_hh"
  #       names(temp)[ncol(temp)]<-"sum_hhdtype.9999"
  #     }else{
  #       temp$sum_hhdtype.9999<-NA
  #     }
  # 
  #     hhdtype_diff<-temp[,grepl("da_hhdtype",names(temp)),with=FALSE]-temp[,grepl("sum_hhdtype",names(temp)),with=FALSE][,1:2]
  #     hhdtype_diff[is.na(hhdtype_diff)]<-c(-999999)
  #     temp1<-apply(hhdtype_diff,1,function(x) rank(x,ties.method="first"))
  #     temp1<-transpose(data.table(temp1))
  #     names(temp1)<-paste("diff_hhdtype.",1:2,sep="")
  #     temp<-cbind(temp,temp1)
  # 
  #   }
  # 
  #   BT[BT$da_id==i]<-temp
  # 
  # }
  # temp_hhdtype<-BT[,c("bld_id","bld_type")]
  # #Finalize BT
  # BT<-fread("Output/BT_nearestDB.csv")
  # BT<-BT[,2:8]
  # names(BT)[names(BT)=="DBUID"]<-"db_id"
  # BT<-BT[BT$bld_id%in%GIS$bld_id,]
  # DBlatlon<-fread("Input/Census/DB/DB_MTL.csv")
  # DBlatlon<-DBlatlon[,c(1,ncol(DBlatlon)-1,ncol(DBlatlon)-2),with=FALSE]
  # names(DBlatlon)<-c("db_id","db_lat","db_lon")
  # DBlatlon<-DBlatlon[DBlatlon$db_id%in%GIS$db_id]
  # BT<-rbind(BT,data.table(bld_id=DBT$db_id[!DBT$db_id%in%BT$db_id] ,bld_dwellings=DBT$db_dwell[!DBT$db_id%in%BT$db_id] ,bld_consyr=9999 ,bld_type="Unknown" ,bld_lat=DBlatlon$db_lat[!DBlatlon$db_id%in%BT$db_id] ,bld_lon=DBlatlon$db_lon[!DBlatlon$db_id%in%BT$db_id] ,db_id=DBT$db_id[!DBT$db_id%in%BT$db_id]))
  # BT$bld_consyr[BT$bld_consyr==9999]<-temp_hhdbuilt$bld_consyr[temp_hhdbuilt$bld_id%in%BT$bld_id[BT$bld_consyr==9999]]
  # BT$bld_consyr[BT$bld_consyr>5 & BT$bld_consyr<=1960]<-1
  # BT$bld_consyr[BT$bld_consyr>=1961 & BT$bld_consyr<=1980]<-2
  # BT$bld_consyr[BT$bld_consyr>=1981 & BT$bld_consyr<=2000]<-3
  # BT$bld_consyr[BT$bld_consyr>=2001 & BT$bld_consyr<=2016]<-4
  # BT$bld_type[BT$bld_type=="Unknown"]<-temp_hhdtype$bld_type[temp_hhdtype$bld_id%in%BT$bld_id[BT$bld_type=="Unknown"]]
  # BT$bld_type[BT$bld_type=="Condominium"]<-1
  # BT$bld_type[BT$bld_type=="Regular"]<-2
  # BT$bld_type<-as.numeric(BT$bld_type)
  # 
  # CDT<-fread("Output/hCDT.csv")
  # CDT<-CDT[,-1]
  # DAT<-fread("Output/hDAT.csv")
  # DAT<-DAT[,-1]
  # 
  # ###DBT
  # #Control variables
  # result<-data.table()
  # for (i in as.numeric(DBT$db_id)){
  #   print(i)
  #   DBT_i<-DBT[DBT$db_id==i]
  # 
  #   temp<-data.table(
  #     geo=unique(DBT_i$db_id)
  # 
  #     ,hhcount=DBT_i$db_urdwell
  # 
  #     # ,hhdcondo.1= sum(BT$bld_dwellings[BT$bld_id%in%GIS$bld_id[GIS$db_id%in%DBT_i$db_id] & BT$bld_type=="Condominium"])*(DBT_i$db_urdwell/DBT_i$db_dwell) #condominium
  #     # ,hhdcondo.2= sum(BT$bld_dwellings[BT$bld_id%in%GIS$bld_id[GIS$db_id%in%DBT_i$db_id] & BT$bld_type=="Regular"])*(DBT_i$db_urdwell/DBT_i$db_dwell) #not condominium
  #     #
  #     # ,hhdbuilt.1= sum(BT$bld_dwellings[BT$bld_id%in%GIS$bld_id[GIS$db_id%in%DBT_i$db_id] & BT$bld_consyr<=1960])*(DBT_i$db_urdwell/DBT_i$db_dwell) #<=1960
  #     # ,hhdbuilt.2= sum(BT$bld_dwellings[BT$bld_id%in%GIS$bld_id[GIS$db_id%in%DBT_i$db_id] & BT$bld_consyr>=1961 & BT$bld_consyr<=1980])*(DBT_i$db_urdwell/DBT_i$db_dwell) #[1961,1980]
  #     # ,hhdbuilt.3= sum(BT$bld_dwellings[BT$bld_id%in%GIS$bld_id[GIS$db_id%in%DBT_i$db_id] & BT$bld_consyr>=1981 & BT$bld_consyr<=2000])*(DBT_i$db_urdwell/DBT_i$db_dwell) #[1981,2000]
  #     # ,hhdbuilt.4= sum(BT$bld_dwellings[BT$bld_id%in%GIS$bld_id[GIS$db_id%in%DBT_i$db_id] & BT$bld_consyr>=2001 & BT$bld_consyr<=2016])*(DBT_i$db_urdwell/DBT_i$db_dwell) #[2001,2016]
  # 
  #     ,ppcount=DBT_i$db_pop
  #   )
  #   result<-rbind(result,temp)
  # }
  # DBT<-result
  # 
  # 
  # 
  # ###BT
  # GIS$bld_id<-as.numeric(GIS$bld_id)
  # GIS$db_id<-as.numeric(GIS$db_id)
  # GIS$bld_id[is.na(GIS$bld_id)]<-GIS$db_id[is.na(GIS$bld_id)]
  # #DB occupation rate
  # OR<-fread("Input/Census/DB/2016_92-151_XBB.csv") #Dissemination blocks
  # OR<-data.table(db_id=OR$`DBuid/IDidu`, db_pop=OR$`DBpop2016/IDpop2016`, db_dwell=OR$`DBtdwell2016/IDtlog2016`, db_urdwell=OR$`DBurdwell2016/IDrh2016`)
  # OR$db_id<-as.numeric(OR$db_id)
  # BT$db_id<-as.numeric(BT$db_id)
  # OR<-OR[OR$db_id%in%GIS$db_id,]
  # OR$db_or<-OR$db_urdwell/OR$db_dwell
  # OR<-OR[,c(1,5)]
  # BT<-merge(BT,OR,by="db_id")
  # BT$bld_hh<-BT$bld_dwellings*BT$db_or
  # #BT
  # temp<-data.table(geo=BT$bld_id,hhcount=0,hhdcondo.1=0,hhdcondo.2=0,hhdbuilt.1=0,hhdbuilt.2=0,hhdbuilt.3=0,hhdbuilt.4=0)
  # temp$hhcount<-BT$bld_hh
  # temp$hhdcondo.1[BT$bld_type==1]<-BT$bld_hh[BT$bld_type==1]
  # temp$hhdcondo.2[BT$bld_type==2]<-BT$bld_hh[BT$bld_type==2]
  # temp$hhdbuilt.1[BT$bld_consyr==1]<-BT$bld_hh[BT$bld_consyr==1]
  # temp$hhdbuilt.2[BT$bld_consyr==2]<-BT$bld_hh[BT$bld_consyr==2]
  # temp$hhdbuilt.3[BT$bld_consyr==3]<-BT$bld_hh[BT$bld_consyr==3]
  # temp$hhdbuilt.4[BT$bld_consyr==4]<-BT$bld_hh[BT$bld_consyr==4]
  # BT<-temp
  # BT$geo<-as.numeric(BT$geo)
  # #TRS hhcount
  # for (i in unique(GIS$db_id)){
  #   print(as.character(i))
  #   hhcount_i<-BT[BT$geo%in%GIS$bld_id[GIS$db_id==i],"hhcount"]
  #   hhcountint_i<-floor(hhcount_i)
  #   hhcountrem_i<-hhcount_i-hhcountint_i
  #   hhcountdef_i<-round(sum(hhcountrem_i))
  #   if (hhcountdef_i>0){
  #     incr<-sample(nrow(hhcount_i),size=unlist(hhcountdef_i),prob=unlist(hhcountrem_i))
  #     BT[BT$geo%in%GIS$bld_id[GIS$db_id==i],"hhcount"][incr]<-floor(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i],"hhcount"][incr])+1
  #     BT[BT$geo%in%GIS$bld_id[GIS$db_id==i],"hhcount"]<-floor(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i],"hhcount"])
  #   }else{
  #     BT[BT$geo%in%GIS$bld_id[GIS$db_id==i],"hhcount"]<-floor(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i],"hhcount"])
  #   }
  # }
  # BT<-BT[BT$hhcount!=0]
  # 
  # 
  # ##Intra-stratum harmonization of household variables
  # eps<-0.00001
  # BT<-as.data.frame(BT)
  # BT[BT<=0]<-eps #Tweak remaining 0s
  # BT<-as.data.table(BT)
  # 
  # 
  # ##Harmonize households variables
  # setorder(BT, geo)
  # variables<-unique(gsub("\\..*","",names(BT)))
  # hhvariables<-variables[grepl("hh",variables)]
  # BT<-data.frame(BT)
  # 
  # discrepancy<-c()
  # for (j in 2:length(hhvariables)){
  #   discrepancy<-c(discrepancy,max(abs(BT[,names(BT)[grepl(hhvariables[1],names(BT))]]-rowSums(BT[,names(BT)[grepl(hhvariables[j],names(BT))]]))))
  # }
  # print(max(discrepancy))
  # #Intra-stratum harmonization
  # for (j in 2:length(hhvariables)){
  #   temp<-BT[,names(BT)[grepl(hhvariables[j],names(BT))]]*unlist(BT[,names(BT)[grepl(hhvariables[1],names(BT))]]/rowSums(BT[,names(BT)[grepl(hhvariables[j],names(BT))]]))
  #   BT[,names(BT)[grepl(hhvariables[j],names(BT))]]<-temp
  # }
  # BT<-round(BT,0)
  # #Test
  # discrepancy<-c()
  # for (j in 2:length(hhvariables)){
  #   discrepancy<-c(discrepancy,max(abs(BT[,names(BT)[grepl(hhvariables[1],names(BT))]]-rowSums(BT[,names(BT)[grepl(hhvariables[j],names(BT))]]))))
  # }
  # print(max(discrepancy))
  # 
  # write.csv(BT,"Output/nonhBT.csv")
}



###Inputs
CDT<-fread("Output/hCDT.csv")
CDT<-CDT[,-1]
DAT<-fread("Output/hDAT.csv")
DAT<-DAT[,-1]
BT<-fread("Output/nonhBT.csv")
BT<-BT[,-1]
BT$geo<-as.numeric(BT$geo)
PUMF<-fread("Output/hPUMF.csv")
PUMF<-PUMF[,-1]
GIS<-fread("Output/hGIS.csv")
GIS<-GIS[,-1]
#Filter targets to variables kept for the PUMF
CDT<-CDT[,gsub("\\..*","",names(CDT))%in%c("geo",names(PUMF)),with=FALSE]
DAT<-DAT[,gsub("\\..*","",names(DAT))%in%c("geo",names(PUMF)),with=FALSE]



###PUMF matrix (A)
# PUMF_MAT<-data.table(matrix(nrow=length(unique(PUMF$hhid)),ncol=length(names(CDT))))
# PUMF_MAT[is.na(PUMF_MAT)]<-0
# names(PUMF_MAT)<-names(CDT)
# names(PUMF_MAT)[1]<-"hhid"
# PUMF_MAT$hhid<-unique(PUMF$hhid)
# PUMF_MAT$hhcount<-1
# for (i in unique(PUMF$hhid)){
#   PUMF_MAT$ppcount[PUMF_MAT$hhid==i]<-unique(PUMF$ppcount[PUMF$hhid==i])
# }
# PUMF_MAT<-data.frame(PUMF_MAT)
# for (i in unique(PUMF$ppid)){
#   PUMF_MAT[PUMF_MAT$hhid==PUMF$hhid[PUMF$ppid==i],names(PUMF_MAT)%in%paste(names(PUMF),".",PUMF[PUMF$ppid==i,],sep="") & grepl("hh",names(PUMF_MAT))]<-1
#   PUMF_MAT[PUMF_MAT$hhid==PUMF$hhid[PUMF$ppid==i],names(PUMF_MAT)%in%paste(names(PUMF),".",PUMF[PUMF$ppid==i,],sep="") & grepl("pp",names(PUMF_MAT))]<-
#     PUMF_MAT[PUMF_MAT$hhid==PUMF$hhid[PUMF$ppid==i],names(PUMF_MAT)%in%paste(names(PUMF),".",PUMF[PUMF$ppid==i,],sep="") & grepl("pp",names(PUMF_MAT))]+1
# }

# milestone1<-PUMF_MAT
# write.csv(milestone1,"Output/PUMF_MAT.csv")
PUMF_MAT<-fread("Output/PUMF_MAT.csv")
PUMF_MAT<-PUMF_MAT[,-1]


###Population synthesis   #3154 is the biggest : 2060511*6391   #41 is a small example (2 buildings)    length(unique(GIS$da_id))
for (k in 1:3192){
  tryCatch({
    print(k)
    
    ##Inputs_i
    DAT_i<-DAT[DAT$geo%in%unique(GIS$da_id)[k]]
    BT_i<-BT[BT$geo%in%GIS$bld_id[GIS$da_id%in%DAT_i$geo]]
    GIS_i<-GIS[GIS$da_id%in%unique(GIS$da_id)[k]]
    PUMF_MAT_i<-data.table(da_id=unique(GIS_i$da_id),PUMF_MAT)
    if(length(names(DAT_i)[DAT_i==0])>0){
      PUMF_MAT_i<-PUMF_MAT_i[rowSums(PUMF_MAT_i[,names(DAT_i)[DAT_i==0],with=FALSE])==0,]
    }
    PUMF_MAT_i$hhid<-1:nrow(PUMF_MAT_i)
    dacat<-names(DAT_i)[names(DAT_i)!="geo"]
    
    
    ##A_i
    if(class(try(matrix(0,nrow=nrow(PUMF_MAT_i),ncol=length(dacat)*length(unique(GIS_i$da_id)))))[1]=="try-error"){
      A_i<-Matrix(0,nrow=nrow(PUMF_MAT_i),ncol=length(dacat)*length(unique(GIS_i$da_id)),sparse=TRUE)
    }else{
      A_i<-matrix(0,nrow=nrow(PUMF_MAT_i),ncol=length(dacat)*length(unique(GIS_i$da_id)))
    }
    colnames(A_i)<-c(paste(paste("DA",sort(rep(unique(GIS_i$da_id),length(dacat))),sep=""),dacat,sep="_"))
    rownames(A_i)<-PUMF_MAT_i$hhid
    temp_i<-PUMF_MAT_i[,c("da_id","hhid",dacat),with=FALSE]
    for (i in unique(GIS_i$da_id)){
      A_i[unlist(temp_i[temp_i$da_id==i,"hhid"]),grepl(as.character(i),colnames(A_i)) & grepl("DA",colnames(A_i))]<-as.matrix(temp_i[temp_i$da_id==i,dacat,with=FALSE])
      # print(i)
    }
    if(class(try(matrix(0,nrow=nrow(PUMF_MAT_i),ncol=length(dacat)*length(unique(GIS_i$da_id)))))[1]=="try-error"){
      A_i<-Matrix(A_i,sparse=TRUE)
    }
    A_i<-t(A_i)
    gc()
    
    
    ##B_i
    B_i<-Matrix(0,nrow=length(dacat)*length(unique(GIS_i$da_id)),ncol=1,sparse=TRUE)
    B_i[1:length(dacat)*length(unique(GIS_i$da_id)),]<-unlist(matrix(t(DAT_i[,names(DAT_i)%in%dacat,with=FALSE])))
    rownames(B_i)<-rownames(A_i)
    gc()
    
    
    ##Optimization_i
    #optmeth: glmnet
    time_est<-Sys.time()
    glmnet_i<-glmnet(A_i,B_i,alpha=1, lambda = 0, lower.limits=0, intercept=F, thres = 1E-10)
    time_est<-difftime(Sys.time(), time_est, units = 'secs')
    X_i<-coef(glmnet_i)[,1][-1]
    X_i<-X_i[X_i>0]
    A_i<-A_i[,colnames(A_i)%in%names(X_i)]
    
    #intmeth: probability to increase a weight is proportional to the decimal
    rmse<-function(x){
      sqrt(sum((A_i%*%x-B_i)^2)/nrow(B_i))
    }
    temp<-c()
    for (i in 1:1000){
      W1_i<-X_i
      W1int_i<-floor(W1_i)
      W1rem_i<-W1_i-W1int_i
      W1def_i<-round(sum(W1rem_i))
      incr<-sample(length(W1_i),size=W1def_i,prob=W1rem_i)
      W1_i[incr]<-floor(W1_i[incr])+1
      W1_i<-floor(W1_i)
      W1_i<-c(W1_i,rmse(W1_i))
      temp<-rbind(temp,W1_i)
    }
    W1_i<-temp[temp[,ncol(temp)]==min(temp[,ncol(temp)]),-ncol(temp)]
    if(!is.null(nrow(W1_i))){
      W1_i<-W1_i[1,]
    }
    W1_i<-W1_i[W1_i>0]
    
    
    ##Synthetic population
    SP_i<-data.table(PUMF_MAT_i[PUMF_MAT_i$hhid%in%names(W1_i)],weight=W1_i)
    saveRDS(data.table(PUMF_MAT_i[PUMF_MAT_i$hhid%in%names(X_i)],weight=X_i),paste("Output/1RS_Weighted population/WP_",k,sep=""))
    saveRDS(data.table(PUMF_MAT_i[PUMF_MAT_i$hhid%in%names(W1_i)],weight=W1_i),paste("Output/1RS_Synthetic population/SP_",k,sep=""))
    
    
    ##Spatialization
    SSP_i<-SP_i[,c("da_id","hhid","weight"),with=FALSE]
    SSP_i$type<-paste(SP_i$hhdcondo.1,SP_i$hhdcondo.2,SP_i$hhdbuilt.1,SP_i$hhdbuilt.2,SP_i$hhdbuilt.3,SP_i$hhdbuilt.4)
    SSP_i$type[SSP_i$type=="1 0 1 0 0 0"]<-"1 1"
    SSP_i$type[SSP_i$type=="1 0 0 1 0 0"]<-"1 2"
    SSP_i$type[SSP_i$type=="1 0 0 0 1 0"]<-"1 3"
    SSP_i$type[SSP_i$type=="1 0 0 0 0 1"]<-"1 4"
    SSP_i$type[SSP_i$type=="0 1 1 0 0 0"]<-"2 1"
    SSP_i$type[SSP_i$type=="0 1 0 1 0 0"]<-"2 2"
    SSP_i$type[SSP_i$type=="0 1 0 0 1 0"]<-"2 3"
    SSP_i$type[SSP_i$type=="0 1 0 0 0 1"]<-"2 4" 
    SSP_i<-SSP_i[rep(seq(.N),weight),!"weight"]
    temp<-as.data.frame(BT_i)
    temp[,grepl("\\.",names(temp))][temp[,grepl("\\.",names(temp))]>1]<-1
    
    
    BT_i$type<-paste(temp$hhdcondo.1,temp$hhdcondo.2,temp$hhdbuilt.1,temp$hhdbuilt.2,temp$hhdbuilt.3,temp$hhdbuilt.4)
    BT_i$type[BT_i$type=="1 0 1 0 0 0"]<-"1 1"
    BT_i$type[BT_i$type=="1 0 0 1 0 0"]<-"1 2"
    BT_i$type[BT_i$type=="1 0 0 0 1 0"]<-"1 3"
    BT_i$type[BT_i$type=="1 0 0 0 0 1"]<-"1 4"
    BT_i$type[BT_i$type=="0 1 1 0 0 0"]<-"2 1"
    BT_i$type[BT_i$type=="0 1 0 1 0 0"]<-"2 2"
    BT_i$type[BT_i$type=="0 1 0 0 1 0"]<-"2 3"
    BT_i$type[BT_i$type=="0 1 0 0 0 1"]<-"2 4" 
    BT_i<-BT_i[,!grepl("\\.",names(BT_i)),with=FALSE]
    names(BT_i)[1]<-"bld_id"
    BT_i<-BT_i[rep(seq(.N),hhcount),!"hhcount"]
    BT_i$hhdcondo<-as.numeric(substr(BT_i$type,1,1))
    BT_i$hhdbuilt<-as.numeric(substr(BT_i$type,3,3))
    SSP_i$hhdcondo<-as.numeric(substr(SSP_i$type,1,1))
    SSP_i$hhdbuilt<-as.numeric(substr(SSP_i$type,3,3))
    SSP_i$bld_id<-0
    temp<-BT_i
    
    #Assign a household to a building of the same type (matching on hhdcondo and hhdbuilt)
    for (i in unique(SSP_i$type)){
      SSP_i$bld_id[SSP_i$type==i][0:min(length(SSP_i$bld_id[SSP_i$type==i]),nrow(BT_i[BT_i$type==i]))]<-BT_i$bld_id[BT_i$type==i][0:min(length(SSP_i$bld_id[SSP_i$type==i]),nrow(BT_i[BT_i$type==i]))]
    }

    #Update BT_i
    BT_i<-c()
    for (i in unique(temp$bld_id)){
      BT_i<-rbind(BT_i,temp[temp$bld_id==i][0:(nrow(temp[temp$bld_id==i])-nrow(SSP_i[SSP_i$bld_id==i]))])
    }

    #Assign households which have matching buildings on hhdcondo but do not have matching buildings on hhdbuilt
    for (i in unique(SSP_i$hhdcondo[SSP_i$bld_id==0 & SSP_i$hhdcondo%in%BT_i$hhdcondo & !SSP_i$hhdbuilt%in%BT_i$hhdbuilt])){
      SSP_i$bld_id[SSP_i$bld_id==0 & SSP_i$hhdcondo==i & !SSP_i$hhdbuilt%in%BT_i$hhdbuilt][0:min(length(SSP_i$bld_id[SSP_i$bld_id==0 & SSP_i$hhdcondo==i & !SSP_i$hhdbuilt%in%BT_i$hhdbuilt]),length(BT_i$bld_id[BT_i$hhdcondo==i]))]<-
        BT_i$bld_id[BT_i$hhdcondo==i][0:min(length(SSP_i$bld_id[SSP_i$bld_id==0 & SSP_i$hhdcondo==i & !SSP_i$hhdbuilt%in%BT_i$hhdbuilt]),length(BT_i$bld_id[BT_i$hhdcondo==i]))]
    }
    
    #Update BT_i
    BT_i<-c()
    for (i in unique(temp$bld_id)){
      BT_i<-rbind(BT_i,temp[temp$bld_id==i][0:(nrow(temp[temp$bld_id==i])-nrow(SSP_i[SSP_i$bld_id==i]))])
    }

    #Assign households which have matching buildings on hhdbuilt but do not have matching buildings on hhdcondo
    for (i in unique(SSP_i$hhdbuilt[SSP_i$bld_id==0 & SSP_i$hhdbuilt%in%BT_i$hhdbuilt & !SSP_i$hhdcondo%in%BT_i$hhdcondo])){
      SSP_i$bld_id[SSP_i$bld_id==0 & SSP_i$hhdbuilt==i & !SSP_i$hhdcondo%in%BT_i$hhdcondo][0:min(length(SSP_i$bld_id[SSP_i$bld_id==0 & SSP_i$hhdbuilt==i & !SSP_i$hhdcondo%in%BT_i$hhdcondo]),length(BT_i$bld_id[BT_i$hhdbuilt==i]))]<-
        BT_i$bld_id[BT_i$hhdbuilt==i][0:min(length(SSP_i$bld_id[SSP_i$bld_id==0 & SSP_i$hhdbuilt==i & !SSP_i$hhdcondo%in%BT_i$hhdcondo]),length(BT_i$bld_id[BT_i$hhdbuilt==i]))]
    }
    
    #Update BT_i
    BT_i<-c()
    for (i in unique(temp$bld_id)){
      BT_i<-rbind(BT_i,temp[temp$bld_id==i][0:(nrow(temp[temp$bld_id==i])-nrow(SSP_i[SSP_i$bld_id==i]))])
    }

    #Assign households to buildings matching on hhdcondo
    for (i in unique(BT_i$hhdcondo)){
      SSP_i$bld_id[SSP_i$bld_id==0 & SSP_i$hhdcondo==i][0:min(length(SSP_i$bld_id[SSP_i$bld_id==0 & SSP_i$hhdcondo==i]),length(BT_i$bld_id[BT_i$hhdcondo==i]))]<-
        BT_i$bld_id[BT_i$hhdcondo==i][0:min(length(SSP_i$bld_id[SSP_i$bld_id==0 & SSP_i$hhdcondo==i]),length(BT_i$bld_id[BT_i$hhdcondo==i]))]
    }
    
    #Update BT_i
    BT_i<-c()
    for (i in unique(temp$bld_id)){
      BT_i<-rbind(BT_i,temp[temp$bld_id==i][0:(nrow(temp[temp$bld_id==i])-nrow(SSP_i[SSP_i$bld_id==i]))])
    }

    #Assign households to buildings matching on hhdbuilt
    for (i in unique(BT_i$hhdbuilt)){
      SSP_i$bld_id[SSP_i$bld_id==0 & SSP_i$hhdbuilt==i][0:min(length(SSP_i$bld_id[SSP_i$bld_id==0 & SSP_i$hhdbuilt==i]),length(BT_i$bld_id[BT_i$hhdbuilt==i]))]<-
        BT_i$bld_id[BT_i$hhdbuilt==i][0:min(length(SSP_i$bld_id[SSP_i$bld_id==0 & SSP_i$hhdbuilt==i]),length(BT_i$bld_id[BT_i$hhdbuilt==i]))]
    }
    
    #Update BT_i
    BT_i<-c()
    for (i in unique(temp$bld_id)){
      BT_i<-rbind(BT_i,temp[temp$bld_id==i][0:(nrow(temp[temp$bld_id==i])-nrow(SSP_i[SSP_i$bld_id==i]))])
    }    
   
    #Assign remaining households to remaining dwelling units
    SSP_i$bld_id[SSP_i$bld_id==0][0:min(length(SSP_i$bld_id[SSP_i$bld_id==0]),length(BT_i$bld_id))]<-BT_i$bld_id[0:min(length(SSP_i$bld_id[SSP_i$bld_id==0]),length(BT_i$bld_id))]
    
    #Update BT_i
    BT_i<-c()
    for (i in unique(temp$bld_id)){
      BT_i<-rbind(BT_i,temp[temp$bld_id==i][0:(nrow(temp[temp$bld_id==i])-nrow(SSP_i[SSP_i$bld_id==i]))])
    }        
    
    #Distribute remaining households over all buildings
    while (length(SSP_i$bld_id[SSP_i$bld_id==0])>0){
      SSP_i$bld_id[SSP_i$bld_id==0][0:min(length(SSP_i$bld_id[SSP_i$bld_id==0]),length(unique(temp$bld_id)))]<-unique(temp$bld_id)[0:min(length(SSP_i$bld_id[SSP_i$bld_id==0]),length(unique(temp$bld_id)))]
    }

    #Update BT_i
    BT_i<-c()
    for (i in unique(temp$bld_id)){
      BT_i<-rbind(BT_i,temp[temp$bld_id==i][0:max(0,(nrow(temp[temp$bld_id==i])-nrow(SSP_i[SSP_i$bld_id==i])))])
    }

    #Remainders
    REM<-data.table(hhdcondo.1=nrow(SSP_i[SSP_i$bld_id==0 & SSP_i$hhdcondo==1]),
                    hhdcondo.2=nrow(SSP_i[SSP_i$bld_id==0 & SSP_i$hhdcondo==2]),
                    hhdbuilt.1=nrow(SSP_i[SSP_i$bld_id==0 & SSP_i$hhdbuilt==1]),
                    hhdbuilt.2=nrow(SSP_i[SSP_i$bld_id==0 & SSP_i$hhdbuilt==2]),
                    hhdbuilt.3=nrow(SSP_i[SSP_i$bld_id==0 & SSP_i$hhdbuilt==3]),
                    hhdbuilt.4=nrow(SSP_i[SSP_i$bld_id==0 & SSP_i$hhdbuilt==4]),
                    bld_hhdcondo.1=nrow(BT_i[BT_i$hhdcondo==1])-nrow(SSP_i[SSP_i$bld_id!=0 & SSP_i$hhdcondo==1]),
                    bld_hhdcondo.2=nrow(BT_i[BT_i$hhdcondo==2])-nrow(SSP_i[SSP_i$bld_id!=0 & SSP_i$hhdcondo==2]),
                    bld_hhdbuilt.1=nrow(BT_i[BT_i$hhdbuilt==1])-nrow(SSP_i[SSP_i$bld_id!=0 & SSP_i$hhdbuilt==1]),
                    bld_hhdbuilt.2=nrow(BT_i[BT_i$hhdbuilt==2])-nrow(SSP_i[SSP_i$bld_id!=0 & SSP_i$hhdbuilt==2]),
                    bld_hhdbuilt.3=nrow(BT_i[BT_i$hhdbuilt==3])-nrow(SSP_i[SSP_i$bld_id!=0 & SSP_i$hhdbuilt==3]),
                    bld_hhdbuilt.4=nrow(BT_i[BT_i$hhdbuilt==4])-nrow(SSP_i[SSP_i$bld_id!=0 & SSP_i$hhdbuilt==4]))
    
    if(nrow(SSP_i[SSP_i$bld_id==0])>0){print("error")}
    
    
    ##Spatialized synthetic population
    temp<-merge(SSP_i[,-"type"],SP_i[,-c("da_id","weight")],by="hhid")
    temp<-setDT(temp)[, .N, by = c(names(temp))]
    temp<-merge(temp,GIS[,c("bld_id","db_id")],by="bld_id",all.x=TRUE)
    names(temp)[names(temp)=="N"]<-"weight"
    names_SP<-readRDS("Output/Synthetic population/SP_41")
    SSP_i<-temp[,names(names_SP),with=FALSE]
    saveRDS(SSP_i,paste("Output/1RS_Spatialized synthetic population/SSP_",k,sep=""))

    
    ##Cleaning
    rm(DAT_i,GIS_i,PUMF_MAT_i,temp_i,temp,A_i,B_i,X_i,W1_i)
    gc()
    
  })
}




