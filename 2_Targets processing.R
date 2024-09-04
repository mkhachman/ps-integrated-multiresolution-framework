rm(list=ls())

###Packages
library(data.table)
# library(sf)
library(dplyr)
library(stringr)
library(gsubfn)

###Import totals
#GIS structure (standard geographic areas correspondence)
GIS<-fread("Input/Census/DB/2016_92-151_XBB.csv")
BT<-fread("Output/BT_nearestDB.csv")
BT<-BT[,2:8]
names(BT)[names(BT)=="DBUID"]<-"db_id"
GIS<-merge(GIS,BT,by.x="DBuid/IDidu",by.y="db_id",all.x=TRUE) #Include buildings in GIS
names(GIS)[names(GIS)=="DBuid/IDidu"]<-"db_id"
names(GIS)[names(GIS)=="DAuid/ADidu"]<-"da_id"
names(GIS)[names(GIS)=="CDuid/DRidu"]<-"cd_id"
GIS<-GIS[,c("bld_id","db_id","da_id","cd_id")]

###Finalize GIS
#Census divisions totals (CDT)
CDT<-fread("Input/Census/CD/98-401-X2016060_English_CSV_data.csv") #Census divisons
CDT<-data.table(cd_id=CDT$`GEO_CODE (POR)`, cd_var=CDT$`DIM: Profile of Census Divisions (2247)`, cd_varid= CDT$`Notes: Profile of Census Divisions (2247)`, cd_catid=CDT$`Member ID: Profile of Census Divisions (2247)`, cd_total=CDT$`Dim: Sex (3): Member ID: [1]: Total - Sex`, cd_men=CDT$`Dim: Sex (3): Member ID: [2]: Male`, cd_women=CDT$`Dim: Sex (3): Member ID: [3]: Female`)
CDT<-CDT[CDT$cd_id==2466,] #Filter to Montreal
CDT$cd_total<-as.numeric(CDT$cd_total)
CDT$cd_men<-as.numeric(CDT$cd_men)
CDT$cd_women<-as.numeric(CDT$cd_women)
CDT[is.na(CDT)]<-0
GIS<-GIS[GIS$cd_id%in%CDT$cd_id,] #Update GIS
#Dissemination areas totals (DAT)
#DAT<-fread("Input/Census/DA/98-401-X2016044_English_CSV_data.csv") #Dissemination areas
# DAT_MTL<-DAT[DAT$`GEO_CODE (POR)`%in%unique(GIS$`DAuid/ADidu`[GIS$`CDname/DRnom`=="Montr?al"]),c("GEO_CODE (POR)","DIM: Profile of Dissemination Areas (2247)","Member ID: Profile of Dissemination Areas (2247)","Notes: Profile of Dissemination Areas (2247)","Dim: Sex (3): Member ID: [1]: Total - Sex","Dim: Sex (3): Member ID: [2]: Male","Dim: Sex (3): Member ID: [3]: Female")] #Census targets for dissemination areas in Montreal island
# write.csv(DAT_MTL,"Input/Census/DA/MTL_98-401-X2016044_English_CSV_data.csv")
DAT<-fread("Input/Census/DA/MTL_98-401-X2016044_English_CSV_data.csv")
DAT<-data.table(da_id=DAT$`GEO_CODE (POR)`, da_var=DAT$`DIM: Profile of Dissemination Areas (2247)`, da_varid= DAT$`Notes: Profile of Dissemination Areas (2247)`, da_catid=DAT$`Member ID: Profile of Dissemination Areas (2247)`, da_total=DAT$`Dim: Sex (3): Member ID: [1]: Total - Sex`, da_men=DAT$`Dim: Sex (3): Member ID: [2]: Male`, da_women=DAT$`Dim: Sex (3): Member ID: [3]: Female`)
DAT$da_total<-as.numeric(DAT$da_total)
DAT$da_men<-as.numeric(DAT$da_men)
DAT$da_women<-as.numeric(DAT$da_women)
DAT[is.na(DAT)]<-0
DAT<-DAT[!DAT$da_id%in%DAT$da_id[(DAT$da_catid==1 & DAT$da_total==0)|(DAT$da_catid==5 & DAT$da_total==0)]] #only keep DAs with a population and private dwellings occupied by usual residents
setorder(DAT,da_id)
GIS<-GIS[GIS$da_id%in%DAT$da_id] #update GIS
#DBT
DBT<-fread("Input/Census/DB/2016_92-151_XBB.csv") #Dissemination blocks
DBT<-data.table(db_id=DBT$`DBuid/IDidu`, db_pop=DBT$`DBpop2016/IDpop2016`, db_dwell=DBT$`DBtdwell2016/IDtlog2016`, db_urdwell=DBT$`DBurdwell2016/IDrh2016`)
DBT<-DBT[DBT$db_id%in%GIS$db_id,]
DBT<-DBT[!(DBT$db_pop==0 | DBT$db_urdwell==0)] #only keep DAs with a population and private dwellings occupied by usual residents
GIS<-GIS[GIS$db_id%in%DBT$db_id] #update GIS
#Update CDT and DAT
CDT<-CDT[CDT$cd_id%in%GIS$cd_id]
DAT<-DAT[DAT$da_id%in%GIS$da_id]

#Building totals (BT)
#hhdbuilt.9999 distribution
BT<-fread("Output/BT_nearestDB.csv")
DBlatlon<-fread("Input/Census/DB/DB_MTL.csv")
DBlatlon<-DBlatlon[,c(1,ncol(DBlatlon)-1,ncol(DBlatlon)-2),with=FALSE]
names(DBlatlon)<-c("db_id","db_lat","db_lon")
BT<-BT[,2:8]
names(BT)[names(BT)=="DBUID"]<-"db_id"
BT<-BT[BT$bld_id%in%GIS$bld_id,]
DBlatlon<-DBlatlon[DBlatlon$db_id%in%GIS$db_id]
BT<-rbind(BT,data.table(bld_id=DBT$db_id[!DBT$db_id%in%BT$db_id] ,bld_dwellings=DBT$db_dwell[!DBT$db_id%in%BT$db_id] ,bld_consyr=9999 ,bld_type="Unknown" ,bld_lat=DBlatlon$db_lat[!DBlatlon$db_id%in%BT$db_id] ,bld_lon=DBlatlon$db_lon[!DBlatlon$db_id%in%BT$db_id] ,db_id=DBT$db_id[!DBT$db_id%in%BT$db_id]))
BT<-merge(BT,unique(GIS[,c("db_id","da_id")]),by="db_id")
BT<-merge(BT,data.table(da_id=unique(DAT$da_id)
           ,da_hhdbuilt.1=DAT$da_total[DAT$da_catid==1644]  #<=1960
           ,da_hhdbuilt.2=DAT$da_total[DAT$da_catid==1645] #[1961,1980]
           ,da_hhdbuilt.3=DAT$da_total[DAT$da_catid==1646]+DAT$da_total[DAT$da_catid==1647] #[1981,2000]
           ,da_hhdbuilt.4=DAT$da_total[DAT$da_catid==1648]+DAT$da_total[DAT$da_catid==1649]+DAT$da_total[DAT$da_catid==1650] #[2001,2016]
           )
          ,by="da_id")
DBT$db_or<-DBT$db_urdwell/DBT$db_dwell
BT<-merge(BT,DBT[,c("db_id","db_or")],by="db_id")
BT$bld_hh<-BT$bld_dwellings*BT$db_or
BT$bld_consyr[BT$bld_consyr<=1960]<-1
BT$bld_consyr[BT$bld_consyr>=1961 & BT$bld_consyr<=1980]<-2
BT$bld_consyr[BT$bld_consyr>=1981 & BT$bld_consyr<=2000]<-3
BT$bld_consyr[BT$bld_consyr>=2001 & BT$bld_consyr<=2016]<-4
BT<-merge(BT,aggregate(data=BT[BT$bld_consyr==1],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
names(BT)[names(BT)=="bld_hh.x"]<-"bld_hh"
names(BT)[ncol(BT)]<-"sum_hhdbuilt.1"
BT<-merge(BT,aggregate(data=BT[BT$bld_consyr==2],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
names(BT)[names(BT)=="bld_hh.x"]<-"bld_hh"
names(BT)[ncol(BT)]<-"sum_hhdbuilt.2"
BT<-merge(BT,aggregate(data=BT[BT$bld_consyr==3],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
names(BT)[names(BT)=="bld_hh.x"]<-"bld_hh"
names(BT)[ncol(BT)]<-"sum_hhdbuilt.3"
BT<-merge(BT,aggregate(data=BT[BT$bld_consyr==4],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
names(BT)[names(BT)=="bld_hh.x"]<-"bld_hh"
names(BT)[ncol(BT)]<-"sum_hhdbuilt.4"
BT<-merge(BT,aggregate(data=BT[BT$bld_consyr==9999],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
names(BT)[names(BT)=="bld_hh.x"]<-"bld_hh"
names(BT)[ncol(BT)]<-"sum_hhdbuilt.9999"
BT<-BT[BT$da_id%in%BT$da_id[BT$sum_hhdbuilt.9999!=0]]
hhdbuilt_diff<-BT[,grepl("da_hhdbuilt",names(BT)),with=FALSE]-BT[,grepl("sum_hhdbuilt",names(BT)),with=FALSE][,1:4]
temp<-apply(hhdbuilt_diff,1,rank)
temp<-transpose(data.table(temp))
names(temp)<-paste("diff_hhdbuilt.",1:4,sep="")
BT<-cbind(BT,temp)
for (i in unique(BT$da_id)){
  print(i)
  temp<-BT[BT$da_id==i]
  temp_9999<-as.numeric(temp$bld_id[temp$bld_consyr==9999])
  
    for (j in temp_9999){
        temp$bld_consyr[temp$bld_id==j]<-as.numeric(substring(names(temp[temp$bld_id==j,grepl("diff",names(temp)) & temp[temp$bld_id==j,]==4,with=FALSE]),nchar(names(temp[temp$bld_id==j,grepl("diff",names(temp)) & temp[temp$bld_id==j,]==4,with=FALSE]))))
        #Update temp
        
        temp<-temp[,c(1:14)]
        
        if(nrow(temp[temp$bld_consyr==1])>0){
          temp<-merge(temp,aggregate(data=temp[temp$bld_consyr==1],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
          names(temp)[names(temp)=="bld_hh.x"]<-"bld_hh"
          names(temp)[ncol(temp)]<-"sum_hhdbuilt.1"
        }else{
          temp$sum_hhdbuilt.1<-NA
        }
        
        if(nrow(temp[temp$bld_consyr==2])>0){
          temp<-merge(temp,aggregate(data=temp[temp$bld_consyr==2],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
          names(temp)[names(temp)=="bld_hh.x"]<-"bld_hh"
          names(temp)[ncol(temp)]<-"sum_hhdbuilt.2"
        }else{
          temp$sum_hhdbuilt.2<-NA
        }        
        
        if(nrow(temp[temp$bld_consyr==3])>0){
          temp<-merge(temp,aggregate(data=temp[temp$bld_consyr==3],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
          names(temp)[names(temp)=="bld_hh.x"]<-"bld_hh"
          names(temp)[ncol(temp)]<-"sum_hhdbuilt.3"
        }else{
          temp$sum_hhdbuilt.3<-NA
        }        
        
        if(nrow(temp[temp$bld_consyr==4])>0){
          temp<-merge(temp,aggregate(data=temp[temp$bld_consyr==4],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
          names(temp)[names(temp)=="bld_hh.x"]<-"bld_hh"
          names(temp)[ncol(temp)]<-"sum_hhdbuilt.4"
        }else{
          temp$sum_hhdbuilt.4<-NA
        }   
        
        if(nrow(temp[temp$bld_consyr==9999])>0){
          temp<-merge(temp,aggregate(data=temp[temp$bld_consyr==9999],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
          names(temp)[names(temp)=="bld_hh.x"]<-"bld_hh"
          names(temp)[ncol(temp)]<-"sum_hhdbuilt.9999"
        }else{
          temp$sum_hhdbuilt.9999<-NA
        }   
        
        hhdbuilt_diff<-temp[,grepl("da_hhdbuilt",names(temp)),with=FALSE]-temp[,grepl("sum_hhdbuilt",names(temp)),with=FALSE][,1:4]
        hhdbuilt_diff[is.na(hhdbuilt_diff)]<-c(-999999)
        temp1<-apply(hhdbuilt_diff,1,function(x) rank(x,ties.method="first"))
        temp1<-transpose(data.table(temp1))
        names(temp1)<-paste("diff_hhdbuilt.",1:4,sep="")
        temp<-cbind(temp,temp1)
        
    }
  
  BT[BT$da_id==i]<-temp
  
}
temp_hhdbuilt<-BT[,c("bld_id","bld_consyr")]
#hhdtype.9999 distribution
BT<-fread("Output/BT_nearestDB.csv")
DBlatlon<-fread("Input/Census/DB/DB_MTL.csv")
DBlatlon<-DBlatlon[,c(1,ncol(DBlatlon)-1,ncol(DBlatlon)-2),with=FALSE]
names(DBlatlon)<-c("db_id","db_lat","db_lon")
BT<-BT[,2:8]
names(BT)[names(BT)=="DBUID"]<-"db_id"
BT<-BT[BT$bld_id%in%GIS$bld_id,]
DBlatlon<-DBlatlon[DBlatlon$db_id%in%GIS$db_id]
BT<-rbind(BT,data.table(bld_id=DBT$db_id[!DBT$db_id%in%BT$db_id] ,bld_dwellings=DBT$db_dwell[!DBT$db_id%in%BT$db_id] ,bld_consyr=9999 ,bld_type="Unknown" ,bld_lat=DBlatlon$db_lat[!DBlatlon$db_id%in%BT$db_id] ,bld_lon=DBlatlon$db_lon[!DBlatlon$db_id%in%BT$db_id] ,db_id=DBT$db_id[!DBT$db_id%in%BT$db_id]))
BT<-merge(BT,unique(GIS[,c("db_id","da_id")]),by="db_id")
BT<-merge(BT,data.table(da_id=unique(DAT$da_id)
                        ,da_hhdtype.1=DAT$da_total[DAT$da_catid==1622]  #Condo
                        ,da_hhdtype.2=DAT$da_total[DAT$da_catid==1623] #Not condo
)
,by="da_id")
DBT$db_or<-DBT$db_urdwell/DBT$db_dwell
BT<-merge(BT,DBT[,c("db_id","db_or")],by="db_id")
BT$bld_hh<-BT$bld_dwellings*BT$db_or
BT$bld_type[BT$bld_type=="Condominium"]<-1
BT$bld_type[BT$bld_type=="Regular"]<-2
BT$bld_type[BT$bld_type=="Unknown"]<-9999
BT<-merge(BT,aggregate(data=BT[BT$bld_type==1],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
names(BT)[names(BT)=="bld_hh.x"]<-"bld_hh"
names(BT)[ncol(BT)]<-"sum_hhdtype.1"
BT<-merge(BT,aggregate(data=BT[BT$bld_type==2],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
names(BT)[names(BT)=="bld_hh.x"]<-"bld_hh"
names(BT)[ncol(BT)]<-"sum_hhdtype.2"
BT<-merge(BT,aggregate(data=BT[BT$bld_type==9999],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
names(BT)[names(BT)=="bld_hh.x"]<-"bld_hh"
names(BT)[ncol(BT)]<-"sum_hhdtype.9999"
BT<-BT[BT$da_id%in%BT$da_id[BT$sum_hhdtype.9999!=0]]
hhdbuilt_diff<-BT[,grepl("da_hhdtype",names(BT)),with=FALSE]-BT[,grepl("sum_hhdtype",names(BT)),with=FALSE][,1:2]
temp<-apply(hhdbuilt_diff,1,rank)
temp<-transpose(data.table(temp))
names(temp)<-paste("diff_hhdtype.",1:2,sep="")
BT<-cbind(BT,temp)
BT$bld_type<-as.numeric(BT$bld_type)
for (i in unique(BT$da_id)){
  print(i)
  temp<-BT[BT$da_id==i]
  temp_9999<-as.numeric(temp$bld_id[temp$bld_type==9999])
  
  for (j in temp_9999){
    temp$bld_type[temp$bld_id==j]<-as.numeric(substring(names(temp[temp$bld_id==j,grepl("diff",names(temp)) & temp[temp$bld_id==j,]==2,with=FALSE]),nchar(names(temp[temp$bld_id==j,grepl("diff",names(temp)) & temp[temp$bld_id==j,]==2,with=FALSE]))))
    
    #Update temp
    temp<-temp[,c(1:12)]
    
    if(nrow(temp[temp$bld_type==1])>0){
      temp<-merge(temp,aggregate(data=temp[temp$bld_type==1],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
      names(temp)[names(temp)=="bld_hh.x"]<-"bld_hh"
      names(temp)[ncol(temp)]<-"sum_hhdtype.1"
    }else{
      temp$sum_hhdtype.1<-NA
    }
    
    if(nrow(temp[temp$bld_type==2])>0){
      temp<-merge(temp,aggregate(data=temp[temp$bld_type==2],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
      names(temp)[names(temp)=="bld_hh.x"]<-"bld_hh"
      names(temp)[ncol(temp)]<-"sum_hhdtype.2"
    }else{
      temp$sum_hhdtype.2<-NA
    }        
    
    if(nrow(temp[temp$bld_type==9999])>0){
      temp<-merge(temp,aggregate(data=temp[temp$bld_type==9999],bld_hh ~ da_id, function(x) sum(x)),by="da_id",all.x=TRUE)
      names(temp)[names(temp)=="bld_hh.x"]<-"bld_hh"
      names(temp)[ncol(temp)]<-"sum_hhdtype.9999"
    }else{
      temp$sum_hhdtype.9999<-NA
    }   
    
    hhdtype_diff<-temp[,grepl("da_hhdtype",names(temp)),with=FALSE]-temp[,grepl("sum_hhdtype",names(temp)),with=FALSE][,1:2]
    hhdtype_diff[is.na(hhdtype_diff)]<-c(-999999)
    temp1<-apply(hhdtype_diff,1,function(x) rank(x,ties.method="first"))
    temp1<-transpose(data.table(temp1))
    names(temp1)<-paste("diff_hhdtype.",1:2,sep="")
    temp<-cbind(temp,temp1)
    
  }
  
  BT[BT$da_id==i]<-temp
  
}
temp_hhdtype<-BT[,c("bld_id","bld_type")]
#Finalize BT
BT<-fread("Output/BT_nearestDB.csv")
BT<-BT[,2:8]
names(BT)[names(BT)=="DBUID"]<-"db_id"
BT<-BT[BT$bld_id%in%GIS$bld_id,]
DBlatlon<-fread("Input/Census/DB/DB_MTL.csv")
DBlatlon<-DBlatlon[,c(1,ncol(DBlatlon)-1,ncol(DBlatlon)-2),with=FALSE]
names(DBlatlon)<-c("db_id","db_lat","db_lon")
DBlatlon<-DBlatlon[DBlatlon$db_id%in%GIS$db_id]
BT<-rbind(BT,data.table(bld_id=DBT$db_id[!DBT$db_id%in%BT$db_id] ,bld_dwellings=DBT$db_dwell[!DBT$db_id%in%BT$db_id] ,bld_consyr=9999 ,bld_type="Unknown" ,bld_lat=DBlatlon$db_lat[!DBlatlon$db_id%in%BT$db_id] ,bld_lon=DBlatlon$db_lon[!DBlatlon$db_id%in%BT$db_id] ,db_id=DBT$db_id[!DBT$db_id%in%BT$db_id]))
BT$bld_consyr[BT$bld_consyr==9999]<-temp_hhdbuilt$bld_consyr[temp_hhdbuilt$bld_id%in%BT$bld_id[BT$bld_consyr==9999]]
BT$bld_consyr[BT$bld_consyr>5 & BT$bld_consyr<=1960]<-1
BT$bld_consyr[BT$bld_consyr>=1961 & BT$bld_consyr<=1980]<-2
BT$bld_consyr[BT$bld_consyr>=1981 & BT$bld_consyr<=2000]<-3
BT$bld_consyr[BT$bld_consyr>=2001 & BT$bld_consyr<=2016]<-4
BT$bld_type[BT$bld_type=="Unknown"]<-temp_hhdtype$bld_type[temp_hhdtype$bld_id%in%BT$bld_id[BT$bld_type=="Unknown"]]
BT$bld_type[BT$bld_type=="Condominium"]<-1
BT$bld_type[BT$bld_type=="Regular"]<-2
BT$bld_type<-as.numeric(BT$bld_type)



###CDT
#Filter to control variables
result<-data.table()
for (i in unique(CDT$cd_id)){
  CDT_i<-CDT[CDT$cd_id==i]
  
  temp<-data.table(
    geo=unique(CDT_i$cd_id)
    
    ,hhcount=CDT_i$cd_total[CDT_i$cd_catid==5]
    
    ,hhsize.1=CDT_i$cd_total[CDT_i$cd_catid==52] #1 person
    ,hhsize.2=CDT_i$cd_total[CDT_i$cd_catid==53] #2 people
    ,hhsize.3=CDT_i$cd_total[CDT_i$cd_catid==54] #3 people
    ,hhsize.4=CDT_i$cd_total[CDT_i$cd_catid==55] #4 people
    ,hhsize.5=CDT_i$cd_total[CDT_i$cd_catid==56] #>=5 people
    
    ,hhtype.1=CDT_i$cd_total[CDT_i$cd_catid==97] #non-census family
    ,hhtype.2=CDT_i$cd_total[CDT_i$cd_catid==94] #census family without children
    ,hhtype.3=CDT_i$cd_total[CDT_i$cd_catid==95] #census family with children
    ,hhtype.4=CDT_i$cd_total[CDT_i$cd_catid==96] #multiple census families
    
    ,hhnetinc.1=sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(781:786)]) #<30k$
    ,hhnetinc.2=sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(787:791)]) #[30k$,60k$[
    ,hhnetinc.3=sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(792:795)]) #[60k$,100k$[
    ,hhnetinc.4=CDT_i$cd_total[CDT_i$cd_catid==796] #>=100k$
    
    ,hhdtype.1=sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(42)]) #single-detached house (detached)
    ,hhdtype.2=sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(43,47,48)]) #apartment (>=5 storeys, duplex, <5 storeys)
    ,hhdtype.3=sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(45,46,49,50)]) #other (semi-detached, row, other single-attached, movable dwelling)
    
    ,hhdcondo.1=CDT_i$cd_total[CDT_i$cd_catid==1622] #condominium
    ,hhdcondo.2=CDT_i$cd_total[CDT_i$cd_catid==1623] #not condominium
    
    ,hhdnbedrm.1=CDT_i$cd_total[CDT_i$cd_catid==1625] #0 bedrm
    ,hhdnbedrm.2=CDT_i$cd_total[CDT_i$cd_catid==1626] #1 bedrm
    ,hhdnbedrm.3=CDT_i$cd_total[CDT_i$cd_catid==1627] #2 bedrm
    ,hhdnbedrm.4=CDT_i$cd_total[CDT_i$cd_catid==1628] #3 bedrm
    ,hhdnbedrm.5=CDT_i$cd_total[CDT_i$cd_catid==1629] #>=4 bedrm
    
    ,hhdbuilt.1=CDT_i$cd_total[CDT_i$cd_catid==1644] #<=1960
    ,hhdbuilt.2=CDT_i$cd_total[CDT_i$cd_catid==1645] #[1961,1980]
    ,hhdbuilt.3=sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(1646,1647)]) #[1981,2000]
    ,hhdbuilt.4=sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(1648,1649,1650)]) #[2001,2016]
    
    ,ppcount=CDT_i$cd_total[CDT_i$cd_catid==1]
    
    ,ppsex.1=CDT_i$cd_men[CDT_i$cd_catid==8] #men
    ,ppsex.2=CDT_i$cd_women[CDT_i$cd_catid==8] #women
    
    ,ppagegrp.1=CDT_i$cd_total[CDT_i$cd_catid==9] #0-14 yo
    ,ppagegrp.2=CDT_i$cd_total[CDT_i$cd_catid==13] #15-64 yo
    ,ppagegrp.3=CDT_i$cd_total[CDT_i$cd_catid==24] #65+ yo
    
    ,ppmarst.1=CDT_i$cd_total[CDT_i$cd_catid==1] - sum(CDT_i$cd_total[CDT_i$cd_catid %in%c(61,62,65,66,67)]) #never legally married (and not living in common law)
    ,ppmarst.2=CDT_i$cd_total[CDT_i$cd_catid==61] #legally married (and not separated)
    ,ppmarst.3=CDT_i$cd_total[CDT_i$cd_catid==62] #living common law
    ,ppmarst.4=sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(65,66,67)]) #separated, divorced, or widowed (and not living in common law)
    
    ,pplfst.1=CDT_i$cd_total[CDT_i$cd_catid==1867] #employed
    ,pplfst.2=CDT_i$cd_total[CDT_i$cd_catid==1868] #unemployed
    ,pplfst.3=sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(9,1869)]) #not in the labour force
    
    ,ppind.1=sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(1900,1901)]) #agriculture, forestry, fishing and hunting - mining, quarrying, and oil and gas extraction
    ,ppind.2=sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(1902,1903)]) #utilities - construction
    ,ppind.3=CDT_i$cd_total[CDT_i$cd_catid==1904] #manufacturing
    ,ppind.4=sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(1905,1906,1907)]) #wholesale and retail trade
    ,ppind.5=sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(1908:1913)]) #finance and real estate
    ,ppind.6=CDT_i$cd_total[CDT_i$cd_catid==1915] #health care and social assistance
    ,ppind.7=CDT_i$cd_total[CDT_i$cd_catid==1914] #educational services
    ,ppind.8=sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(1916,1917)]) #business services
    ,ppind.9=CDT_i$cd_total[CDT_i$cd_catid==1919] #public administration
    ,ppind.10=CDT_i$cd_total[CDT_i$cd_catid==1918] #other services
    ,ppind.99=CDT_i$cd_total[CDT_i$cd_catid==1] - CDT_i$cd_total[CDT_i$cd_catid==1899] #not applicable
    
    ,ppjtwmode.1=sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(1931,1932)]) #car, truck, van
    ,ppjtwmode.2=CDT_i$cd_total[CDT_i$cd_catid==1933] #public transit
    ,ppjtwmode.3=sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(1934,1935)]) #active mode
    ,ppjtwmode.4=CDT_i$cd_total[CDT_i$cd_catid==1936] #other method
    ,ppjtwmode.99=CDT_i$cd_total[CDT_i$cd_catid==1] - sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(1931:1936)]) #not applicable
    
    ,ppjtwdur.1=CDT_i$cd_total[CDT_i$cd_catid==1938] #less than 15 minutes
    ,ppjtwdur.2=CDT_i$cd_total[CDT_i$cd_catid==1939] #15 to 29 minutes
    ,ppjtwdur.3=CDT_i$cd_total[CDT_i$cd_catid==1940] #30 to 44 minutes
    ,ppjtwdur.4=CDT_i$cd_total[CDT_i$cd_catid==1941] #45 to 59 minutes
    ,ppjtwdur.5=CDT_i$cd_total[CDT_i$cd_catid==1942] #60 minutes and over
    ,ppjtwdur.99=CDT_i$cd_total[CDT_i$cd_catid==1] - sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(1938:1942)]) #not applicable
    
    ,ppjtwleave.1=CDT_i$cd_total[CDT_i$cd_catid==1944] #between 5 a.m. and 5:59 a.m.
    ,ppjtwleave.2=CDT_i$cd_total[CDT_i$cd_catid==1945] #between 6 a.m. and 6:59 a.m.
    ,ppjtwleave.3=CDT_i$cd_total[CDT_i$cd_catid==1946] #between 7 a.m. and 7:59 a.m.
    ,ppjtwleave.4=CDT_i$cd_total[CDT_i$cd_catid==1947] #between 8 a.m. and 8:59 a.m.
    ,ppjtwleave.5=sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(1948,1949)]) #between 9 a.m. and 4:59 a.m.
    ,ppjtwleave.99=CDT_i$cd_total[CDT_i$cd_catid==1] - sum(CDT_i$cd_total[CDT_i$cd_catid%in%c(1944:1949)]) #not applicable
  )
  result<-rbind(result,temp)
}
CDT<-result
#Harmonize CDT
variables<-unique(gsub("\\..*","",names(CDT)))
hhvariables<-variables[grepl("hh",variables)]
ppvariables<-variables[grepl("pp",variables)]
for (j in 1:length(hhvariables)){
  temp<-CDT[1,CDT[1,names(CDT)[grepl(hhvariables[j],names(CDT))]],with=FALSE]*as.numeric(CDT[1,CDT[1,names(CDT)[grepl(hhvariables[1],names(CDT))]],with=FALSE])/sum(CDT[1,CDT[1,names(CDT)[grepl(hhvariables[j],names(CDT))]],with=FALSE])
  rem<-sum(temp-floor(temp))
  if (rem>0){
    temp[,names(sort(temp,decreasing = TRUE))[1:rem]]<-floor(temp[,names(sort(temp,decreasing = TRUE))[1:rem],with=FALSE])+1
    temp<-floor(temp)
    }
  CDT[1,CDT[1,names(CDT)[grepl(hhvariables[j],names(CDT))]]]<-temp
}
for (j in 1:length(ppvariables)){
  temp<-CDT[1,CDT[1,names(CDT)[grepl(ppvariables[j],names(CDT))]],with=FALSE]*as.numeric(CDT[1,CDT[1,names(CDT)[grepl(ppvariables[1],names(CDT))]],with=FALSE])/sum(CDT[1,CDT[1,names(CDT)[grepl(ppvariables[j],names(CDT))]],with=FALSE])
  rem<-sum(temp-floor(temp))
  if (rem>0){
    temp[,names(sort(temp,decreasing = TRUE))[1:rem]]<-floor(temp[,names(sort(temp,decreasing = TRUE))[1:rem],with=FALSE])+1
    temp<-floor(temp)
  }
  CDT[1,CDT[1,names(CDT)[grepl(ppvariables[j],names(CDT))]]]<-temp
}
# write.csv(CDT,"Output/hCDT.csv")

#Test
# for (j in 1:length(hhvariables)){
#   print(sum(CDT[1,CDT[1,names(CDT)[grepl(hhvariables[j],names(CDT))]],with=FALSE])-CDT[1,CDT[1,names(CDT)[grepl(hhvariables[1],names(CDT))]],with=FALSE])
# }
# 
# for (j in 1:length(ppvariables)){
#   print(sum(CDT[1,CDT[1,names(CDT)[grepl(ppvariables[j],names(CDT))]],with=FALSE])-CDT[1,CDT[1,names(CDT)[grepl(ppvariables[1],names(CDT))]],with=FALSE])
# }



###DAT
#Filter to control variables
result<-data.table()
for (i in unique(DAT$da_id)){
  DAT_i<-DAT[DAT$da_id==i]
  
  temp<-data.table(
    geo=unique(DAT_i$da_id)
    
    ,hhcount=DAT_i$da_total[DAT_i$da_catid==5]
    
    ,hhsize.1=DAT_i$da_total[DAT_i$da_catid==52] #1 person
    ,hhsize.2=DAT_i$da_total[DAT_i$da_catid==53] #2 people
    ,hhsize.3=DAT_i$da_total[DAT_i$da_catid==54] #3 people
    ,hhsize.4=DAT_i$da_total[DAT_i$da_catid==55] #4 people
    ,hhsize.5=DAT_i$da_total[DAT_i$da_catid==56] #>=5 people
    
    ,hhtype.1=DAT_i$da_total[DAT_i$da_catid==97] #non-census family
    ,hhtype.2=DAT_i$da_total[DAT_i$da_catid==94] #census family without children
    ,hhtype.3=DAT_i$da_total[DAT_i$da_catid==95] #census family with children
    ,hhtype.4=DAT_i$da_total[DAT_i$da_catid==96] #multiple census families
    
    ,hhnetinc.1=sum(DAT_i$da_total[DAT_i$da_catid%in%c(781:786)]) #<30k$
    ,hhnetinc.2=sum(DAT_i$da_total[DAT_i$da_catid%in%c(787:791)]) #[30k$,60k$[
    ,hhnetinc.3=sum(DAT_i$da_total[DAT_i$da_catid%in%c(792:795)]) #[60k$,100k$[
    ,hhnetinc.4=DAT_i$da_total[DAT_i$da_catid==796] #>=100k$
    
    ,hhdtype.1=sum(DAT_i$da_total[DAT_i$da_catid%in%c(42)]) #single-detached house (detached)
    ,hhdtype.2=sum(DAT_i$da_total[DAT_i$da_catid%in%c(43,47,48)]) #apartment (>=5 storeys, duplex, <5 storeys)
    ,hhdtype.3=sum(DAT_i$da_total[DAT_i$da_catid%in%c(45,46,49,50)]) #other (semi-detached, row, other single-attached, movable dwelling)
    
    ,hhdcondo.1=DAT_i$da_total[DAT_i$da_catid==1622] #condominium
    ,hhdcondo.2=DAT_i$da_total[DAT_i$da_catid==1623] #not condominium
    
    ,hhdnbedrm.1=DAT_i$da_total[DAT_i$da_catid==1625] #0 bedrm
    ,hhdnbedrm.2=DAT_i$da_total[DAT_i$da_catid==1626] #1 bedrm
    ,hhdnbedrm.3=DAT_i$da_total[DAT_i$da_catid==1627] #2 bedrm
    ,hhdnbedrm.4=DAT_i$da_total[DAT_i$da_catid==1628] #3 bedrm
    ,hhdnbedrm.5=DAT_i$da_total[DAT_i$da_catid==1629] #>=4 bedrm
    
    ,hhdbuilt.1=DAT_i$da_total[DAT_i$da_catid==1644] #<=1960
    ,hhdbuilt.2=DAT_i$da_total[DAT_i$da_catid==1645] #[1961,1980]
    ,hhdbuilt.3=sum(DAT_i$da_total[DAT_i$da_catid%in%c(1646,1647)]) #[1981,2000]
    ,hhdbuilt.4=sum(DAT_i$da_total[DAT_i$da_catid%in%c(1648,1649,1650)]) #[2001,2016]
    
    ,ppcount=DAT_i$da_total[DAT_i$da_catid==1]
    
    ,ppsex.1=DAT_i$da_men[DAT_i$da_catid==8] #men
    ,ppsex.2=DAT_i$da_women[DAT_i$da_catid==8] #women
    
    ,ppagegrp.1=DAT_i$da_total[DAT_i$da_catid==9] #0-14 yo
    ,ppagegrp.2=DAT_i$da_total[DAT_i$da_catid==13] #15-64 yo
    ,ppagegrp.3=DAT_i$da_total[DAT_i$da_catid==24] #65+ yo
    
    ,ppmarst.1=DAT_i$da_total[DAT_i$da_catid==1] - sum(DAT_i$da_total[DAT_i$da_catid %in%c(61,62,65,66,67)]) #never legally married (and not living in common law)
    ,ppmarst.2=DAT_i$da_total[DAT_i$da_catid==61] #legally married (and not separated)
    ,ppmarst.3=DAT_i$da_total[DAT_i$da_catid==62] #living common law
    ,ppmarst.4=sum(DAT_i$da_total[DAT_i$da_catid%in%c(65,66,67)]) #separated, divorced, or widowed (and not living in common law)
    
    ,pplfst.1=DAT_i$da_total[DAT_i$da_catid==1867] #employed
    ,pplfst.2=DAT_i$da_total[DAT_i$da_catid==1868] #unemployed
    ,pplfst.3=sum(DAT_i$da_total[DAT_i$da_catid%in%c(9,1869)]) #not in the labour force
    
    ,ppind.1=sum(DAT_i$da_total[DAT_i$da_catid%in%c(1900,1901)]) #agriculture, forestry, fishing and hunting - mining, quarrying, and oil and gas extraction
    ,ppind.2=sum(DAT_i$da_total[DAT_i$da_catid%in%c(1902,1903)]) #utilities - construction
    ,ppind.3=DAT_i$da_total[DAT_i$da_catid==1904] #manufacturing
    ,ppind.4=sum(DAT_i$da_total[DAT_i$da_catid%in%c(1905,1906,1907)]) #wholesale and retail trade
    ,ppind.5=sum(DAT_i$da_total[DAT_i$da_catid%in%c(1908:1913)]) #finance and real estate
    ,ppind.6=DAT_i$da_total[DAT_i$da_catid==1915] #health care and social assistance
    ,ppind.7=DAT_i$da_total[DAT_i$da_catid==1914] #educational services
    ,ppind.8=sum(DAT_i$da_total[DAT_i$da_catid%in%c(1916,1917)]) #business services
    ,ppind.9=DAT_i$da_total[DAT_i$da_catid==1919] #public administration
    ,ppind.10=DAT_i$da_total[DAT_i$da_catid==1918] #other services
    ,ppind.99=DAT_i$da_total[DAT_i$da_catid==1] - DAT_i$da_total[DAT_i$da_catid==1899] #not applicable
    
    ,ppjtwmode.1=sum(DAT_i$da_total[DAT_i$da_catid%in%c(1931,1932)]) #car, truck, van
    ,ppjtwmode.2=DAT_i$da_total[DAT_i$da_catid==1933] #public transit
    ,ppjtwmode.3=sum(DAT_i$da_total[DAT_i$da_catid%in%c(1934,1935)]) #active mode
    ,ppjtwmode.4=DAT_i$da_total[DAT_i$da_catid==1936] #other method
    ,ppjtwmode.99=DAT_i$da_total[DAT_i$da_catid==1] - sum(DAT_i$da_total[DAT_i$da_catid%in%c(1931:1936)]) #not applicable
    
    ,ppjtwdur.1=DAT_i$da_total[DAT_i$da_catid==1938] #less than 15 minutes
    ,ppjtwdur.2=DAT_i$da_total[DAT_i$da_catid==1939] #15 to 29 minutes
    ,ppjtwdur.3=DAT_i$da_total[DAT_i$da_catid==1940] #30 to 44 minutes
    ,ppjtwdur.4=DAT_i$da_total[DAT_i$da_catid==1941] #45 to 59 minutes
    ,ppjtwdur.5=DAT_i$da_total[DAT_i$da_catid==1942] #60 minutes and over
    ,ppjtwdur.99=DAT_i$da_total[DAT_i$da_catid==1] - sum(DAT_i$da_total[DAT_i$da_catid%in%c(1938:1942)]) #not applicable
    
    ,ppjtwleave.1=DAT_i$da_total[DAT_i$da_catid==1944] #between 5 a.m. and 5:59 a.m.
    ,ppjtwleave.2=DAT_i$da_total[DAT_i$da_catid==1945] #between 6 a.m. and 6:59 a.m.
    ,ppjtwleave.3=DAT_i$da_total[DAT_i$da_catid==1946] #between 7 a.m. and 7:59 a.m.
    ,ppjtwleave.4=DAT_i$da_total[DAT_i$da_catid==1947] #between 8 a.m. and 8:59 a.m.
    ,ppjtwleave.5=sum(DAT_i$da_total[DAT_i$da_catid%in%c(1948,1949)]) #between 9 a.m. and 4:59 a.m.
    ,ppjtwleave.99=DAT_i$da_total[DAT_i$da_catid==1] - sum(DAT_i$da_total[DAT_i$da_catid%in%c(1944:1949)]) #not applicable
  )
  result<-rbind(result,temp)
}
DAT<-result

##Impute values for remaining 0s in DAMAT
for (i in 1:length(DAT$geo)){
  for (j in 1:length(hhvariables)){
    print(paste(i,",",j))
    temp<-DAT[i,DAT[i,names(DAT)[grepl(hhvariables[j],names(DAT))]],with=FALSE]
    if(sum(temp)==0){
      DAT[i,names(temp)]<-(CDT[,names(temp),with=FALSE]-colSums(DAT[,names(temp),with=FALSE]))*as.numeric(DAT[i,hhvariables[1],with=FALSE])/as.numeric(sum(DAT[,hhvariables[1],with=FALSE]))
    }
  }
}

eps<-0.00001
DAT<-as.data.frame(DAT)
DAT[DAT<=0]<-eps #Tweak remaining 0s
DAT<-as.data.table(DAT)

##Harmonize hhcount and ppcount between DAT and CDT
DAT$hhcount<-DAT$hhcount*CDT$hhcount/sum(DAT$hhcount)
DAT$ppcount<-DAT$ppcount*CDT$ppcount/sum(DAT$ppcount)
#Integerization of hhcount and ppcount
DAT$hhcount[rank(DAT$hhcount-floor(DAT$hhcount), ties.method = "first")%in%(length(DAT$hhcount)):(length(DAT$hhcount)+1-(sum(DAT$hhcount)-sum(floor(DAT$hhcount))))]<-floor(DAT$hhcount[rank(DAT$hhcount-floor(DAT$hhcount), ties.method = "first")%in%(length(DAT$hhcount)):(length(DAT$hhcount)+1-(sum(DAT$hhcount)-sum(floor(DAT$hhcount))))])+1
DAT$hhcount<-floor(DAT$hhcount)
DAT$ppcount[rank(DAT$ppcount-floor(DAT$ppcount), ties.method = "first")%in%(length(DAT$ppcount)):(length(DAT$ppcount)+1-(sum(DAT$ppcount)-sum(floor(DAT$ppcount))))]<-floor(DAT$ppcount[rank(DAT$ppcount-floor(DAT$ppcount), ties.method = "first")%in%(length(DAT$ppcount)):(length(DAT$ppcount)+1-(sum(DAT$ppcount)-sum(floor(DAT$ppcount))))])+1
DAT$ppcount<-floor(DAT$ppcount)


variables<-unique(gsub("\\..*","",names(DAT)))
hhvariables<-variables[grepl("hh",variables)]
ppvariables<-variables[grepl("pp",variables)]
DAT_temp<-DAT
##Harmonize households variables
#Intra-stratum discrepancies
discrepancy<-c()
for (j in 1:length(hhvariables)){
  temp<-max(abs(DAT$hhcount-rowSums(DAT[,names(DAT)[grepl(hhvariables[j],names(DAT))],with=FALSE])))
  discrepancy<-append(discrepancy,temp)
}
print(max(discrepancy))
#IPF
while (max(discrepancy)>eps){
  #Intra-stratum harmonization
  for (i in 1:length(DAT$geo)){
    for (j in 1:length(hhvariables)){
      temp<-DAT_temp[i,names(DAT_temp)[grepl(hhvariables[j],names(DAT_temp))],with=FALSE]*as.numeric(DAT_temp[i,hhvariables[1],with=FALSE])/sum(DAT_temp[i,names(DAT_temp)[grepl(hhvariables[j],names(DAT_temp))],with=FALSE])
      DAT[i,DAT[i,names(DAT)[grepl(hhvariables[j],names(DAT))]]]<-temp
    }
  }
  DAT_temp<-DAT
  #Inter-stratum harmonization
  for (i in 1:length(DAT$geo)){
    for (j in 1:length(hhvariables)){
      temp<-DAT_temp[i,names(DAT_temp)[grepl(hhvariables[j],names(DAT_temp))],with=FALSE]*CDT[,names(CDT)[grepl(hhvariables[j],names(CDT))],with=FALSE]/colSums(DAT_temp[,names(DAT_temp)[grepl(hhvariables[j],names(DAT_temp))],with=FALSE])
      DAT[i,DAT[i,names(DAT)[grepl(hhvariables[j],names(DAT))]]]<-temp
    }
  }
  DAT_temp<-DAT
  #Intra-stratum discrepancies
  discrepancy<-c()
  for (j in 1:length(hhvariables)){
    temp<-max(abs(DAT$hhcount-rowSums(DAT[,names(DAT)[grepl(hhvariables[j],names(DAT))],with=FALSE])))
    discrepancy<-append(discrepancy,temp)
  }
  print(max(discrepancy))
}

##Harmonize people variables
#Intra-stratum discrepancies
discrepancy<-c()
for (j in 1:length(ppvariables)){
  temp<-max(abs(DAT$ppcount-rowSums(DAT[,names(DAT)[grepl(ppvariables[j],names(DAT))],with=FALSE])))
  discrepancy<-append(discrepancy,temp)
}
print(max(discrepancy))
#IPF
while (max(discrepancy)>eps){
  #Intra-stratum harmonization
  for (i in 1:length(DAT$geo)){
    for (j in 1:length(ppvariables)){
      temp<-DAT_temp[i,names(DAT_temp)[grepl(ppvariables[j],names(DAT_temp))],with=FALSE]*as.numeric(DAT_temp[i,ppvariables[1],with=FALSE])/sum(DAT_temp[i,names(DAT_temp)[grepl(ppvariables[j],names(DAT_temp))],with=FALSE])
      DAT[i,DAT[i,names(DAT)[grepl(ppvariables[j],names(DAT))]]]<-temp
    }
  }
  DAT_temp<-DAT
  #Inter-stratum harmonization
  for (i in 1:length(DAT$geo)){
    for (j in 1:length(ppvariables)){
      temp<-DAT_temp[i,names(DAT_temp)[grepl(ppvariables[j],names(DAT_temp))],with=FALSE]*CDT[,names(CDT)[grepl(ppvariables[j],names(CDT))],with=FALSE]/colSums(DAT_temp[,names(DAT_temp)[grepl(ppvariables[j],names(DAT_temp))],with=FALSE])
      DAT[i,DAT[i,names(DAT)[grepl(ppvariables[j],names(DAT))]]]<-temp
    }
  }
  DAT_temp<-DAT
  #Intra-stratum discrepancies
  discrepancy<-c()
  for (j in 1:length(ppvariables)){
    temp<-max(abs(DAT$ppcount-rowSums(DAT[,names(DAT)[grepl(ppvariables[j],names(DAT))],with=FALSE])))
    discrepancy<-append(discrepancy,temp)
  }
  print(max(discrepancy))
}
##DAT integerization
DAT_rem<-DAT-floor(DAT)
DAT<-floor(DAT)
for (i in 1:length(DAT$geo)){
  for (j in 1:length(hhvariables)){
    print(paste(i,",",j))
    temp<-data.frame(DAT[i,names(DAT)[grepl(hhvariables[j],names(DAT))],with=FALSE])
    temp_rem<-data.frame(DAT_rem[i,names(DAT)[grepl(hhvariables[j],names(DAT))],with=FALSE])
    temp_intrarem<-DAT[i,names(DAT)[grepl(hhvariables[1],names(DAT))],with=FALSE]-sum(temp)
    temp_interrem<-data.frame(CDT[,names(CDT)[grepl(hhvariables[j],names(CDT))],with=FALSE]-colSums(DAT[,names(DAT)[grepl(hhvariables[j],names(DAT))],with=FALSE]))
    while(temp_intrarem!=0){
      temp[,temp_interrem>0][rank(temp_rem[,temp_interrem>0],ties.method = "first")%in%(length(temp[,temp_interrem>0]):(length(temp[,temp_interrem>0])+1-min(temp_intrarem,length(temp[,temp_interrem>0]))))]<-
        temp[,temp_interrem>0][rank(temp_rem[,temp_interrem>0],ties.method = "first")%in%(length(temp[,temp_interrem>0]):(length(temp[,temp_interrem>0])+1-min(temp_intrarem,length(temp[,temp_interrem>0]))))] + 1
      DAT[i,names(DAT)[grepl(hhvariables[j],names(DAT))]]<-temp
      temp_intrarem<-DAT[i,names(DAT)[grepl(hhvariables[1],names(DAT))],with=FALSE]-sum(temp)
      temp_interrem<-data.frame(CDT[,names(CDT)[grepl(hhvariables[j],names(CDT))],with=FALSE]-colSums(DAT[,names(DAT)[grepl(hhvariables[j],names(DAT))],with=FALSE]))
    }
  }
}
for (i in 1:length(DAT$geo)){
  for (j in 1:length(ppvariables)){
    print(paste(i,",",j))
    temp<-data.frame(DAT[i,names(DAT)[grepl(ppvariables[j],names(DAT))],with=FALSE])
    temp_rem<-data.frame(DAT_rem[i,names(DAT)[grepl(ppvariables[j],names(DAT))],with=FALSE])
    temp_intrarem<-DAT[i,names(DAT)[grepl(ppvariables[1],names(DAT))],with=FALSE]-sum(temp)
    temp_interrem<-data.frame(CDT[,names(CDT)[grepl(ppvariables[j],names(CDT))],with=FALSE]-colSums(DAT[,names(DAT)[grepl(ppvariables[j],names(DAT))],with=FALSE]))
    while(temp_intrarem!=0){
      temp[,temp_interrem>0][rank(temp_rem[,temp_interrem>0],ties.method = "first")%in%(length(temp[,temp_interrem>0]):(length(temp[,temp_interrem>0])+1-min(temp_intrarem,length(temp[,temp_interrem>0]))))]<-
        temp[,temp_interrem>0][rank(temp_rem[,temp_interrem>0],ties.method = "first")%in%(length(temp[,temp_interrem>0]):(length(temp[,temp_interrem>0])+1-min(temp_intrarem,length(temp[,temp_interrem>0]))))] + 1
      DAT[i,names(DAT)[grepl(ppvariables[j],names(DAT))]]<-temp
      temp_intrarem<-DAT[i,names(DAT)[grepl(ppvariables[1],names(DAT))],with=FALSE]-sum(temp)
      temp_interrem<-data.frame(CDT[,names(CDT)[grepl(ppvariables[j],names(CDT))],with=FALSE]-colSums(DAT[,names(DAT)[grepl(ppvariables[j],names(DAT))],with=FALSE]))
    }
  }
}
# write.csv(DAT,"Output/hDAT.csv")



###DBT
#Control variables
result<-data.table()
for (i in as.numeric(DBT$db_id)){
  print(i)
  DBT_i<-DBT[DBT$db_id==i]
  
  temp<-data.table(
    geo=unique(DBT_i$db_id)
    
    ,hhcount=DBT_i$db_urdwell
    
    # ,hhdcondo.1= sum(BT$bld_dwellings[BT$bld_id%in%GIS$bld_id[GIS$db_id%in%DBT_i$db_id] & BT$bld_type=="Condominium"])*(DBT_i$db_urdwell/DBT_i$db_dwell) #condominium
    # ,hhdcondo.2= sum(BT$bld_dwellings[BT$bld_id%in%GIS$bld_id[GIS$db_id%in%DBT_i$db_id] & BT$bld_type=="Regular"])*(DBT_i$db_urdwell/DBT_i$db_dwell) #not condominium
    # 
    # ,hhdbuilt.1= sum(BT$bld_dwellings[BT$bld_id%in%GIS$bld_id[GIS$db_id%in%DBT_i$db_id] & BT$bld_consyr<=1960])*(DBT_i$db_urdwell/DBT_i$db_dwell) #<=1960
    # ,hhdbuilt.2= sum(BT$bld_dwellings[BT$bld_id%in%GIS$bld_id[GIS$db_id%in%DBT_i$db_id] & BT$bld_consyr>=1961 & BT$bld_consyr<=1980])*(DBT_i$db_urdwell/DBT_i$db_dwell) #[1961,1980]
    # ,hhdbuilt.3= sum(BT$bld_dwellings[BT$bld_id%in%GIS$bld_id[GIS$db_id%in%DBT_i$db_id] & BT$bld_consyr>=1981 & BT$bld_consyr<=2000])*(DBT_i$db_urdwell/DBT_i$db_dwell) #[1981,2000]
    # ,hhdbuilt.4= sum(BT$bld_dwellings[BT$bld_id%in%GIS$bld_id[GIS$db_id%in%DBT_i$db_id] & BT$bld_consyr>=2001 & BT$bld_consyr<=2016])*(DBT_i$db_urdwell/DBT_i$db_dwell) #[2001,2016]
        
    ,ppcount=DBT_i$db_pop
  )
  result<-rbind(result,temp)
}
DBT<-result

eps<-0.00001
DBT<-as.data.frame(DBT)
DBT[DBT<=0]<-eps #Tweak remaining 0s
DBT<-as.data.table(DBT)
##Harmonize hhcount and ppcount between DBT and DAT
DBT$hhcount<-as.numeric(DBT$hhcount)
DBT$ppcount<-as.numeric(DBT$ppcount)
for (i in unique(DAT$geo)){
  DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount<-DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount*DAT[DAT$geo==i]$hhcount/sum(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount)
  DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount<-DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount*DAT[DAT$geo==i]$ppcount/sum(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount)

  if (DAT$hhcount[DAT$geo==i]-sum(floor(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount))!=0){
    DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount[rank(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount-floor(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount), ties.method = "first")%in%(length(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount):(length(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount)+1-(sum(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount)-sum(floor(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount)))))]<-floor(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount[rank(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount-floor(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount), ties.method = "first")%in%(length(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount)):(length(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount)+1-(sum(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount)-sum(floor(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount))))])+1
    DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount<-floor(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount)
  }
  
  if (DAT$ppcount[DAT$geo==i]-sum(floor(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount))!=0){
    DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount[rank(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount-floor(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount), ties.method = "first")%in%(length(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount):(length(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount)+1-(sum(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount)-sum(floor(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount)))))]<-floor(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount[rank(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount-floor(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount), ties.method = "first")%in%(length(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount)):(length(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount)+1-(sum(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount)-sum(floor(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount))))])+1
    DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount<-floor(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount)
  }
  #Test
  if(DAT$hhcount[DAT$geo==i]-sum(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$hhcount)!=0){
    print(i)
  }
  if(DAT$ppcount[DAT$geo==i]-sum(DBT[DBT$geo%in%GIS$db_id[GIS$da_id==i]]$ppcount)!=0){
    print(i)
  }
}
# #Test
# for (k in unique(DAT$geo)){
#   a<-DAT$hhcount[DAT$geo==k]-sum(DBT$hhcount[DBT$geo%in%unique(GIS$db_id[GIS$da_id==k])])
#   b<-DAT$ppcount[DAT$geo==k]-sum(DBT$ppcount[DBT$geo%in%unique(GIS$db_id[GIS$da_id==k])])
#   if(a>0 | b>0){
#     print(k)
#   }
# }

# ##Harmonize households variables
# setorder(DBT, geo)
# variables<-unique(gsub("\\..*","",names(DBT)))
# hhvariables<-variables[grepl("hh",variables)]
# ppvariables<-variables[grepl("pp",variables)]
# DBT<-data.frame(DBT)
# #Intra-stratum discrepancies
# discrepancy<-c()
# for (j in 2:length(hhvariables)){
#   discrepancy<-c(discrepancy,max(abs(DBT[,names(DBT)[grepl(hhvariables[1],names(DBT))]]-rowSums(DBT[,names(DBT)[grepl(hhvariables[j],names(DBT))]]))))
# }
# print(max(discrepancy))
# 
# while(max(discrepancy)>eps*100){
#   #Intra-stratum harmonization
#   for (j in 2:length(hhvariables)){
#     temp<-DBT[,names(DBT)[grepl(hhvariables[j],names(DBT))]]*unlist(DBT[,names(DBT)[grepl(hhvariables[1],names(DBT))]]/rowSums(DBT[,names(DBT)[grepl(hhvariables[j],names(DBT))]]))
#     DBT[,names(DBT)[grepl(hhvariables[j],names(DBT))]]<-temp
#   }
#   
#   #Inter-stratum harmonization
#   for (j in names(DBT)[!(grepl("count",names(DBT)) |  grepl("geo",names(DBT)))]){
#     temp<-merge(DBT,unique(data.table(db_id=GIS$db_id,da_id=GIS$da_id)),by.x="geo",by.y="db_id")
#     temp<-merge(temp,aggregate(data=temp, eval(parse(text=j)) ~ da_id, function(x) sum(x)),by.x="da_id",by.y="da_id")
#     temp<-merge(temp,DAT[,names(DAT)%in%c("geo",j),with=FALSE],by.x="da_id",by.y="geo")
#     setorder(temp, geo)
#     DBT[,names(DBT)[grepl(j,names(DBT))]][temp[,names(temp)[grepl("eval",names(temp))]]!=0]<-temp[,names(temp)[grepl(j,names(temp))][1]][temp[,names(temp)[grepl("eval",names(temp))]]!=0]*temp[,names(temp)[grepl(j,names(temp))][2]][temp[,names(temp)[grepl("eval",names(temp))]]!=0]/temp[,names(temp)[grepl("eval",names(temp))]][temp[,names(temp)[grepl("eval",names(temp))]]!=0]
#   }
#   
#   # #Inter-stratum discrepancies
#   # for (j in names(DBT)[!(grepl("count",names(DBT)) |  grepl("geo",names(DBT)))]){
#   #   temp<-merge(DBT,unique(data.table(db_id=GIS$db_id,da_id=GIS$da_id)),by.x="geo",by.y="db_id")
#   #   temp<-merge(temp,aggregate(data=temp, eval(parse(text=j)) ~ da_id, function(x) sum(x)),by="da_id")
#   #   temp<-merge(temp,DAT[,names(DAT)%in%c("geo",j),with=FALSE],by.x="da_id",by.y="geo")
#   #   print(max(abs(temp[,names(temp)[grepl(j,names(temp))][2]]-temp[,names(temp)[grepl("eval",names(temp))]])))
#   # }
#   
#   discrepancy<-c()
#   for (j in 2:length(hhvariables)){
#     discrepancy<-c(discrepancy,max(abs(DBT[,names(DBT)[grepl(hhvariables[1],names(DBT))]]-rowSums(DBT[,names(DBT)[grepl(hhvariables[j],names(DBT))]]))))
#   }
#   print(max(discrepancy))
# }
# 
# ##DBT integerization (households only as we only have ppcount for people)
# DBT<-data.table(DBT)
# for (k in unique(DAT$geo)){
#   
#   print(k)
#   
#   DAT_i<-DAT[DAT$geo==k]
#   DA_i_DBT<-DBT[DBT$geo%in%GIS$db_id[GIS$da_id==k]]
#   DA_i_DBT_temp<-DA_i_DBT
#   DA_i_DBT_rem<-DA_i_DBT-floor(DA_i_DBT)
#   DA_i_DBT<-floor(DA_i_DBT)
#   
#   for (i in 1:length(DA_i_DBT$geo)){
#     for (j in 1:length(hhvariables)){
#       print(paste(i,",",j))
#       temp<-data.frame(DA_i_DBT[i,names(DA_i_DBT)[grepl(hhvariables[j],names(DA_i_DBT))],with=FALSE])
#       temp_rem<-data.frame(DA_i_DBT_rem[i,names(DA_i_DBT)[grepl(hhvariables[j],names(DA_i_DBT))],with=FALSE])
#       temp_intrarem<-DA_i_DBT[i,names(DA_i_DBT)[grepl(hhvariables[1],names(DA_i_DBT))],with=FALSE]-sum(temp)
#       temp_interrem<-data.frame(DAT_i[,names(DAT_i)[grepl(hhvariables[j],names(DAT_i))],with=FALSE]-colSums(DA_i_DBT[,names(DA_i_DBT)[grepl(hhvariables[j],names(DA_i_DBT))],with=FALSE]))
#       
#       while(temp_intrarem!=0){
#         temp[,temp_interrem>0][rank(temp_rem[,temp_interrem>0],ties.method = "first")%in%(length(temp[,temp_interrem>0]):(length(temp[,temp_interrem>0])+1-min(temp_intrarem,length(temp[,temp_interrem>0]))))]<-
#           temp[,temp_interrem>0][rank(temp_rem[,temp_interrem>0],ties.method = "first")%in%(length(temp[,temp_interrem>0]):(length(temp[,temp_interrem>0])+1-min(temp_intrarem,length(temp[,temp_interrem>0]))))] + 1
#         DA_i_DBT[i,names(DA_i_DBT)[grepl(hhvariables[j],names(DA_i_DBT))]]<-temp
#         temp_intrarem<-DA_i_DBT[i,names(DA_i_DBT)[grepl(hhvariables[1],names(DA_i_DBT))],with=FALSE]-sum(temp)
#         temp_interrem<-data.frame(DAT_i[,names(DAT_i)[grepl(hhvariables[j],names(DAT_i))],with=FALSE]-colSums(DA_i_DBT[,names(DA_i_DBT)[grepl(hhvariables[j],names(DA_i_DBT))],with=FALSE]))
#       }
#     }
#   }
#   DBT[DBT$geo%in%GIS$db_id[GIS$da_id==k]]<-DA_i_DBT
# }

# #Tests
# DBT<-data.frame(DBT)
# #Inter-stratum discrepancies
# for (j in names(DBT)[!(grepl("count",names(DBT)) |  grepl("geo",names(DBT)))]){
#   temp<-merge(DBT,unique(data.table(db_id=GIS$db_id,da_id=GIS$da_id)),by.x="geo",by.y="db_id")
#   temp<-merge(temp,aggregate(data=temp, eval(parse(text=j)) ~ da_id, function(x) sum(x)),by="da_id")
#   temp<-merge(temp,DAT[,names(DAT)%in%c("geo",j),with=FALSE],by.x="da_id",by.y="geo")
#   print(max(abs(temp[,names(temp)[grepl(j,names(temp))][2]]-temp[,names(temp)[grepl("eval",names(temp))]])))
# }
# #Intra-stratum discrepancies
# discrepancy<-c()
# for (j in 2:length(hhvariables)){
#   discrepancy<-c(discrepancy,max(abs(DBT[,names(DBT)[grepl(hhvariables[1],names(DBT))]]-rowSums(DBT[,names(DBT)[grepl(hhvariables[j],names(DBT))]]))))
# }
# print(max(discrepancy))

# write.csv(DBT,"Output/hDBT.csv")



###BT
GIS$bld_id<-as.numeric(GIS$bld_id)
GIS$db_id<-as.numeric(GIS$db_id)
GIS$bld_id[is.na(GIS$bld_id)]<-GIS$db_id[is.na(GIS$bld_id)]
#DB occupation rate
OR<-fread("Input/Census/DB/2016_92-151_XBB.csv") #Dissemination blocks
OR<-data.table(db_id=OR$`DBuid/IDidu`, db_pop=OR$`DBpop2016/IDpop2016`, db_dwell=OR$`DBtdwell2016/IDtlog2016`, db_urdwell=OR$`DBurdwell2016/IDrh2016`)
OR$db_id<-as.numeric(OR$db_id)
BT$db_id<-as.numeric(BT$db_id)
OR<-OR[OR$db_id%in%GIS$db_id,]
OR$db_or<-OR$db_urdwell/OR$db_dwell
OR<-OR[,c(1,5)]
BT<-merge(BT,OR,by="db_id")
BT$bld_hh<-BT$bld_dwellings*BT$db_or
#BT
temp<-data.table(geo=BT$bld_id,hhcount=0,hhdcondo.1=0,hhdcondo.2=0,hhdbuilt.1=0,hhdbuilt.2=0,hhdbuilt.3=0,hhdbuilt.4=0)
temp$hhcount<-BT$bld_hh
temp$hhdcondo.1[BT$bld_type==1]<-BT$bld_hh[BT$bld_type==1]
temp$hhdcondo.2[BT$bld_type==2]<-BT$bld_hh[BT$bld_type==2]
temp$hhdbuilt.1[BT$bld_consyr==1]<-BT$bld_hh[BT$bld_consyr==1]
temp$hhdbuilt.2[BT$bld_consyr==2]<-BT$bld_hh[BT$bld_consyr==2]
temp$hhdbuilt.3[BT$bld_consyr==3]<-BT$bld_hh[BT$bld_consyr==3]
temp$hhdbuilt.4[BT$bld_consyr==4]<-BT$bld_hh[BT$bld_consyr==4]
BT<-temp
#Harmonize hhcount between BT and DBT
DBT$geo<-as.numeric(DBT$geo)
BT$geo<-as.numeric(BT$geo)
temp<-merge(BT,GIS[,c("bld_id","db_id")],by.x="geo",by.y="bld_id",all.x=TRUE)
temp$db_id<-as.numeric(temp$db_id)
temp<-merge(temp,DBT[,c("geo","hhcount")],by.x="db_id",by.y="geo")
temp<-merge(temp,data.table(aggregate(data=temp, hhcount.x ~ db_id, function(x) sum(x))),by="db_id")
temp$hhcount.x.x<-temp$hhcount.x.x*temp$hhcount.y/temp$hhcount.x.y
BT$hhcount<-temp$hhcount.x.x
#Integerize BT hhcount
BT$hhcount<-as.numeric(BT$hhcount)
for (i in unique(DBT$geo)){
  if (DBT$hhcount[DBT$geo==i]-sum(floor(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount))!=0){
    BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount[rank(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount-floor(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount), ties.method = "first")%in%(length(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount):(length(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount)+1-(sum(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount)-sum(floor(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount)))))]<-floor(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount[rank(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount-floor(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount), ties.method = "first")%in%(length(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount)):(length(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount)+1-(sum(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount)-sum(floor(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount))))])+1
    BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount<-floor(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount)
  }else{
    BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount<-floor(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount)
  }
  
  #Test
  if(DBT$hhcount[DBT$geo==i]-sum(BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]$hhcount)!=0){
    print(i)
  }
}

# #Test
# for (k in unique(DBT$geo)){
#   a<-DBT$hhcount[DBT$geo==k]-sum(BT$hhcount[BT$geo%in%unique(GIS$bld_id[GIS$db_id==k])])
#   if(a>0){
#     print(k)
#   }
# }

eps<-0.00001
BT<-as.data.frame(BT)
BT[BT<=0]<-eps #Tweak remaining 0s
BT<-as.data.table(BT)

##Harmonize households variables
setorder(BT, geo)
variables<-unique(gsub("\\..*","",names(BT)))
hhvariables<-variables[grepl("hh",variables)]
BT<-data.frame(BT)

DBT$geo<-as.numeric(DBT$geo)

discrepancy<-c()
for (j in 2:length(hhvariables)){
  discrepancy<-c(discrepancy,max(abs(BT[,names(BT)[grepl(hhvariables[1],names(BT))]]-rowSums(BT[,names(BT)[grepl(hhvariables[j],names(BT))]]))))
}
print(max(discrepancy))

while(max(discrepancy)>eps){
  #Intra-stratum harmonization
  for (j in 2:length(hhvariables)){
    temp<-BT[,names(BT)[grepl(hhvariables[j],names(BT))]]*unlist(BT[,names(BT)[grepl(hhvariables[1],names(BT))]]/rowSums(BT[,names(BT)[grepl(hhvariables[j],names(BT))]]))
    BT[,names(BT)[grepl(hhvariables[j],names(BT))]]<-temp
  }
  
  #Inter-stratum harmonization
  for (j in names(BT)[!(grepl("count",names(BT)) |  grepl("geo",names(BT)))]){
    temp<-merge(BT,unique(data.table(bld_id=GIS$bld_id,da_id=GIS$da_id)),by.x="geo",by.y="bld_id")
    temp<-merge(temp,aggregate(data=temp, eval(parse(text=j)) ~ da_id, function(x) sum(x)),by.x="da_id",by.y="da_id")
    temp<-merge(temp,DAT[,names(DAT)%in%c("geo",j),with=FALSE],by.x="da_id",by.y="geo")
    setorder(temp, geo)
    BT[,names(BT)[grepl(j,names(BT))]][temp[,names(temp)[grepl("eval",names(temp))]]!=0]<-temp[,names(temp)[grepl(j,names(temp))][1]][temp[,names(temp)[grepl("eval",names(temp))]]!=0]*temp[,names(temp)[grepl(j,names(temp))][2]][temp[,names(temp)[grepl("eval",names(temp))]]!=0]/temp[,names(temp)[grepl("eval",names(temp))]][temp[,names(temp)[grepl("eval",names(temp))]]!=0]
  }
  
  # #Inter-stratum discrepancies
  # for (j in names(BT)[!(grepl("count",names(BT)) |  grepl("geo",names(BT)))]){
  #   temp<-merge(BT,unique(data.table(bld_id=GIS$bld_id,da_id=GIS$da_id)),by.x="geo",by.y="bld_id")
  #   temp<-merge(temp,aggregate(data=temp, eval(parse(text=j)) ~ da_id, function(x) sum(x)),by="da_id")
  #   temp<-merge(temp,DAT[,names(DAT)%in%c("geo",j),with=FALSE],by.x="da_id",by.y="geo")
  #   print(max(abs(temp[,names(temp)[grepl(j,names(temp))][2]]-temp[,names(temp)[grepl("eval",names(temp))]])))
  # }
  
  discrepancy<-c()
  for (j in 2:length(hhvariables)){
    discrepancy<-c(discrepancy,max(abs(BT[,names(BT)[grepl(hhvariables[1],names(BT))]]-rowSums(BT[,names(BT)[grepl(hhvariables[j],names(BT))]]))))
  }
  print(max(discrepancy))
}

##BT integerization (households only)
BT<-data.table(BT)
GIS$da_id<-as.numeric(GIS$da_id)

for (k in unique(DAT$geo)){
  
  DAT_i<-DAT[DAT$geo==k]
  DA_i_BT<-BT[BT$geo%in%GIS$bld_id[GIS$da_id==k]]
  DA_i_BT_temp<-DA_i_BT
  DA_i_BT_rem<-DA_i_BT-floor(DA_i_BT)
  DA_i_BT<-floor(DA_i_BT)
  
  for (i in 1:length(DA_i_BT$geo)){
    for (j in 1:length(hhvariables)){
      temp<-data.frame(DA_i_BT[i,names(DA_i_BT)[grepl(hhvariables[j],names(DA_i_BT))],with=FALSE])
      temp_rem<-data.frame(DA_i_BT_rem[i,names(DA_i_BT)[grepl(hhvariables[j],names(DA_i_BT))],with=FALSE])
      temp_intrarem<-DA_i_BT[i,names(DA_i_BT)[grepl(hhvariables[1],names(DA_i_BT))],with=FALSE]-sum(temp)
      temp_interrem<-data.frame(DAT_i[,names(DAT_i)[grepl(hhvariables[j],names(DAT_i))],with=FALSE]-colSums(DA_i_BT[,names(DA_i_BT)[grepl(hhvariables[j],names(DA_i_BT))],with=FALSE]))
      
      while(temp_intrarem!=0){
        temp[,temp_interrem>0][rank(temp_rem[,temp_interrem>0],ties.method = "first")%in%(length(temp[,temp_interrem>0]):(length(temp[,temp_interrem>0])+1-min(temp_intrarem,length(temp[,temp_interrem>0]))))]<-
          temp[,temp_interrem>0][rank(temp_rem[,temp_interrem>0],ties.method = "first")%in%(length(temp[,temp_interrem>0]):(length(temp[,temp_interrem>0])+1-min(temp_intrarem,length(temp[,temp_interrem>0]))))] + 1
        DA_i_BT[i,names(DA_i_BT)[grepl(hhvariables[j],names(DA_i_BT))]]<-temp
        temp_intrarem<-DA_i_BT[i,names(DA_i_BT)[grepl(hhvariables[1],names(DA_i_BT))],with=FALSE]-sum(temp)
        temp_interrem<-data.frame(DAT_i[,names(DAT_i)[grepl(hhvariables[j],names(DAT_i))],with=FALSE]-colSums(DA_i_BT[,names(DA_i_BT)[grepl(hhvariables[j],names(DA_i_BT))],with=FALSE]))
      }
    }
  }
  
  #Intra-stratum discrepancies
  DA_i_BT<-data.frame(DA_i_BT)
  discrepancy<-c()
  for (j in 2:length(hhvariables)){
    discrepancy<-c(discrepancy,max(abs(DA_i_BT[,names(DA_i_BT)[grepl(hhvariables[1],names(DA_i_BT))]]-rowSums(DA_i_BT[,names(DA_i_BT)[grepl(hhvariables[j],names(DA_i_BT))]]))))
  }
  #Inter-stratum discrepancies
  for (j in names(DA_i_BT)[!(grepl("count",names(DA_i_BT)) |  grepl("geo",names(DA_i_BT)))]){
    temp<-merge(DA_i_BT,unique(data.table(bld_id=GIS$bld_id,da_id=GIS$da_id)),by.x="geo",by.y="bld_id")
    temp<-merge(temp,aggregate(data=temp, eval(parse(text=j)) ~ da_id, function(x) sum(x)),by="da_id")
    temp<-merge(temp,DAT_i[,names(DAT_i)%in%c("geo",j),with=FALSE],by.x="da_id",by.y="geo")
    discrepancy<-c(discrepancy,(max(abs(temp[,names(temp)[grepl(j,names(temp))][2]]-temp[,names(temp)[grepl("eval",names(temp))]]))))
  }
  if(max(abs(discrepancy))!=0  | max(abs(DA_i_BT-floor(DA_i_BT)))!=0){
    print(paste(k,",",i,",",j))
  }
  BT[BT$geo%in%GIS$bld_id[GIS$da_id==k]]<-DA_i_BT
}

# write.csv(BT,"Output/hBT.csv")
# write.csv(GIS,"Output/hGIS.csv")


###hGIS_XY
GIS_XY<-fread("Output/hGIS.csv")
GIS_XY<-merge(GIS_XY,unique(Addresses16[,c("ID_ADRESSE","Latitude","Longitude")]),by.x="bld_id",by.y="ID_ADRESSE",all.x=TRUE)
names(GIS_XY)[ncol(GIS_XY)]<-"bld_lon"
names(GIS_XY)[ncol(GIS_XY)-1]<-"bld_lat"
temp<-unlist(GIS_XY[is.na(GIS_XY$bld_lat),"bld_id"])
for (i in temp){
  print(i)
  GIS_XY$bld_lat[GIS_XY$bld_id==as.numeric(i)]<-DBlatlon$db_lat[DBlatlon$db_id==as.numeric(i)]
  GIS_XY$bld_lon[GIS_XY$bld_id==as.numeric(i)]<-DBlatlon$db_lon[DBlatlon$db_id==as.numeric(i)]
}
GIS_XY<-data.table(bld_id=GIS_XY$bld_id,db_id=GIS_XY$db_id,da_id=GIS_XY$da_id,cd_id=GIS_XY$cd_id,bld_lat=GIS_XY$bld_lat,bld_lon=GIS_XY$bld_lon)
# write.csv(GIS_XY,"Output/hGIS_XY.csv")



#1 : Population 2016
#4 : Total private dwellings
#5 : Private dwellings occupied by usual residents
#8-33 : Age groups
#41-50 : Dwellings' structural types
#51-56 : Households' sizes
#57 : Number of persons in private households
#59-67 : Marital status
#92-99 : Households types
#112-380 : Mother tongue
#780-799 : Household after-tax income
#1624-1629 : Dwellings' number of bedrooms
#1630-1635 : Dwellings' number of rooms
#1643-1650 : Dwellings' construction period
#1865-1869 : Labour force status
#1897-1919 : Industry sector
#1930-1936 : JTW commuting mode
#1937-1942 : JTW commuting duration
#1943-1949 : JTW leaving time