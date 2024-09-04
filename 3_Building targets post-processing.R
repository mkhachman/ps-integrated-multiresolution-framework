rm(list=ls())
options(scipen = 10)

###Packages
library(data.table)
library(dplyr)
library(stringr)
library(gsubfn)

expand<-function(x){
  res<-c()
  for (j in unique(x$geo)){
    ERR_ij<-x[x$geo==j]
    temp<-ERR_ij[rep(1:.N,hhcount)]
    temp<-data.frame(temp)
    temp[,grepl("hh",names(temp))]<-0
    temp$hhcount<-1
    temp$hhdcondo.1[ERR_ij$hhdcondo.1!=0][1:ERR_ij$hhdcondo.1]<-1
    temp$hhdcondo.2[ERR_ij$hhdcondo.2!=0][(1+ERR_ij$hhdcondo.1):(ERR_ij$hhdcondo.1+ERR_ij$hhdcondo.2)]<-1
    temp$hhdbuilt.1[ERR_ij$hhdbuilt.1!=0][1:ERR_ij$hhdbuilt.1]<-1
    temp$hhdbuilt.2[ERR_ij$hhdbuilt.2!=0][(1+ERR_ij$hhdbuilt.1):(ERR_ij$hhdbuilt.1+ERR_ij$hhdbuilt.2)]<-1
    temp$hhdbuilt.3[ERR_ij$hhdbuilt.3!=0][(1+ERR_ij$hhdbuilt.1+ERR_ij$hhdbuilt.2):(ERR_ij$hhdbuilt.1+ERR_ij$hhdbuilt.2+ERR_ij$hhdbuilt.3)]<-1
    temp$hhdbuilt.4[ERR_ij$hhdbuilt.4!=0][(1+ERR_ij$hhdbuilt.1+ERR_ij$hhdbuilt.2+ERR_ij$hhdbuilt.3):(ERR_ij$hhdbuilt.1+ERR_ij$hhdbuilt.2+ERR_ij$hhdbuilt.3+ERR_ij$hhdbuilt.4)]<-1
    res<-rbind(res,temp)
  }
  return(res)
}

GIS<-fread("Output/hGIS.csv")
BT<-fread("Output/hBT.csv")
RES<-data.frame()



###Purify buildings within each DB
for (i in as.numeric(unique(GIS$db_id))){
  print(i)
  BT_i<-BT[BT$geo%in%GIS$bld_id[GIS$db_id==i]]
  ERR_i<-BT_i[(((BT_i$hhdcondo.1==0) + (BT_i$hhdcondo.2==0)) <1) | (((BT_i$hhdbuilt.1==0) + (BT_i$hhdbuilt.2==0) + (BT_i$hhdbuilt.3==0) + (BT_i$hhdbuilt.4==0))<3)]
  REM_i<-BT_i[!BT_i$geo%in%ERR_i$geo]
  REM_i<-data.frame(REM_i)


  if (nrow(ERR_i)==0){
    RES<-rbind(RES,BT_i)
  }else{
    ERR_i<-expand(ERR_i)
    ERR_i$type<-paste(ERR_i$hhdcondo.1,ERR_i$hhdcondo.2,ERR_i$hhdbuilt.1,ERR_i$hhdbuilt.2,ERR_i$hhdbuilt.3,ERR_i$hhdbuilt.4)
    temp<-REM_i
    temp[temp>1]<-1
    REM_i$type<-paste(temp$hhdcondo.1,temp$hhdcondo.2,temp$hhdbuilt.1,temp$hhdbuilt.2,temp$hhdbuilt.3,temp$hhdbuilt.4)


    ##Transfer lacking types in REM, from ERR to REM, each at the location where it is the most frequent (distinct locations as far as possible)
    temp1<-table(ERR_i[!(ERR_i$type%in%REM_i$type),"geo"],ERR_i[!(ERR_i$type%in%REM_i$type),"type"])
    if(nrow(temp1!=0)){
      temp1<-as.data.frame(temp1)
      temp1<-temp1[temp1$Freq!=0,]
      for (j in as.vector(arrange(aggregate(data=temp1,Freq~Var2,sum),Freq)$Var2)){
        if(length(temp1$Freq[!(as.numeric(as.vector(temp1$Var1))%in%REM_i$geo) & temp1$Var2==j])>0){
          temp2<-ERR_i[ERR_i$geo==as.numeric(as.vector(temp1$Var1[!(as.numeric(as.vector(temp1$Var1))%in%REM_i$geo) & temp1$Var2==j & temp1$Freq==max(temp1$Freq[!(as.numeric(as.vector(temp1$Var1))%in%REM_i$geo) & temp1$Var2==j])][1])) & ERR_i$type==j,]
        }else{
          temp2<-ERR_i[ERR_i$geo==as.numeric(as.vector(temp1$Var1[temp1$Var2==j & temp1$Freq==max(temp1$Freq[temp1$Var2==j])][1])) & ERR_i$type==j,]
        }
        temp2<-data.table(V1=unique(temp2$V1),
                          geo=unique(temp2$geo),
                          hhcount=sum(temp2$hhcount),
                          hhdcondo.1=sum(temp2$hhdcondo.1),
                          hhdcondo.2=sum(temp2$hhdcondo.2),
                          hhdbuilt.1=sum(temp2$hhdbuilt.1),
                          hhdbuilt.2=sum(temp2$hhdbuilt.2),
                          hhdbuilt.3=sum(temp2$hhdbuilt.3),
                          hhdbuilt.4=sum(temp2$hhdbuilt.4),
                          type=unique(temp2$type))
        REM_i<-data.frame(rbind(REM_i,temp2))
        ERR_i<-data.frame(ERR_i[!(ERR_i$geo==temp2$geo & ERR_i$type==temp2$type),])
      }
    }


    ##Transfer lacking geos in REM, from ERR to REM considering the most frequent type at the geo
    temp1<-table(ERR_i[!(ERR_i$geo%in%REM_i$geo),"type"],ERR_i[!(ERR_i$geo%in%REM_i$geo),"geo"])
    if (nrow(temp1)!=0){
      temp1<-as.data.frame(temp1)
      temp1<-temp1[temp1$Freq!=0,]
      for (j in unique(temp1$Var2)){
        temp2<-ERR_i[ERR_i$geo==j & ERR_i$type==as.vector(temp1$Var1[temp1$Var2==j & temp1$Freq==max(temp1$Freq[temp1$Var2==j])][1]),]
        temp2<-data.table(V1=unique(temp2$V1),
                          geo=unique(temp2$geo),
                          hhcount=sum(temp2$hhcount),
                          hhdcondo.1=sum(temp2$hhdcondo.1),
                          hhdcondo.2=sum(temp2$hhdcondo.2),
                          hhdbuilt.1=sum(temp2$hhdbuilt.1),
                          hhdbuilt.2=sum(temp2$hhdbuilt.2),
                          hhdbuilt.3=sum(temp2$hhdbuilt.3),
                          hhdbuilt.4=sum(temp2$hhdbuilt.4),
                          type=unique(temp2$type))
        REM_i<-data.frame(rbind(REM_i,temp2))
        ERR_i<-data.frame(ERR_i[!(ERR_i$geo==temp2$geo & ERR_i$type==temp2$type),])
      }
    }


    ##Distribute remaining HUs in ERR by correspondence of types
    if(nrow(ERR_i!=0)){
      for (j in unique(ERR_i$type)){
        temp1<-unique(colSums(REM_i[REM_i$type==j,names(REM_i)[grepl("hhd",names(REM_i))][as.logical(as.numeric(unlist(strsplit(j," "))))]]))
        temp2<-nrow(ERR_i[ERR_i$type==j,])
        temp3<-temp2
        while(unique(colSums(REM_i[REM_i$type==j,names(REM_i)[grepl("hhd",names(REM_i))][as.logical(as.numeric(unlist(strsplit(j," "))))]]))!=(temp1+temp2)){
          REM_i[REM_i$type==j,c("hhcount",names(REM_i)[grepl("hhd",names(REM_i))][as.logical(as.numeric(unlist(strsplit(j," "))))])][0:min(nrow(REM_i[REM_i$type==j,]),temp3),]<-REM_i[REM_i$type==j,c("hhcount",names(REM_i)[grepl("hhd",names(REM_i))][as.logical(as.numeric(unlist(strsplit(j," "))))])][0:min(nrow(REM_i[REM_i$type==j,]),temp3),]+1
          temp3<-temp1+temp2-unique(colSums(REM_i[REM_i$type==j,c("hhcount",names(REM_i)[grepl("hhd",names(REM_i))][as.logical(as.numeric(unlist(strsplit(j," "))))])]))
        }
      }
    }

    if(max(abs(colSums(REM_i[,grepl("hh",names(REM_i))])-colSums(BT_i[,grepl("hh",names(BT_i)),with=FALSE])))!=0){print("error")}

    RES<-rbind(RES,REM_i[,-ncol(REM_i)])

  }
}
BT<-data.table(RES)


###Distribute remaining HUs in ERR on addresses that were not associated to a UEF within each DB
GIS_XY<-fread("Output/hGIS_XY.csv")
GIS_XY$Location<-paste(GIS_XY$bld_lat,GIS_XY$bld_lon)
ERR<-BT[as.numeric(BT$geo)%in%unique(as.numeric(names(table(BT$geo)[table(BT$geo)>1]))),]
ADD<-fread("Input/Addresses/ADD_DB.csv")
# ADD<-fread("Input/Addresses/ADD_nearestDB.csv")
ADD<-ADD[!is.na(ADD$DBUID)]
ADD$Location<-paste(ADD$Latitude,ADD$Longitude)
ADD<-ADD[!ADD$Location%in%GIS_XY$Location]
ADD<-data.table(ad_id=ADD$ID_ADRESSE,db_id=ADD$DBUID,ad_lat=ADD$Latitude,ad_lon=ADD$Longitude)
ADD<-ADD[ADD$db_id%in%GIS_XY$db_id]
GIS_XY<-GIS_XY[,-ncol(GIS_XY),with=FALSE]
ERR$geo<-as.numeric(ERR$geo)
ERR<-merge(ERR,GIS_XY[,c("bld_id","db_id"),with=FALSE],by.x="geo",by.y="bld_id",all.x=TRUE)
ADD<-ADD[as.numeric(ADD$db_id)%in%as.numeric(ERR$db_id)]
BT<-data.frame(BT)
for (i in as.numeric(unique(ERR$db_id))[as.numeric(unique(ERR$db_id))%in%as.numeric(unique(ADD$db_id))]){
  print(i)
  ERR_i<-arrange(ERR[ERR$db_id==i],hhcount)
  temp<-ERR_i$geo
  ERR_i$geo[1:min((nrow(ERR[ERR$db_id==i])-1),nrow(ADD[ADD$db_id==i]))]<-ADD[ADD$db_id==i]$ad_id[1:min((nrow(ERR[ERR$db_id==i])-1),nrow(ADD[ADD$db_id==i]))]
  ERR_i<-data.table(V1=ERR_i$V1, geo=ERR_i$geo, ERR_i[,c(-1,-2,-ncol(ERR_i)),with=FALSE])
  BT[as.numeric(BT$geo)%in%temp,]<-ERR_i
}
temp<-merge(data.table(bld_id=ADD$ad_id[ADD$ad_id%in%as.numeric(BT$geo)],db_id=ADD$db_id[ADD$ad_id%in%as.numeric(BT$geo)],cd_id=2466),GIS[,c("db_id","da_id")],by="db_id",all.x=TRUE)
temp<-unique(temp)
GIS<-rbind(GIS[,-1],data.table(bld_id=temp$bld_id,db_id=temp$db_id,da_id=temp$da_id,cd_id=temp$cd_id))
temp<-unique(merge(data.table(bld_id=ADD$ad_id[ADD$ad_id%in%as.numeric(BT$geo)],db_id=ADD$db_id[ADD$ad_id%in%as.numeric(BT$geo)],cd_id=2466,bld_lat=ADD$ad_lat[ADD$ad_id%in%as.numeric(BT$geo)],bld_lon=ADD$ad_lon[ADD$ad_id%in%as.numeric(BT$geo)]),GIS[,c("db_id","da_id")],by="db_id",all.x=TRUE))
GIS_XY<-rbind(GIS_XY[,-1],data.table(bld_id=temp$bld_id,db_id=temp$db_id,da_id=temp$da_id,cd_id=temp$cd_id,bld_lat=temp$bld_lat,bld_lon=temp$bld_lon))
GIS<-arrange(GIS,da_id,db_id,bld_id)
GIS_XY<-arrange(GIS_XY,da_id,db_id,bld_id)



###Distribute remaining HUs randomly within each DB
library(sf)
DB_geom<-read_sf("Intermediate/DB.shp")
DB_geom<-DB_geom[DB_geom$DBUID%in%as.numeric(GIS$db_id),]
ERR<-BT[as.numeric(BT$geo)%in%unique(as.numeric(names(table(BT$geo)[table(BT$geo)>1]))),]
ERR$geo<-as.numeric(ERR$geo)
ERR<-merge(ERR,GIS_XY[,c("bld_id","db_id"),with=FALSE],by.x="geo",by.y="bld_id",all.x=TRUE)
GIS_XY$bld_location<-paste(GIS_XY$bld_lat,GIS_XY$bld_lon)
RADD<-data.frame()
j=30000000000
# i=as.numeric(unique(ERR$db_id))[1]
for (i in as.numeric(unique(ERR$db_id))){
  print(i)
  ERR_i<-ERR[as.numeric(ERR$db_id)==i,]
  temp<-st_sample(DB_geom$geometry[DB_geom$DBUID==i],size=(nrow(ERR_i)-1))
  temp<-unlist(temp)
  RADD_i<-data.table(bld_id=j:(j+nrow(ERR_i)-2),db_id=i,bld_lat=temp[names(temp)=="lat"],bld_lon=temp[names(temp)=="lon"])
  RADD_i$bld_location<-paste(RADD_i$bld_lat,RADD_i$bld_lon)
  while(length(RADD_i$bld_location[!RADD_i$bld_location%in%GIS_XY$bld_location])!=length(RADD_i$bld_location)){
    temp<-st_sample(DB_geom$geometry[DB_geom$DBUID==i],size=(nrow(ERR_i)-1))
    temp<-unlist(temp)
    RADD_i<-data.table(bld_id=j:(j+nrow(ERR_i)-2),db_id=i,bld_lat=temp[names(temp)=="lat"],bld_lon=temp[names(temp)=="lon"])
    RADD_i$bld_location<-paste(RADD_i$bld_lat,RADD_i$bld_lon)
  }
  RADD<-rbind(RADD,RADD_i)
  j=RADD_i$bld_id[nrow(RADD_i)]+1
}
RADD<-RADD[,-ncol(RADD),with=FALSE]
for (i in as.numeric(unique(ERR$db_id))[as.numeric(unique(ERR$db_id))%in%as.numeric(unique(RADD$db_id))]){
  print(i)
  ERR_i<-arrange(ERR[ERR$db_id==i,],hhcount)
  temp<-ERR_i$geo
  ERR_i$geo[1:min((nrow(ERR[ERR$db_id==i,])-1),nrow(RADD[RADD$db_id==i]))]<-RADD[RADD$db_id==i]$bld_id[1:min((nrow(ERR[ERR$db_id==i,])-1),nrow(RADD[RADD$db_id==i]))]
  ERR_i<-data.table(V1=ERR_i$V1, geo=ERR_i$geo, ERR_i[,c(-1,-2,-ncol(ERR_i))])
  BT[as.numeric(BT$geo)%in%temp,]<-ERR_i
}
GIS$db_id<-as.numeric(GIS$db_id)
temp<-merge(data.table(bld_id=RADD$bld_id[RADD$bld_id%in%as.numeric(BT$geo)],db_id=RADD$db_id[RADD$bld_id%in%as.numeric(BT$geo)],cd_id=2466),GIS[,c("db_id","da_id")],by="db_id",all.x=TRUE)
temp<-unique(temp)
GIS<-rbind(GIS,data.table(bld_id=temp$bld_id,db_id=temp$db_id,da_id=temp$da_id,cd_id=temp$cd_id))
temp<-unique(merge(data.table(bld_id=RADD$bld_id[RADD$bld_id%in%as.numeric(BT$geo)],db_id=RADD$db_id[RADD$bld_id%in%as.numeric(BT$geo)],cd_id=2466,bld_lat=RADD$bld_lat[RADD$bld_id%in%as.numeric(BT$geo)],bld_lon=RADD$bld_lon[RADD$bld_id%in%as.numeric(BT$geo)]),GIS[,c("db_id","da_id")],by="db_id",all.x=TRUE))
GIS_XY$db_id<-as.numeric(GIS_XY$db_id)
GIS_XY<-rbind(GIS_XY[,-ncol(GIS_XY),with=FALSE],data.table(bld_id=temp$bld_id,db_id=temp$db_id,da_id=temp$da_id,cd_id=temp$cd_id,bld_lat=temp$bld_lat,bld_lon=temp$bld_lon))
GIS<-arrange(GIS,da_id,db_id,bld_id)
GIS_XY<-arrange(GIS_XY,da_id,db_id,bld_id)

# write.csv(BT,"Output/hBT.csv")

# #####################################################################
# options(scipen = 10)
# library(glmnet)
# 
# BT<-fread("Output/milestone1.csv")
# ###BT
# GIS$bld_id<-as.numeric(GIS$bld_id)
# GIS$db_id<-as.numeric(GIS$db_id)
# GIS$bld_id[is.na(GIS$bld_id)]<-GIS$db_id[is.na(GIS$bld_id)]
# 
# #OR
# BT$bld_hh<-BT$bld_dwellings
# 
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
# 
# BT$geo<-as.numeric(BT$geo)
# DBT$geo<-as.numeric(DBT$geo)
# BT<-merge(BT,GIS[,c("bld_id","db_id","da_id")],by.x="geo",by.y="bld_id")
# BT<-merge(BT,DBT[,c("geo","hhcount")],by.x="db_id",by.y="geo")
# BT<-merge(BT,DAT[,c("geo","hhdcondo.1","hhdcondo.2","hhdbuilt.1","hhdbuilt.2","hhdbuilt.3","hhdbuilt.4")],by.x="da_id",by.y="geo")
# dacat<-c("hhdcondo.1","hhdcondo.2","hhdbuilt.1","hhdbuilt.2","hhdbuilt.3","hhdbuilt.4")
# dbcat<-c("hhcount")
# 
# 
# i=unique(GIS$da_id)[1]
# GIS_i<-arrange(GIS[GIS$da_id==i],bld_id)
# DAT_i<-DAT[DAT$geo==i,]
# DBT_i<-DBT[DBT$geo%in%GIS_i$db_id]
# BT_i<-BT[BT$da_id==i]
# A_i<-matrix(0,length(BT_i$geo),length(unique(BT_i$db_id))*length(dbcat)+length(unique(BT_i$da_id))*length(dacat))
# colnames(A_i)<-c(paste(paste("DA",sort(rep(unique(GIS_i$da_id),length(dacat))),sep=""),dacat,sep="_"),paste(paste("DB",sort(rep(unique(GIS_i$db_id),length(dbcat))),sep=""),dbcat,sep="_"))
# rownames(A_i)<-sort(BT_i$geo)
# temp_i<-arrange(BT_i[,c("da_id","geo",paste(dacat,".x",sep="")),with=FALSE],geo)
# for (i in unique(GIS_i$da_id)){
#   A_i[rownames(A_i)%in%sort(unlist(temp_i[temp_i$da_id==i,"geo"])),grepl(paste("DA",as.character(i),"_",sep=""),colnames(A_i))]<-as.matrix(temp_i[temp_i$da_id==i,paste(dacat,".x",sep=""),with=FALSE])
#   # print(i)
# }
# temp_i<-arrange(BT_i[,c("db_id","geo",paste(dbcat,".x",sep="")),with=FALSE],geo)
# temp_i$db_id<-as.numeric(temp_i$db_id)
# for (i in unique(GIS_i$db_id)){
#   A_i[rownames(A_i)%in%sort(unlist(temp_i[temp_i$db_id==i,"geo"])),grepl(paste("DB",as.character(i),"_",sep=""),colnames(A_i))]<-as.matrix(temp_i[temp_i$db_id==i,paste(dbcat,".x",sep=""),with=FALSE])
#   # print(i)
# }
# A_i<-t(A_i)
# 
# ##B_i
# B_i<-matrix(0,nrow=length(dacat)*length(unique(GIS_i$da_id))+length(dbcat)*length(unique(GIS_i$db_id)),ncol=1)
# rownames(B_i)<-rownames(A_i)
# B_i[1:length(dacat)*length(unique(GIS_i$da_id)),]<-unlist(matrix(t(DAT_i[,names(DAT_i)%in%dacat,with=FALSE])))
# B_i[(length(dacat)*length(unique(GIS_i$da_id))+1):(length(dacat)*length(unique(GIS_i$da_id))+length(dbcat)*length(unique(GIS_i$db_id))),]<-unlist(matrix(t(arrange(DBT_i,geo)[,names(DBT_i)%in%dbcat,with=FALSE]))) 
# gc()
# 
# ##Optimization_i
# #optmeth: glmnet
# glmnet_i<-glmnet(A_i,B_i,alpha=1, lambda = 0, lower.limits=0, intercept=F, thres = 1E-10)
# X_i<-coef(glmnet_i)[,1][-1]
# X_i
# X_i<-X_i[X_i>0]
# A_i<-A_i[,colnames(A_i)%in%names(X_i)]
# 
# 
# ##################IPU
# rm(list=ls())
# ###Packages
# library(data.table)
# library(dplyr)
# library(stringr)
# library(gsubfn)
# 
# GIS<-fread("Output/hGIS.csv")
# DAT<-fread("Output/hDAT.csv")
# DBT<-fread("Output/hDBT.csv")
# BT<-fread("Output/milestone1.csv")
# 
# ###BT
# GIS$bld_id<-as.numeric(GIS$bld_id)
# GIS$db_id<-as.numeric(GIS$db_id)
# GIS$bld_id[is.na(GIS$bld_id)]<-GIS$db_id[is.na(GIS$bld_id)]
# 
# #OR
# BT$bld_hh<-BT$bld_dwellings
# 
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
# DBT$geo<-as.numeric(DBT$geo)
# BT<-merge(BT,GIS[,c("bld_id","db_id","da_id")],by.x="geo",by.y="bld_id")
# # BT<-merge(BT,aggregate(data=BT,hhcount~db_id,sum),by.x="db_id",by.y="db_id")
# # BT<-merge(BT,aggregate(data=BT,cbind(hhdcondo.1,hhdcondo.2,hhdbuilt.1,hhdbuilt.2,hhdbuilt.3,hhdbuilt.4)~da_id,sum),by.x="da_id",by.y="da_id")
# # BT<-merge(BT,DBT[,c("geo","hhcount")],by.x="db_id",by.y="geo")
# # BT<-merge(BT,DAT[,c("geo","hhdcondo.1","hhdcondo.2","hhdbuilt.1","hhdbuilt.2","hhdbuilt.3","hhdbuilt.4")],by.x="da_id",by.y="geo")
# # names(BT)[(ncol(BT)-6):ncol(BT)]<-paste(names(BT)[(ncol(BT)-6):ncol(BT)],".z",sep="")
# 
# BT_seed<-data.frame(arrange(BT,geo))
# names(BT_seed)[1]<-"bld_id"
# DAT<-data.frame(DAT)
# DBT<-data.frame(DBT)
# GIS<-data.frame(GIS)
# 
# BT_seed<-BT_seed[BT_seed$da_id==unique(GIS$da_id)[1],]
# DAT<-DAT[DAT$geo==unique(GIS$da_id)[1],]
# DBT<-DBT[DBT$geo%in%GIS$db_id[GIS$da_id==unique(GIS$da_id)[1]],]
# 
# BT_seed
# 
# # BT_seed[BT_seed==0]<-0.00001
# 
# delta_a<-max(max(abs(arrange(DBT,geo)[,c("hhcount")]-arrange(aggregate(data=BT_seed,hhcount~db_id,sum),db_id)[,"hhcount"])),
#              max(abs(arrange(DAT,geo)[,c("hhdcondo.1")]-arrange(aggregate(data=BT_seed,hhdcondo.1~da_id,sum),da_id)[,"hhdcondo.1"])),
#              max(abs(arrange(DAT,geo)[,c("hhdcondo.2")]-arrange(aggregate(data=BT_seed,hhdcondo.2~da_id,sum),da_id)[,"hhdcondo.2"])),
#              max(abs(arrange(DAT,geo)[,c("hhdbuilt.1")]-arrange(aggregate(data=BT_seed,hhdbuilt.1~da_id,sum),da_id)[,"hhdbuilt.1"])),
#              max(abs(arrange(DAT,geo)[,c("hhdbuilt.2")]-arrange(aggregate(data=BT_seed,hhdbuilt.2~da_id,sum),da_id)[,"hhdbuilt.2"])),
#              max(abs(arrange(DAT,geo)[,c("hhdbuilt.3")]-arrange(aggregate(data=BT_seed,hhdbuilt.3~da_id,sum),da_id)[,"hhdbuilt.3"])),
#              max(abs(arrange(DAT,geo)[,c("hhdbuilt.4")]-arrange(aggregate(data=BT_seed,hhdbuilt.4~da_id,sum),da_id)[,"hhdbuilt.4"])))
# print(delta_a)
# 
# 
# max(abs(arrange(DAT,geo)[,c("hhdbuilt.1")]-arrange(aggregate(data=BT_seed,hhdbuilt.1~da_id,sum),da_id)[,"hhdbuilt.1"]))
# max(abs(arrange(DAT,geo)[,c("hhdbuilt.2")]-arrange(aggregate(data=BT_seed,hhdbuilt.2~da_id,sum),da_id)[,"hhdbuilt.2"]))
# max(abs(arrange(DAT,geo)[,c("hhdbuilt.3")]-arrange(aggregate(data=BT_seed,hhdbuilt.3~da_id,sum),da_id)[,"hhdbuilt.3"]))
# max(abs(arrange(DAT,geo)[,c("hhdbuilt.4")]-arrange(aggregate(data=BT_seed,hhdbuilt.4~da_id,sum),da_id)[,"hhdbuilt.4"]))
# max(abs(arrange(DAT,geo)[,c("hhdcondo.1")]-arrange(aggregate(data=BT_seed,hhdcondo.1~da_id,sum),da_id)[,"hhdcondo.1"]))
# max(abs(arrange(DAT,geo)[,c("hhdcondo.2")]-arrange(aggregate(data=BT_seed,hhdcondo.2~da_id,sum),da_id)[,"hhdcondo.2"]))
# max(abs(arrange(DBT,geo)[,c("hhcount")]-arrange(aggregate(data=BT_seed,hhcount~db_id,sum),db_id)[,"hhcount"]))
# 
# 
# agg<-arrange(merge(BT_seed[,grepl("id",names(BT_seed))],aggregate(data=BT_seed,hhdbuilt.1~da_id,sum),by="da_id"),bld_id)[,"hhdbuilt.1"]
# tot<-arrange(merge(BT_seed[,grepl("id",names(BT_seed))],DAT[,c("geo","hhdbuilt.1")],by.x="da_id",by.y="geo"),bld_id)[,"hhdbuilt.1"]
# BT_seed[agg!=0,!grepl("id",names(BT_seed))]<-BT_seed[agg!=0,!grepl("id",names(BT_seed))]*unlist(tot[agg!=0]/agg[agg!=0])
# 
# agg<-arrange(merge(BT_seed[,grepl("id",names(BT_seed))],aggregate(data=BT_seed,hhdbuilt.2~da_id,sum),by="da_id"),bld_id)[,"hhdbuilt.2"]
# tot<-arrange(merge(BT_seed[,grepl("id",names(BT_seed))],DAT[,c("geo","hhdbuilt.2")],by.x="da_id",by.y="geo"),bld_id)[,"hhdbuilt.2"]
# BT_seed[agg!=0,!grepl("id",names(BT_seed))]<-BT_seed[agg!=0,!grepl("id",names(BT_seed))]*unlist(tot[agg!=0]/agg[agg!=0])
# 
# agg<-arrange(merge(BT_seed[,grepl("id",names(BT_seed))],aggregate(data=BT_seed,hhdbuilt.3~da_id,sum),by="da_id"),bld_id)[,"hhdbuilt.3"]
# tot<-arrange(merge(BT_seed[,grepl("id",names(BT_seed))],DAT[,c("geo","hhdbuilt.3")],by.x="da_id",by.y="geo"),bld_id)[,"hhdbuilt.3"]
# BT_seed[agg!=0,!grepl("id",names(BT_seed))]<-BT_seed[agg!=0,!grepl("id",names(BT_seed))]*unlist(tot[agg!=0]/agg[agg!=0])
# 
# agg<-arrange(merge(BT_seed[,grepl("id",names(BT_seed))],aggregate(data=BT_seed,hhdbuilt.4~da_id,sum),by="da_id"),bld_id)[,"hhdbuilt.4"]
# tot<-arrange(merge(BT_seed[,grepl("id",names(BT_seed))],DAT[,c("geo","hhdbuilt.4")],by.x="da_id",by.y="geo"),bld_id)[,"hhdbuilt.4"]
# BT_seed[agg!=0,!grepl("id",names(BT_seed))]<-BT_seed[agg!=0,!grepl("id",names(BT_seed))]*unlist(tot[agg!=0]/agg[agg!=0])
# 
# agg<-arrange(merge(BT_seed[,grepl("id",names(BT_seed))],aggregate(data=BT_seed,hhdcondo.1~da_id,sum),by="da_id"),bld_id)[,"hhdcondo.1"]
# tot<-arrange(merge(BT_seed[,grepl("id",names(BT_seed))],DAT[,c("geo","hhdcondo.1")],by.x="da_id",by.y="geo"),bld_id)[,"hhdcondo.1"]
# BT_seed[agg!=0,!grepl("id",names(BT_seed))]<-BT_seed[agg!=0,!grepl("id",names(BT_seed))]*unlist(tot[agg!=0]/agg[agg!=0])
# 
# agg<-arrange(merge(BT_seed[,grepl("id",names(BT_seed))],aggregate(data=BT_seed,hhdcondo.2~da_id,sum),by="da_id"),bld_id)[,"hhdcondo.2"]
# tot<-arrange(merge(BT_seed[,grepl("id",names(BT_seed))],DAT[,c("geo","hhdcondo.2")],by.x="da_id",by.y="geo"),bld_id)[,"hhdcondo.2"]
# BT_seed[agg!=0,!grepl("id",names(BT_seed))]<-BT_seed[agg!=0,!grepl("id",names(BT_seed))]*unlist(tot[agg!=0]/agg[agg!=0])
# 
# agg<-arrange(merge(BT_seed[,grepl("id",names(BT_seed))],aggregate(data=BT_seed,hhcount~db_id,sum),by="db_id"),bld_id)[,"hhcount"]
# tot<-arrange(merge(BT_seed[,grepl("id",names(BT_seed))],DBT[,c("geo","hhcount")],by.x="db_id",by.y="geo"),bld_id)[,"hhcount"]
# BT_seed[agg!=0,!grepl("id",names(BT_seed))]<-BT_seed[agg!=0,!grepl("id",names(BT_seed))]*unlist(tot[agg!=0]/agg[agg!=0])
# 
# delta_b<-max(max(abs(arrange(DBT,geo)[,c("hhcount")]-arrange(aggregate(data=BT_seed,hhcount~db_id,sum),db_id)[,"hhcount"])),
#              max(abs(arrange(DAT,geo)[,c("hhdcondo.1")]-arrange(aggregate(data=BT_seed,hhdcondo.1~da_id,sum),da_id)[,"hhdcondo.1"])),
#              max(abs(arrange(DAT,geo)[,c("hhdcondo.2")]-arrange(aggregate(data=BT_seed,hhdcondo.2~da_id,sum),da_id)[,"hhdcondo.2"])),
#              max(abs(arrange(DAT,geo)[,c("hhdbuilt.1")]-arrange(aggregate(data=BT_seed,hhdbuilt.1~da_id,sum),da_id)[,"hhdbuilt.1"])),
#              max(abs(arrange(DAT,geo)[,c("hhdbuilt.2")]-arrange(aggregate(data=BT_seed,hhdbuilt.2~da_id,sum),da_id)[,"hhdbuilt.2"])),
#              max(abs(arrange(DAT,geo)[,c("hhdbuilt.3")]-arrange(aggregate(data=BT_seed,hhdbuilt.3~da_id,sum),da_id)[,"hhdbuilt.3"])),
#              max(abs(arrange(DAT,geo)[,c("hhdbuilt.4")]-arrange(aggregate(data=BT_seed,hhdbuilt.4~da_id,sum),da_id)[,"hhdbuilt.4"])))
# print(delta_b)
# BT_seed

###############################################################################################