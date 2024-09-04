rm(list=ls())
gc()
memory.limit(size=1E6)
options(scipen = 999)



###Packages
library(data.table)
library(dplyr)
library(glmnet)
library(Matrix)
# library(slam)
# library(rgenoud)



###Inputs 
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
# remaining<-c(1:3192)[!c(1:3192)%in%as.numeric(substring(list.files("Output/Synthetic population"),4))]
# k<-remaining[1]     3162,3169,3176,2726

for (k in 1:length(unique(GIS$da_id))){
  
  tryCatch({
  print(k)
  # SP<-Matrix(0,nrow=2E6,ncol=35,sparse=TRUE)
  # colnames(SP)<-c("bld_id","hhcount",colnames(PUMF_MAT),"db_id","da_id")
  # WP<-Matrix(0,nrow=2E6,ncol=35,sparse=TRUE)
  # colnames(WP)<-c("bld_id","hhcount",colnames(PUMF_MAT),"db_id","da_id")
  
  
  ##Inputs_i
  DAT_i<-DAT[DAT$geo%in%unique(GIS$da_id)[k]]
  DBT_i<-DBT[DBT$geo%in%GIS$db_id[GIS$da_id%in%DAT_i$geo]]
  BT_i<-BT[BT$geo%in%GIS$bld_id[GIS$da_id%in%DAT_i$geo]]
  GIS_i<-GIS[GIS$da_id%in%unique(GIS$da_id)[k]]
  GIS_i$db_id<-as.numeric(GIS_i$db_id)
  PUMF_MAT_i<-left_join(data.table(temp=rep(1,length(GIS_i$bld_id)),bld_id=GIS_i$bld_id),data.table(temp=rep(1,nrow(PUMF_MAT)),PUMF_MAT),by="temp")
  PUMF_MAT_i<-PUMF_MAT_i[,-"temp"]
  temp_i<-data.frame(left_join(PUMF_MAT_i,BT_i,c("bld_id"="geo")))
  temp_i[,grepl("\\.y",names(temp_i))][temp_i[,grepl("\\.y",names(temp_i))]>=1]<-1
  PUMF_MAT_i<-data.table(PUMF_MAT_i[rowSums(temp_i[,grepl("\\.x",names(temp_i))]!=0 & temp_i[,grepl("\\.y",names(temp_i))]==0)==0,])
  PUMF_MAT_i<-left_join(PUMF_MAT_i,data.table(bld_id=GIS_i$bld_id,db_id=GIS_i$db_id,da_id=GIS_i$da_id),by="bld_id")
  if(length(names(DAT_i)[DAT_i==0])>0){
    PUMF_MAT_i<-PUMF_MAT_i[rowSums(PUMF_MAT_i[,names(DAT_i)[DAT_i==0],with=FALSE])==0,]
  }
  PUMF_MAT_i$hhid<-1:nrow(PUMF_MAT_i)
  dacat<-names(DAT_i)[!(names(DAT_i)%in%names(DBT_i) | names(DAT_i)%in%names(BT_i))]
  dbcat<-names(DBT_i)[!names(DBT_i)%in%names(BT_i)]
  bcat<-names(BT_i)[names(BT_i)!="geo"]
  gc()

  
  ##A_i
  if (class(try({
    A_i<-matrix(0,nrow=nrow(PUMF_MAT_i),ncol=length(dacat)*length(unique(GIS_i$da_id))+length(dbcat)*length(unique(GIS_i$db_id))+length(bcat)*length(unique(GIS_i$bld_id)))
    colnames(A_i)<-c(paste(paste("DA",sort(rep(unique(GIS_i$da_id),length(dacat))),sep=""),dacat,sep="_"),paste(paste("DB",sort(rep(unique(GIS_i$db_id),length(dbcat))),sep=""),dbcat,sep="_"),paste(paste("B",sort(rep(unique(GIS_i$bld_id),length(bcat))),sep=""),bcat,sep="_"))
    rownames(A_i)<-PUMF_MAT_i$hhid
    temp_i<-PUMF_MAT_i[,c("da_id","hhid",dacat),with=FALSE]
    for (i in unique(GIS_i$da_id)){
      if(ncol(A_i[unlist(temp_i[temp_i$da_id==i,"hhid"]),grepl(paste("DA",as.character(i),"_",sep=""),colnames(A_i))])!=ncol(as.matrix(temp_i[temp_i$da_id==i,dacat,with=FALSE]))){print("error1")}
      A_i[unlist(temp_i[temp_i$da_id==i,"hhid"]),grepl(paste("DA",as.character(i),"_",sep=""),colnames(A_i))]<-as.matrix(temp_i[temp_i$da_id==i,dacat,with=FALSE])
      # print(i)
    }
    temp_i<-PUMF_MAT_i[,c("db_id","hhid",dbcat),with=FALSE]
    for (i in unique(GIS_i$db_id)){
      if(length(A_i[unlist(temp_i[temp_i$db_id==i,"hhid"]),grepl(paste("DB",as.character(i),"_",sep=""),colnames(A_i))])!=length(as.matrix(temp_i[temp_i$db_id==i,dbcat,with=FALSE]))){print("error2")}
      A_i[unlist(temp_i[temp_i$db_id==i,"hhid"]),grepl(paste("DB",as.character(i),"_",sep=""),colnames(A_i))]<-as.matrix(temp_i[temp_i$db_id==i,dbcat,with=FALSE])
      # print(i)
    }
    temp_i<-PUMF_MAT_i[,c("bld_id","hhid",bcat),with=FALSE]
    for (i in unique(GIS_i$bld_id)){
      if(ncol(A_i[unlist(temp_i[temp_i$bld_id==i,"hhid"]),grepl(paste("B",as.character(i),"_",sep=""),colnames(A_i)) & !grepl("DB",colnames(A_i))])!=ncol(as.matrix(temp_i[temp_i$bld_id==i,bcat,with=FALSE]))){print("error3")}
      A_i[unlist(temp_i[temp_i$bld_id==i,"hhid"]),grepl(paste("B",as.character(i),"_",sep=""),colnames(A_i)) & !grepl("DB",colnames(A_i))]<-as.matrix(temp_i[temp_i$bld_id==i,bcat,with=FALSE])
      # A_i[unlist(temp_i[temp_i$bld_id==i,"hhid"]),grepl(as.character(i),colnames(A_i)) & !grepl("DB",colnames(A_i)) & grepl("B",colnames(A_i))]<-as.matrix(temp_i[temp_i$bld_id==i,bcat,with=FALSE])
      # print(i)
    }
    A_i<-Matrix(A_i,sparse=TRUE)
    A_i<-t(A_i)
    gc()
  }))[1]=="try-error"){
    rm(A_i)
    gc()
    A_i<-Matrix(0,nrow=nrow(PUMF_MAT_i),ncol=length(dacat)*length(unique(GIS_i$da_id))+length(dbcat)*length(unique(GIS_i$db_id))+length(bcat)*length(unique(GIS_i$bld_id)),sparse=TRUE)
    colnames(A_i)<-c(paste(paste("DA",sort(rep(unique(GIS_i$da_id),length(dacat))),sep=""),dacat,sep="_"),paste(paste("DB",sort(rep(unique(GIS_i$db_id),length(dbcat))),sep=""),dbcat,sep="_"),paste(paste("B",sort(rep(unique(GIS_i$bld_id),length(bcat))),sep=""),bcat,sep="_"))
    rownames(A_i)<-PUMF_MAT_i$hhid
    temp_i<-PUMF_MAT_i[,c("da_id","hhid",dacat),with=FALSE]
    for (i in unique(GIS_i$da_id)){
      if(ncol(A_i[unlist(temp_i[temp_i$da_id==i,"hhid"]),grepl(paste("DA",as.character(i),"_",sep=""),colnames(A_i))])!=ncol(as.matrix(temp_i[temp_i$da_id==i,dacat,with=FALSE]))){print("error1")}
      A_i[unlist(temp_i[temp_i$da_id==i,"hhid"]),grepl(paste("DA",as.character(i),"_",sep=""),colnames(A_i))]<-as.matrix(temp_i[temp_i$da_id==i,dacat,with=FALSE])
      # print(i)
    }
    temp_i<-PUMF_MAT_i[,c("db_id","hhid",dbcat),with=FALSE]
    for (i in unique(GIS_i$db_id)){
      if(length(A_i[unlist(temp_i[temp_i$db_id==i,"hhid"]),grepl(paste("DB",as.character(i),"_",sep=""),colnames(A_i))])!=length(as.matrix(temp_i[temp_i$db_id==i,dbcat,with=FALSE]))){print("error2")}
      A_i[unlist(temp_i[temp_i$db_id==i,"hhid"]),grepl(paste("DB",as.character(i),"_",sep=""),colnames(A_i))]<-as.matrix(temp_i[temp_i$db_id==i,dbcat,with=FALSE])
      # print(i)
    }
    temp_i<-PUMF_MAT_i[,c("bld_id","hhid",bcat),with=FALSE]
    for (i in unique(GIS_i$bld_id)){
      if(ncol(A_i[unlist(temp_i[temp_i$bld_id==i,"hhid"]),grepl(paste("B",as.character(i),"_",sep=""),colnames(A_i)) & !grepl("DB",colnames(A_i))])!=ncol(as.matrix(temp_i[temp_i$bld_id==i,bcat,with=FALSE]))){print("error3")}
      A_i[unlist(temp_i[temp_i$bld_id==i,"hhid"]),grepl(paste("B",as.character(i),"_",sep=""),colnames(A_i)) & !grepl("DB",colnames(A_i))]<-as.matrix(temp_i[temp_i$bld_id==i,bcat,with=FALSE])
      # A_i[unlist(temp_i[temp_i$bld_id==i,"hhid"]),grepl(as.character(i),colnames(A_i)) & !grepl("DB",colnames(A_i)) & grepl("B",colnames(A_i))]<-as.matrix(temp_i[temp_i$bld_id==i,bcat,with=FALSE])
      # print(i)
    }
    A_i<-t(A_i)
    gc()
  }else{
    # rm(A_i)
    # gc()
    # A_i<-matrix(0,nrow=nrow(PUMF_MAT_i),ncol=length(dacat)*length(unique(GIS_i$da_id))+length(dbcat)*length(unique(GIS_i$db_id))+length(bcat)*length(unique(GIS_i$bld_id)))
    # colnames(A_i)<-c(paste(paste("DA",sort(rep(unique(GIS_i$da_id),length(dacat))),sep=""),dacat,sep="_"),paste(paste("DB",sort(rep(unique(GIS_i$db_id),length(dbcat))),sep=""),dbcat,sep="_"),paste(paste("B",sort(rep(unique(GIS_i$bld_id),length(bcat))),sep=""),bcat,sep="_"))
    # rownames(A_i)<-PUMF_MAT_i$hhid
    # temp_i<-PUMF_MAT_i[,c("da_id","hhid",dacat),with=FALSE]
    # for (i in unique(GIS_i$da_id)){
    #   A_i[unlist(temp_i[temp_i$da_id==i,"hhid"]),grepl(as.character(i),colnames(A_i)) & grepl("DA",colnames(A_i))]<-as.matrix(temp_i[temp_i$da_id==i,dacat,with=FALSE])
    #   # print(i)
    # }
    # temp_i<-PUMF_MAT_i[,c("db_id","hhid",dbcat),with=FALSE]
    # for (i in unique(GIS_i$db_id)){
    #   A_i[unlist(temp_i[temp_i$db_id==i,"hhid"]),grepl(as.character(i),colnames(A_i)) & grepl("DB",colnames(A_i))]<-as.matrix(temp_i[temp_i$db_id==i,dbcat,with=FALSE])
    #   # print(i)
    # }
    # temp_i<-PUMF_MAT_i[,c("bld_id","hhid",bcat),with=FALSE]
    # for (i in unique(GIS_i$bld_id)){
    #   A_i[unlist(temp_i[temp_i$bld_id==i,"hhid"]),grepl(paste("B",as.character(i),"_",sep=""),colnames(A_i)) & !grepl("DB",colnames(A_i))]<-as.matrix(temp_i[temp_i$bld_id==i,bcat,with=FALSE])
    #   # A_i[unlist(temp_i[temp_i$bld_id==i,"hhid"]),grepl(as.character(i),colnames(A_i)) & !grepl("DB",colnames(A_i)) & grepl("B",colnames(A_i))]<-as.matrix(temp_i[temp_i$bld_id==i,bcat,with=FALSE])
    #   # print(i)
    # }
    # A_i<-t(A_i)
    # gc()
  }
  # if(class(try(matrix(0,nrow=nrow(PUMF_MAT_i),ncol=length(dacat)*length(unique(GIS_i$da_id))+length(dbcat)*length(unique(GIS_i$db_id))+length(bcat)*length(unique(GIS_i$bld_id)))))[1]=="try-error"){
  #   A_i<-Matrix(0,nrow=nrow(PUMF_MAT_i),ncol=length(dacat)*length(unique(GIS_i$da_id))+length(dbcat)*length(unique(GIS_i$db_id))+length(bcat)*length(unique(GIS_i$bld_id)),sparse=TRUE)
  # }else{
  #   A_i<-matrix(0,nrow=nrow(PUMF_MAT_i),ncol=length(dacat)*length(unique(GIS_i$da_id))+length(dbcat)*length(unique(GIS_i$db_id))+length(bcat)*length(unique(GIS_i$bld_id)))
  # }
  # colnames(A_i)<-c(paste(paste("DA",sort(rep(unique(GIS_i$da_id),length(dacat))),sep=""),dacat,sep="_"),paste(paste("DB",sort(rep(unique(GIS_i$db_id),length(dbcat))),sep=""),dbcat,sep="_"),paste(paste("B",sort(rep(unique(GIS_i$bld_id),length(bcat))),sep=""),bcat,sep="_"))
  # rownames(A_i)<-PUMF_MAT_i$hhid
  # temp_i<-PUMF_MAT_i[,c("da_id","hhid",dacat),with=FALSE]
  # for (i in unique(GIS_i$da_id)){
  #   A_i[unlist(temp_i[temp_i$da_id==i,"hhid"]),grepl(as.character(i),colnames(A_i)) & grepl("DA",colnames(A_i))]<-as.matrix(temp_i[temp_i$da_id==i,dacat,with=FALSE])
  #   # print(i)
  # }
  # temp_i<-PUMF_MAT_i[,c("db_id","hhid",dbcat),with=FALSE]
  # for (i in unique(GIS_i$db_id)){
  #   A_i[unlist(temp_i[temp_i$db_id==i,"hhid"]),grepl(as.character(i),colnames(A_i)) & grepl("DB",colnames(A_i))]<-as.matrix(temp_i[temp_i$db_id==i,dbcat,with=FALSE])
  #   # print(i)
  # }
  # temp_i<-PUMF_MAT_i[,c("bld_id","hhid",bcat),with=FALSE]
  # for (i in unique(GIS_i$bld_id)){
  #   A_i[unlist(temp_i[temp_i$bld_id==i,"hhid"]),grepl(paste("B",as.character(i),"_",sep=""),colnames(A_i)) & !grepl("DB",colnames(A_i))]<-as.matrix(temp_i[temp_i$bld_id==i,bcat,with=FALSE])
  #   # A_i[unlist(temp_i[temp_i$bld_id==i,"hhid"]),grepl(as.character(i),colnames(A_i)) & !grepl("DB",colnames(A_i)) & grepl("B",colnames(A_i))]<-as.matrix(temp_i[temp_i$bld_id==i,bcat,with=FALSE])
  #   # print(i)
  # }
  # if(class(try(matrix(0,nrow=nrow(PUMF_MAT_i),ncol=length(dacat)*length(unique(GIS_i$da_id))+length(dbcat)*length(unique(GIS_i$db_id))+length(bcat)*length(unique(GIS_i$bld_id)))))[1]=="try-error"){
  #   A_i<-Matrix(A_i,sparse=TRUE)
  # }
  # A_i<-t(A_i)
  # gc()

  
  ##B_i
  B_i<-Matrix(0,nrow=length(dacat)*length(unique(GIS_i$da_id))+length(dbcat)*length(unique(GIS_i$db_id))+length(bcat)*length(unique(GIS_i$bld_id)),ncol=1,sparse=TRUE)
  B_i[1:length(dacat)*length(unique(GIS_i$da_id)),]<-unlist(matrix(t(DAT_i[,names(DAT_i)%in%dacat,with=FALSE])))
  B_i[(length(dacat)*length(unique(GIS_i$da_id))+1):(length(dacat)*length(unique(GIS_i$da_id))+length(dbcat)*length(unique(GIS_i$db_id))),]<-unlist(matrix(t(arrange(DBT_i,geo)[,names(DBT_i)%in%dbcat,with=FALSE]))) 
  B_i[(length(dacat)*length(unique(GIS_i$da_id))+length(dbcat)*length(unique(GIS_i$db_id))+1):(length(dacat)*length(unique(GIS_i$da_id))+length(dbcat)*length(unique(GIS_i$db_id))+length(bcat)*length(unique(GIS_i$bld_id))),]<-unlist(matrix(t(arrange(BT_i,geo)[,names(BT_i)%in%bcat,with=FALSE])))  
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
  gc()
  
  
  ##Synthetic population
  saveRDS(data.table(PUMF_MAT_i[PUMF_MAT_i$hhid%in%names(X_i)],weight=X_i),paste("Output/Weighted population/WP_",k,sep=""))
  saveRDS(data.table(PUMF_MAT_i[PUMF_MAT_i$hhid%in%names(W1_i)],weight=W1_i),paste("Output/Synthetic population/SP_",k,sep=""))
  # SP_i<-Matrix(as.matrix(data.table(PUMF_MAT_i[PUMF_MAT_i$hhid%in%names(W1_i)],weight=W1_i)))
  # WP_i<-Matrix(as.matrix(data.table(PUMF_MAT_i[PUMF_MAT_i$hhid%in%names(X_i)],weight=X_i)))
  # SP[SP[,ncol(SP)]==0,][1:nrow(SP_i),]<-SP_i                  
  # WP[WP[,ncol(WP)]==0,][1:nrow(WP_i),]<-WP_i   
  
  #Cleaning
  rm(DAT_i,GIS_i,PUMF_MAT_i,temp_i,temp,A_i,B_i,X_i,W1_i) #,SP_i,WP_i
  gc()
  
  },error=function(e){})
}





###Libraries
# library(ff)
# library(Rcpp)
# library(bigmemory)
# library(bigmemoryExt)
# options(bigmemory.allow.dimnames=TRUE)
# library(bigstatsr)
# library(sf)
# library(plyr)
# library(stringr)
# library(gsubfn)
# library(Cyclops)

###Optimization methods
# #optmeth2: lm
# lm_i<-lm(as.matrix(B_i)~as.matrix(A_i))
# X_i<-lm_i$coefficients[-1]
# X_i[is.na(X_i)]<-0
# rmse(X_i)
# Bhat_i<-as.vector(A_i %*% X_i)
# Res_i<-data.table(var=as.vector(rownames(B_i)),bhat=Bhat_i,b=as.vector(B_i))
# Res_i[ ,sqerr:=(b-bhat)^2]
# Res_i[ ,optmeth:="Least-Squares (GDML)"]
# plot(bhat~b, Res_i)
# RMSE_i<-sqrt(sum(Res_i$sqerr)/nrow(Res_i))
# RMSN_i<-RMSE_i/mean(Res_i$b)
# X_i<-X_i[X_i>0]

# #optmeth3: cyclops
# counts<-c(18,17,15,20,10,20,25,13,12)
# outcome<-gl(3,1,9)
# treatment<-gl(3,3)
# cyclopsData<-createCyclopsData(
#   counts ~ outcome,
#   modelType="sccs"
# )
# cyclopsData
# cyclopsFit<-fitCyclopsModel(cyclopsData)
# cyclopsFit$estimation



###Integerization methods
# #intmeth2 : weights with the highest decimals are increased
# W_i<-as.vector(X_i[X_i>0])
# Wint_i<-floor(W_i)
# Wrem_i<-W_i-Wint_i
# Wdef_i<-round(sum(Wrem_i))
# time_sel_i_2<-Sys.time()
# incr<-rank(Wrem_i)%in%sort(rank(Wrem_i))[(length(Wrem_i)-Wdef_i+1):length(Wrem_i)]
# W_i[incr]<-floor(W_i[incr])+1
# W_i<-floor(W_i)
# time_sel_i_2<-difftime(Sys.time(), time_sel_i_2, units = 'secs')
# temp<-X_i
# temp[X_i>0]<-W_i
# W_i<-temp
# Bhat_sel_i<-as.vector(A_i%*%W_i)
# Res_sel_i_2<-data.table(var=as.vector(rownames(B_i)),bhat=Bhat_sel_i,b=as.vector(B_i))
# Res_sel_i_2[ ,sqerr:=(b-bhat)^2]
# # plot(bhat~b, Res_sel_i_2)
# RMSE_sel_i_2<-sqrt(sum(Res_sel_i_2$sqerr)/nrow(Res_sel_i_2))
# RMSN_sel_i_2<-RMSE_sel_i_2/mean(Res_sel_i_2$b)
# RMSE_sel_i_2

# #intmeth3 : iteratively increase weights and assess RMSE to minimze it 
# W_i<-as.vector(X_i)
# Wint_i<-floor(W_i)
# Wrem_i<-W_i-Wint_i
# Wdef_i<-round(sum(Wrem_i))
# time_sel_i_3<-Sys.time()
# RMSE<-c()
# for (i in 1:length(W_i[W_i>0])){
#   temp<-Wint_i
#   temp[W_i>0][i]<-temp[W_i>0][i]+1
#   RMSE<-c(RMSE,sqrt(sum((as.vector(A_i%*%temp)-as.vector(B_i))^2)/nrow(A_i)))
# }
# for (j in 1:Wdef_i){
#   RMSE<-c()
#   for (i in 1:length(W_i[W_i>0])){
#     temp<-Wint_i
#     temp[W_i>0][i]<-temp[W_i>0][i]+1
#     RMSE<-c(RMSE,sqrt(sum((as.vector(A_i%*%temp)-as.vector(B_i))^2)/nrow(A_i)))
#   }
#   print(min(RMSE))
#   Wint_i[W_i>0][which.min(RMSE)]<-floor(Wint_i[W_i>0][which.min(RMSE)])+1
# }
# time_sel_i_3<-difftime(Sys.time(), time_sel_i_3, units = 'secs')
# Bhat_sel_i<-as.vector(A_i%*%Wint_i)
# Res_sel_i_3<-data.table(var=as.vector(rownames(B_i)),bhat=Bhat_sel_i,b=as.vector(B_i))
# Res_sel_i_3[ ,sqerr:=(b-bhat)^2]
# # plot(bhat~b, Res_sel_i_3)
# RMSE_sel_i_3<-sqrt(sum(Res_sel_i_3$sqerr)/nrow(Res_sel_i_3))
# RMSN_sel_i_3<-RMSE_sel_i_3/mean(Res_sel_i_3$b)

# #intmeth4: try floor and ceiling for each factor and keep what minimizes RMSE
# W_i<-X_i[X_i>0]
# A_i<-A_i[,colnames(A_i)%in%names(W_i)]
# temp_i<-W_i
# for (j in 1:length(W_i)){
#   temp_i[j]<-floor(W_i[j])
#   RMSE_floor<-sqrt(sum((A_i%*%temp_i-B_i)^2)/nrow(B_i))
#   temp_i[j]<-ceiling(W_i[j])
#   RMSE_ceiling<-sqrt(sum((A_i%*%temp_i-B_i)^2)/nrow(B_i))
#   if(RMSE_floor<RMSE_ceiling){
#     W_i[j]<-floor(W_i[j])
#   }else{
#     W_i[j]<-floor(W_i[j]) + 1
#   }
#   print(sqrt(sum((A_i%*%W_i-B_i)^2)/nrow(B_i)))
# }

# #intmeth5: probability to increase a weight is proportional to the weight itself
# W2_i<-X_i
# W2int_i<-floor(W2_i)
# W2rem_i<-W2_i-W2int_i
# W2def_i<-round(sum(W2rem_i))
# incr<-sample(length(W2_i),size=W2def_i,prob=W2int_i)
# W2_i[incr]<-floor(W2_i[incr])+1
# W2_i<-floor(W2_i)

# #intmeth6 : genoud
# time_sel.2<-Sys.time()
# test<-genoud(fn=rmse,
#              nvars=ncol(A_i),
#              max=FALSE,
#              pop.size=10000,
#              starting.values=X_i,
#              data.type.int=TRUE,
#              Domains=as.matrix(cbind(floor(X_i),floor(X_i)+1)),
#              max.generations=100,
#              wait.generations=5,
#              hard.generation.limit=FALSE,
#              MemoryMatrix=TRUE,
#              solution.tolerance=0.1,
#              boundary.enforcement=0,
#              print.level=1,
#              optim.method="SANN"
# )
# test$value
# rmse(W1_i)
# time_sel.2<-difftime(Sys.time(), time_sel.2, units = 'secs')



###Memory solutions
# ###cbind solution
# PUMF_MAT_i_DA<-matrix(nrow=nrow(PUMF_MAT_i),ncol=length(dacat)*length(unique(GIS_i$da_id)),0)
# colnames(PUMF_MAT_i_DA)<-paste(paste("DA",sort(rep(unique(GIS_i$da_id),length(dacat))),sep=""),dacat,sep="_")
# temp_i<-PUMF_MAT_i[,c("da_id","hhid",dacat),with=FALSE]
# for (i in unique(GIS_i$da_id)){
#   PUMF_MAT_i_DA[unlist(temp_i[temp_i$da_id==i,"hhid"]),grepl(as.character(i),colnames(PUMF_MAT_i_DA))]<-as.matrix(temp_i[temp_i$da_id==i,dacat,with=FALSE])
#   print(i)
# }
# PUMF_MAT_i_DB<-matrix(nrow=nrow(PUMF_MAT_i),ncol=length(dbcat)*length(unique(GIS_i$db_id)),0)
# colnames(PUMF_MAT_i_DB)<-paste(paste("DB",sort(rep(unique(GIS_i$db_id),length(dbcat))),sep=""),dbcat,sep="_")
# temp_i<-PUMF_MAT_i[,c("db_id","hhid",dbcat),with=FALSE]
# for (i in unique(GIS_i$db_id)){
#   PUMF_MAT_i_DB[unlist(temp_i[temp_i$db_id==i,"hhid"]),grepl(as.character(i),colnames(PUMF_MAT_i_DB))]<-as.matrix(temp_i[temp_i$db_id==i,dbcat,with=FALSE])
#   print(i)
# }
# PUMF_MAT_i_B<-matrix(nrow=nrow(PUMF_MAT_i),ncol=length(bcat)*length(unique(GIS_i$bld_id)),0)
# colnames(PUMF_MAT_i_B)<-paste(paste("B",sort(rep(unique(GIS_i$bld_id),length(bcat))),sep=""),bcat,sep="_")
# temp_i<-PUMF_MAT_i[,c("bld_id","hhid",bcat),with=FALSE]
# for (i in unique(GIS_i$bld_id)){
#   PUMF_MAT_i_B[unlist(temp_i[temp_i$bld_id==i,"hhid"]),grepl(as.character(i),colnames(PUMF_MAT_i_B))]<-as.matrix(temp_i[temp_i$bld_id==i,bcat,with=FALSE])
#   print(i)
# }
# A_i<-cbind(PUMF_MAT_i_DA,PUMF_MAT_i_DB,PUMF_MAT_i_B)
# rownames(A_i)<-PUMF_MAT_i$hhid
# rm(PUMF_MAT_i_B,PUMF_MAT_i_DB,PUMF_MAT_i_DA,PUMF_MAT_i,PUMF,temp_i)
# gc()


# ###Bigmemory solution
# PUMF_MAT_i_DA<-big.matrix(nrow=nrow(PUMF_MAT_i),ncol=length(dacat)*length(unique(GIS_i$da_id)),type="integer",init=0,backingfile="PUMF_MAT_i_DA",backingpath = "D:/A2")
# colnames(PUMF_MAT_i_DA)<-paste(paste("DA",sort(rep(unique(GIS_i$da_id),length(dacat))),sep=""),dacat,sep="_")
# temp_i<-PUMF_MAT_i[,c("da_id","hhid",dacat),with=FALSE]
# for (i in unique(GIS_i$da_id)){
#   PUMF_MAT_i_DA[unlist(temp_i[temp_i$da_id==i,"hhid"]),grepl(as.character(i),colnames(PUMF_MAT_i_DA))]<-as.matrix(temp_i[temp_i$da_id==i,dacat,with=FALSE])
#   print(i)
# }
# 
# PUMF_MAT_i_DB<-big.matrix(nrow=nrow(PUMF_MAT_i),ncol=length(dbcat)*length(unique(GIS_i$db_id)),type="integer",init=0,backingfile="PUMF_MAT_i_DB", backingpath = "D:/A2")
# colnames(PUMF_MAT_i_DB)<-paste(paste("DB",sort(rep(unique(GIS_i$db_id),length(dbcat))),sep=""),dbcat,sep="_")
# temp_i<-PUMF_MAT_i[,c("db_id","hhid",dbcat),with=FALSE]
# for (i in unique(GIS_i$db_id)){
#   PUMF_MAT_i_DB[unlist(temp_i[temp_i$db_id==i,"hhid"]),grepl(as.character(i),colnames(PUMF_MAT_i_DB))]<-as.matrix(temp_i[temp_i$db_id==i,dbcat,with=FALSE])
#   print(i)
# }
# 
# PUMF_MAT_i_B<-big.matrix(nrow=nrow(PUMF_MAT_i),ncol=length(bcat)*length(unique(GIS_i$bld_id)),type="integer",init=0,backingfile="PUMF_MAT_i_B", backingpath = "D:/A2")
# colnames(PUMF_MAT_i_B)<-paste(paste("B",sort(rep(unique(GIS_i$bld_id),length(bcat))),sep=""),bcat,sep="_")
# temp_i<-PUMF_MAT_i[,c("bld_id","hhid",bcat),with=FALSE]
# for (i in unique(GIS_i$bld_id)){
#   PUMF_MAT_i_B[unlist(temp_i[temp_i$bld_id==i,"hhid"]),grepl(as.character(i),colnames(PUMF_MAT_i_B))]<-as.matrix(temp_i[temp_i$bld_id==i,bcat,with=FALSE])
#   print(i)
# }


# ###Bigstatsr FBM
# PUMF_MAT_i_DA<-FBM(nrow=nrow(PUMF_MAT_i),ncol=length(dacat)*length(unique(GIS_i$da_id)),type="integer",init=0,backingfile="D:/A2/PUMF_MAT_i_DA")
# attr(PUMF_MAT_i_DA,"fbm_names")<-paste(paste("DA",sort(rep(unique(GIS_i$da_id),length(dacat))),sep=""),dacat,sep="_")
# temp_i<-PUMF_MAT_i[,c("da_id","hhid",dacat),with=FALSE]
# for (i in unique(GIS_i$da_id)){
#   PUMF_MAT_i_DA[unlist(temp_i[temp_i$da_id==i,"hhid"]),grepl(as.character(i),attr(PUMF_MAT_i_DA,"fbm_names"))]<-as.matrix(temp_i[temp_i$da_id==i,dacat,with=FALSE])
#   print(i)
# }
# 
# PUMF_MAT_i_DB<-FBM(nrow=nrow(PUMF_MAT_i),ncol=length(dbcat)*length(unique(GIS_i$db_id)),type="integer",init=0,backingfile="D:/A2/PUMF_MAT_i_DB")
# attr(PUMF_MAT_i_DB,"fbm_names")<-paste(paste("DB",sort(rep(unique(GIS_i$db_id),length(dbcat))),sep=""),dbcat,sep="_")
# temp_i<-PUMF_MAT_i[,c("db_id","hhid",dbcat),with=FALSE]
# for (i in unique(GIS_i$db_id)){
#   PUMF_MAT_i_DB[unlist(temp_i[temp_i$db_id==i,"hhid"]),grepl(as.character(i),attr(PUMF_MAT_i_DB,"fbm_names"))]<-as.matrix(temp_i[temp_i$db_id==i,dbcat,with=FALSE])
#   print(i)
# }
# 
# PUMF_MAT_i_B<-FBM(nrow=nrow(PUMF_MAT_i),ncol=length(bcat)*length(unique(GIS_i$bld_id)),type="integer",init=0,backingfile="D:/A2/PUMF_MAT_i_B")
# attr(PUMF_MAT_i_B,"fbm_names")<-paste(paste("B",sort(rep(unique(GIS_i$bld_id),length(bcat))),sep=""),bcat,sep="_")
# temp_i<-PUMF_MAT_i[,c("bld_id","hhid",bcat),with=FALSE]
# for (i in unique(GIS_i$bld_id)){
#   PUMF_MAT_i_B[unlist(temp_i[temp_i$bld_id==i,"hhid"]),grepl(as.character(i),attr(PUMF_MAT_i_B,"fbm_names"))]<-as.matrix(temp_i[temp_i$bld_id==i,bcat,with=FALSE])
#   print(i)
# }


# ###ff solution
# PUMF_MAT_i_B<-ff(vmode="double",dim=c(nrow(PUMF_MAT_i),length(bcat)*length(unique(GIS_i$bld_id))))


# BT_i$drop_hhdcondo<-0
# BT_i$drop_hhdbuilt<-0
# drop_hhdcondo<-function(x){
#   x[names(BT_i)=="drop_hhdcondo"]<-paste("",gsub(".*\\.","",names(BT_i))[x==0 & grepl("hhdcondo",names(BT_i)) & !grepl("drop",names(BT_i))],collapse="")
# }
# drop_hhdbuilt<-function(x){
#   x[names(BT_i)=="drop_hhdbuilt"]<-paste("",gsub(".*\\.","",names(BT_i))[x==0 & grepl("hhdbuilt",names(BT_i)) & !grepl("drop",names(BT_i))],collapse="")
# }
# BT_i$drop_hhdcondo<-unlist(apply(BT_i,1,drop_hhdcondo))
# BT_i$drop_hhdbuilt<-unlist(apply(BT_i,1,drop_hhdbuilt))

# temp_i<-data.table()
# for (j in GIS_i$bld_id){
#   PUMF_MAT_j<-PUMF_MAT[rowSums(PUMF_MAT[,names(PUMF_MAT)%in%names(BT[BT$geo==j])[BT[BT$geo==j]>0][-1:-2],with=FALSE])>=2,]
#   PUMF_MAT_j<-cbind(rep(j,nrow(PUMF_MAT_j)),PUMF_MAT_j)
#   temp_i<-rbind(temp_i,PUMF_MAT_j)
#   print(j)
# }
# names(temp_i)[1]<-"bld_id"
# temp_i<-merge(temp_i,GIS_i,by="bld_id")
# temp_i$hhid<-1:nrow(temp_i)

# temp<-c()
# for (k in BT_i$geo){
#   temp<-c(temp,paste(paste("B",k,sep=""),bcat,sep="_"))
# }
# categories<-c(paste(paste("DA",DAT_i$geo,sep=""),dacat,sep="_"),paste(paste("DB",DBT_i$geo,sep=""),dbcat,sep="_"),temp)