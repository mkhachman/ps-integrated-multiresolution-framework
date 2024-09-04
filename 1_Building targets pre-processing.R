rm(list=ls())

###Packages
library(data.table)
library(sf)
library(dplyr)
library(stringr)
library(gsubfn)

###Functions
fun <- function(x, y) {
  grepl(x, y)
}
indicators<-function(y){
  #Addresses for each UEV
  agg<-y %>% group_by(UEF16_ID_UEV)%>% summarise(UEF16_ADD = paste(unique(ID_ADRESSE), collapse=','))
  y<-merge(y,agg,by="UEF16_ID_UEV",all=TRUE)
  #UEVs for each address
  agg<-y %>% group_by(ID_ADRESSE)%>% summarise(ADD_UEV = paste(unique(UEF16_ID_UEV), collapse=','))
  y<-merge(y,agg,by="ID_ADRESSE",all=TRUE)
  #UEVs duplicates
  agg <- aggregate(data=y, UEF16_ID_UEV ~ UEF16_ADD, function(x) length(unique(x)))
  names(agg)[2]<-"UEV_Duplicates"
  y<-merge(y,agg,by="UEF16_ADD",all=TRUE)
  #UEVs overlaps
  agg <- aggregate(data=y, UEF16_ADD ~ ID_ADRESSE, function(x) length(unique(x)))
  names(agg)[2]<-"UEV_Overlaps"
  y<-merge(y,agg,by="ID_ADRESSE",all=TRUE)
  agg <- aggregate(data=y,  UEV_Overlaps ~ UEF16_ADD, function(x) max(x))
  names(agg)[2]<-"UEV_Overlaps"
  y<-y[,-"UEV_Overlaps"]
  y<-merge(y,agg,by="UEF16_ADD",all=TRUE)
  #Number of distinct buildings per UEV
  agg <- aggregate(data=y, ID_ADRESSE ~ UEF16_ID_UEV, function(x) length(unique(x)))
  names(agg)[2]<-"distinct_bld_id"
  y<-merge(y, agg, by="UEF16_ID_UEV", all=TRUE)
  #Number of distinct UEVs per building
  agg <- aggregate(data=y, UEF16_ID_UEV ~ ID_ADRESSE, function(x) length(unique(x)))
  names(agg)[2]<-"distinct_id_uev"
  y<-merge(y, agg, by="ID_ADRESSE", all=TRUE)
  return(y)
}
distribution<-function(temp){
  agg<-merge(data.table(UEF16_ID_UEV=unique(temp$UEF16_ID_UEV)),unique.data.frame(data.table(UEF16_ID_UEV=temp$UEF16_ID_UEV,UEF16_NOMBRE_LOG=temp$UEF16_NOMBRE_LOG,distinct_bld_id=temp$distinct_bld_id)), by="UEF16_ID_UEV")
  agg$bld_dwellings<-agg$UEF16_NOMBRE_LOG/agg$distinct_bld_id
  agg<-merge(unique.data.frame(data.table(ID_ADRESSE=temp$ID_ADRESSE,UEF16_ID_UEV=temp$UEF16_ID_UEV)),agg,by="UEF16_ID_UEV")
  agg<-data.table(bld_id=agg$ID_ADRESSE,bld_dwellings=agg$bld_dwellings)
  agg<-data.table(aggregate(data=agg, bld_dwellings ~ bld_id, function(x) sum(x)))
  agg<-data.table(merge(agg,unique.data.frame(data.table(bld_id=temp$ID_ADRESSE,bld_consyr=temp$UEF16_ANNEE_CONS,bld_type=temp$UEF16_CATEGORIE_))))
  agg<-merge(agg,data.table(aggregate(data=agg, bld_consyr ~ bld_id, function(x) length(unique(x)))),by="bld_id",all=TRUE)
  names(agg)[ncol(agg)]<-"count_bld_consyr"
  names(agg)[names(agg)=="bld_consyr.x"]<-"bld_consyr"
  agg<-merge(agg,data.table(aggregate(data=agg, bld_type ~ bld_id, function(x) length(unique(x)))),by="bld_id",all=TRUE)
  names(agg)[ncol(agg)]<-"count_bld_types"
  names(agg)[names(agg)=="bld_type.x"]<-"bld_type"
  agg<-agg[!(agg$count_bld_consyr>1 & agg$bld_consyr==9999)]
  agg<-merge(agg,data.table(aggregate(data=agg, bld_consyr ~ bld_id, function(x) max(x))), by="bld_id",all=TRUE)
  names(agg)[ncol(agg)]<-"max_bld_consyr"
  names(agg)[names(agg)=="bld_consyr.x"]<-"bld_consyr"
  agg$bld_consyr<-agg$max_bld_consyr
  agg$bld_type[agg$count_bld_types>1]<-"Condominium"
  temp<-unique.data.frame(agg[,1:4])
  return(temp)
}

###Addresses16
Addresses16<-fread("Input/Addresses/Addresses16.csv")
#Rename UEF16 variables with a prefix
names(Addresses16)[15:30]<-paste("UEF16_",names(Addresses16)[15:30],sep="")
#Remove non-residential addresses
Addresses16<-Addresses16[!is.na(Addresses16$UEF16_NOMBRE_LOG)]
#Keep unique rows only (remove UEFs duplicated by error i.e. with the same ID and same characteristics)
Addresses16<-unique.data.frame(Addresses16)
#Correcting special characters
Addresses16$SPECIFIQUE<-gsub("ÃƒÂ´","o",Addresses16$SPECIFIQUE) #ô
Addresses16$UEF16_NOM_RUE<-gsub("Ã´","o",Addresses16$UEF16_NOM_RUE) #ô
Addresses16$SPECIFIQUE<-gsub("ÃƒÂ©","e",Addresses16$SPECIFIQUE) #é
Addresses16$UEF16_NOM_RUE<-gsub("Ã©","e",Addresses16$UEF16_NOM_RUE) #é
Addresses16$SPECIFIQUE<-gsub("Ãƒâ€°","E",Addresses16$SPECIFIQUE) # é majuscule
Addresses16$UEF16_NOM_RUE<-gsub("Ã‰","E",Addresses16$UEF16_NOM_RUE) # é majuscule
Addresses16$SPECIFIQUE<-gsub("ÃƒÂ¨","e",Addresses16$SPECIFIQUE) #è
Addresses16$UEF16_NOM_RUE<-gsub("Ã¨","e",Addresses16$UEF16_NOM_RUE) #è
Addresses16$SPECIFIQUE<-gsub("ÃƒÂª","e",Addresses16$SPECIFIQUE) #ê
Addresses16$UEF16_NOM_RUE<-gsub("Ãª","e",Addresses16$UEF16_NOM_RUE) #ê
Addresses16$SPECIFIQUE<-gsub("ÃƒÂ¢","a",Addresses16$SPECIFIQUE) #â
Addresses16$UEF16_NOM_RUE<-gsub("Ã¢","a",Addresses16$UEF16_NOM_RUE) #â
Addresses16$SPECIFIQUE<-gsub("ÃƒÂ®","i",Addresses16$SPECIFIQUE) #î
Addresses16$UEF16_NOM_RUE<-gsub("Ã®","i",Addresses16$UEF16_NOM_RUE) #î
Addresses16$SPECIFIQUE<-gsub("ÃƒÂ¯","i",Addresses16$SPECIFIQUE) #î
Addresses16$UEF16_NOM_RUE<-gsub("Ã¯","i",Addresses16$UEF16_NOM_RUE) #î
Addresses16$SPECIFIQUE<-gsub("ÃƒÂ§","c",Addresses16$SPECIFIQUE) #c
Addresses16$UEF16_NOM_RUE<-gsub("Ã§","c",Addresses16$UEF16_NOM_RUE) #c



###Buildings' totals
BT<-data.table(bld_id=sort(unique(Addresses16$ID_ADRESSE)))
#One building only on one or multiple UEVs without overlaps
Addresses16<-indicators(Addresses16)
temp<-Addresses16[Addresses16$UEV_Overlaps==1 & Addresses16$distinct_bld_id==1]
Addresses16<-Addresses16[!Addresses16$UEF16_ID_UEV%in%temp$UEF16_ID_UEV]
temp<-temp[,-c("UEF16_ADD","ADD_UEV","UEV_Duplicates","UEV_Overlaps","distinct_bld_id","distinct_id_uev")]
temp<-indicators(temp)
temp<-distribution(temp)
Result<-merge(BT,temp,by="bld_id")

#Exactly corresponding addresses between buildings and UEVs
temp<-Addresses16[Addresses16$ADDR_DE==Addresses16$UEF16_CIVIQUE_DE & Addresses16$ADDR_A==Addresses16$UEF16_CIVIQUE_FI & mapply(fun, Addresses16$SPECIFIQUE, Addresses16$UEF16_NOM_RUE)]
Addresses16<-Addresses16[!Addresses16$UEF16_ID_UEV%in%temp$UEF16_ID_UEV]
temp<-temp[,-c("UEF16_ADD","ADD_UEV","UEV_Duplicates","UEV_Overlaps","distinct_bld_id","distinct_id_uev")]
temp<-indicators(temp)
temp<-distribution(temp)
Result<-rbind(Result,merge(BT,temp,by="bld_id"))

#Building addresses comprising UEVs addresses
temp<-Addresses16[Addresses16$ADDR_DE<=Addresses16$UEF16_CIVIQUE_DE & Addresses16$ADDR_A>=Addresses16$UEF16_CIVIQUE_FI & mapply(fun, Addresses16$SPECIFIQUE, Addresses16$UEF16_NOM_RUE)]
Addresses16<-Addresses16[!Addresses16$UEF16_ID_UEV%in%temp$UEF16_ID_UEV]
temp<-temp[,-c("UEF16_ADD","ADD_UEV","UEV_Duplicates","UEV_Overlaps","distinct_bld_id","distinct_id_uev")]
temp<-indicators(temp)
temp<-distribution(temp)
Result<-rbind(Result,merge(BT,temp,by="bld_id"))

#One or multiple buildings on one or multiple UEVs without overlaps
Addresses16<-Addresses16[,-c("UEF16_ADD","ADD_UEV","UEV_Duplicates","UEV_Overlaps","distinct_bld_id","distinct_id_uev")]
Addresses16<-indicators(Addresses16)
temp<-Addresses16[Addresses16$UEV_Overlaps==1]
Addresses16<-Addresses16[!Addresses16$UEF16_ID_UEV%in%temp$UEF16_ID_UEV]
temp<-temp[,-c("UEF16_ADD","ADD_UEV","UEV_Duplicates","UEV_Overlaps","distinct_bld_id","distinct_id_uev")]
temp<-indicators(temp)
temp<-distribution(temp)
Result<-rbind(Result,merge(BT,temp,by="bld_id"))

#Building addresses and UEVs addresses have the same road name
temp<-Addresses16[mapply(fun, Addresses16$SPECIFIQUE, Addresses16$UEF16_NOM_RUE)]
Addresses16<-Addresses16[!Addresses16$UEF16_ID_UEV%in%temp$UEF16_ID_UEV]
temp<-temp[,-c("UEF16_ADD","ADD_UEV","UEV_Duplicates","UEV_Overlaps","distinct_bld_id","distinct_id_uev")]
temp<-indicators(temp)
temp<-distribution(temp)
Result<-rbind(Result,merge(BT,temp,by="bld_id"))

#One or multiple buildings on one or multiple UEVs without overlaps
Addresses16<-Addresses16[,-c("UEF16_ADD","ADD_UEV","UEV_Duplicates","UEV_Overlaps","distinct_bld_id","distinct_id_uev")]
Addresses16<-indicators(Addresses16)
temp<-Addresses16[Addresses16$UEV_Overlaps==1]
Addresses16<-Addresses16[!Addresses16$UEF16_ID_UEV%in%temp$UEF16_ID_UEV]
temp<-temp[,-c("UEF16_ADD","ADD_UEV","UEV_Duplicates","UEV_Overlaps","distinct_bld_id","distinct_id_uev")]
temp<-indicators(temp)
temp<-distribution(temp)
Result<-rbind(Result,merge(BT,temp,by="bld_id"))

###BT processing
#Group dwellings for the same address in a single row
BT<-data.table(bld_id=unique(Result$bld_id))
agg<-aggregate(data=Result, bld_dwellings ~ bld_id,function(x) sum(x))
BT<-merge(BT,agg,by="bld_id")
BT<-merge(BT,data.table(bld_id=Result$bld_id,bld_consyr=Result$bld_consyr, bld_type=Result$bld_type), by="bld_id")
#Harmonize BT for multiple construction years and/or building types
BT<-merge(BT,data.table(aggregate(data=BT, bld_consyr ~ bld_id, function(x) length(unique(x)))),by="bld_id",all=TRUE)
names(BT)[ncol(BT)]<-"count_bld_consyr"
names(BT)[names(BT)=="bld_consyr.x"]<-"bld_consyr"
BT<-merge(BT,data.table(aggregate(data=BT, bld_type ~ bld_id, function(x) length(unique(x)))),by="bld_id",all=TRUE)
names(BT)[ncol(BT)]<-"count_bld_types"
names(BT)[names(BT)=="bld_type.x"]<-"bld_type"
BT<-BT[!(BT$count_bld_consyr>1 & BT$bld_consyr==9999)]
BT<-merge(BT,data.table(aggregate(data=BT, bld_consyr ~ bld_id, function(x) max(x))), by="bld_id",all=TRUE)
names(BT)[ncol(BT)]<-"max_bld_consyr"
names(BT)[names(BT)=="bld_consyr.x"]<-"bld_consyr"
BT$bld_consyr<-BT$max_bld_consyr
BT$bld_type[BT$count_bld_types>1]<-"Condominium"
BT<-unique.data.frame(BT[,1:4])
#Fix Regular bld_type
BT$bld_type[BT$bld_type=="RÃ©gulier"]<-"Regular"
#Add coordinates
Addresses16<-fread("Input/Addresses/Addresses16.csv")
names(Addresses16)[15:30]<-paste("UEF16_",names(Addresses16)[15:30],sep="")
Addresses16<-Addresses16[!is.na(Addresses16$UEF16_NOMBRE_LOG)]
Addresses16<-unique.data.frame(Addresses16)
Addresses16$SPECIFIQUE<-gsub("ÃƒÂ´","o",Addresses16$SPECIFIQUE) #ô
Addresses16$UEF16_NOM_RUE<-gsub("Ã´","o",Addresses16$UEF16_NOM_RUE) #ô
Addresses16$SPECIFIQUE<-gsub("ÃƒÂ©","e",Addresses16$SPECIFIQUE) #é
Addresses16$UEF16_NOM_RUE<-gsub("Ã©","e",Addresses16$UEF16_NOM_RUE) #é
Addresses16$SPECIFIQUE<-gsub("Ãƒâ€°","E",Addresses16$SPECIFIQUE) # é majuscule
Addresses16$UEF16_NOM_RUE<-gsub("Ã‰","E",Addresses16$UEF16_NOM_RUE) # é majuscule
Addresses16$SPECIFIQUE<-gsub("ÃƒÂ¨","e",Addresses16$SPECIFIQUE) #è
Addresses16$UEF16_NOM_RUE<-gsub("Ã¨","e",Addresses16$UEF16_NOM_RUE) #è
Addresses16$SPECIFIQUE<-gsub("ÃƒÂª","e",Addresses16$SPECIFIQUE) #ê
Addresses16$UEF16_NOM_RUE<-gsub("Ãª","e",Addresses16$UEF16_NOM_RUE) #ê
Addresses16$SPECIFIQUE<-gsub("ÃƒÂ¢","a",Addresses16$SPECIFIQUE) #â
Addresses16$UEF16_NOM_RUE<-gsub("Ã¢","a",Addresses16$UEF16_NOM_RUE) #â
Addresses16$SPECIFIQUE<-gsub("ÃƒÂ®","i",Addresses16$SPECIFIQUE) #î
Addresses16$UEF16_NOM_RUE<-gsub("Ã®","i",Addresses16$UEF16_NOM_RUE) #î
Addresses16$SPECIFIQUE<-gsub("ÃƒÂ¯","i",Addresses16$SPECIFIQUE) #î
Addresses16$UEF16_NOM_RUE<-gsub("Ã¯","i",Addresses16$UEF16_NOM_RUE) #î
Addresses16$SPECIFIQUE<-gsub("ÃƒÂ§","c",Addresses16$SPECIFIQUE) #c
Addresses16$UEF16_NOM_RUE<-gsub("Ã§","c",Addresses16$UEF16_NOM_RUE) #c

BT<-merge(BT,unique.data.frame(data.table(bld_id=Addresses16$ID_ADRESSE,bld_lat=Addresses16$Latitude,bld_lon=Addresses16$Longitude)), by="bld_id", all.x=TRUE)
write.csv(BT,"Output/BT.csv")
