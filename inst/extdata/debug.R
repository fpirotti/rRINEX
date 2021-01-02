geoc<-NA
for(x in vv2){ 
  nn<-(as.numeric(x))
  nn<- nn[!is.na(nn)]
  if(sum(nn==0)==3) next
  geoc<-nn
}
 
