library(tidyverse)
library(dplyr)
library(tidyr)
library(hablar) # for the special sum_ function. if all elements are na, return na, if some but not all elements are na, ignore the na when summing.

# Apply the crosswalk to align tracts in ACS 2010-2014 to census 2000 boundaries
crosswalk<-read.csv(".././boundaries_matching/crosswalk_2010_2000.csv")
crosswalk$trtid00<-as.character(crosswalk$trtid00)
crosswalk2<-crosswalk%>%group_by(trtid00)%>%summarise(trtid10,w=weight/sum(weight))
acs1014_CA<-read.csv("./data/acs1014_CA_tract2.csv")
acs1014_CA2<-acs1014_CA[c(1,2,seq(66,106))]
m<-merge(x=acs1014_CA2,y=crosswalk2,by.x='GEOID',by.y='trtid10',all.x=T)
m2<-cbind(m[c(1,2,44)],m[c(seq(3,43))]*m$w)
m3<-m2%>%group_by(trtid00)%>%summarise_at(vars(p_poverty:p_crowding),sum_)
m3<-m3%>%rename(GEOID=trtid00)
write.csv(m3,"./data/acs1014_CA_2000tract_ttt.csv",row.names = F)


# Apply the crosswalk to align tracts in ACS 2004-2009 to census 2010 boundaries
# crosswalk<-read.csv(".././boundaries_matching/crosswalk_2000_2010.csv")
# acs0509_CA<-read.csv("./data/acs0509_CA_tract2.csv")
# acs0509_CA2<-acs0509_CA[c(1,2,seq(66,106))]
# m<-merge(x=acs0509_CA2,y=crosswalk,by.x='GEOID',by.y='trtid00',all.x=T)
# m2<-cbind(m[c(1,2,45)],m[c(seq(3,44))]*m$weight)
# m3<-m2%>%group_by(trtid10)%>%summarise_at(vars(p_poverty:p_crowding),sum_)
# m3<-m3%>%rename(GEOID=trtid10)
# write.csv(m3,"./data/acs0509_CA_2010tract.csv",row.names = F)
# 
# census2000_CA<-read.csv("./data/census2000_CA_tract2.csv")
# census2000_CA2<-census2000_CA[c(1,2,seq(73,112))]
# m<-merge(x=census2000_CA2,y=crosswalk,by.x='GEOID',by.y='trtid00',all.x=T)
# m2<-cbind(m[c(1,2,43)],m[c(seq(3,42))]*m$weight)
# m3<-m2%>%group_by(trtid10)%>%summarise_at(vars(p_poverty:p_crowding),sum_)
# m3<-m3%>%rename(GEOID=trtid10)
# write.csv(m3,"./data/census2000_CA_2010tract.csv",row.names = F)


