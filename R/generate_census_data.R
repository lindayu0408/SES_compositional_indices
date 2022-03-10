library(tidycensus)
library(tidyverse)
library(dplyr)
library(tidyr)
library(sf)

census_api_key("xxxxx")
acs0509_v<-read.csv("./data/acs0509_variables.csv") # a list of raw indicators from US Census ACS 2005-2009 5 year estimates
acs1014_v<-read.csv("./data/acs1014_variables.csv") # a list of raw indicators from US Census ACS 2010-2014 5 year estimates
census2000_v<-read.csv("./data/census2000_variables.csv") # a list of raw indicators from US Census 2000

#ACS 2005-2009
acs0509_code<-as.character(acs0509_v[,1])
acs0509.CA.tract<-get_acs(state = "CA", geography = "tract",variables = acs0509_code,survey="acs5",year=2009, geometry = F)
acs0509.CA.tract.wide<-spread(acs0509.CA.tract[,1:4],variable,estimate)
# set missing values to NA
acs0509.CA.tract.wide[acs0509.CA.tract.wide==666666666]<-NA
# write the raw values of indicators from US Census ACS 2005-2009 5 year estimates to a csv file
write.csv(acs0509.CA.tract.wide,"./data/acs0509_CA_tract.csv",row.names = F)

# write statistical summary of indicators from US Census ACS 2005-2009 5 year estimates to a csv file
acs0509.CA.tract.summary<-data.frame(unclass(summary(acs0509.CA.tract.wide)), check.names = FALSE, stringsAsFactors = FALSE)
write.csv(acs0509.CA.tract.summary,"./data/acs0509_CA_tract_summary.csv",row.names = F)

# ACS 2010-2014
acs1014_code<-as.character(acs1014_v[,1])
acs1014.CA.tract<-get_acs(state = "CA", geography = "tract",variables = acs1014_code,survey="acs5",year=2014, geometry = F)
acs1014.CA.tract.wide<-spread(acs1014.CA.tract[,1:4],variable,estimate)

# write the raw values of indicators from US Census ACS 2010-2014 5 year estimates to a csv file
write.csv(acs1014.CA.tract.wide,"./data/acs1014_CA_tract.csv",row.names = F)

# write statistical summary of indicators from US Census ACS 2010-2014 5 year estimates to a csv file
acs1014.CA.tract.summary<-data.frame(unclass(summary(acs1014.CA.tract.wide)), check.names = FALSE, stringsAsFactors = FALSE)
write.csv(acs1014.CA.tract.summary,"./data/acs1014_CA_tract_summary.csv",row.names = F)

# Census 2000
census2000_sf3profile<-census2000_v%>%filter(dataset=="sf3profile")
census2000_sf3profile_code<-as.character(census2000_sf3profile[,1])

census2000_sf3<-census2000_v%>%filter(dataset=="sf3")
census2000_sf3_code<-as.character(census2000_sf3[,1])

census2000_sf4profile<-census2000_v%>%filter(dataset=="sf4profile")
census2000_sf4profile_code<-as.character(census2000_sf4profile[,1])

census2000_sf1<-census2000_v%>%filter(dataset=="sf1")
census2000_sf1_code<-as.character(census2000_sf1[,1])

census2000_sf1.CA.tract<-get_decennial(state = "CA", geography = "tract",variables = census2000_sf1_code,year=2000,sumfile="sf1", geometry = F)
census2000_sf1.CA.tract.wide<-spread(census2000_sf1.CA.tract[,1:4],variable,value)

census2000_sf3.CA.tract<-get_decennial(state = "CA", geography = "tract",variables = census2000_sf3_code,year=2000,sumfile="sf3", geometry = F)
census2000_sf3.CA.tract.wide<-spread(census2000_sf3.CA.tract[,1:4],variable,value)

census2000_sf3profile.CA.tract<-get_decennial(state = "CA", geography = "tract",variables = census2000_sf3profile_code,year=2000,sumfile="sf3profile", geometry = F)
census2000_sf3profile.CA.tract.wide<-spread(census2000_sf3profile.CA.tract[,1:4],variable,value)

census2000_sf4profile.CA.tract<-get_decennial(state = "CA",geography = "tract",variables = census2000_sf4profile_code,year=2000,sumfile="sf4profile", geometry = F)
census2000_sf4profile.CA.tract.wide<-spread(census2000_sf4profile.CA.tract[,1:4],variable,value)

census2000.CA.tract<-merge(x=census2000_sf1.CA.tract.wide,y=census2000_sf3.CA.tract.wide,by='GEOID')
census2000.CA.tract <- subset(census2000.CA.tract, select = -c(NAME.y) )
census2000.CA.tract<-merge(x=census2000.CA.tract,y=census2000_sf3profile.CA.tract.wide,by='GEOID',all.x=T)
census2000.CA.tract <- subset(census2000.CA.tract, select = -c(NAME) )
census2000.CA.tract<-merge(x=census2000.CA.tract,y=census2000_sf4profile.CA.tract.wide,by='GEOID',all.x=T)
census2000.CA.tract <- subset(census2000.CA.tract, select = -c(NAME) )

census2000.CA.tract[census2000.CA.tract==-999]<-NA
# write the raw values of indicators from US Census 2000 to a csv file
write.csv(census2000.CA.tract,"./data/census2000_CA_tract.csv",row.names = F)
# write statistical summary of indicators from US Census 2000 to a csv file
census2000.CA.tract.summary<-data.frame(unclass(summary(census2000.CA.tract)), check.names = FALSE, stringsAsFactors = FALSE)
write.csv(census2000.CA.tract.summary,"./data/census2000_CA_tract_summary.csv",row.names = F)

# generate indicators
#acs0509
acs0509.CA.tract.wide$p_poverty<-acs0509.CA.tract.wide$DP03_0103
acs0509.CA.tract.wide$p_assistance<-acs0509.CA.tract.wide$DP03_0073P
acs0509.CA.tract.wide$p_unemployed<-acs0509.CA.tract.wide$DP03_0009
acs0509.CA.tract.wide$p_female_headed1<-acs0509.CA.tract.wide$DP02_0009P # among all households
acs0509.CA.tract.wide$p_female_headed2<-acs0509.CA.tract.wide$DP02_0009/acs0509.CA.tract.wide$DP02_0013*100 # among households with children
acs0509.CA.tract.wide$p_female_headed3<-acs0509.CA.tract.wide$DP02_0009/acs0509.CA.tract.wide$DP02_0003*100 # among family households with children
acs0509.CA.tract.wide$p_black<-acs0509.CA.tract.wide$DP05_0073P
acs0509.CA.tract.wide$p_hispanic<-acs0509.CA.tract.wide$DP05_0066P
acs0509.CA.tract.wide$p_foreign<-acs0509.CA.tract.wide$DP02_0092P
acs0509.CA.tract.wide$p_under_5age<-acs0509.CA.tract.wide$DP05_0004P
acs0509.CA.tract.wide$p_resident_5yr1<-100-acs0509.CA.tract.wide$DP04_0050P # moved in before 2005
acs0509.CA.tract.wide$p_resident_5yr2<-100-acs0509.CA.tract.wide$DP04_0050P-acs0509.CA.tract.wide$DP04_0051P # moved in before 2000
acs0509.CA.tract.wide$p_owner_occupied<-acs0509.CA.tract.wide$DP04_0045P
acs0509.CA.tract.wide$p_renter_occupied<-acs0509.CA.tract.wide$DP04_0046P
acs0509.CA.tract.wide$p_more75000_hincome<-acs0509.CA.tract.wide$DP03_0059P+acs0509.CA.tract.wide$DP03_0060P+acs0509.CA.tract.wide$DP03_0061P+acs0509.CA.tract.wide$DP03_0062P # household income
acs0509.CA.tract.wide$p_more50000_hincome<-acs0509.CA.tract.wide$DP03_0058P+acs0509.CA.tract.wide$DP03_0059P+acs0509.CA.tract.wide$DP03_0060P+acs0509.CA.tract.wide$DP03_0061P+acs0509.CA.tract.wide$DP03_0062P # household income
acs0509.CA.tract.wide$p_less10000_hincome<-acs0509.CA.tract.wide$DP03_0053P # household income
acs0509.CA.tract.wide$p_more75000_fincome<-acs0509.CA.tract.wide$DP03_0083P+acs0509.CA.tract.wide$DP03_0084P+acs0509.CA.tract.wide$DP03_0085P+acs0509.CA.tract.wide$DP03_0086P # family income
acs0509.CA.tract.wide$p_more50000_fincome<-acs0509.CA.tract.wide$DP03_0082P+acs0509.CA.tract.wide$DP03_0083P+acs0509.CA.tract.wide$DP03_0084P+acs0509.CA.tract.wide$DP03_0085P+acs0509.CA.tract.wide$DP03_0086P # family income
acs0509.CA.tract.wide$p_less10000_fincome<-acs0509.CA.tract.wide$DP03_0077P # household income
acs0509.CA.tract.wide$p_9yr_edu<-acs0509.CA.tract.wide$DP02_0059P
acs0509.CA.tract.wide$p_high_school1<-acs0509.CA.tract.wide$DP02_0066 
acs0509.CA.tract.wide$p_high_school2<-acs0509.CA.tract.wide$DP02_0061P+ acs0509.CA.tract.wide$DP02_0062P+acs0509.CA.tract.wide$DP02_0063P+acs0509.CA.tract.wide$DP02_0064P+acs0509.CA.tract.wide$DP02_0065P
acs0509.CA.tract.wide$p_high_school_less<-100-acs0509.CA.tract.wide$p_high_school1
acs0509.CA.tract.wide$p_bachelor1<-acs0509.CA.tract.wide$DP02_0067
acs0509.CA.tract.wide$p_bachelor2<-acs0509.CA.tract.wide$DP02_0064P+acs0509.CA.tract.wide$DP02_0065P
acs0509.CA.tract.wide$p_college<-acs0509.CA.tract.wide$DP02_0062P+acs0509.CA.tract.wide$DP02_0063P+acs0509.CA.tract.wide$DP02_0064P+acs0509.CA.tract.wide$DP02_0065P
acs0509.CA.tract.wide$p_prof_occupation<-acs0509.CA.tract.wide$DP03_0027P
acs0509.CA.tract.wide$p_adults<-acs0509.CA.tract.wide$DP05_0018P
acs0509.CA.tract.wide$population<-acs0509.CA.tract.wide$DP05_0001
acs0509.CA.tract.wide$median_fincome<-acs0509.CA.tract.wide$DP03_0087
acs0509.CA.tract.wide$median_hvalue<-acs0509.CA.tract.wide$DP04_0088
acs0509.CA.tract.wide$median_rent<-acs0509.CA.tract.wide$DP04_0132
acs0509.CA.tract.wide$median_mortage<-acs0509.CA.tract.wide$DP04_0100
acs0509.CA.tract.wide$p_150poverty<-(acs0509.CA.tract.wide$C17002_002+acs0509.CA.tract.wide$C17002_003+acs0509.CA.tract.wide$C17002_004+acs0509.CA.tract.wide$C17002_005)/acs0509.CA.tract.wide$C17002_001*100
acs0509.CA.tract.wide$p_single_parents1<-(acs0509.CA.tract.wide$DP02_0007+acs0509.CA.tract.wide$DP02_0009)/acs0509.CA.tract.wide$DP02_0013*100 # among households with children
acs0509.CA.tract.wide$p_single_parents2<-(acs0509.CA.tract.wide$DP02_0007+acs0509.CA.tract.wide$DP02_0009)/acs0509.CA.tract.wide$DP02_0003*100 # among family households with children
acs0509.CA.tract.wide$p_novehicle<-acs0509.CA.tract.wide$DP04_0057P
acs0509.CA.tract.wide$p_nophone<-acs0509.CA.tract.wide$DP04_0074P
acs0509.CA.tract.wide$p_noplumbing<-acs0509.CA.tract.wide$DP04_0072P
acs0509.CA.tract.wide$p_crowding<-acs0509.CA.tract.wide$DP04_0077P+acs0509.CA.tract.wide$DP04_0078P
acs0509.CA.tract.summary2<-data.frame(unclass(summary(acs0509.CA.tract.wide)), check.names = FALSE, stringsAsFactors = FALSE)
write.csv(acs0509.CA.tract.wide,"./data/acs0509_CA_tract2.csv",row.names = F)
write.csv(acs0509.CA.tract.summary2,"./data/acs0509_CA_tract_summary2.csv",row.names = F)

#acs1014
acs1014.CA.tract.wide$p_poverty<-acs1014.CA.tract.wide$DP03_0119P
acs1014.CA.tract.wide$p_assistance<-acs1014.CA.tract.wide$DP03_0072P
acs1014.CA.tract.wide$p_unemployed<-acs1014.CA.tract.wide$DP03_0009P
acs1014.CA.tract.wide$p_female_headed1<-acs1014.CA.tract.wide$DP02_0009P # among all households
acs1014.CA.tract.wide$p_female_headed2<-acs1014.CA.tract.wide$DP02_0009/acs1014.CA.tract.wide$DP02_0013*100 # among households with children
acs1014.CA.tract.wide$p_female_headed3<-acs1014.CA.tract.wide$DP02_0009/acs1014.CA.tract.wide$DP02_0003*100 # among family households with children
acs1014.CA.tract.wide$p_black<-acs1014.CA.tract.wide$DP05_0073P
acs1014.CA.tract.wide$p_hispanic<-acs1014.CA.tract.wide$DP05_0066P
acs1014.CA.tract.wide$p_foreign<-acs1014.CA.tract.wide$DP02_0092P
acs1014.CA.tract.wide$p_under_5age<-acs1014.CA.tract.wide$DP05_0004P
acs1014.CA.tract.wide$p_resident_5yr1<-100-acs1014.CA.tract.wide$DP04_0050P # moved in before 2010
acs1014.CA.tract.wide$p_resident_5yr2<-100-acs1014.CA.tract.wide$DP04_0050P-acs1014.CA.tract.wide$DP04_0051P # moved in before 2005
acs1014.CA.tract.wide$p_owner_occupied<-acs1014.CA.tract.wide$DP04_0045P
acs1014.CA.tract.wide$p_renter_occupied<-acs1014.CA.tract.wide$DP04_0046P
acs1014.CA.tract.wide$p_more75000_hincome<-acs1014.CA.tract.wide$DP03_0058P+acs1014.CA.tract.wide$DP03_0059P+acs1014.CA.tract.wide$DP03_0060P+acs1014.CA.tract.wide$DP03_0061P # household income
acs1014.CA.tract.wide$p_more50000_hincome<-acs1014.CA.tract.wide$DP03_0057P+acs1014.CA.tract.wide$DP03_0058P+acs1014.CA.tract.wide$DP03_0059P+acs1014.CA.tract.wide$DP03_0060P+acs1014.CA.tract.wide$DP03_0061P # household income
acs1014.CA.tract.wide$p_less10000_hincome<-acs1014.CA.tract.wide$DP03_0052P # household income
acs1014.CA.tract.wide$p_more75000_fincome<-acs1014.CA.tract.wide$DP03_0082P+acs1014.CA.tract.wide$DP03_0083P+acs1014.CA.tract.wide$DP03_0084P+acs1014.CA.tract.wide$DP03_0085P # family income
acs1014.CA.tract.wide$p_more50000_fincome<-acs1014.CA.tract.wide$DP03_0081P+acs1014.CA.tract.wide$DP03_0082P+acs1014.CA.tract.wide$DP03_0083P+acs1014.CA.tract.wide$DP03_0084P+acs1014.CA.tract.wide$DP03_0085P # family income
acs1014.CA.tract.wide$p_less10000_fincome<-acs1014.CA.tract.wide$DP03_0076P # family income
acs1014.CA.tract.wide$p_9yr_edu<-acs1014.CA.tract.wide$DP02_0059P
acs1014.CA.tract.wide$p_high_school1<-acs1014.CA.tract.wide$DP02_0066P
acs1014.CA.tract.wide$p_high_school2<-acs1014.CA.tract.wide$DP02_0061P+ acs1014.CA.tract.wide$DP02_0062P+acs1014.CA.tract.wide$DP02_0063P+acs1014.CA.tract.wide$DP02_0064P+acs1014.CA.tract.wide$DP02_0065P
acs1014.CA.tract.wide$p_high_school_less<-100-acs1014.CA.tract.wide$p_high_school1
acs1014.CA.tract.wide$p_bachelor1<-acs1014.CA.tract.wide$DP02_0067P
acs1014.CA.tract.wide$p_bachelor2<-acs1014.CA.tract.wide$DP02_0064P+acs1014.CA.tract.wide$DP02_0065P
acs1014.CA.tract.wide$p_college<-acs1014.CA.tract.wide$DP02_0062P+acs1014.CA.tract.wide$DP02_0063P+acs1014.CA.tract.wide$DP02_0064P+acs1014.CA.tract.wide$DP02_0065P
acs1014.CA.tract.wide$p_prof_occupation<-acs1014.CA.tract.wide$DP03_0027P
acs1014.CA.tract.wide$p_adults<-acs1014.CA.tract.wide$DP05_0018P
acs1014.CA.tract.wide$population<-acs1014.CA.tract.wide$DP05_0001
acs1014.CA.tract.wide$median_fincome<-acs1014.CA.tract.wide$DP03_0086
acs1014.CA.tract.wide$median_hvalue<-acs1014.CA.tract.wide$DP04_0088
acs1014.CA.tract.wide$median_rent<-acs1014.CA.tract.wide$DP04_0132
acs1014.CA.tract.wide$median_mortage<-acs1014.CA.tract.wide$DP04_0100
acs1014.CA.tract.wide$p_150poverty<-(acs1014.CA.tract.wide$C17002_002+acs1014.CA.tract.wide$C17002_003+acs1014.CA.tract.wide$C17002_004+acs1014.CA.tract.wide$C17002_005)/acs1014.CA.tract.wide$C17002_001*100
acs1014.CA.tract.wide$p_single_parents1<-(acs1014.CA.tract.wide$DP02_0007+acs1014.CA.tract.wide$DP02_0009)/acs1014.CA.tract.wide$DP02_0013*100 # among households with children
acs1014.CA.tract.wide$p_single_parents2<-(acs1014.CA.tract.wide$DP02_0007+acs1014.CA.tract.wide$DP02_0009)/acs1014.CA.tract.wide$DP02_0003*100 # among family households with children
acs1014.CA.tract.wide$p_novehicle<-acs1014.CA.tract.wide$DP04_0057P
acs1014.CA.tract.wide$p_nophone<-acs1014.CA.tract.wide$DP04_0074P
acs1014.CA.tract.wide$p_noplumbing<-acs1014.CA.tract.wide$DP04_0072P
acs1014.CA.tract.wide$p_crowding<-acs1014.CA.tract.wide$DP04_0077P+acs1014.CA.tract.wide$DP04_0078P
acs1014.CA.tract.summary2<-data.frame(unclass(summary(acs1014.CA.tract.wide)), check.names = FALSE, stringsAsFactors = FALSE)
write.csv(acs1014.CA.tract.wide,"./data/acs1014_CA_tract2.csv",row.names = F)
write.csv(acs1014.CA.tract.summary2,"./data/acs1014_CA_tract_summary2.csv",row.names = F)

#2000 census
census2000.CA.tract$p_poverty<-census2000.CA.tract$DP3_C155
census2000.CA.tract$p_assistance<-census2000.CA.tract$DP3_C123
census2000.CA.tract$p_unemployed<-census2000.CA.tract$DP3_C8/census2000.CA.tract$DP3_C4*100
census2000.CA.tract$p_female_headed1<-census2000.CA.tract$DP1_C88 # among all households
census2000.CA.tract$p_female_headed2<-census2000.CA.tract$DP1_C87/census2000.CA.tract$DP1_C95*100 # among households with children
census2000.CA.tract$p_female_headed3<-census2000.CA.tract$DP1_C87/census2000.CA.tract$DP1_C79*100 # among family households with children
census2000.CA.tract$p_black<-census2000.CA.tract$P005004/census2000.CA.tract$P005001*100
census2000.CA.tract$p_hispanic<-census2000.CA.tract$P004002/census2000.CA.tract$P005001*100
census2000.CA.tract$p_foreign<-census2000.CA.tract$DP2_C99
census2000.CA.tract$p_under_5age<-census2000.CA.tract$DP1_C7
census2000.CA.tract$p_resident_5yr<-100-census2000.CA.tract$DP4_C56-census2000.CA.tract$DP4_C58 # moved in before 1995
census2000.CA.tract$p_owner_occupied<-census2000.CA.tract$DP1_C104
census2000.CA.tract$p_renter_occupied<-census2000.CA.tract$DP1_C106
census2000.CA.tract$p_more75000_hincome<-census2000.CA.tract$DP3_C105+census2000.CA.tract$DP3_C107+census2000.CA.tract$DP3_C109+census2000.CA.tract$DP3_C111 # household income
census2000.CA.tract$p_more50000_hincome<-census2000.CA.tract$DP3_C103+census2000.CA.tract$DP3_C105+census2000.CA.tract$DP3_C107+census2000.CA.tract$DP3_C109+census2000.CA.tract$DP3_C111 # household income
census2000.CA.tract$p_less10000_hincome<-census2000.CA.tract$DP3_C93 # household income
census2000.CA.tract$p_more75000_fincome<-census2000.CA.tract$DP3_C143+census2000.CA.tract$DP3_C145+census2000.CA.tract$DP3_C147+census2000.CA.tract$DP3_C149
census2000.CA.tract$p_more50000_fincome<-census2000.CA.tract$DP3_C141+census2000.CA.tract$DP3_C143+census2000.CA.tract$DP3_C145+census2000.CA.tract$DP3_C147+census2000.CA.tract$DP3_C149 # family income
census2000.CA.tract$p_less10000_fincome<-census2000.CA.tract$DP3_C131 # household income
census2000.CA.tract$p_9yr_edu<-census2000.CA.tract$DP2_C15
census2000.CA.tract$p_high_school1<-census2000.CA.tract$DP2_C28
census2000.CA.tract$p_high_school2<-census2000.CA.tract$DP2_C19+census2000.CA.tract$DP2_C21+census2000.CA.tract$DP2_C23+census2000.CA.tract$DP2_C25+census2000.CA.tract$DP2_C27
census2000.CA.tract$p_high_school_less<-100-census2000.CA.tract$p_high_school1
census2000.CA.tract$p_bachelor1<-census2000.CA.tract$DP2_C29
census2000.CA.tract$p_bachelor2<-census2000.CA.tract$DP2_C25+census2000.CA.tract$DP2_C27
census2000.CA.tract$p_college<-census2000.CA.tract$DP2_C21+census2000.CA.tract$DP2_C23+census2000.CA.tract$DP2_C25+census2000.CA.tract$DP2_C27
census2000.CA.tract$p_prof_occupation<-census2000.CA.tract$DP3_C45
census2000.CA.tract$p_adults<-census2000.CA.tract$DP1_C34
census2000.CA.tract$population<-census2000.CA.tract$DP1_C0
census2000.CA.tract$median_fincome<-census2000.CA.tract$P077001
census2000.CA.tract$median_hvalue<-census2000.CA.tract$H085001
census2000.CA.tract$median_rent<-census2000.CA.tract$H063001
census2000.CA.tract$median_mortage<-census2000.CA.tract$H091001
census2000.CA.tract$p_150poverty<-(census2000.CA.tract$P088002+census2000.CA.tract$P088003+census2000.CA.tract$P088004+census2000.CA.tract$P088005+census2000.CA.tract$P088006)/census2000.CA.tract$P088001*100
census2000.CA.tract$p_single_parents1<-census2000.CA.tract$P019005/census2000.CA.tract$P019002*100 ## among households with children
census2000.CA.tract$p_single_parents2<-census2000.CA.tract$P019005/census2000.CA.tract$P019003*100 ## among family households with children
census2000.CA.tract$p_novehicle<-census2000.CA.tract$DP4_C68
census2000.CA.tract$p_nophone<-census2000.CA.tract$DP4_C98
census2000.CA.tract$p_noplumbing<-census2000.CA.tract$DP4_C94
census2000.CA.tract$p_crowding<-census2000.CA.tract$DP4_C104+census2000.CA.tract$DP4_C106
census2000.CA.tract.summary2<-data.frame(unclass(summary(census2000.CA.tract)), check.names = FALSE, stringsAsFactors = FALSE)
write.csv(census2000.CA.tract,"./data/census2000_CA_tract2.csv",row.names = F)
write.csv(census2000.CA.tract.summary2,"./data/census2000_CA_tract_summary2.csv",row.names = F)

#compare the distribution
indicators<-colnames(acs1014.CA.tract.wide)[66:106]

for (i in indicators){
  if (i=="p_resident_5yr1" | i=="p_resident_5yr2"){
    print("here")
    acs0509<-acs0509.CA.tract.wide[i]
    acs1014<-acs1014.CA.tract.wide[i]
    census2000<-as.data.frame(census2000.CA.tract$p_resident_5yr)
  } else{
    acs0509<-acs0509.CA.tract.wide[i]
    acs1014<-acs1014.CA.tract.wide[i]
    census2000<-census2000.CA.tract[i]
  }
  print(i)
  acs0509$dataset<-"2.acs0509"
  acs1014$dataset<-"3.acs1014"
  census2000$dataset<-"1.census2000"
  colnames(acs1014)<-colnames(acs0509)
  colnames(census2000)<-colnames(acs0509)
  res<-rbind(acs0509,acs1014)
  res<-rbind(res,census2000)
  print (ggplot(res,aes_string(x="dataset",y=i))+geom_boxplot()+ggtitle(i))
}
