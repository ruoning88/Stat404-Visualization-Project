library(dplyr)
library(shiny)
library(maps)
library(statebins)
library(ggmap)
library(maptools)
library(rgdal)
library(sp)
library(tools)
library(ggplot2)
library(ggrepel)
library(stringr)
library(cowplot)
library(shinyalert)

#### Reading 2017 data
intvw <- read.csv("fmli171x.csv")
intvw2 <- read.csv("fmli172.csv")
intvw3 <- read.csv("fmli173.csv")
intvw4 <- read.csv("fmli174.csv")
intvw5 <- read.csv("fmli181.csv")
intvw2017 <- rbind(intvw2,intvw3,intvw4,intvw5)
#### Selecting the variables we want and combining them
intvw <- intvw %>% select(DIVISION,AGE_REF,AGE2,ROOMSQ,BATHRMQ,BEDROOMQ,BLS_URBN,BUILDING,CUTENURE,
       EARNCOMP,EDUC_REF,EDUCA2,FAM_SIZE,FSALARYX,FSSIX,
       INC_HRS1,INC_HRS2,INCWEEK1,INCWEEK2,INCOMEY1,INCOMEY2,MARITAL1,NO_EARNR,
       NUM_AUTO,OTHRINCX,PERSLT18,PERSOT64,REF_RACE,RACE2,FOODPQ,HOUSPQ,
       APPARPQ,TRANSPQ,HEALTHPQ,ENTERTPQ,PERSCAPQ,READPQ,EDUCAPQ,TOBACCPQ,
       MISCPQ,CASHCOPQ,PERINSPQ,ELCTRCPQ,NTLGASPQ,ALLFULPQ,TELEPHPQ,WATRPSPQ,HOUSEQPQ,
       SWIMPOOL,STATE,TOTEX4PQ,TTOTALP,VOTHRLOP,PORCH,HIGH_EDU,BUILT)
INTVW2017 <- intvw2017 %>% select(DIVISION,AGE_REF,AGE2,ROOMSQ,BATHRMQ,BEDROOMQ,BLS_URBN,BUILDING,CUTENURE,
                              EARNCOMP,EDUC_REF,EDUCA2,FAM_SIZE,FSALARYX,FSSIX,
                              INC_HRS1,INC_HRS2,INCWEEK1,INCWEEK2,INCOMEY1,INCOMEY2,MARITAL1,NO_EARNR,
                              NUM_AUTO,OTHRINCX,PERSLT18,PERSOT64,REF_RACE,RACE2,FOODPQ,HOUSPQ,
                              APPARPQ,TRANSPQ,HEALTHPQ,ENTERTPQ,PERSCAPQ,READPQ,EDUCAPQ,TOBACCPQ,
                              MISCPQ,CASHCOPQ,PERINSPQ,ELCTRCPQ,NTLGASPQ,ALLFULPQ,TELEPHPQ,WATRPSPQ,HOUSEQPQ,
                              SWIMPOOL,STATE,TOTEX4PQ,TTOTALP,VOTHRLOP,PORCH,HIGH_EDU,BUILT)
intvw2017 <- rbind(intvw,INTVW2017)
rm(intvw,intvw2,intvw3,intvw4,intvw5,INTVW2017)

#### Reading 2016 data
intvw <- read.csv("fmli161x.csv")
intvw2 <- read.csv("fmli162.csv")
intvw3 <- read.csv("fmli163.csv")
intvw4 <- read.csv("fmli164.csv")
intvw2016 <- rbind(intvw2,intvw3,intvw4)
#### Selecting the variables we want and combining them
intvw <- intvw %>% select(DIVISION,AGE_REF,AGE2,ROOMSQ,BATHRMQ,BEDROOMQ,BLS_URBN,BUILDING,CUTENURE,
                          EARNCOMP,EDUC_REF,EDUCA2,FAM_SIZE,FSALARYX,FSSIX,
                          INC_HRS1,INC_HRS2,INCWEEK1,INCWEEK2,INCOMEY1,INCOMEY2,MARITAL1,NO_EARNR,
                          NUM_AUTO,OTHRINCX,PERSLT18,PERSOT64,REF_RACE,RACE2,FOODPQ,HOUSPQ,
                          APPARPQ,TRANSPQ,HEALTHPQ,ENTERTPQ,PERSCAPQ,READPQ,EDUCAPQ,TOBACCPQ,
                          MISCPQ,CASHCOPQ,PERINSPQ,ELCTRCPQ,NTLGASPQ,ALLFULPQ,TELEPHPQ,WATRPSPQ,HOUSEQPQ,
                          SWIMPOOL,STATE,TOTEX4PQ,TTOTALP,VOTHRLOP,PORCH,HIGH_EDU,BUILT)
INTVW2016 <- intvw2016 %>% select(DIVISION,AGE_REF,AGE2,ROOMSQ,BATHRMQ,BEDROOMQ,BLS_URBN,BUILDING,CUTENURE,
                                  EARNCOMP,EDUC_REF,EDUCA2,FAM_SIZE,FSALARYX,FSSIX,
                                  INC_HRS1,INC_HRS2,INCWEEK1,INCWEEK2,INCOMEY1,INCOMEY2,MARITAL1,NO_EARNR,
                                  NUM_AUTO,OTHRINCX,PERSLT18,PERSOT64,REF_RACE,RACE2,FOODPQ,HOUSPQ,
                                  APPARPQ,TRANSPQ,HEALTHPQ,ENTERTPQ,PERSCAPQ,READPQ,EDUCAPQ,TOBACCPQ,
                                  MISCPQ,CASHCOPQ,PERINSPQ,ELCTRCPQ,NTLGASPQ,ALLFULPQ,TELEPHPQ,WATRPSPQ,HOUSEQPQ,
                                  SWIMPOOL,STATE,TOTEX4PQ,TTOTALP,VOTHRLOP,PORCH,HIGH_EDU,BUILT)
intvw2016 <- rbind(intvw,INTVW2016)
rm(intvw,intvw2,intvw3,intvw4,INTVW2016)

#### Reading 2015 data
intvw <- read.csv("fmli151x.csv")
intvw2 <- read.csv("fmli152.csv")
intvw3 <- read.csv("fmli153.csv")
intvw4 <- read.csv("fmli154.csv")
intvw2015 <- rbind(intvw2,intvw3,intvw4)
#### Selecting the variables we want and combining them
intvw <- intvw %>% select(DIVISION,AGE_REF,AGE2,ROOMSQ,BATHRMQ,BEDROOMQ,BLS_URBN,BUILDING,CUTENURE,
                          EARNCOMP,EDUC_REF,EDUCA2,FAM_SIZE,FSALARYX,FSSIX,
                          INC_HRS1,INC_HRS2,INCWEEK1,INCWEEK2,INCOMEY1,INCOMEY2,MARITAL1,NO_EARNR,
                          NUM_AUTO,OTHRINCX,PERSLT18,PERSOT64,REF_RACE,RACE2,FOODPQ,HOUSPQ,
                          APPARPQ,TRANSPQ,HEALTHPQ,ENTERTPQ,PERSCAPQ,READPQ,EDUCAPQ,TOBACCPQ,
                          MISCPQ,CASHCOPQ,PERINSPQ,ELCTRCPQ,NTLGASPQ,ALLFULPQ,TELEPHPQ,WATRPSPQ,HOUSEQPQ,
                          SWIMPOOL,STATE,TOTEX4PQ,TTOTALP,VOTHRLOP,PORCH,HIGH_EDU,BUILT)
INTVW2015 <- intvw2015 %>% select(DIVISION,AGE_REF,AGE2,ROOMSQ,BATHRMQ,BEDROOMQ,BLS_URBN,BUILDING,CUTENURE,
                                  EARNCOMP,EDUC_REF,EDUCA2,FAM_SIZE,FSALARYX,FSSIX,
                                  INC_HRS1,INC_HRS2,INCWEEK1,INCWEEK2,INCOMEY1,INCOMEY2,MARITAL1,NO_EARNR,
                                  NUM_AUTO,OTHRINCX,PERSLT18,PERSOT64,REF_RACE,RACE2,FOODPQ,HOUSPQ,
                                  APPARPQ,TRANSPQ,HEALTHPQ,ENTERTPQ,PERSCAPQ,READPQ,EDUCAPQ,TOBACCPQ,
                                  MISCPQ,CASHCOPQ,PERINSPQ,ELCTRCPQ,NTLGASPQ,ALLFULPQ,TELEPHPQ,WATRPSPQ,HOUSEQPQ,
                                  SWIMPOOL,STATE,TOTEX4PQ,TTOTALP,VOTHRLOP,PORCH,HIGH_EDU,BUILT)
intvw2015 <- rbind(intvw,INTVW2015)
rm(intvw,intvw2,intvw3,intvw4,INTVW2015)
intvw2017 <- rbind(intvw2017,intvw2016,intvw2015)
refstdiv <- unique(data.frame(intvw2017$DIVISION,intvw2017$STATE)) %>% na.omit()
colnames(refstdiv) <- c("DIVISION","STATE")
refstdiv <- refstdiv[-c(27,39),]

#### Reading 2014 data
intvw <- read.csv("fmli141x.csv")
intvw2 <- read.csv("fmli142.csv")
intvw3 <- read.csv("fmli143.csv")
intvw4 <- read.csv("fmli144.csv")
intvw2014 <- rbind(intvw2,intvw3,intvw4)
#### Selecting the variables we want and combining them
intvw <- intvw %>% select(AGE_REF,AGE2,ROOMSQ,BATHRMQ,BEDROOMQ,BLS_URBN,BUILDING,CUTENURE,
                          EARNCOMP,EDUC_REF,EDUCA2,FAM_SIZE,FSALARYX,FSSIX,
                          INC_HRS1,INC_HRS2,INCWEEK1,INCWEEK2,INCOMEY1,INCOMEY2,MARITAL1,NO_EARNR,
                          NUM_AUTO,OTHRINCX,PERSLT18,PERSOT64,REF_RACE,RACE2,FOODPQ,HOUSPQ,
                          APPARPQ,TRANSPQ,HEALTHPQ,ENTERTPQ,PERSCAPQ,READPQ,EDUCAPQ,TOBACCPQ,
                          MISCPQ,CASHCOPQ,PERINSPQ,ELCTRCPQ,NTLGASPQ,ALLFULPQ,TELEPHPQ,WATRPSPQ,HOUSEQPQ,
                          SWIMPOOL,STATE,TOTEX4PQ,TTOTALP,VOTHRLOP,PORCH,HIGH_EDU,BUILT)
INTVW2014 <- intvw2014 %>% select(AGE_REF,AGE2,ROOMSQ,BATHRMQ,BEDROOMQ,BLS_URBN,BUILDING,CUTENURE,
                                  EARNCOMP,EDUC_REF,EDUCA2,FAM_SIZE,FSALARYX,FSSIX,
                                  INC_HRS1,INC_HRS2,INCWEEK1,INCWEEK2,INCOMEY1,INCOMEY2,MARITAL1,NO_EARNR,
                                  NUM_AUTO,OTHRINCX,PERSLT18,PERSOT64,REF_RACE,RACE2,FOODPQ,HOUSPQ,
                                  APPARPQ,TRANSPQ,HEALTHPQ,ENTERTPQ,PERSCAPQ,READPQ,EDUCAPQ,TOBACCPQ,
                                  MISCPQ,CASHCOPQ,PERINSPQ,ELCTRCPQ,NTLGASPQ,ALLFULPQ,TELEPHPQ,WATRPSPQ,HOUSEQPQ,
                                  SWIMPOOL,STATE,TOTEX4PQ,TTOTALP,VOTHRLOP,PORCH,HIGH_EDU,BUILT)
intvw2014 <- rbind(intvw,INTVW2014)
rm(intvw,intvw2,intvw3,intvw4,INTVW2014)

#### Reading 2013 data
intvw <- read.csv("fmli131x.csv")
intvw2 <- read.csv("fmli132.csv")
intvw3 <- read.csv("fmli133.csv")
colnames(intvw2) <- colnames(intvw3)
intvw4 <- read.csv("fmli134.csv")
intvw2013 <- rbind(intvw2,intvw3,intvw4)
#### Selecting the variables we want and combining them
intvw <- intvw %>% select(AGE_REF,AGE2,ROOMSQ,BATHRMQ,BEDROOMQ,BLS_URBN,BUILDING,CUTENURE,
                          EARNCOMP,EDUC_REF,EDUCA2,FAM_SIZE,FSALARYX,FSSIX,
                          INC_HRS1,INC_HRS2,INCWEEK1,INCWEEK2,INCOMEY1,INCOMEY2,MARITAL1,NO_EARNR,
                          NUM_AUTO,OTHRINCX,PERSLT18,PERSOT64,REF_RACE,RACE2,FOODPQ,HOUSPQ,
                          APPARPQ,TRANSPQ,HEALTHPQ,ENTERTPQ,PERSCAPQ,READPQ,EDUCAPQ,TOBACCPQ,
                          MISCPQ,CASHCOPQ,PERINSPQ,ELCTRCPQ,NTLGASPQ,ALLFULPQ,TELEPHPQ,WATRPSPQ,HOUSEQPQ,
                          SWIMPOOL,STATE,TOTEX4PQ,TTOTALP,VOTHRLOP,PORCH,HIGH_EDU,BUILT)
INTVW2013 <- intvw2013 %>% select(AGE_REF,AGE2,ROOMSQ,BATHRMQ,BEDROOMQ,BLS_URBN,BUILDING,CUTENURE,
                                  EARNCOMP,EDUC_REF,EDUCA2,FAM_SIZE,FSALARYX,FSSIX,
                                  INC_HRS1,INC_HRS2,INCWEEK1,INCWEEK2,INCOMEY1,INCOMEY2,MARITAL1,NO_EARNR,
                                  NUM_AUTO,OTHRINCX,PERSLT18,PERSOT64,REF_RACE,RACE2,FOODPQ,HOUSPQ,
                                  APPARPQ,TRANSPQ,HEALTHPQ,ENTERTPQ,PERSCAPQ,READPQ,EDUCAPQ,TOBACCPQ,
                                  MISCPQ,CASHCOPQ,PERINSPQ,ELCTRCPQ,NTLGASPQ,ALLFULPQ,TELEPHPQ,WATRPSPQ,HOUSEQPQ,
                                  SWIMPOOL,STATE,TOTEX4PQ,TTOTALP,VOTHRLOP,PORCH,HIGH_EDU,BUILT)
intvw2013 <- rbind(intvw,INTVW2013)
rm(intvw,intvw2,intvw3,intvw4,INTVW2013)
intvw2014 <- rbind(intvw2014,intvw2013)
rm(intvw2013,intvw2015,intvw2016)
intvw2014 <- left_join(intvw2014,refstdiv,by="STATE")
intvw2014 <- intvw2014[which(!is.na(intvw2014$STATE)),]
intvw2014$DIVISION[which(intvw2014$STATE==54)] <- 5
intvw2014$DIVISION[which(intvw2014$STATE==16)] <- 8 
intvw2014$DIVISION[which(intvw2014$STATE==23)] <- 1
intvw2017 <- rbind(intvw2014,intvw2017)
  
#### 1. TAB1: Map of Household Expeditures
#### 1.1 Selecting the variables necessary
intvw1 <- intvw2017 %>% select(DIVISION, STATE,FOODPQ,HOUSPQ,
                           APPARPQ,TRANSPQ,HEALTHPQ,ENTERTPQ,PERSCAPQ,READPQ,EDUCAPQ,TOBACCPQ,MISCPQ,CASHCOPQ,PERINSPQ,REF_RACE,BUILT)

#### 1.2 Defining the state names and joining them with the original data
statename <- data.frame(STATE=c(1,2,4,5,6,8,9,10,11,12,13,seq(15,42,by=1),seq(44,51,by=1),53,54,55,56,72),
                        SN=c("alabama","alaska","arizona","arkansas","california","colorado","connecticut",
                             "delaware","district of columbia","florida","georgia","hawaii","idaho","illinois",
                             "indiana","iowa","kansas","kentucky","louisiana","maine","maryland","massachusetts",
                             "michigan","minnesota","mississippi","missouri","montana","nebraska","nevada","new hampshire",
                             "new jersey","new mexico","new york","north carolina","north dakota","ohio","oklahoma","oregon",
                             "pennsylvania","rhode island","south carolina","south dakota","tennessee","texas","utah","vermont",
                             "virginia","washington","west virginia","wisconsin","wyoming","puerto rico"))
statename$STATE <- as.factor(statename$STATE)
intvw1$STATE <- as.factor(intvw1$STATE)
intvw1 <- inner_join(intvw1,statename,by="STATE")
intvw1 <- intvw1[-which(intvw1$SN=="alaska"),]
intvw1 <- intvw1[-which(intvw1$SN=="hawaii"),]
intvw1$SN <- as.character(intvw1$SN)

#### 1.3 Recoding Division/Region into actual names instaed of intergers
intvw1$DIVISION <- as.factor(intvw1$DIVISION)
intvw1$DIVISION <- recode_factor(intvw1$DIVISION,"1"="New England",
                                 "2"="Middle Atlantic",
                                 "3"="East North Central",
                                 "4"="West North Central",
                                 "5"="South Atlantic",
                                 "6"="East South Central",
                                 "7"="West South Central",
                                 "8"="Mountain",
                                 "9"="Pacific")

#### 1.4 Recoding Race into the actual names of the race
intvw1$REF_RACE <- recode_factor(intvw1$REF_RACE,"1"="White",
                                     "2"="Black","3"="Native American",
                                     "4"="Asian","5"="Pacific Islander",
                                     "6"="Multi-Race")

#### 1.5 Consolidating the years into 5 years range
intvw1$BUILT <- gsub("1$|2$|3$|4$","5",intvw1$BUILT)
intvw1$BUILT <- as.numeric(intvw1$BUILT)
indx <- grep("6$|7$|8$|9$",intvw1$BUILT)
intvw1$BUILT[indx] <- round(intvw1$BUILT[indx],-1)
indx2 <- which(is.na(intvw1$BUILT))
intvw1<- intvw1[-indx2,]
intvw1$BUILT[which(intvw1$BUILT==2020)]<-2017

#### 1.6 Summarizing the data
intvw_chor <- intvw1 %>% mutate(TOTEXP=FOODPQ+HOUSPQ+APPARPQ+TRANSPQ+HEALTHPQ+ENTERTPQ+PERSCAPQ+READPQ+EDUCAPQ+MISCPQ+CASHCOPQ+PERINSPQ+TOBACCPQ) %>% 
  group_by(DIVISION,SN,BUILT,REF_RACE) %>% summarise(TOT_avg=mean(TOTEXP,na.rm=T),FOOD_avg=mean(FOODPQ,na.rm=T),HOUS_avg=mean(HOUSPQ,na.rm=T),
                                      APP_avg=mean(APPARPQ,na.rm=T),TRANS_avg=mean(TRANSPQ,na.rm=T),HEALTH_avg=mean(HEALTHPQ,na.rm=T),
                                      ENTER_avg=mean(ENTERTPQ,na.rm=T),PERSCA_avg=mean(PERSCAPQ,na.rm=T),READ_avg=mean(READPQ,na.rm=T),
                                      EDUCA_avg=mean(EDUCAPQ,na.rm=T), TOBAC_avg=mean(TOBACCPQ,na.rm=T), MISC_avg=mean(MISCPQ,na.rm=T),
                                      CASHCO_avg=mean(CASHCOPQ,na.rm=T), PERINSPQ_avg=mean(PERINSPQ,na.rm=T))


#### 1.7 cleaning the data to construct subplots
intvw1 <- intvw1 %>% select(-DIVISION,-STATE) %>% 
  mutate(TOTEXP=FOODPQ+HOUSPQ+APPARPQ+TRANSPQ+HEALTHPQ+ENTERTPQ+PERSCAPQ+READPQ+EDUCAPQ+MISCPQ+CASHCOPQ+PERINSPQ+TOBACCPQ)


###--------------------------------------------------------------------------------------------
#### 2 Bar Plot data
intvw2 <- intvw2017 %>% select(FOODPQ,HOUSPQ,
                               APPARPQ,TRANSPQ,HEALTHPQ,ENTERTPQ,PERSCAPQ,READPQ,
                               EDUCAPQ,TOBACCPQ,MISCPQ,CASHCOPQ,PERINSPQ,
                              EDUC_REF,INC_HRS1,INCWEEK1,FAM_SIZE,MARITAL1,PERSLT18,PERSOT64)
intvw2$FAM_SIZE[which(intvw2$FAM_SIZE>15)] <- 20
intvw2$INC_HRS1 <- as.character(intvw2$INC_HRS1)
intvw2$INC_HRS1[which(intvw2$INC_HRS1==".")] <- "0"
intvw2$INC_HRS1 <- as.numeric(intvw2$INC_HRS1)
intvw2$EDUC_REF <- as.factor(intvw2$EDUC_REF)
intvw2$EDUC_REF <- recode_factor(intvw2$EDUC_REF,"0"="Never Attend",
                                 "10"="First through eighth grade",
                                  "11"="Ninth through twelfth grade",
                                  "12"="High school graduate",
                                  "13"="Some college, less than college graduate",
                                 "14"="Associate Degree",
                                 "15"="Bachelor's Degree",
                                 "16"="Master's Degree","17"="Master's Degree")
intvw2$PERSLT18 <- as.factor(intvw2$PERSLT18)
intvw2$PERSLT18 <- recode_factor(intvw2$PERSLT18, "0"="None",
                               "1"="Less than 5","2"="Less than 5",
                               "3"="Less than 5","4"="Less than 5",
                               "5"="Less than 5","6"="Between 5 and 10",
                               "7"="Between 5 and 10","8"="Between 5 and 10",
                               "9"="Between 5 and 10","10"="Between 5 and 10",
                               "11"="Greater than 10","14"="Greater than 10")
intvw2$PERSOT64<- as.factor(intvw2$PERSOT64)
intvw2$MARITAL1<- as.factor(intvw2$MARITAL1)  
intvw2$MARITAL1 <- recode_factor(intvw2$MARITAL1, 
                                 "1"="Married","2"="Widowed",
                                 "3"="Divorced","4"="Separated",
                                 "5"="Never Married")
intvw2 <- intvw2 %>% mutate(TOTEXP=FOODPQ+HOUSPQ+APPARPQ+TRANSPQ+HEALTHPQ+ENTERTPQ+PERSCAPQ+READPQ+EDUCAPQ+MISCPQ+CASHCOPQ+PERINSPQ+TOBACCPQ)
save(intvw2, file="BarPlot.RData")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------
####2. creating data necessary for line plot
#Selecting and Combining 2013 Data 
fmli13.1 <- read.csv("fmli131.csv")
fmli13.2 <- read.csv("fmli132.csv")
fmli13.3 <- read.csv("fmli133.csv")
fmli13.4 <- read.csv("fmli134.csv")

fmli13.1s <- fmli13.1 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ) %>%
  rename(CUID = CUID, FoodExp.13.1= FOODCQ, TotalExp.13.1 = TOTEXPCQ, AlcoholExp.13.1 = ALCBEVCQ, 
         HouseExp.13.1 = HOUSCQ, MaintenanceExp.13.1 = MRPINSCQ, ChildExp.13.1 = BBYDAYCQ, 
         HousefurnitureExp.13.1 = HOUSEQCQ, ClothesExp.13.1 = APPARCQ, 
         TransportExp.13.1 = TRANSCQ, HealthcareExp.13.1 = HEALTHCQ, 
         EntertainExp.13.1 = ENTERTCQ, EducationExp.13.1 = EDUCACQ)

fmli13.2s <- fmli13.2 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ) %>%
  rename(CUID = CUID, FoodExp.13.2= FOODCQ, TotalExp.13.2 = TOTEXPCQ, AlcoholExp.13.2 = ALCBEVCQ, 
         HouseExp.13.2 = HOUSCQ, MaintenanceExp.13.2 = MRPINSCQ, ChildExp.13.2 = BBYDAYCQ, 
         HousefurnitureExp.13.2 = HOUSEQCQ, ClothesExp.13.2 = APPARCQ, 
         TransportExp.13.2 = TRANSCQ, HealthcareExp.13.2 = HEALTHCQ, 
         EntertainExp.13.2 = ENTERTCQ, EducationExp.13.2 = EDUCACQ)

fmli13.3s <- fmli13.3 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ) %>%
  rename(CUID = CUID, FoodExp.13.3= FOODCQ, TotalExp.13.3 = TOTEXPCQ, AlcoholExp.13.3 = ALCBEVCQ, 
         HouseExp.13.3 = HOUSCQ, MaintenanceExp.13.3 = MRPINSCQ, ChildExp.13.3 = BBYDAYCQ, 
         HousefurnitureExp.13.3 = HOUSEQCQ, ClothesExp.13.3 = APPARCQ, 
         TransportExp.13.3 = TRANSCQ, HealthcareExp.13.3 = HEALTHCQ, 
         EntertainExp.13.3 = ENTERTCQ, EducationExp.13.3 = EDUCACQ) 


fmli13.4s <- fmli13.4 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ)%>%
  rename(CUID = CUID, FoodExp.13.4= FOODCQ, TotalExp.13.4 = TOTEXPCQ, AlcoholExp.13.4 = ALCBEVCQ, 
         HouseExp.13.4 = HOUSCQ, MaintenanceExp.13.4 = MRPINSCQ, ChildExp.13.4 = BBYDAYCQ, 
         HousefurnitureExp.13.4 = HOUSEQCQ, ClothesExp.13.4 = APPARCQ, 
         TransportExp.13.4 = TRANSCQ, HealthcareExp.13.4 = HEALTHCQ, 
         EntertainExp.13.4 = ENTERTCQ, EducationExp.13.4 = EDUCACQ) 


fmli13.join <- full_join(fmli13.2s, fmli13.1s, by = c("CUID" = "CUID"))
fmli13.join2 <- full_join(fmli13.3s, fmli13.join, by = c("CUID" = "CUID"))
fmli13.joinall <- full_join(fmli13.4s, fmli13.join2, by = c("CUID" = "CUID"))

save(fmli13.joinall, file="fmli13.joinall.Rdata")

#Selecting and Combining 2014 Data 
fmli14.1 <- read.csv("fmli141.csv")
fmli14.2 <- read.csv("fmli142.csv")
fmli14.3 <- read.csv("fmli143.csv")
fmli14.4 <- read.csv("fmli144.csv")

fmli14.1s <- fmli14.1 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ) %>%
  rename(CUID = CUID, FoodExp.14.1= FOODCQ, TotalExp.14.1 = TOTEXPCQ, AlcoholExp.14.1 = ALCBEVCQ, 
         HouseExp.14.1 = HOUSCQ, MaintenanceExp.14.1 = MRPINSCQ, ChildExp.14.1 = BBYDAYCQ, 
         HousefurnitureExp.14.1 = HOUSEQCQ, ClothesExp.14.1 = APPARCQ, 
         TransportExp.14.1 = TRANSCQ, HealthcareExp.14.1 = HEALTHCQ, 
         EntertainExp.14.1 = ENTERTCQ, EducationExp.14.1 = EDUCACQ)

fmli14.2s <- fmli14.2 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ)%>%
  rename(CUID = CUID, FoodExp.14.2= FOODCQ, TotalExp.14.2 = TOTEXPCQ, AlcoholExp.14.2 = ALCBEVCQ, 
         HouseExp.14.2 = HOUSCQ, MaintenanceExp.14.2 = MRPINSCQ, ChildExp.14.2 = BBYDAYCQ, 
         HousefurnitureExp.14.2 = HOUSEQCQ, ClothesExp.14.2 = APPARCQ, 
         TransportExp.14.2 = TRANSCQ, HealthcareExp.14.2 = HEALTHCQ, 
         EntertainExp.14.2 = ENTERTCQ, EducationExp.14.2 = EDUCACQ)

fmli14.3s <- fmli14.3 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ)%>%
  rename(CUID = CUID, FoodExp.14.3= FOODCQ, TotalExp.14.3 = TOTEXPCQ, AlcoholExp.14.3 = ALCBEVCQ, 
         HouseExp.14.3 = HOUSCQ, MaintenanceExp.14.3 = MRPINSCQ, ChildExp.14.3 = BBYDAYCQ, 
         HousefurnitureExp.14.3 = HOUSEQCQ, ClothesExp.14.3 = APPARCQ, 
         TransportExp.14.3 = TRANSCQ, HealthcareExp.14.3 = HEALTHCQ, 
         EntertainExp.14.3 = ENTERTCQ, EducationExp.14.3 = EDUCACQ)

fmli14.4s <- fmli14.4 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ)%>%
  rename(CUID = CUID, FoodExp.14.4= FOODCQ, TotalExp.14.4 = TOTEXPCQ, AlcoholExp.14.4 = ALCBEVCQ, 
         HouseExp.14.4 = HOUSCQ, MaintenanceExp.14.4 = MRPINSCQ, ChildExp.14.4 = BBYDAYCQ, 
         HousefurnitureExp.14.4 = HOUSEQCQ, ClothesExp.14.4 = APPARCQ, 
         TransportExp.14.4 = TRANSCQ, HealthcareExp.14.4 = HEALTHCQ, 
         EntertainExp.14.4 = ENTERTCQ, EducationExp.14.4 = EDUCACQ)

fmli14.join <- full_join(fmli14.2s, fmli14.1s, by = c("CUID" = "CUID"))
fmli14.join2 <- full_join(fmli14.3s, fmli14.join, by = c("CUID" = "CUID"))
fmli14.joinall <- full_join(fmli14.4s, fmli14.join2, by = c("CUID" = "CUID"))

save(fmli14.joinall, file="fmli14.joinall.Rdata")

#Selecting and Combining 2015 Data 
fmli15.1 <- read.csv("fmli151.csv")
fmli15.2 <- read.csv("fmli152.csv")
fmli15.3 <- read.csv("fmli153.csv")
fmli15.4 <- read.csv("fmli154.csv")
fmli15.1s <- fmli15.1 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ) %>%
  rename(CUID = CUID, FoodExp.15.1= FOODCQ, TotalExp.15.1 = TOTEXPCQ, AlcoholExp.15.1 = ALCBEVCQ, 
         HouseExp.15.1 = HOUSCQ, MaintenanceExp.15.1 = MRPINSCQ, ChildExp.15.1 = BBYDAYCQ, 
         HousefurnitureExp.15.1 = HOUSEQCQ, ClothesExp.15.1 = APPARCQ, 
         TransportExp.15.1 = TRANSCQ, HealthcareExp.15.1 = HEALTHCQ, 
         EntertainExp.15.1 = ENTERTCQ, EducationExp.15.1 = EDUCACQ)

fmli15.2s <- fmli15.2 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ) %>%
  rename(CUID = CUID, FoodExp.15.2= FOODCQ, TotalExp.15.2 = TOTEXPCQ, AlcoholExp.15.2 = ALCBEVCQ, 
         HouseExp.15.2 = HOUSCQ, MaintenanceExp.15.2 = MRPINSCQ, ChildExp.15.2 = BBYDAYCQ, 
         HousefurnitureExp.15.2 = HOUSEQCQ, ClothesExp.15.2 = APPARCQ, 
         TransportExp.15.2 = TRANSCQ, HealthcareExp.15.2 = HEALTHCQ, 
         EntertainExp.15.2 = ENTERTCQ, EducationExp.15.2 = EDUCACQ)

fmli15.3s <- fmli15.3 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ) %>%
  rename(CUID = CUID, FoodExp.15.3= FOODCQ, TotalExp.15.3 = TOTEXPCQ, AlcoholExp.15.3 = ALCBEVCQ, 
         HouseExp.15.3 = HOUSCQ, MaintenanceExp.15.3 = MRPINSCQ, ChildExp.15.3 = BBYDAYCQ, 
         HousefurnitureExp.15.3 = HOUSEQCQ, ClothesExp.15.3 = APPARCQ, 
         TransportExp.15.3 = TRANSCQ, HealthcareExp.15.3 = HEALTHCQ, 
         EntertainExp.15.3 = ENTERTCQ, EducationExp.15.3 = EDUCACQ)

fmli15.4s <- fmli15.4 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ) %>%
  rename(CUID = CUID, FoodExp.15.4= FOODCQ, TotalExp.15.4 = TOTEXPCQ, AlcoholExp.15.4 = ALCBEVCQ, 
         HouseExp.15.4 = HOUSCQ, MaintenanceExp.15.4 = MRPINSCQ, ChildExp.15.4 = BBYDAYCQ, 
         HousefurnitureExp.15.4 = HOUSEQCQ, ClothesExp.15.4 = APPARCQ, 
         TransportExp.15.4 = TRANSCQ, HealthcareExp.15.4 = HEALTHCQ, 
         EntertainExp.15.4 = ENTERTCQ, EducationExp.15.4 = EDUCACQ)

fmli15.join <- full_join(fmli15.2s, fmli15.1s, by = c("CUID" = "CUID"))
fmli15.join2 <- full_join(fmli15.3s, fmli15.join, by = c("CUID" = "CUID"))
fmli15.joinall <- full_join(fmli15.4s, fmli15.join2, by = c("CUID" = "CUID"))

save(fmli15.joinall, file="fmli15.joinall.Rdata")

#Selecting and Combining 2016 Data 
fmli16.1 <- read.csv("fmli161.csv")
fmli16.2 <- read.csv("fmli162.csv")
fmli16.3 <- read.csv("fmli163.csv")
fmli16.4 <- read.csv("fmli164.csv")

fmli16.1s <- fmli16.1 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ) %>%
  rename(CUID = CUID, FoodExp.16.1= FOODCQ, TotalExp.16.1 = TOTEXPCQ, AlcoholExp.16.1 = ALCBEVCQ, 
         HouseExp.16.1 = HOUSCQ, MaintenanceExp.16.1 = MRPINSCQ, ChildExp.16.1 = BBYDAYCQ, 
         HousefurnitureExp.16.1 = HOUSEQCQ, ClothesExp.16.1 = APPARCQ, 
         TransportExp.16.1 = TRANSCQ, HealthcareExp.16.1 = HEALTHCQ, 
         EntertainExp.16.1 = ENTERTCQ, EducationExp.16.1 = EDUCACQ)

fmli16.2s <- fmli16.2 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ)  %>%
  rename(CUID = CUID, FoodExp.16.2= FOODCQ, TotalExp.16.2 = TOTEXPCQ, AlcoholExp.16.2 = ALCBEVCQ, 
         HouseExp.16.2 = HOUSCQ, MaintenanceExp.16.2 = MRPINSCQ, ChildExp.16.2 = BBYDAYCQ, 
         HousefurnitureExp.16.2 = HOUSEQCQ, ClothesExp.16.2 = APPARCQ, 
         TransportExp.16.2 = TRANSCQ, HealthcareExp.16.2 = HEALTHCQ, 
         EntertainExp.16.2 = ENTERTCQ, EducationExp.16.2 = EDUCACQ)

fmli16.3s <- fmli16.3 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ) %>%
  rename(CUID = CUID, FoodExp.16.3= FOODCQ, TotalExp.16.3 = TOTEXPCQ, AlcoholExp.16.3 = ALCBEVCQ, 
         HouseExp.16.3 = HOUSCQ, MaintenanceExp.16.3 = MRPINSCQ, ChildExp.16.3 = BBYDAYCQ, 
         HousefurnitureExp.16.3 = HOUSEQCQ, ClothesExp.16.3 = APPARCQ, 
         TransportExp.16.3 = TRANSCQ, HealthcareExp.16.3 = HEALTHCQ, 
         EntertainExp.16.3 = ENTERTCQ, EducationExp.16.3 = EDUCACQ)

fmli16.4s <- fmli16.4 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ) %>%
  rename(CUID = CUID, FoodExp.16.4= FOODCQ, TotalExp.16.4 = TOTEXPCQ, AlcoholExp.16.4 = ALCBEVCQ, 
         HouseExp.16.4 = HOUSCQ, MaintenanceExp.16.4 = MRPINSCQ, ChildExp.16.4 = BBYDAYCQ, 
         HousefurnitureExp.16.4 = HOUSEQCQ, ClothesExp.16.4 = APPARCQ, 
         TransportExp.16.4 = TRANSCQ, HealthcareExp.16.4 = HEALTHCQ, 
         EntertainExp.16.4 = ENTERTCQ, EducationExp.16.4 = EDUCACQ)

fmli16.join <- full_join(fmli16.2s, fmli16.1s, by = c("CUID" = "CUID"))
fmli16.join2 <- full_join(fmli16.3s, fmli16.join, by = c("CUID" = "CUID"))
fmli16.joinall <- full_join(fmli16.4s, fmli16.join2, by = c("CUID" = "CUID"))

save(fmli16.joinall, file="fmli16.joinall.Rdata")

#Selecting and Combining 2017 Data 
fmli17.1 <- read.csv("fmli171.csv", stringsAsFactors = FALSE)
fmli17.2 <- read.csv("fmli172.csv", stringsAsFactors = FALSE)
fmli17.3 <- read.csv("fmli173.csv", stringsAsFactors = FALSE)
fmli17.4 <- read.csv("fmli174.csv", stringsAsFactors = FALSE)

fmli17.1s <- fmli17.1 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ) %>%
  rename(CUID = CUID, FoodExp.17.1= FOODCQ, TotalExp.17.1 = TOTEXPCQ, AlcoholExp.17.1 = ALCBEVCQ, 
         HouseExp.17.1 = HOUSCQ, MaintenanceExp.17.1 = MRPINSCQ, ChildExp.17.1 = BBYDAYCQ, 
         HousefurnitureExp.17.1 = HOUSEQCQ, ClothesExp.17.1 = APPARCQ, 
         TransportExp.17.1 = TRANSCQ, HealthcareExp.17.1 = HEALTHCQ, 
         EntertainExp.17.1 = ENTERTCQ, EducationExp.17.1 = EDUCACQ)

fmli17.2s <- fmli17.2 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ) %>%
  rename(CUID = CUID, FoodExp.17.2= FOODCQ, TotalExp.17.2 = TOTEXPCQ, AlcoholExp.17.2 = ALCBEVCQ, 
         HouseExp.17.2 = HOUSCQ, MaintenanceExp.17.2 = MRPINSCQ, ChildExp.17.2 = BBYDAYCQ, 
         HousefurnitureExp.17.2 = HOUSEQCQ, ClothesExp.17.2 = APPARCQ, 
         TransportExp.17.2 = TRANSCQ, HealthcareExp.17.2 = HEALTHCQ, 
         EntertainExp.17.2 = ENTERTCQ, EducationExp.17.2 = EDUCACQ)

fmli17.3s <- fmli17.3 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ)  %>%
  rename(CUID = CUID, FoodExp.17.3= FOODCQ, TotalExp.17.3 = TOTEXPCQ, AlcoholExp.17.3 = ALCBEVCQ, 
         HouseExp.17.3 = HOUSCQ, MaintenanceExp.17.3 = MRPINSCQ, ChildExp.17.3 = BBYDAYCQ, 
         HousefurnitureExp.17.3 = HOUSEQCQ, ClothesExp.17.3 = APPARCQ, 
         TransportExp.17.3 = TRANSCQ, HealthcareExp.17.3 = HEALTHCQ, 
         EntertainExp.17.3 = ENTERTCQ, EducationExp.17.3 = EDUCACQ)

fmli17.4s <- fmli17.4 %>% 
  select(CUID, FOODCQ, TOTEXPCQ, ALCBEVCQ, HOUSCQ, 
         MRPINSCQ, BBYDAYCQ, HOUSEQCQ, APPARCQ, TRANSCQ, 
         HEALTHCQ, ENTERTCQ, EDUCACQ) %>%
  rename(CUID = CUID, FoodExp.17.4= FOODCQ, TotalExp.17.4 = TOTEXPCQ, AlcoholExp.17.4 = ALCBEVCQ, 
         HouseExp.17.4 = HOUSCQ, MaintenanceExp.17.4 = MRPINSCQ, ChildExp.17.4 = BBYDAYCQ, 
         HousefurnitureExp.17.4 = HOUSEQCQ, ClothesExp.17.4 = APPARCQ, 
         TransportExp.17.4 = TRANSCQ, HealthcareExp.17.4 = HEALTHCQ, 
         EntertainExp.17.4 = ENTERTCQ, EducationExp.17.4 = EDUCACQ)

fmli17.join <- full_join(fmli17.2s, fmli17.1s, by = c("CUID" = "CUID"))
fmli17.join2 <- full_join(fmli17.3s, fmli17.join, by = c("CUID" = "CUID"))
fmli17.joinall <- full_join(fmli17.4s, fmli17.join2, by = c("CUID" = "CUID"))

fmli17.joinall$CUID<- as.numeric(fmli17.joinall$CUID)
fmli17.joinall$ChildExp.17.1 <- as.numeric(fmli17.joinall$ChildExp.17.1)
fmli17.joinall$EducationExp.17.1 <- as.numeric(fmli17.joinall$EducationExp.17.1)
glimpse(fmli16.joinall)
save(fmli17.joinall, file="fmli17.joinall.Rdata")


#2.2 Load All Combine Year Files
load(file="fmli13.joinall.Rdata")
load(file="fmli14.joinall.Rdata")
load(file="fmli15.joinall.Rdata")
load(file="fmli16.joinall.Rdata")
load(file="fmli17.joinall.Rdata")

fmli.joint1 <- full_join(fmli14.joinall, fmli13.joinall, by = c("CUID" = "CUID"))
fmli.joint2 <- full_join(fmli15.joinall, fmli.joint1, by = c("CUID" = "CUID"))
fmli.joint3 <- full_join(fmli16.joinall, fmli.joint2, by = c("CUID" = "CUID"))

fmli17.joinall$CUID<- as.numeric(fmli17.joinall$CUID)
fmli.joint3$CUID <-as.numeric(fmli.joint3$CUID)
fmli.join.total <- full_join(fmli17.joinall, fmli.joint3, by = c("CUID" = "CUID"))

#2.3 Quarter DataSet
fmli.jt.rest <- gather(fmli.join.total, expense, expenseamount, -CUID, na.rm=TRUE)

fmli.final.quarter <- fmli.jt.rest %>%
  separate(expense, into = c("expensetype", "year", "quarter")) %>%
  spread(expensetype, expenseamount) %>%
  group_by(year, quarter) %>%
  summarize(Total = mean(TotalExp),
            Food = mean(FoodExp), 
            Alcohol = mean(AlcoholExp), 
            House = mean(HouseExp), 
            Furniture = mean(HousefurnitureExp),
            Clothes = mean(ClothesExp),
            Transportation = mean(TransportExp),
            Healthcare = mean(HealthcareExp),
            Entertainment = mean(EntertainExp),
            Education = mean(EducationExp)) %>%
  unite(time, year:quarter, sep=".") %>%
  gather(expensetype, expenseamount, -time) %>%
  separate(time, into=c("year", "quarter"))

fmli.final.quarter$year <- paste0("20", fmli.final.quarter$year)
fmli.final.quarter$year <-as.numeric(fmli.final.quarter$year)
fmli.final.quarter$quarter <-as.numeric(fmli.final.quarter$quarter)
save(fmli.final.quarter, file="fmli.final.quarter.Rdata")
load(file="fmli.final.quarter.Rdata")

#2.4 Year DataSet
fmli.final.year <- fmli.jt.rest %>%
  separate(expense, into = c("expensetype", "year", "quarter")) %>%
  spread(expensetype, expenseamount) %>%
  group_by(year) %>%
  summarize(Total = mean(TotalExp),
            Food = mean(FoodExp), 
            Alcohol = mean(AlcoholExp), 
            House = mean(HouseExp), 
            Furniture = mean(HousefurnitureExp),
            Clothes = mean(ClothesExp),
            Transportation = mean(TransportExp),
            Healthcare = mean(HealthcareExp),
            Entertainment = mean(EntertainExp),
            Education = mean(EducationExp))  %>% 
  gather(expensetype, expenseamount, -year) 

fmli.final.year$year <- paste0("20", fmli.final.year$year)
fmli.final.year$year <-as.numeric(fmli.final.year$year)
save(fmli.final.year, file="fmli.final.year.Rdata")




