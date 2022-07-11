##libraries
library(tidyverse)
library(dplyr)
library(lubridate)                        
library(readr)   
library(Rcpp)
library(sf)
library(ggmap)
library(data.table)

## PHS Cancelled Planned operations

## 1. Cancelled operations by health board


CBHB <- read.csv("https://www.opendata.nhs.scot/dataset/479848ef-41f8-44c5-bfb5-666e0df8f574/resource/0f1cf6b1-ebf6-4928-b490-0a721cc98884/download/cancellations_by_board_february_2022.csv")

## 2. Cancelled operations Scotland

CS <- read.csv("https://www.opendata.nhs.scot/dataset/479848ef-41f8-44c5-bfb5-666e0df8f574/resource/df65826d-0017-455b-b312-828e47df325b/download/cancellations_scotland_february_2022.csv")


names(CS)[names(CS) == 'Country'] <- 'HBT'

## Merge Scotland and boards

cancelledops <- rbind(CS, CBHB)


##Health boards and special health boards


HB <- read.csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/hb14_hb19.csv")

HB <- HB[c("HB","HBName")]

names(HB)[names(HB) == 'HB'] <- 'HBT'

SHB <- read.csv("https://www.opendata.nhs.scot/dataset/65402d20-f0f1-4cee-a4f9-a960ca560444/resource/0450a5a2-f600-4569-a9ae-5d6317141899/download/special-health-boards_19022021.csv")

SHB <- SHB[c("SHB","SHBName")]

names(SHB)[names(SHB) == 'SHB'] <- 'HBT'
names(SHB)[names(SHB) == 'SHBName'] <- 'HBName'

healthboards <- rbind(HB, SHB)

## Scotland

ScotID <- data.frame("S92000003", "Scotland")  
names(ScotID) <- c("HBT", "HBName")

lookups <- rbind(healthboards,ScotID)

## join
cancelledops <- cancelledops %>% 
  left_join(lookups)


##date formatting

names(cancelledops)[names(cancelledops) == 'Month'] <- 'Date1'
cancelledops <- cancelledops %>%
  mutate(Date2=as.Date(paste0(as.character(Date1), '01'), format='%Y%m%d')) %>%
  mutate(Year=format(as.Date(Date2, format="%d/%m/%Y"),"%Y")) %>%
  mutate(Month=format(as.Date(Date2, format="%d/%m/%Y"),"%b"))


##calculated fields: performed (planned - cancelled), and % calculations 
##(cancellations by type as % of all planned ops and as a % of cancelled operations)
##duplicate columns for Flourish series error

cancelledops <- cancelledops %>%
  mutate(Performed=TotalOperations-TotalCancelled) %>%
  mutate(Cancelled_By_Patient_pc_of_planned_ops=round(CancelledByPatientReason/TotalOperations*100, digit = 1)) %>%
  mutate(Cancelled_clinical_reason_pc_of_planned_ops=round(ClinicalReason/TotalOperations*100, digit = 1)) %>%
  mutate(Non_clinical_capacity_reason_pc_of_planned_ops=round(NonClinicalCapacityReason/TotalOperations*100, digit =1)) %>%
  mutate(Other_Reason_pc_of_planned_ops=round(OtherReason/TotalOperations*100, digit = 1)) %>%
  mutate(Cancelled_By_Patient_pc_of_cancelled_ops=round(CancelledByPatientReason/TotalCancelled*100, digit = 1)) %>%
  mutate(Cancelled_clinical_reason_pc_of_cancelled_ops=round(ClinicalReason/TotalCancelled*100, digit = 1)) %>%
  mutate(Non_clinical_capacity_reason_pc_of_cancelled_ops=round(NonClinicalCapacityReason/TotalCancelled*100, digit = 1)) %>%
  mutate(Other_Reason_pc_of_cancelled_ops=round(OtherReason/TotalCancelled*100, digit = 1)) %>%
  mutate(Performed_PC=round(Performed/TotalOperations*100, digit = 1)) %>%
  mutate(Cancelled_PC=round(TotalCancelled/TotalOperations*100, digit = 1)) %>%
  mutate(PerformedPopUp=Performed) %>%
  mutate(CancelledPopUp=TotalCancelled)


cancelledops <- cancelledops[c("Date2", "Month","Year","HBT","HBName","TotalOperations",
                               "TotalCancelled","Performed","Performed_PC", "Cancelled_PC",
                               "CancelledByPatientReason",
                               "ClinicalReason","NonClinicalCapacityReason","OtherReason",
                               "Cancelled_By_Patient_pc_of_planned_ops","Cancelled_clinical_reason_pc_of_planned_ops",
                               "Non_clinical_capacity_reason_pc_of_planned_ops","Other_Reason_pc_of_planned_ops",
                               "Cancelled_By_Patient_pc_of_cancelled_ops","Cancelled_clinical_reason_pc_of_cancelled_ops",
                               "Non_clinical_capacity_reason_pc_of_cancelled_ops","Other_Reason_pc_of_cancelled_ops", 
                               "PerformedPopUp", "CancelledPopUp")]

names(cancelledops)[names(cancelledops) == 'TotalCancelled'] <- 'Cancelled'
names(cancelledops)[names(cancelledops) == 'CancelledByPatientReason'] <- 'Cancelled by patient'
names(cancelledops)[names(cancelledops) == 'NonClinicalCapacityReason'] <- 'Capacity reason'
names(cancelledops)[names(cancelledops) == 'OtherReason'] <- 'Other reason'
names(cancelledops)[names(cancelledops) == 'ClinicalReason'] <- 'Clinical reason'


## 3. Cancelled operations by hospital
## remove duplicates issue by URN


CBHOS <- read.csv("https://www.opendata.nhs.scot/dataset/479848ef-41f8-44c5-bfb5-666e0df8f574/resource/bcc860a4-49f4-4232-a76b-f559cf6eb885/download/cancellations_by_hospital_march_2022.csv")

CBHOS <- CBHOS %>%
  mutate(URN=paste(Month,Hospital, sep=""))

CBHOS <- CBHOS[-which(duplicated(CBHOS$URN)), ]

## Hospital locations (open only)

currenthospitals <- read.csv("https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-d7bdcfb120f0/resource/c698f450-eeed-41a0-88f7-c1e40a568acc/download/current-hospital_flagged20211216.csv")


## convert to lat / long
## excludes NA coordinates

hospitalslookup <- currenthospitals[c("Location","LocationName", 
                                     "Postcode","AddressLine", "HB","CA","XCoordinate",
                                     "YCoordinate")] %>%
  filter(!is.na(XCoordinate)| !is.na(YCoordinate))

hospitalslookup <- st_as_sf(hospitalslookup, coords = c("XCoordinate", "YCoordinate"), crs = 7405) ##EPSG:7405: OSGB36 / British National Grid + ODN height

hospitalslookup <- st_transform(hospitalslookup, crs = 4326 ) 
hospitalslookup <- hospitalslookup %>%
  mutate( lon = st_coordinates(hospitalslookup)[,1],
          lat = st_coordinates(hospitalslookup)[,2])


## council areas

CA <- read.csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/967937c4-8d67-4f39-974f-fd58c4acfda5/download/ca11_ca19.csv")

CA <- CA %>%
  filter(is.na(CADateArchived)) %>%
  filter(is.na(HSCPDateArchived))%>%
  filter(is.na(HBDateArchived))

CA <- CA[c("CA","CAName")]

##joins

hospitalslookup <- hospitalslookup %>%
  left_join(CA)

names(CBHOS)[names(CBHOS) == 'Hospital'] <- 'Location'


CBHOS <- CBHOS %>% 
  left_join(hospitalslookup) 


##date formatting

names(CBHOS)[names(CBHOS) == 'Month'] <- 'Date1'
CBHOS <- CBHOS %>%
  mutate(Date2=as.Date(paste0(as.character(Date1), '01'), format='%Y%m%d')) %>%
  mutate(Year=format(as.Date(Date2, format="%d/%m/%Y"),"%Y")) %>%
  mutate(Month=format(as.Date(Date2, format="%d/%m/%Y"),"%b"))


##calculated fields: performed (planned - cancelled), and % calculations 
##(cancellations by type as % of all planned ops and as a % of canceled operations)
##duplicate columns for Flourish series error

CBHOS <- CBHOS %>%
  mutate(Performed=TotalOperations-TotalCancelled) %>%
  mutate(Cancelled_By_Patient_pc_of_planned_ops=round(CancelledByPatientReason/TotalOperations*100, digit = 1)) %>%
  mutate(Cancelled_clinical_reason_pc_of_planned_ops=round(ClinicalReason/TotalOperations*100, digit = 1)) %>%
  mutate(Non_clinical_capacity_reason_pc_of_planned_ops=round(NonClinicalCapacityReason/TotalOperations*100, digit =1)) %>%
  mutate(Other_Reason_pc_of_planned_ops=round(OtherReason/TotalOperations*100, digit = 1)) %>%
  mutate(Cancelled_By_Patient_pc_of_cancelled_ops=round(CancelledByPatientReason/TotalCancelled*100, digit = 1)) %>%
  mutate(Cancelled_clinical_reason_pc_of_cancelled_ops=round(ClinicalReason/TotalCancelled*100, digit = 1)) %>%
  mutate(Non_clinical_capacity_reason_pc_of_cancelled_ops=round(NonClinicalCapacityReason/TotalCancelled*100, digit = 1)) %>%
  mutate(Other_Reason_pc_of_cancelled_ops=round(OtherReason/TotalCancelled*100, digit = 1)) %>%
  mutate(Performed_PC=round(Performed/TotalOperations*100, digit = 1)) %>%
  mutate(Cancelled_PC=round(TotalCancelled/TotalOperations*100, digit = 1)) %>%
  mutate(Group=Cancelled_PC) %>%
  mutate(PerformedPopUp=Performed) %>%
  mutate(CancelledPopUp=TotalCancelled)


CBHOS <- CBHOS[c("Date2","Month","Year","Location", "LocationName",
                 "Postcode", "AddressLine", "lon", "lat", "CAName", "TotalOperations",
                 "TotalCancelled","Performed","Performed_PC", "Cancelled_PC", "Group",
                 "CancelledByPatientReason",
                 "ClinicalReason","NonClinicalCapacityReason","OtherReason",
                 "Cancelled_By_Patient_pc_of_planned_ops","Cancelled_clinical_reason_pc_of_planned_ops",
                 "Non_clinical_capacity_reason_pc_of_planned_ops","Other_Reason_pc_of_planned_ops",
                 "Cancelled_By_Patient_pc_of_cancelled_ops","Cancelled_clinical_reason_pc_of_cancelled_ops",
                 "Non_clinical_capacity_reason_pc_of_cancelled_ops","Other_Reason_pc_of_cancelled_ops", 
                 "PerformedPopUp", "CancelledPopUp")]

names(CBHOS)[names(CBHOS) == 'TotalCancelled'] <- 'Cancelled'
names(CBHOS)[names(CBHOS) == 'CancelledByPatientReason'] <- 'Cancelled by patient'
names(CBHOS)[names(CBHOS) == 'NonClinicalCapacityReason'] <- 'Capacity reason'
names(CBHOS)[names(CBHOS) == 'OtherReason'] <- 'Other reason'
names(CBHOS)[names(CBHOS) == 'ClinicalReason'] <- 'Clinical reason'




##exclude NA locations & locations without latest month data
## reorder chronologically


latestdate <- max(CBHOS$Date2)

CBHOS <- CBHOS %>%
  mutate(filename=gsub(" ", "", LocationName)) %>%
  filter(lat != "")

time_series_hosp <- CBHOS %>%
  group_by(Location) %>%
  filter(any(Date2==latestdate)) 


time_series_hosp <- time_series_hosp[order(as.Date(time_series_hosp$Date2, format="%d/%m/%Y")),]


## filter to latest date for map base
hospitalmapbase <- CBHOS %>% 
  filter(Date2==max(Date2))

## categories & flourish marker icons for % cancelled in latest month


markerlookup <- data.frame(Group=rep(c('0-10', '10-20', '20-30', '30-40', '50+')),
                           Marker=rep(c('https://public.flourish.studio/uploads/654810/6e42adcc-51f4-4a13-9a1b-2f4cb1c18b89.png', 
                                        'https://public.flourish.studio/uploads/654810/752b0d31-bca6-45d2-843c-a2b913e0aadd.png', 
                                        'https://public.flourish.studio/uploads/654810/102c623a-98e7-4419-b8a1-2acd7f49d14f.png', 
                                        'https://public.flourish.studio/uploads/654810/d51b7238-8264-4440-ad9c-4e7dad619021.png', 
                                        'https://public.flourish.studio/uploads/654810/80fa7955-f2fd-4fb2-a6e6-75ea8901a5df.png')))




hospitalmapbase$Group<-c( "0-10", "10-20", "20-30", "30-40", "40-50", "50+")[
  findInterval(hospitalmapbase$Group , c(-Inf, 9.99, 19.99, 29.99, 39.99, 49.99, Inf) ) ]

hospitalmapbase <- hospitalmapbase %>% 
  left_join(markerlookup)

## lookup flourish chart IDs

Chart_ID <- read.csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/Cancelled-operations-hospital-chart-ids_apr2022.csv")

hospitalmapbase <- hospitalmapbase %>%
  left_join(Chart_ID, by = 'Location')

## exports 

## 1. cancelled ops by healthboard
write.csv(cancelledops, "data/cancelled-operations/scheduled_and_cancelled_ops_by_HB.csv", row.names = FALSE)

## 2. hospital map base 
write.csv(hospitalmapbase, "data/cancelled-operations/map_base.csv", row.names = FALSE) ## all hospitals

##3 hospitals time series
time_series_hosp %>% 
  group_by(filename) %>% 
  group_walk(~ write_csv(.x, paste0("data/cancelled-operations/hospitals/", .y$filename, ".csv")))

