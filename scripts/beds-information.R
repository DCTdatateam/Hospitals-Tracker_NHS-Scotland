library(curl)
library(tidyverse)


##Health boards and special health boards

temp <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/hb14_hb19.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

HB <- read.csv(temp)

HB <- HB[c("HB","HBName")]

temp1 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/65402d20-f0f1-4cee-a4f9-a960ca560444/resource/0450a5a2-f600-4569-a9ae-5d6317141899/download/special-health-boards_19022021.csv"
temp1 <- curl_download(url=source, destfile=temp1, quiet=FALSE, mode="wb")

SHB <- read.csv(temp1)

SHB <- SHB[c("SHB","SHBName")]

names(SHB)[names(SHB) == 'SHB'] <- 'HB'
names(SHB)[names(SHB) == 'SHBName'] <- 'HBName'

healthboards <- rbind(HB, SHB)

## Scotland

ScotID <- data.frame("S92000003", "Scotland")  
names(ScotID) <- c("HB", "HBName")

lookups <- rbind(healthboards,ScotID)


## Beds information by health board (BIHB)


temp2 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/554b0e7a-ccac-4bb6-82db-1a8b306fcb36/resource/f272bb7d-5320-4491-84c1-614a2c064007/download/beds_by_nhs_board_of_treatment_and_specialty.csv"
temp2 <- curl_download(url=source, destfile=temp2, quiet=FALSE, mode="wb")

BIHB <- read.csv(temp2)

## lookup area names
BIHB <- BIHB %>% 
  left_join(lookups)

## remove Golden Jubilee hosp & 
## filter to aggregate health board only & main specialty groupings

BIHB_aggregate <- BIHB %>%
  filter(HB != "SB0801") %>%
  filter(LocationQF=="d") 
  

## reorder Scotland first


BIHB_aggregate$HB <- factor(BIHB_aggregate$HB, levels = c("S92000003", "S08000015", "S08000016", "S08000017",
                                       "S08000018", "S08000019", "S08000020", "S08000021",
                                       "S08000022", "S08000023", "S08000024", "S08000025",
                                       "S08000026", "S08000027", "S08000028", "S08000029",
                                       "S08000030", "S08000031", "S08000032")) 

HB_order <- c("S92000003", "S08000015", "S08000016", "S08000017",
              "S08000018", "S08000019", "S08000020", "S08000021",
              "S08000022", "S08000023", "S08000024", "S08000025",
              "S08000026", "S08000027", "S08000028", "S08000029",
              "S08000030", "S08000031", "S08000032")

BIHB_aggregate <- BIHB_aggregate[ order(match(BIHB_aggregate$HB, HB_order)), ]


## date formatting
n_last <- 2                                # Specify number of characters to extract for Q


BIHB_aggregate <- BIHB_aggregate %>%
  mutate(Year=substr(Quarter, 1, 4)) %>%                                      ##year 
  mutate(Q=substr(Quarter, nchar(Quarter) - n_last + 1, nchar(Quarter))) %>%  ##q
  mutate(Date=paste(Q,Year, sep=" "))                                          ##concatenate

## select columns

BIHB_aggregate <- BIHB_aggregate[c("Quarter", "Year", "Q","Date","HB", "HBName", 
                              "SpecialtyName","AllStaffedBeddays",
                              "TotalOccupiedBeddays", "AverageAvailableStaffedBeds",
                              "AverageOccupiedBeds", "PercentageOccupancy")]

## pivot specialties percentage occupancy
##Unique ref for specialty totals

PercentageOccupancy <- BIHB_aggregate %>%
  filter(SpecialtyName =="All Acute" | SpecialtyName =="Surgery Grouping"| SpecialtyName =="Medical Grouping"| SpecialtyName =="Obstetics"| SpecialtyName =="Geriatric Medicine - long stay unit"|
           SpecialtyName =="General Psychiatry (Mental Illness)" | SpecialtyName =="Psychiatry of Old Age"|
           SpecialtyName =="Child & Adolescent Psychiatry" | SpecialtyName =="All Specialties")

PercentageOccupancy <- PercentageOccupancy[c("Quarter", "Year", "Q","Date","HB", "HBName", 
                                      "SpecialtyName","PercentageOccupancy")]

PercentageOccupancy <- PercentageOccupancy %>%
  mutate(URN=paste(Quarter,HB, sep="")) %>%
  mutate(RoundedPercentageOccupancy=round(PercentageOccupancy, digits = 2))


## filter and rejoin to transpose specialties


pivot_percentage_occupancy = pivot_wider(PercentageOccupancy, id_cols = URN, names_from = SpecialtyName, 
                              values_from = RoundedPercentageOccupancy)

## rejoin to dates & HB info

date_lookup <- PercentageOccupancy %>%
  filter(SpecialtyName=="All Acute")

date_lookup <- date_lookup[c("URN", "Quarter", "Year", "Q","Date","HB", "HBName")]

pivot_percentage_occupancy <- pivot_percentage_occupancy %>%
  left_join(date_lookup)

## summary % occupancy
  
maindepartments_percentageoccupancy <- pivot_percentage_occupancy %>%
  mutate(`All Capacity`=100-`All Specialties`) %>%
  mutate(`Acute Capacity`=100-`All Acute`) %>%
  mutate(Top=100)%>%
  mutate(Base=0)

names(maindepartments_percentageoccupancy)[names(maindepartments_percentageoccupancy) == 'Medical Grouping'] <- 'Medical'
names(maindepartments_percentageoccupancy)[names(maindepartments_percentageoccupancy) == 'Surgery Grouping'] <- 'Surgery'



## number of beds
Bed_numbers_Acute <- BIHB_aggregate %>%
  filter(SpecialtyName=="All Acute") %>%
  mutate(Unoccupied=AverageAvailableStaffedBeds-AverageOccupiedBeds)
  
Bed_numbers_Acute <- Bed_numbers_Acute %>%
  mutate(`Average Available Staffed Beds`=round(Bed_numbers_Acute$AverageAvailableStaffedBeds, 0)) %>%
  mutate(`Average Occupied Beds`=round(Bed_numbers_Acute$AverageOccupiedBeds, 0))%>%
  mutate(`Average Unoccupied Beds`=round(Bed_numbers_Acute$Unoccupied, 0)) %>%
  mutate(`Percentage Occupied`=round(Bed_numbers_Acute$PercentageOccupancy, 1)) %>%
  mutate(`Percentage Unoccupied`=round(100-`Percentage Occupied`, 1))


## exports

write.csv(maindepartments_percentageoccupancy, "data/beds-information/percent_occupancy.csv", row.names = FALSE)
write.csv(Bed_numbers_Acute, "data/beds-information/bed_numbers.csv", row.names = FALSE)
