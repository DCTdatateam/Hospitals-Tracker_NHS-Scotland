
library(tidyverse)


##Health boards and special health boards


HB <- read.csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/hb14_hb19.csv")

HB <- HB[c("HB","HBName")]

SHB <- read.csv("https://www.opendata.nhs.scot/dataset/65402d20-f0f1-4cee-a4f9-a960ca560444/resource/0450a5a2-f600-4569-a9ae-5d6317141899/download/special-health-boards_19022021.csv")

SHB <- SHB[c("SHB","SHBName")]

names(SHB)[names(SHB) == 'SHB'] <- 'HB'
names(SHB)[names(SHB) == 'SHBName'] <- 'HBName'

healthboards <- rbind(HB, SHB)

## Scotland

ScotID <- data.frame("S92000003", "Scotland")  
names(ScotID) <- c("HB", "HBName")

lookups <- rbind(healthboards,ScotID)


## Beds information by health board (BIHB)

BIHB <- read.csv("https://www.opendata.nhs.scot/dataset/554b0e7a-ccac-4bb6-82db-1a8b306fcb36/resource/f272bb7d-5320-4491-84c1-614a2c064007/download/beds_by_nhs_board_of_treatment_and_specialty.csv")

## lookup area names
BIHB <- BIHB %>% 
  left_join(lookups)

## remove Golden Jubilee hosp & 
## filter to aggregate health board only

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
  mutate(Year=substr(Quarter, 1, 4)) %>%                                       
  mutate(Q=substr(Quarter, nchar(Quarter) - n_last + 1, nchar(Quarter))) %>% 
  mutate(Date=paste(Q,Year, sep=" "))                                          


## percentage occupancy by main acute specialties

acute_groups_percentage_occupancy <- BIHB_aggregate %>% 
  filter(SpecialtyName == "All Acute" | SpecialtyName =="Surgery Grouping"| 
           SpecialtyName =="Medical Grouping") %>% 
  group_by(Quarter, Date, HB, HBName, SpecialtyName) %>% 
  summarise(`% Occupancy`= round(PercentageOccupancy, 1)) %>% 
  pivot_wider(names_from = SpecialtyName, values_from = `% Occupancy`)
  
names(acute_groups_percentage_occupancy)[names(acute_groups_percentage_occupancy) == 'Medical Grouping'] <- 'Medical'
names(acute_groups_percentage_occupancy)[names(acute_groups_percentage_occupancy) == 'Surgery Grouping'] <- 'Surgery'



## number of staffed beds

bed_numbers_acute <- BIHB_aggregate %>% 
  filter(SpecialtyName == "All Acute") %>% 
  mutate(Unoccupied = AverageAvailableStaffedBeds-AverageOccupiedBeds)%>%
  mutate(`Average Available Staffed Beds`= round(AverageAvailableStaffedBeds, 0)) %>%
  mutate(`Average Occupied Beds` = round(AverageOccupiedBeds, 0))%>%
  mutate(`Average Unoccupied Beds` = round(AverageAvailableStaffedBeds-AverageOccupiedBeds, 0)) %>%
  mutate(`Percentage Occupied` = round(PercentageOccupancy, 1)) %>%
  mutate(`Percentage Unoccupied` = round(100-`Percentage Occupied`, 1)) %>% 
  group_by(Quarter, Date, HB, HBName, SpecialtyName) %>% 
  summarise(`Average Available Staffed Beds`,
            `Average Occupied Beds`, `Average Unoccupied Beds`,
            `Percentage Occupied`, `Percentage Unoccupied`)


## exports

write.csv(acute_groups_percentage_occupancy, "data/beds-information/percent_occupancy.csv", row.names = FALSE)
write.csv(bed_numbers_acute, "data/beds-information/bed_numbers.csv", row.names = FALSE)
