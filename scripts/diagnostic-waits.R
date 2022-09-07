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


## Diagnostic waiting times by health board

DWTHB <- read.csv("https://www.opendata.nhs.scot/dataset/3d1f49b2-f770-492f-82c9-ebefdc56ece4/resource/10dfe6f3-32de-4039-84c2-7e7794a06b31/download/diagnostics_by_board_december_2021.csv")

names(DWTHB)[names(DWTHB) == 'HBT'] <- 'HB'

## Diagnostic waiting times Scotland

DWTS <- read.csv("https://www.opendata.nhs.scot/dataset/3d1f49b2-f770-492f-82c9-ebefdc56ece4/resource/df75544f-4ba1-488d-97c7-30ab6258270d/download/diagnostics_scotland_december_2021.csv")

names(DWTS)[names(DWTS) == 'Country'] <- 'HB'

## Merge Scotland and boards and look up area codes

diagnostic_waits <- rbind(DWTS, DWTHB) %>% 
  left_join(lookups)

##date formatting and remove NA values

diagnostic_waits <- diagnostic_waits %>%
  mutate(Date=as.Date(as.character(MonthEnding), format="%Y%m%d"))

diagnostic_waits$NumberOnList <- replace(diagnostic_waits$NumberOnList, is.na(diagnostic_waits$NumberOnList), 0)




## group formatting
## 15-21days missing space

diagnostic_waits$WaitingTime <- factor(diagnostic_waits$WaitingTime, 
                                       levels = c("0-7 days", "8-14 days", "15-21days", "22-28 days",
                                                  "29-35 days", "36-42 days", "43-49 days", "50-56 days",
                                                  "57-63 days", "64-70 days", "71-77 days", "78-84 days",
                                                  "85-91 days", "92-182 days", "183-273 days", "274-364 days",
                                                  "365 days and over")) 

Group_order <- c("0-7 days", "8-14 days", "15-21days", "22-28 days",
              "29-35 days", "36-42 days", "43-49 days", "50-56 days",
              "57-63 days", "64-70 days", "71-77 days", "78-84 days",
              "85-91 days", "92-182 days", "183-273 days", "274-364 days",
              "365 days and over")

diagnostic_waits <- diagnostic_waits[ order(match(diagnostic_waits$WaitingTime, Group_order)), ]

## pivot data 

diagnostic_waits_pivot <- diagnostic_waits %>% 
  group_by(MonthEnding, HB, HBName, DiagnosticTestType, DiagnosticTestDescription) %>% 
  pivot_wider(names_from = WaitingTime, values_from = NumberOnList) %>% 
  rowwise() %>% mutate(Total = sum(c_across(`0-7 days`:`365 days and over`))) %>% 
  rowwise() %>% mutate(`Within 6 weeks` = sum(c_across(`0-7 days`:`36-42 days`))) %>%
  rowwise() %>% mutate(`Over 6 weeks` = sum(c_across(`43-49 days`:`365 days and over`))) %>%
  mutate(`% on target`=round (`Under 6 weeks`/`Total`*100, digit=1)) %>% 
  mutate(`% not on target`=round (`Over 6 weeks`/`Total`*100, digit=1))
  
## export

diagnostic_waits_pivot %>% 
  group_by(DiagnosticTestDescription) %>% 
  group_walk(~ write_csv(.x, paste0("data/diagnostic-waits/", .y$DiagnosticTestDescription, ".csv")))
