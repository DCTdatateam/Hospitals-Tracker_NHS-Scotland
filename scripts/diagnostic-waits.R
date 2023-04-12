##libraries
library(tidyverse)
library(dplyr)


##Health boards and special health boards

HB <- read.csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/hb14_hb19.csv") %>%
  select(c("HB","HBName")) 


SHB <- read.csv("https://www.opendata.nhs.scot/dataset/65402d20-f0f1-4cee-a4f9-a960ca560444/resource/0450a5a2-f600-4569-a9ae-5d6317141899/download/special-health-boards_19022021.csv") %>% 
  select(c("SHB","SHBName")) %>% 
  rename(HB = SHB,
         HBName = SHBName)

healthboards <- rbind(HB, SHB)

## Scotland

ScotID <- data.frame("S92000003", "Scotland")  
names(ScotID) <- c("HB", "HBName")

lookups <- rbind(healthboards,ScotID)


## Diagnostic waiting times by health board

DWTHB <- read.csv("https://www.opendata.nhs.scot/dataset/3d1f49b2-f770-492f-82c9-ebefdc56ece4/resource/10dfe6f3-32de-4039-84c2-7e7794a06b31/download/diagnostics_by_board_december_2021.csv")

## compare to previous 

old <- read.csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/diagnostics_by_board_december_2022.csv")

## column compare
columns_equal <- setequal(names(old), names(DWTHB))

columns_added <- setdiff(names(DWTHB), names(old))
columns_dropped <- setdiff(names(old), names(DWTHB))

added_empty <- identical(columns_added, character(0))
dropped_empty <- identical(columns_dropped, character(0))


if (dropped_empty == TRUE & added_empty == FALSE) {
  message= paste("Column(s) added: ", list(columns_added))
} else if (dropped_empty == FALSE & added_empty == TRUE) {
  message= paste("Column(s) removed: ", list(columns_dropped))
} else if (dropped_empty == FALSE & added_empty == FALSE) {
  message= paste("Column(s) removed: ", list(columns_dropped), 
                 "Column(s) added: ", list(columns_added))
} else {message <- NULL}

column_compare <- 
  if(columns_equal == FALSE) {
    message(paste("Warning: Column names changed in diagnostic waits by HB", message))
  }else if(columns_equal == TRUE) {
    print('Diagnostic waits by HB column names match')
  }

## group compare waiting times
group_equal <- setequal(DWTHB$WaitingTime,old$WaitingTime)

group_added <- setdiff(DWTHB$WaitingTime,old$WaitingTime)

group_removed <- setdiff(old$WaitingTime, DWTHB$WaitingTime)

group_added_empty <- identical(group_added, character(0))
group_removed_empty <- identical(group_removed, character(0))

if (group_removed_empty == TRUE & group_added_empty == FALSE) {
  message= paste("Group(s) added: ", list(group_added))
} else if (group_removed_empty == FALSE & group_added_empty == TRUE) {
  message= paste("Group(s) removed: ", list(group_removed))
} else if (group_removed_empty == FALSE & group_added_empty == FALSE) {
  message= paste("Group(s) removed: ", list(group_removed), 
                 "Group(s) added: ", list(group_added))
} else {message <- NULL}

group_compare <- 
  if(group_equal == FALSE) {
    message(paste("Warning: Waiting time group change in diagnostic waits by HB.", message))
  }else if(group_equal == TRUE) {
    print('Diagnostic waiting times groups unchanged')
  }

## Diagnostic waiting times Scotland

DWTS <- read.csv("https://www.opendata.nhs.scot/dataset/3d1f49b2-f770-492f-82c9-ebefdc56ece4/resource/df75544f-4ba1-488d-97c7-30ab6258270d/download/diagnostics_scotland_december_2021.csv")

## compare to previous 

old <- read.csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/diagnostics_scotland_june_2022_new.csv")


## column compare
columns_equal <- setequal(names(old), names(DWTS))

columns_added <- setdiff(names(DWTS), names(old))
columns_dropped <- setdiff(names(old), names(DWTS))

added_empty <- identical(columns_added, character(0))
dropped_empty <- identical(columns_dropped, character(0))


if (dropped_empty == TRUE & added_empty == FALSE) {
  message= paste("Column(s) added: ", list(columns_added))
} else if (dropped_empty == FALSE & added_empty == TRUE) {
  message= paste("Column(s) removed: ", list(columns_dropped))
} else if (dropped_empty == FALSE & added_empty == FALSE) {
  message= paste("Column(s) removed: ", list(columns_dropped), 
                 "Column(s) added: ", list(columns_added))
} else {message <- NULL}

column_compare <- 
  if(columns_equal == FALSE) {
    message(paste("Warning: Column names changed in diagnostic waits Scotland", message))
  }else if(columns_equal == TRUE) {
    print('Diagnostic waits Scotland column names match')
  }

## group compare waiting times
group_equal <- setequal(DWTS$WaitingTime,old$WaitingTime)

group_added <- setdiff(DWTS$WaitingTime,old$WaitingTime)

group_removed <- setdiff(old$WaitingTime, DWTS$WaitingTime)

group_added_empty <- identical(group_added, character(0))
group_removed_empty <- identical(group_removed, character(0))

if (group_removed_empty == TRUE & group_added_empty == FALSE) {
  message= paste("Group(s) added: ", list(group_added))
} else if (group_removed_empty == FALSE & group_added_empty == TRUE) {
  message= paste("Group(s) removed: ", list(group_removed))
} else if (group_removed_empty == FALSE & group_added_empty == FALSE) {
  message= paste("Group(s) removed: ", list(group_removed), 
                 "Group(s) added: ", list(group_added))
} else {message <- NULL}

group_compare <- 
  if(group_equal == FALSE) {
    message(paste("Warning: Waiting time group change in diagnostic waits Scotland.", message))
  }else if(group_equal == TRUE) {
    print('Diagnostic waiting times Scotland groups unchanged')
  }

## Merge Scotland and boards and look up area codes
names(DWTHB)[names(DWTHB) == 'HBT'] <- 'HB'
names(DWTS)[names(DWTS) == 'Country'] <- 'HB'

diagnostic_waits <- rbind(DWTS, DWTHB) %>% 
  left_join(lookups)

##date formatting and remove blanks 
## and remove 'total waiting' (only given for March & April 2020 for Glasgow with no further breakdown of waiting times)

diagnostic_waits <- diagnostic_waits %>%
  mutate(Date=as.Date(as.character(MonthEnding), format="%Y%m%d")) %>% 
  filter(NumberOnListQF != "d:",
         NumberOnListQF != ":",
         WaitingTime != "Total Number Waiting")

## group formatting
## 15-21days missing space


diagnostic_waits$WaitingTime <- factor(diagnostic_waits$WaitingTime, 
                                       levels = c("0-7 days", "8-14 days", "15-21days", "22-28 days",
                                                  "29-35 days", "36-42 days", "43-49 days", "50-56 days",
                                                  "57-63 days", "64-70 days", "71-77 days", "78-84 days",
                                                  "85-91 days", "92-182 days", "183-273 days", "274-364 days", 
                                                  "92 days and over", "365 days and over")) 

Group_order <- c("0-7 days", "8-14 days", "15-21days", "22-28 days",
                 "29-35 days", "36-42 days", "43-49 days", "50-56 days",
                 "57-63 days", "64-70 days", "71-77 days", "78-84 days",
                 "85-91 days", "92-182 days", "183-273 days", "274-364 days", 
                 "92 days and over", "365 days and over")

diagnostic_waits <- diagnostic_waits[ order(match(diagnostic_waits$WaitingTime, Group_order)), ]

## pivot data & sum totals

diagnostic_waits_pivot <- diagnostic_waits %>% 
  group_by(MonthEnding, HB, HBName, DiagnosticTestType, DiagnosticTestDescription) %>% 
  pivot_wider(names_from = WaitingTime, values_from = NumberOnList)
  
## replace NA with 0 for row totals

diagnostic_waits_pivot$`92-182 days` <- replace(diagnostic_waits_pivot$`92-182 days`, is.na(diagnostic_waits_pivot$`92-182 days`), 0)
diagnostic_waits_pivot$`183-273 days` <- replace(diagnostic_waits_pivot$`183-273 days`, is.na(diagnostic_waits_pivot$`183-273 days`), 0)
diagnostic_waits_pivot$`274-364 days` <- replace(diagnostic_waits_pivot$`274-364 days`, is.na(diagnostic_waits_pivot$`274-364 days`), 0)
diagnostic_waits_pivot$`92 days and over` <- replace(diagnostic_waits_pivot$`92 days and over`, is.na(diagnostic_waits_pivot$`92 days and over`), 0)
diagnostic_waits_pivot$`365 days and over` <- replace(diagnostic_waits_pivot$`365 days and over`, is.na(diagnostic_waits_pivot$`365 days and over`), 0)

## totals & %s
diagnostic_waits_pivot <- diagnostic_waits_pivot %>% 
  rowwise() %>% mutate(Total = sum(c_across(`0-7 days`:`365 days and over`)),
                       `Within 6 weeks` = sum(c_across(`0-7 days`:`36-42 days`)),
                       `Over 6 weeks` = sum(c_across(`43-49 days`:`365 days and over`))) %>%
  mutate(`% on target`=round (`Within 6 weeks`/`Total`*100, digit=1),
         `% not on target`=round (`Over 6 weeks`/`Total`*100, digit=1))

## export

diagnostic_waits_pivot %>% 
  group_by(DiagnosticTestDescription) %>% 
  group_walk(~ write_csv(.x, paste0("data/diagnostic-waits/", .y$DiagnosticTestDescription, ".csv")))
