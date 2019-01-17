library(dplyr)
library(magrittr)
## Read Admission Data ---------------------------------------------------------------------------------
dat <- read.csv("Data/aha_data_main_indicators_2011_2017.csv", header=TRUE) %>% filter(Year %in% c(2016,2017))
sysupdate <- openxlsx::read.xlsx("Data/AHA_HealthSystem_Amend.xlsx")

# Update System_member
dat$AHA.ID <- as.character(dat$AHA.ID)
sysupdate$AHA.ID <- as.character(sysupdate$AHA.ID)
dat$System.ID <- as.character(dat$System.ID)

systbl <- dat %>% filter(AHA.ID %in% sysupdate$AHA.ID) %>%
  left_join(sysupdate[,c(1,5:6)], by = "AHA.ID") %>%
  mutate(System.member = "Y",
         System.ID = System.ID.y,
         System.Name = System.Name.y) %>%
  select(-c(System.ID.x,System.Name.x,System.ID.y,System.Name.y))

dat <- bind_rows((dat %>% filter(! AHA.ID %in% sysupdate$AHA.ID)),
                     systbl)

makeColNamesUserFriendly <- function(df) {
  # Convert any number of consecutive dots to a single space.
  names(df) <- gsub(x = names(df),
                    pattern = "(\\.)+",
                    replacement = "_")
  
  # Drop the trailing spaces.
  names(df) <- gsub(x = names(df),
                    pattern = "_$",
                    replacement = "")
  df
}
dat <- makeColNamesUserFriendly(dat)
dat$Medicare_Provider_ID <- as.numeric(as.character(dat$Medicare_Provider_ID))

rm(sysupdate,systbl,makeColNamesUserFriendly)

# Add HMA SYS
mappinglist <- openxlsx::read.xlsx("D:/GitHub/HMAResearch/HSData/Data/HMA_Mapping_List_201901.xlsx",sheet = "AHA Hospital List 2017")

dat2017 <- dat %>% filter(Year == 2017) %>%
  left_join(mappinglist[c(1:3)],by="AHA_ID") %>%
  select(2:4,15:17,36:37,27)
dat2016 <- dat %>% filter(Year == 2016) %>%
  left_join(mappinglist[c(1:3)],by="AHA_ID") %>%
  select(2:4,15:17,36:37,27)

rm(mappinglist,dat)

## Part 1: Electronic Lab Result -----------------------------------------------------------------------
stru_measures <- read.csv("Data/Structural Measures - Hospital.csv", header = TRUE) %>%
  filter(Measure.ID == "OP_17") 

labdat <- inner_join(dat2016, stru_measures[,c(1,11)],by=c("Medicare_Provider_ID" = "Provider.ID"))
labdat$AHA_ID <- as.numeric(labdat$AHA_ID)
# HMA Members
hma <- labdat %>% filter(HMA_Member == "Y") %>%
  count(HMA_System_Name,Measure.Response) %>%
  group_by(HMA_System_Name) %>%
  mutate(prop = n / sum(n)) %>%
  select(-n) %>%
  tidyr::spread(key = Measure.Response, value = prop)

# top 100 
top100mapping <- openxlsx::read.xlsx("Data/2017Top100HS_mapping_0117.xlsx",sheet = "Top_Operating_Revenue")

# Split by AHA_System_ID and AHA.ID
top100_sys <- top100mapping[,c(1:6)] %>% filter(!is.na(AHA_SyStem_ID)) %>% 
  left_join(labdat, by = c("AHA_SyStem_ID" = "System_ID")) %>%
  count(oprevrank,a_Name,Measure.Response) %>%
  filter(!is.na(Measure.Response)) %>%
  group_by(oprevrank,a_Name) %>%
  mutate(prop = n / sum(n)) %>%
  select(-n) %>%
  tidyr::spread(key = Measure.Response, value = prop)

top100_hos <- top100mapping %>% filter(!is.na(AHA.ID)) %>% 
  left_join(labdat, by = c("AHA.ID" = "AHA_ID")) %>%
  count(oprevrank,a_Name,Measure.Response) %>%
  filter(!is.na(Measure.Response)) %>%
  group_by(oprevrank,a_Name) %>%
  mutate(prop = n / sum(n)) %>%
  select(-n) %>%
  tidyr::spread(key = Measure.Response, value = prop)

top100 <- bind_rows(top100_sys,top100_hos) %>% arrange(oprevrank)

top100$elepercent_percentile <- cut(top100$Yes, breaks=c(0,mean(top100$Yes,na.rm = TRUE),1), 
                                    labels=c("Below Average","Above Agerage"))
