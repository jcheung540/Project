library(dplyr)
library(magrittr)
library(openxlsx)

## Read Admission Data ---------------------------------------------------------------------------------
dat <- read.csv("Data/aha_data_main_indicators_2011_2017.csv", header=TRUE) %>% filter(Year %in% c(2016,2017))
sysupdate <- read.xlsx("Data/AHA_HealthSystem_Amend.xlsx")

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
mappinglist <- read.xlsx("D:/GitHub/HMAResearch/HSData/Data/HMA_Mapping_List_201901.xlsx",sheet = "AHA Hospital List 2017")

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
top100mapping <- read.xlsx("Data/2017Top100HS_mapping_0117.xlsx",sheet = "Top_Operating_Revenue")

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

top100$electrical_lab_percentile <- cut(top100$Yes, breaks=c(0,mean(top100$Yes,na.rm = TRUE),1), 
                                    labels=c("Below Average","Above Average"))

hma %<>% left_join(top100mapping[,c(1,4)], by = "HMA_System_Name")

stat <- data.frame(Metrics = "Average % of Hospitals use electrical lab result in Top 100 HS", 
                   value = mean(top100$Yes,na.rm = TRUE))

list_of_datasets <- list("KeyStat" = stat, "Top100" = top100, "HMA" = hma)
write.xlsx(list_of_datasets, file = "Result/electrical_lab_result.xlsx")

rm(hma,labdat,list_of_datasets,stat,stru_measures,top100,top100_hos,top100_sys)


## Part 2: Readmission -----------------------------------------------------------------------
readmisson <- read.csv("Data/Unplanned Hospital Visits - Hospital.csv", header = TRUE) %>%
  filter(Measure.ID == "READM_30_PN") 
readmisson$Provider.ID <- as.numeric(as.character(readmisson$Provider.ID))
readmisson$Score <- as.numeric(as.character(readmisson$Score))

readm <- inner_join(dat2017, (readmisson[,c(1,13)] %>% filter(!is.na(Provider.ID))),
                    by=c("Medicare_Provider_ID" = "Provider.ID"))
readm$AHA_ID <- as.numeric(readm$AHA_ID)

# HMA Members
hma <- readm %>% filter(HMA_Member == "Y" & !is.na(Score)) %>%
  group_by(HMA_System_Name) %>%
  summarize(
    avg_readm_score = (Admissions %*% Score)/sum(Admissions))

# top 100 
# Split by AHA_System_ID and AHA.ID
top100_sys <- top100mapping[,c(1:6)] %>% filter(!is.na(AHA_SyStem_ID)) %>% 
  left_join(readm, by = c("AHA_SyStem_ID" = "System_ID")) %>%
  filter(!is.na(Score)) %>%
  group_by(oprevrank,a_Name) %>%
  summarize(
    avg_readm_score = (Admissions %*% Score)/sum(Admissions))

top100_hos <- top100mapping %>% filter(!is.na(AHA.ID)) %>% 
  left_join(readm, by = c("AHA.ID" = "AHA_ID")) %>%
  filter(!is.na(Score)) %>%
  group_by(oprevrank,a_Name) %>%
  summarize(
    avg_readm_score = (Admissions %*% Score)/sum(Admissions))

top100 <- bind_rows(top100_sys,top100_hos) %>% arrange(oprevrank)

readm_cutoff <- quantile(top100$avg_readm_score,na.rm = TRUE, probs = c(0.1,0.25,0.5,0.75,0.9))


top100$readm_percentile <- cut(top100$avg_readm_score, breaks=c(0,readm_cutoff,100), 
                                       labels=c("Below 10%","10 percentile","25 percentile",
                                                "50 percentile","75 percentile","90 percentile"))

hma %<>% left_join(top100mapping[,c(1,4)], by = "HMA_System_Name")

stat <- data.frame( percentile = row.names(as.data.frame(readm_cutoff)),
                    readm_score_cutoff = readm_cutoff)


list_of_datasets <- list("Keycutoff" = stat, "Top100" = top100, "HMA" = hma)
write.xlsx(list_of_datasets, file = "Result/readmission_result.xlsx")

rm(hma,readmisson, readm,list_of_datasets,stat,top100,top100_hos,top100_sys)

## Part 3: Infection -----------------------------------------------------------------------
infection <- read.csv("Data/Complications and Deaths - Hospital.csv", header = TRUE) %>%
  filter(Measure.ID == "PSI_13_POST_SEPSIS") 
infection$Provider.ID <- as.numeric(as.character(infection$Provider.ID))
infection$Score <- as.numeric(as.character(infection$Score))

infec <- inner_join(dat2017, (infection[,c(1,13)] %>% filter(!is.na(Provider.ID))),
                    by=c("Medicare_Provider_ID" = "Provider.ID"))
infec$AHA_ID <- as.numeric(infec$AHA_ID)

# HMA Members
hma <- infec %>% filter(HMA_Member == "Y" & !is.na(Score)) %>%
  group_by(HMA_System_Name) %>%
  summarize(
    avg_infection_score = (Admissions %*% Score)/sum(Admissions))

# top 100 
# Split by AHA_System_ID and AHA.ID
top100_sys <- top100mapping[,c(1:6)] %>% filter(!is.na(AHA_SyStem_ID)) %>% 
  left_join(infec, by = c("AHA_SyStem_ID" = "System_ID")) %>%
  filter(!is.na(Score)) %>%
  group_by(oprevrank,a_Name) %>%
  summarize(
    avg_infection_score = (Admissions %*% Score)/sum(Admissions))

top100_hos <- top100mapping %>% filter(!is.na(AHA.ID)) %>% 
  left_join(infec, by = c("AHA.ID" = "AHA_ID")) %>%
  filter(!is.na(Score)) %>%
  group_by(oprevrank,a_Name) %>%
  summarize(
    avg_infection_score = (Admissions %*% Score)/sum(Admissions))

top100 <- bind_rows(top100_sys,top100_hos) %>% arrange(oprevrank)

infection_cutoff <- quantile(top100$avg_infection_score,na.rm = TRUE, probs = c(0.1,0.25,0.5,0.75,0.9))


top100$infection_percentile <- cut(top100$avg_infection_score, breaks=c(0,infection_cutoff,100), 
                         labels=c("Below 10%","10 percentile","25 percentile",
                                  "50 percentile","75 percentile","90 percentile"))

hma %<>% left_join(top100mapping[,c(1,4)], by = "HMA_System_Name")

stat <- data.frame( percentile = row.names(as.data.frame(infection_cutoff)),
                    infection_score_cutoff = infection_cutoff)


list_of_datasets <- list("Keycutoff" = stat, "Top100" = top100, "HMA" = hma)
write.xlsx(list_of_datasets, file = "Result/infection_result.xlsx")

rm(hma,infection,infec,list_of_datasets,stat,top100,top100_hos,top100_sys)

## Part 4: HAI -----------------------------------------------------------------------
haidat <- read.csv("Data/Healthcare Associated Infections - Hospital.csv", header = TRUE) %>%
  filter(Measure.ID %in% c("HAI-5-SIR","HAI-6-SIR")) 

haidat$Score <- as.numeric(as.character(haidat$Score))

hai <- inner_join(dat2017, (haidat[,c(1,10,12)] %>% filter(!is.na(Provider.ID) & !is.na(Score))),
                    by=c("Medicare_Provider_ID" = "Provider.ID")) %>%
  tidyr::spread(key = Measure.ID, value = Score)
hai$AHA_ID <- as.numeric(hai$AHA_ID)

# HMA Members
hma <- hai %>% filter(HMA_Member == "Y") %>%
  group_by(HMA_System_Name) %>%
  summarize(
    avg_HAI5_score = sum(Admissions*`HAI-5-SIR`, na.rm = TRUE)/sum(Admissions,na.rm = TRUE),
    avg_HAI6_score = sum(Admissions*`HAI-6-SIR`, na.rm = TRUE)/sum(Admissions,na.rm = TRUE))

# top 100 
# Split by AHA_System_ID and AHA.ID
top100_sys <- top100mapping[,c(1:6)] %>% filter(!is.na(AHA_SyStem_ID)) %>% 
  left_join(hai, by = c("AHA_SyStem_ID" = "System_ID")) %>%
  group_by(oprevrank,a_Name) %>%
  summarize(
    avg_HAI5_score = sum(Admissions*`HAI-5-SIR`, na.rm = TRUE)/sum(Admissions,na.rm = TRUE),
    avg_HAI6_score = sum(Admissions*`HAI-6-SIR`, na.rm = TRUE)/sum(Admissions,na.rm = TRUE))

top100_hos <- top100mapping %>% filter(!is.na(AHA.ID)) %>% 
  left_join(hai, by = c("AHA.ID" = "AHA_ID")) %>%
  group_by(oprevrank,a_Name) %>%
  summarize(
    avg_HAI5_score = sum(Admissions*`HAI-5-SIR`, na.rm = TRUE)/sum(Admissions,na.rm = TRUE),
    avg_HAI6_score = sum(Admissions*`HAI-6-SIR`, na.rm = TRUE)/sum(Admissions,na.rm = TRUE))

top100 <- bind_rows(top100_sys,top100_hos) %>% arrange(oprevrank) %>% filter(!is.na(avg_HAI6_score))

cutoff6 <- quantile(top100$avg_HAI6_score,na.rm = TRUE, probs = c(0.1,0.25,0.5,0.75,0.9))
cutoff5 <- quantile(top100$avg_HAI5_score,na.rm = TRUE, probs = c(0.1,0.25,0.5,0.75,0.9))

top100$HAI_5_percentile <- cut(top100$avg_HAI5_score, breaks=c(0,cutoff5,10), 
                         labels=c("Below 10%","10 percentile","25 percentile",
                                  "50 percentile","75 percentile","90 percentile"))
top100$HAI_6_percentile <- cut(top100$avg_HAI6_score, breaks=c(0,cutoff6,10), 
                               labels=c("Below 10%","10 percentile","25 percentile",
                                        "50 percentile","75 percentile","90 percentile"))

hma %<>% left_join(top100mapping[,c(1,4)], by = "HMA_System_Name")

stat <- data.frame(percentile = row.names(as.data.frame(cutoff5)),
                   HAI5_cutoff = cutoff5,
                   HAI6_cutoff = cutoff6)


list_of_datasets <- list("Keycutoff" = stat, "Top100" = top100, "HMA" = hma)
write.xlsx(list_of_datasets, file = "Result/HAI_result.xlsx")

rm(hma,haidat,hai,list_of_datasets,stat,top100,top100_hos,top100_sys)

## Part 5: Combine ----------------------------------------------------------------------------
keycutoff <- data.frame(percentile = row.names(as.data.frame(cutoff5)),
                   readm_score_cutoff = readm_cutoff,
                   infection_score_cutoff = infection_cutoff,
                   HAI_5_cutoff = cutoff5,
                   HAI_6_cutoff = cutoff6)

top100readm <- read.xlsx("Result/readmission_result.xlsx", sheet = 2)
top100infec <- read.xlsx("Result/infection_result.xlsx", sheet = 2)
top100hai <- read.xlsx("Result/HAI_result.xlsx", sheet = 2)
top100elelab <- read.xlsx("Result/electrical_lab_result.xlsx", sheet = 2)

top100full <- top100elelab %>% 
  left_join(top100readm, by = c("oprevrank","a_Name")) %>%
  left_join(top100infec, by = c("oprevrank","a_Name")) %>%
  left_join(top100hai, by = c("oprevrank","a_Name"))

hma_readm <- read.xlsx("Result/readmission_result.xlsx", sheet = 3)
hma_infec <- read.xlsx("Result/infection_result.xlsx", sheet = 3)
hma_hai <- read.xlsx("Result/HAI_result.xlsx", sheet = 3)
hma_elelab <- read.xlsx("Result/electrical_lab_result.xlsx", sheet = 3)

hma_full <- hma_elelab %>%
  left_join(hma_readm, by = c("HMA_System_Name","oprevrank")) %>%
  left_join(hma_infec, by = c("HMA_System_Name","oprevrank")) %>%
  left_join(hma_hai, by = c("HMA_System_Name","oprevrank")) %>%
  select(5,1:4,6:9)

list_of_datasets <- list("Keycutoff" = keycutoff, "Top100" = top100full, "HMA" = hma_full)
write.xlsx(list_of_datasets, file = "Result/combined_result.xlsx")


hist(top100elelab$Yes, # histogram
     col = "peachpuff", # column color
     border = "black", 
     prob = FALSE, # show frequencies instead of densities
     xlim = c(0,1),
     ylim = c(0,50),
     xlab = "% of Hospitals use Electrical Lab Result ")
abline(v = mean(top100elelab$Yes, na.rm = TRUE),
       col = "royalblue",
       lwd = 2)
abline(v = median(top100elelab$Yes, na.rm = TRUE),
       col = "red",
       lwd = 2)

legend(x = "topleft", # location of legend within plot area
       c("Mean", "Median"),
       col = c("royalblue", "red"),
       lwd = c(2, 2))
