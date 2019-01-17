library(dplyr)
# Read Data ------------------------------------------------------------------------------------------------------
dat <- read.csv("AHA_Admissions_2016.csv")
caregroup <- openxlsx::read.xlsx("D:/GitHub/HMAResearch/HSData/Data/CareGroup.xlsx")
baycare <- openxlsx::read.xlsx("D:/GitHub/HMAResearch/HSData/Data/BayCare.xlsx")
dat <- dat[c(-1,-23)]
# AHA Data ------------------------------------------------------------------------------------------------------
dat$System.Name <- as.character(dat$System.Name)
# Add CareGroup as system into AHA table
ind <- which(dat$AHA.ID %in% caregroup$AHA.ID)
dat[ind,14:18] <- caregroup[1,8:12]
# Amend BayCare AHA System ID and Name
ind <- which(dat$AHA.ID %in% baycare$AHA.ID)
dat[ind,14:16] <- baycare[1,4:6]

# Update column names
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

# Add HMA SYS
mappinglist <- openxlsx::read.xlsx("D:/GitHub/HMAResearch/HSData/Data/HMA_Mapping_List_201812.xlsx",sheet = "AHA Hospital List 2017")

dat <- left_join(dat,mappinglist[c(1:3)],by="AHA_ID")
dat$Medicare_Provider_ID <- as.numeric(as.character(dat$Medicare_Provider_ID))

# Inner Join pain managemnet score file ------------------------------------------------------------------------------------
painscore <- openxlsx::read.xlsx("HospitalCompare_PainManagement.xlsx")
painscore <- filter(painscore, `HCAHPS.Question` %in% c("Pain management - linear mean score", "Pain management - star rating"))

#dat <- left_join(hospitalsinfo,rating,by=c("Medicare_Provider_ID" = "Provider.ID"))
mydat <- inner_join(dat,painscore,by=c("Medicare_Provider_ID" = "Provider.ID"))
mydat2 <- left_join(painscore, dat[,c(1:3,14:16,21:23)],by=c("Provider.ID"="Medicare_Provider_ID"))
mydat2 <- filter(mydat2,!is.na(AHA_ID))

lmvaluedf <- mydat[,c(1:3,14:16,21:24,32,38)] %>% 
  filter(`HCAHPS.Question` == "Pain management - linear mean score" & `HCAHPS.Linear.Mean.Value` != "Not Available")
starratingdf <- mydat[,c(1:3,14:16,21:24,32,34)] %>% 
  filter(`HCAHPS.Question` == "Pain management - star rating" & `Patient.Survey.Star.Rating` != "Not Available")

lmvaluedf$HCAHPS.Linear.Mean.Value <- as.numeric(lmvaluedf$HCAHPS.Linear.Mean.Value)
starratingdf$Patient.Survey.Star.Rating <- as.numeric(starratingdf$Patient.Survey.Star.Rating)

combineddf <- inner_join(lmvaluedf[-11],starratingdf[,c(1,12)],by="AHA_ID")

# HMA Members -------------------------------------------------------------------------------------------------------
hma <- combineddf %>% filter(HMA_Member == "Y") %>%
  group_by(HMA_System_Name) %>%
  summarize(
    avg_linear_mean_value = (Admissions %*% `HCAHPS.Linear.Mean.Value`)/sum(Admissions),
    avg_star_rating = (Admissions %*% `Patient.Survey.Star.Rating`)/sum(Admissions))

write.csv(hma,"result/hmahspainscore.csv",row.names = FALSE)

# top 100 ------------------------------------------------------------------------------------------------------------
top100mapping <- openxlsx::read.xlsx("2017top100HS_1014_mapping.xlsx")

# Split by AHA_System_ID and AHA.ID
top100_sys <- top100mapping[,c(1:5)] %>% filter(!is.na(AHA_SyStem_ID)) %>% 
  left_join(combineddf[,c(1:7,11:12)], by = c("AHA_SyStem_ID" = "System_ID")) %>%
  group_by(Rank,a_Name) %>%
  summarize(
    avg_linear_mean_value = (Admissions %*% `HCAHPS.Linear.Mean.Value`)/sum(Admissions),
    avg_star_rating = (Admissions %*% `Patient.Survey.Star.Rating`)/sum(Admissions))


top100_hos <- top100mapping %>% filter(!is.na(AHA.ID)) %>% 
  left_join(combineddf[,c(1:7,11:12)], by = c("AHA.ID" = "AHA_ID")) %>%
  select(Rank,a_Name,`HCAHPS.Linear.Mean.Value`,`Patient.Survey.Star.Rating`) %>%
  magrittr::set_colnames(c("Rank", "a_Name", "avg_linear_mean_value", "avg_star_rating"))

top100 <- bind_rows(top100_sys,top100_hos) %>% arrange(Rank)

cutoff <- quantile(top100$avg_linear_mean_value,na.rm = TRUE, probs = c(0.1,0.25,0.5,0.75,0.9))
starcutoff <- quantile(top100$avg_star_rating,na.rm = TRUE, probs = c(0.1,0.25,0.5,0.75,0.9))

top100$lmvalue_percentile <- cut(top100$avg_linear_mean_value, breaks=c(0,cutoff,100), labels=c("Below 10%","10 percentile","25 percentile","50 percentile","75 percentile","90 percentile"))
top100$starrating_percentile <- cut(top100$avg_star_rating, breaks=c(0,starcutoff,5), labels=c("Below 10%","10 percentile","25 percentile","50 percentile","75 percentile","90 percentile"))


write.csv(top100,"result/top100painscore.csv",row.names = FALSE)
