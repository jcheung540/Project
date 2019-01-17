library(dplyr)
# Read Data ------------------------------------------------------------------------------------------------------
dat <- openxlsx::read.xlsx("AHA_Admissions_2017.xlsx")
caregroup <- openxlsx::read.xlsx("D:/GitHub/HMAResearch/HSData/Data/CareGroup.xlsx")
baycare <- openxlsx::read.xlsx("D:/GitHub/HMAResearch/HSData/Data/BayCare.xlsx")

# AHA Data ------------------------------------------------------------------------------------------------------
# Add CareGroup as system into AHA table
ind <- which(dat$AHA.ID %in% caregroup$AHA.ID)
dat[ind,17:21] <- caregroup[1,8:12]
# Amend BayCare AHA System ID and Name
ind <- which(dat$AHA.ID %in% baycare$AHA.ID)
dat[ind,17:19] <- baycare[1,4:6]

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

# Inner Join overall hospital rate file ------------------------------------------------------------------------------------
rating <- openxlsx::read.xlsx("HospitalCompare_General_Information.xlsx")
#dat <- left_join(hospitalsinfo,rating,by=c("Medicare_Provider_ID" = "Provider.ID"))
mydat <- inner_join(dat,rating,by=c("Medicare_Provider_ID" = "Provider.ID"))

ratedf <- mydat[,c(1:3,17:19,25:27,39)] %>% filter(`Hospital.overall.rating` != "Not Available")
ratedf$Hospital.overall.rating <- as.numeric(ratedf$Hospital.overall.rating)


# HMA Members -------------------------------------------------------------------------------------------------------
hma <- ratedf %>% filter(HMA_Member == "Y") %>%
  group_by(HMA_System_Name) %>%
  summarize(
    avg_rating = (Adjusted_Admissions %*% `Hospital.overall.rating`)/sum(Adjusted_Admissions))

write.csv(hma,"hmahsrating.csv",row.names = FALSE)

# top 100 ------------------------------------------------------------------------------------------------------------
top100mapping <- openxlsx::read.xlsx("2017top100HS_1014_mapping.xlsx")


# Split by AHA_System_ID and AHA.ID
top100_sys <- top100mapping[,c(1:5)] %>% filter(!is.na(AHA_SyStem_ID)) %>% 
  left_join(ratedf[,c(1:7,10)], by = c("AHA_SyStem_ID" = "System_ID")) %>%
  group_by(Rank,a_Name) %>%
  summarize(
            avg_rating = (Adjusted_Admissions %*% `Hospital.overall.rating`)/sum(Adjusted_Admissions))


top100_hos <- top100mapping %>% filter(!is.na(AHA.ID)) %>% 
  left_join(ratedf[,c(1:7,10)], by = c("AHA.ID" = "AHA_ID")) %>%
  select(Rank,a_Name,`Hospital.overall.rating`) %>%
  magrittr::set_colnames(c("Rank", "a_Name", "avg_rating"))

top100 <- bind_rows(top100_sys,top100_hos) %>% arrange(Rank)

cutoff <- quantile(top100$avg_rating,na.rm = TRUE, probs = c(0.25,0.5,0.75,0.9))



top100$percentile <- cut(top100$avg_rating, breaks=c(0,cutoff,5), labels=c("Below 25%","25 percentile","50 percentile","75 percentile","90 percentile"))

write.csv(top100,"healthsystemrating.csv",row.names = FALSE)
