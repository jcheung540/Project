library(dplyr)
##################################################################################################################
# 2016
##################################################################################################################
# Read Data ------------------------------------------------------------------------------------------------------
dat2016 <- read.csv("Data/AHA_Admissions_2016.csv")
caregroup <- openxlsx::read.xlsx("D:/GitHub/HMAResearch/HSData/Data/CareGroup.xlsx")
baycare <- openxlsx::read.xlsx("D:/GitHub/HMAResearch/HSData/Data/BayCare.xlsx")
dat2016 <- dat2016[c(-1,-23)]
# AHA Data ------------------------------------------------------------------------------------------------------
dat2016$System.Name <- as.character(dat2016$System.Name)
# Add CareGroup as system into AHA table
ind <- which(dat2016$AHA.ID %in% caregroup$AHA.ID)
dat2016[ind,14:18] <- caregroup[1,8:12]
# Amend BayCare AHA System ID and Name
ind <- which(dat2016$AHA.ID %in% baycare$AHA.ID)
dat2016[ind,14:16] <- baycare[1,4:6]

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
dat2016 <- makeColNamesUserFriendly(dat2016)

# Add HMA SYS
mappinglist <- openxlsx::read.xlsx("D:/GitHub/HMAResearch/HSData/Data/HMA_Mapping_List_201812.xlsx",sheet = "AHA Hospital List 2017")

dat2016 <- left_join(dat2016,mappinglist[c(1:3)],by="AHA_ID")
dat2016$Medicare_Provider_ID <- as.numeric(as.character(dat2016$Medicare_Provider_ID))

# Inner Join pain managemnet score file ------------------------------------------------------------------------------------
painmgmt2016 <- openxlsx::read.xlsx("Data/HospitalCompare_PainManagement.xlsx")
painmgmt2016 <- filter(painmgmt2016, `HCAHPS.Question` %in% c('Patients who reported that their pain was "Always" well controlled'))

percent16df <- inner_join(dat2016[,c(1:3,14:16,21:23)],painmgmt2016[,c(1,10,14)],by=c("Medicare_Provider_ID" = "Provider.ID")) %>% 
  filter(`HCAHPS.Answer.Percent` != "Not Available")

percent16df$HCAHPS.Answer.Percent <- as.numeric(percent16df$HCAHPS.Answer.Percent)
names(percent16df)[c(7,11)] <- c("Admissions_2016","HCAHPS.Answer.Percent.2016")

# HMA Members -------------------------------------------------------------------------------------------------------
hma <- percent16df %>% filter(HMA_Member == "Y") %>%
  group_by(HMA_System_Name) %>%
  summarize(
    avg_answer_percent_2016 = (Admissions_2016 %*% `HCAHPS.Answer.Percent`)/sum(Admissions_2016))


# top 100 ------------------------------------------------------------------------------------------------------------
top100mapping <- openxlsx::read.xlsx("Data/2017top100HS_1014_mapping.xlsx")

# Split by AHA_System_ID and AHA.ID
top100_sys <- top100mapping[,c(1:5)] %>% filter(!is.na(AHA_SyStem_ID)) %>% 
  left_join(percent16df[,c(1:7,11)], by = c("AHA_SyStem_ID" = "System_ID")) %>%
  group_by(Rank,a_Name) %>%
  summarize(
    avg_answer_percent = (Admissions %*% `HCAHPS.Answer.Percent`)/sum(Admissions))


top100_hos <- top100mapping %>% filter(!is.na(AHA.ID)) %>% 
  left_join(percent16df[,c(1:7,11)], by = c("AHA.ID" = "AHA_ID")) %>%
  select(Rank,a_Name,`HCAHPS.Answer.Percent`) %>%
  magrittr::set_colnames(c("Rank", "a_Name", "avg_answer_percent"))

top100 <- bind_rows(top100_sys,top100_hos) %>% arrange(Rank)

cutoff2016 <- quantile(top100$avg_answer_percent,na.rm = TRUE, probs = c(0.1,0.25,0.5,0.75,0.9))


top100$answerpercent_percentile <- cut(top100$avg_answer_percent, breaks=c(0,cutoff,100), labels=c("Below 10%","10 percentile","25 percentile","50 percentile","75 percentile","90 percentile"))

names(top100)[3:4] <- c("2016_avg_answer_percent","2016_percentile")


##################################################################################################################
# 2011
##################################################################################################################
# Read Data ------------------------------------------------------------------------------------------------------
dat2011 <- openxlsx::read.xlsx("Data/AHA_Admissions_2011.xlsx")
dat2011 <- dat2011[c(-1)]
# AHA Data ------------------------------------------------------------------------------------------------------
dat2011$System.Name <- as.character(dat2011$System.Name)
# Add CareGroup as system into AHA table
ind <- which(dat2011$AHA.ID %in% caregroup$AHA.ID)
dat2011[ind,9:14] <- caregroup[1,8:12]
# Amend BayCare AHA System ID and Name
ind <- which(dat2011$AHA.ID %in% baycare$AHA.ID)
dat2011[ind,9:11] <- baycare[1,4:6]

# Update column names
dat2011 <- makeColNamesUserFriendly(dat2011)

# Add HMA SYS
dat2011 <- left_join(dat2011,mappinglist[c(1:3)],by="AHA_ID")
dat2011$Medicare_Provider_ID <- as.numeric(as.character(dat2011$Medicare_Provider_ID))

# Inner Join pain managemnet score file ------------------------------------------------------------------------------------
painmgmt2011 <- openxlsx::read.xlsx("Data/HospitalCompare_HCAHPS_2011.xlsx")

percent11df <- inner_join(dat2011[,c(1:3,9:11,17:19)],painmgmt2011[,c(1,22)],by=c("Medicare_Provider_ID" = "Provider.Number")) %>% 
  filter(`Percent.of.patients.who.reported.that.their.pain.was.\"Always\".well.controlled.` != "Not Available")
names(percent11df)[c(7,10)] <- c("Admissions_2011","HCAHPS.Answer.Percent.2011")

percent11df$HCAHPS.Answer.Percent.2011 <- as.numeric(percent11df$HCAHPS.Answer.Percent.2011)


answerpercent <- full_join(percent16df,percent11df[,c(1:3,7,10)], by = "AHA_ID")
answerpercent$System_ID[which(answerpercent$HMA_System_Name == "Advocate Aurora Health")] <- "5991032"



##################################################################################################################
# Combine Data
##################################################################################################################

# HMA Members -------------------------------------------------------------------------------------------------------
#hma <- percent16df %>% filter(HMA_Member == "Y") %>%
#  group_by(HMA_System_Name) %>%
#  summarize(
#    avg_answer_percent_2016 = (Admissions %*% `HCAHPS.Answer.Percent`)/sum(Admissions))

#hma11 <- percent11df %>% filter(HMA_Member == "Y") %>%
#  group_by(HMA_System_Name) %>%
#  summarize(
#    avg_answer_percent_2011 = (Admissions %*% `HCAHPS.Answer.Percent`)/sum(Admissions))

hma <- left_join(percent16df %>% filter(HMA_Member == "Y") %>%
                   group_by(HMA_System_Name) %>%
                   summarize(
                     avg_answer_percent_2016 = (Admissions_2016 %*% `HCAHPS.Answer.Percent.2016`)/sum(Admissions_2016)),
                 percent11df %>% filter(HMA_Member == "Y") %>%
                   group_by(HMA_System_Name) %>%
                   summarize(
                     avg_answer_percent_2011 = (Admissions_2011 %*% `HCAHPS.Answer.Percent.2011`)/sum(Admissions_2011)),
                 by = "HMA_System_Name")

hma2 <- filter(answerpercent,HMA_Member == "Y") %>%
  group_by(HMA_System_Name) %>%
  summarize(
    avg_answer_percent_2016 = sum(Admissions_2016*`HCAHPS.Answer.Percent.2016`, na.rm = TRUE)/sum(Admissions_2016,na.rm = TRUE),
    avg_answer_percent_2011 = sum(Admissions_2011*`HCAHPS.Answer.Percent.2011`,na.rm = TRUE)/sum(Admissions_2011,na.rm = TRUE))


write.csv(hma2,"result/HMA_HS_AnswerPercent_v2.csv",row.names = FALSE)

# top 100 ------------------------------------------------------------------------------------------------------------
top100mapping <- openxlsx::read.xlsx("Data/2017top100HS_1014_mapping.xlsx")

# Split by AHA_System_ID and AHA.ID
top100_sys <- top100mapping[,c(1:5)] %>% filter(!is.na(AHA_SyStem_ID)) %>% 
  left_join(answerpercent, by = c("AHA_SyStem_ID" = "System_ID")) %>%
  group_by(Rank,a_Name) %>%
  summarize(
    avg_answer_percent_2016 = sum(Admissions_2016*`HCAHPS.Answer.Percent.2016`, na.rm = TRUE)/sum(Admissions_2016,na.rm = TRUE),
    avg_answer_percent_2011 = sum(Admissions_2011*`HCAHPS.Answer.Percent.2011`,na.rm = TRUE)/sum(Admissions_2011,na.rm = TRUE))


top100_hos <- top100mapping %>% filter(!is.na(AHA.ID)) %>% 
  left_join(answerpercent, by = c("AHA.ID" = "AHA_ID")) %>%
  select(Rank,a_Name,`HCAHPS.Answer.Percent.2016`,`HCAHPS.Answer.Percent.2011`) %>%
  magrittr::set_colnames(c("Rank", "a_Name", "avg_answer_percent_2016", "avg_answer_percent_2011"))

top100 <- bind_rows(top100_sys,top100_hos) %>% arrange(Rank)

cutoff2016 <- quantile(top100$avg_answer_percent_2016,na.rm = TRUE, probs = c(0.1,0.25,0.5,0.75,0.9))
cutoff2011 <- quantile(top100$avg_answer_percent_2011,na.rm = TRUE, probs = c(0.1,0.25,0.5,0.75,0.9))

top100$Y2016_percentile <- cut(top100$avg_answer_percent_2016, breaks=c(0,cutoff2016,100), labels=c("Below 10%","10 percentile","25 percentile","50 percentile","75 percentile","90 percentile"))
top100$Y2011_percentile <- cut(top100$avg_answer_percent_2011, breaks=c(0,cutoff2011,100), labels=c("Below 10%","10 percentile","25 percentile","50 percentile","75 percentile","90 percentile"))





write.csv(top100,"result/top100answerpercent.csv",row.names = FALSE)
