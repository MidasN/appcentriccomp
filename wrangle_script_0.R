# Note: NA means the participant was not asked to answer this question, not that they answered it blank

#Set the correct working directory
#setwd("Documents/syncthing/HCI/Aarhus/CHI19")

library(tidyverse)
library(haven)
library(stringr)

#Set the correct character encoding so Danish characters render properly
Sys.setlocale("LC_ALL", "en_GB.UTF-8")

# Read in csv file

df <- read_spss("KnowledgeWorkerSurvey_originaldata.sav")

datamap <- list()

datamap$DISCO <- c("Chief executives, senior officials, and legislators" = 1,
  "Administrative and commercial managers" = 2,
  "Production and specialised services managers" = 3,
  "Hospitality, retail and other services managers" = 4,
  "Science and engineering professionals" = 5,
  "Health professionals" = 6,
  "Teaching professionals" = 7,
  "Business and administration professionals" = 8,
  "ICT professionals" = 9,
  "Legal, social, and cultural professionals" = 10,
  "Science and engineering, shipping and aviation associate professionals" = 11,
  "Health associate professionals" = 12,
  "Business, economics, and administration associate professionals" = 13,
  "Legal, social, and cultural associated professionals" = 14,
  "ICT technicians" = 15)
 
datamap$DISCO_Consolidated <- c("Chief executives, senior officials, and legislators" = 1,
                                "General management" = 2,
                                "General management" = 3,
                                "Hospitality and retail" = 4,
                                "Science and engineering" = 5,
                                "Health" = 6,
                                "Teaching" = 7,
                                "Business and administration" = 8,
                                "ICT" = 9,
                                "Legal, social, and cultural" = 10,
                                "Science and engineering" = 11,
                                "Health" = 12,
                                "Business, economics, and administration" = 13,
                                "Legal, social, and cultural" = 14,
                                "ICT" = 15)


df %>% 
  filter(status == 1) %>% # Filter to only included completed surveys
  select(-(sc1_4:sc1_11_43)) %>% # Delete screen-out occupation columns 
  mutate(Occupation_Num = rowSums(select(., sc1_1:sc1_3), na.rm = T), # Merge columns of occupations
         Occupation_DISCO = names(datamap$DISCO[Occupation_Num]), # English labels for DISCO 2008 Occupations
         Occupation = names(datamap$DISCO_Consolidated[Occupation_Num])) %>% # Merged sector labels for Occupations
  select(-(sc1_1:sc1_3)) %>% # Remove unnecessary occupation columns
  mutate_at(vars(Q3_1:Q3_4), funs(.-1))-> df_clean# %>% # Q3_1:Q3_4, 5 means 5+ 


# Export Software Names for Cleaning
df_clean %>% 
  select(RecordNo, Q4:Q7) %>% 
  gather(q, a, -RecordNo) %>% 
  separate_rows(a, sep = ";") %>%
  mutate(a = str_trim(.$a)) %>%
  write_csv("softwarenames.csv")
  
# Cleaned Using OpenRefine, see changefile at DataCleaningSoftwareNames_OpenRefine.gz
software_cleaned <- read_tsv("softwarenames_cleaned.tsv") %>% 
  separate_rows(a, sep = ";") %>% 
  mutate(a = str_trim(a)) %>%
  #Remove all year versions from MS
  mutate(a2 = str_replace(a, " [:digit:][:digit:][:digit:][:digit:]$", "")) %>%
  #Collapse SAP versions
  mutate(a2 = str_replace(a2, "SAP.*", "SAP")) %>%
  #Remove NA
  filter(!str_detect(a2, "NA"))

#How many apps are there that are mentioned more than once: 431
table(software_cleaned$a)[(table(software_cleaned$a) > 1)] %>% length

#How many unique applications are mentioned: 1066
unique(software_cleaned$a) %>% length

software_aggregated <- software_cleaned %>% 
  #Group and order high to low
  group_by(a2) %>% count(a2) %>% arrange(desc(n)) %>% 
  # Show cumulative relative frequency of apps
  ungroup() %>% mutate(relcumsum = cumsum(n/4131 * 100)) 

# Create new data frame with gender and app data
gender <- df_clean[, c("RecordNo", "gender")]
software_and_gender <- merge(x = software_cleaned, y = gender, by="RecordNo") %>%
  group_by(gender) %>% count(a) %>% arrange(desc(n)) 

library(reshape)
# reshape the df so that apps, female, and male are the variables 
recast_soft_gen <- cast(software_and_gender, a ~ gender, mean, value = "n")
# Change row names to the app names
rownames(recast_soft_gen) <- recast_soft_gen[,1]
# Remove app variable from dataframe
recast_soft_gen <- recast_soft_gen[,-1]
# Change all NaN to 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
recast_soft_gen[is.nan(recast_soft_gen)] <- 0

# Turn data.frame into table
soft_gen_as_table <- as.table(rbind(recast_soft_gen$`1`, recast_soft_gen$`2`))
#ChiSquare test
chisq.test(recast_soft_gen[1:20,])
chisq.test(soft_gen_as_table)

#T-test
t.test(recast_soft_gen$`1`[1:20], recast_soft_gen$`2`[1:20])



# Jonas CST export
software_cleaned <- read_tsv("softwarenames_cleaned.tsv") %>% separate_rows(a, sep = ";") %>% mutate(a = str_trim(a))
software_cleaned %>% 
  mutate(a2 = str_replace(a, " [:digit:][:digit:][:digit:][:digit:]$", "")) %>%
  mutate(a2 = str_replace(a2, "SAP.*", "SAP")) %>%
  group_by(a2) %>% count(a2) %>% arrange(desc(n)) #%>% filter(!str_detect(a, "^MS"))%>%
jobtitleanddescription <- df_clean[, c("RecordNo", "Q18_1_open", "Q18a")]
merged <- merge(x = software_cleaned, y = jobtitleanddescription, by= "RecordNo") %>% View()

