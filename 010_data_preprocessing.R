
library(tidyverse)
library(haven)
library(stringr)
library(reshape)
library(magrittr)


df <- read_spss("./data/KnowledgeWorkerSurvey_originaldata.sav")

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


# Forget about consolidated for now

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



# Only included completed surveys


df_clean = df %>%
  filter(status == 1) %>% # Filter to only included completed surveys
  select(-(sc1_4:sc1_11_43)) %>% # Delete screen-out occupation columns
  mutate(Occupation_Num = rowSums(select(., sc1_1:sc1_3), na.rm = T), # Merge columns of occupations
         Occupation_DISCO = names(datamap$DISCO[Occupation_Num]), # English labels for DISCO 2008 Occupations
         Occupation = names(datamap$DISCO_Consolidated[Occupation_Num])) %>% # Merged sector labels for Occupations
  select(-(sc1_1:sc1_3)) %>% # Remove unnecessary occupation columns
  mutate_at(vars(Q3_1:Q3_4), funs(.-1)) # Q3_1:Q3_4, 5 means 5+


# Precleaning Using OpenRefine


## Cleaned Using OpenRefine, see changefile at DataCleaningSoftwareNames_OpenRefine.gz
software_cleaned <- read_tsv("./data/softwarenames_cleaned.tsv") %>%
  separate_rows(a, sep = ";") %>%
  mutate(a = str_trim(a)) %>%  # Remove all year versions from MS
  mutate(a2 = str_replace(a, " [:digit:][:digit:][:digit:][:digit:]$", "")) %>% # Remove all year versions from MS
  mutate(a2 = str_replace(a2, "SAP.*", "SAP")) %>% # Remove NA
  filter(!str_detect(a2, "NA"))

software_cleaned$RecordNo %<>% as.numeric()


# Aggregated Values


software_aggregated <- software_cleaned %>% # Group and order high to low
  group_by(a2) %>% count(a2) %>% arrange(desc(n)) %>% # Show cumulative relative frequency of apps
  ungroup() %>%
  mutate(relcumsum = cumsum(n / 4131 * 100))



jobtitleanddescription <- df_clean[, c("RecordNo", "Q18_1_open", "Q18a", "Occupation_DISCO", "Occupation")]
merged <- merge(x = software_cleaned, y = jobtitleanddescription,
                by = "RecordNo") %>%
  select(-a, -Q18_1_open, -Q18a, -Occupation_DISCO)

