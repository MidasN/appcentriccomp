# Export Software Names for Cleaning

df_clean %<>%
  select(RecordNo, Q4:Q7) %>%
  gather(q, a, -RecordNo) %>%
  separate_rows(a, sep = ";") %>%
  mutate(a = str_trim(.$a)) %>%
  write_csv("softwarenames.csv")


# Descriptive
# ----------
#How many apps are there that are mentioned more than once: 431
table(software_cleaned$a)[(table(software_cleaned$a) > 1)] %>% length

#How many unique applications are mentioned: 1066
unique(software_cleaned$a) %>% length




# Testing block
# -------------
# Chisq and t-test


# Create new data frame with gender and app data
gender <- df_clean[, c("RecordNo", "gender")]
software_and_gender <- merge(x = software_cleaned, y = gender, by="RecordNo") %>%
  group_by(gender) %>% count(a) %>% arrange(desc(n))

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

recast_soft_gen %>% View


# Turn data.frame into table
soft_gen_as_table <- as.table(rbind(recast_soft_gen$`1`, recast_soft_gen$`2`))
#ChiSquare test
chisq.test(recast_soft_gen[1:20,])
chisq.test(soft_gen_as_table)

#T-test
t.test(recast_soft_gen$`1`[1:20], recast_soft_gen$`2`[1:20])



# MDS
# ---------

d = dist(pre_dist[,-1]) # euclidean distances between the rows
d = dist(pre_dist[,-1], method = 'maximum' ) # euclidean distances between the rows
d = dist(pre_dist[,-1], method = 'manhattan' ) # euclidean distances between the rows
d = dist(pre_dist[,-1], method = 'minkowski' ) # euclidean distances between the rows
d = dist(pre_dist[,-1], method = 'binary' ) # euclidean distances between the rows

library(MASS)
fit <- isoMDS(d, k=2) # k is the number of dim
fit # view results


# Pre-LPA cleaning
# ----------------


### manualy choosing non-string columns
## write_csv("~/projects/appcentriccomp/data/columns.csv", x = data.frame(colnames = colnames(df),
##            status = "")
##           )


## clmn = read_csv("~/projects/appcentriccomp/data/columns.csv")


# clmn %>% View()

df_clean %>% View()

## summary(clmn$status == "-")
## clmn$colnames[is.na(clmn$status)]

## df_clmns_excluded = df_clean[,colnames(df_clean) %in%
## clmn$colnames[is.na(clmn$status)]]


## df_clmns_excluded %>% View


na_vector[na_vector == na_vector %>% max()]


to_kick_off = data.frame(
           var_names = names(na_vector),
           na_quantity = na_vector) %>%
  dplyr::arrange(desc(na_quantity))


clmn$status[clmn$colnames %in% to_kick_off$var_names[to_kick_off$na_quantity > 0]] = "-"

write_csv("~/projects/appcentriccomp/data/columns_2.csv", x = clmn)



## 4_.RMD
## ----------


## mered_with_prfiles = merged %>%
##   group_by(q, RecordNo) %>%
##   mutate(ind = row_number()) %>%
##   spread(q, a, fill = "Nothing") %>%
##   select(-ind) %>%
##   View()
