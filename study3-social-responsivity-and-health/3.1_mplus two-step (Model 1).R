library(tidyverse)
library(haven)

# 1. MERGE MPLUS RESPONSIVITY OUTPUT TO MAIN DF ----
# (Ran Mplus code to make and save responsivity)
# Read in data and label columns (based on Mplus otput)
responsivity <- read.table("Mplus/responsivity_fscores.dat",
                     header = FALSE,
                     sep = "",
                     stringsAsFactors = FALSE)

colnames(responsivity) <- c( # insert variable names based on Mplus output
  "connection",
  "pos_interaction",
  "respons_mplus",
  "respons_se",
  "b_connect",
  "b_connect_se",
  "id"
)

responsivity <- responsivity %>% 
  select(id, everything()) %>% 
  arrange(id)

df <- read.csv("data/M2-R1_2026_LF.csv")

# Merge mplus output to main df
df_merge <- df %>%
  left_join(
    responsivity %>%
      select(id, respons_mplus) %>%
      distinct(),
    by = "id"
  )

cor.test(df_merge$social_respons, df_merge$respons_mplus) # basically the exact same as r responsivity

# Save as a .dat
df_merge <- df_merge[, -1] 

# make variables into numeric
df_merge <- df_merge %>% 
  rename(
    m2 = study,
    white = race,
  ) %>% 
  mutate(
    m2 = ifelse(m2 == "M2", 1, 0),
    white = ifelse(white == "(1) white", 1, 0)
  ) %>% 
  select(
    -sr_health,
  )

# Save this out for two-line approach
write.csv(df_merge, "data/M2-R1_Mplus-respons.csv")

# rename variables to be under character limit
df_merge <- df_merge %>% 
  rename(
    pos_int = pos_interaction,
    connect = connection, 
    neg_aff = neg_affect,
    pos_aff = pos_affect,
    chro_or = chronic_condition_original,
    chro_new = chronic_condition_remake,
    srh_or = sr_health_original,
    srh_rev = sr_health_reverse,
    respons = social_respons,
    respons_m = respons_mplus,
    func_lim = func_limitation
  )

cat(names(df_merge), sep = " ")
# id m2 day pos_int closeness belonging connect stressor age gender life_satisfaction neg_aff pos_aff chro_or education white household adl iadl chro_new educ_num srh_or srh_rev func_lim respons connection_m pos_int_m respons_m 

# make NA -999
df_merge[is.na(df_merge)] <- -999

# save
write.table(df_merge,
            file = "Mplus/M2-R1_041526_LF.dat",
            sep = " ",
            row.names = FALSE,
            col.names = FALSE)

# write.table(df_merge,
#             file = "Mplus/M2-R1_030626_LF.dat",
#             sep = " ",
#             row.names = FALSE,
#             col.names = FALSE)


# 2. WHAT VARS IN THIS .DAT ----
df_dat <- read.table("Mplus/M2-R1_041526_LF.dat",
                     header = FALSE,
                     sep = "",
                     stringsAsFactors = FALSE)

