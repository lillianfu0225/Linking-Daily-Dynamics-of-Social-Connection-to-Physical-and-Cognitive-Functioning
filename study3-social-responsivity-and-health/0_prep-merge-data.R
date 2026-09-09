# load packages
library(tidyverse)
library(haven)
library(lme4)

# ---- (0) load data ----
obj1 <- load(file = "data/MIDUS2_DS1.rda")
M2 <- get(obj1)

obj2 <- load(file = "data/MIDUS2_daily.rda")
M2_daily <- get(obj2)

obj3 <- load(file = "data/Refresher1_DS1.rda")
R1 <- get(obj3)

obj4 <- load(file = "data/Refresher1_daily.rda")
R1_daily <- get(obj4)

rm(list = c(obj1, obj2, obj3, obj4))

M2mke <- read_sav(file = "data/M2ID_MILWAUKEE_DATA WITH 19 NEW SAQS_N=592_3-6-12.sav")
# M2mke2 <- read_sav(file = "data/M2_P1_Milwaukee P1_MERGED_N=5555_3-7-12.sav")

# write.csv(M2mke2, "data/Susan's version of merged data.csv")
# M2mke_check <- M2mke %>% 
#   select(
#     M2ID, BACSATIS, BACNEGAF, BACPOSAF, BACCHRON, BACAS11A:BACAS11DD, BACB1
#   )

# write.csv(M2mke_check,"data/M2mke_check.csv")

# (0a) select variables ----

M2_df <- M2 %>% 
  select(
    M2ID, B1SSATIS, B1SNEGAF, B1SPOSAF, B1PA1, B1SCHRON, B1SA11A:B1SA11DD, B1PB1, B1PF7A, B1PHHSIZE,
    B1SBADL2, B1SMADL
  ) %>% 
  select(
    -B1SA11T, -B1SA11U # emotional disorder, drug/alcohol
  ) %>% 
  rename(
    id = M2ID,
    life_satisfaction = B1SSATIS,
    neg_affect = B1SNEGAF,
    pos_affect = B1SPOSAF, 
    sr_health = B1PA1,
    chronic_condition = B1SCHRON,
    education = B1PB1,
    race = B1PF7A,
    household = B1PHHSIZE,
    adl = B1SBADL2, 
    iadl = B1SMADL
  )

M2mke_df <- M2mke %>%
  select(
    M2ID, BACSATIS, BACNEGAF, BACPOSAF, BACA1, BACCHRON, BACAS11A:BACAS11DD, BACB1, BACF7A,
    BACBADL, BACIADL
    # BACHHMBR, BACKIDHH
  ) %>%
  select(
    -BACAS11T, -BACAS11U # emotional disorder, drug/alcohol
  ) %>%
  rename(
    id = M2ID,
    life_satisfaction = BACSATIS,
    neg_affect = BACNEGAF,
    pos_affect = BACPOSAF,
    sr_health = BACA1,
    chronic_condition = BACCHRON,
    education = BACB1,
    race = BACF7A,
    adl = BACBADL, 
    iadl = BACIADL
    # household = BACHHMBR + BACKIDHH + 1
  ) 

M2_daily_df <- M2_daily %>% 
  select(
    M2ID, B2DDAY, B2DF8, B2DC21, B2DC22, B2DA_STR,
    B1PAGE_M2, B1PGENDER
  ) %>% 
  rename(
    id = M2ID, 
    day = B2DDAY, 
    pos_interaction = B2DF8,
    closeness = B2DC21,
    belonging = B2DC22,
    age = B1PAGE_M2,
    gender = B1PGENDER,
    stressor = B2DA_STR
  )

R1_df <- R1 %>% 
  select(
    MRID, RA1SSATIS, RA1SNEGAF, RA1SPOSAF, RA1PA1, RA1SCHRON, RA1SA11A:RA1SA11MM, RA1PB1, RA1PF7A,
    RA1PHHSIZE, RA1SBADL2, RA1SMADL
  ) %>% 
  select(
    -RA1SA11T, -RA1SA11U # emotional disorder, drug/alcohol
  ) %>% 
  rename(
    id = MRID,
    life_satisfaction = RA1SSATIS,
    neg_affect = RA1SNEGAF,
    pos_affect = RA1SPOSAF, 
    sr_health = RA1PA1,
    chronic_condition = RA1SCHRON, 
    education = RA1PB1,
    race = RA1PF7A,
    household = RA1PHHSIZE, 
    adl = RA1SBADL2, 
    iadl = RA1SMADL
  )

R1_daily_df <- R1_daily %>% 
  select(
    MRID, RA2DDAY,
    RA2DF8, RA2DC21, RA2DC22, RA2DA_STR,
    RA1PRAGE, RA1PRSEX
  ) %>% 
  rename(
    id = MRID, 
    day = RA2DDAY, 
    pos_interaction = RA2DF8,
    closeness = RA2DC21,
    belonging = RA2DC22,
    age = RA1PRAGE,
    gender = RA1PRSEX,
    stressor = RA2DA_STR
  )

# (0b) Chronic condition composite ----

# Step 1: Recode factors
M2_df <- M2_df %>% 
  mutate(
    across(starts_with("B1SA11"),
           ~ as.numeric(recode(.x,
                               "(1) Yes" = 1,
                               "(2) No" = 0
           )))
  ) %>% 
  # Step 2: Make a summed composite
  mutate(chronic_condition_remake = rowSums(across(B1SA11A:B1SA11DD), na.rm = TRUE)) %>% 
  # Step 3: Make NAs consistent
  mutate(chronic_condition_remake = if_else(is.na(chronic_condition), NA_real_, chronic_condition_remake)) %>% 
  rename(chronic_condition_original = chronic_condition)

# M2_df %>%
#   select(B1SA11A:B1SA11DD) %>%
#   ncol() # 28

M2mke_df <- M2mke_df %>% 
  mutate(
    across(starts_with("BACAS11"),
           ~ ifelse(.x == 2, 0, .x)
           )
    ) %>% 
  mutate(chronic_condition_remake = rowSums(across(BACAS11A:BACAS11DD), na.rm = TRUE)) %>% 
  mutate(chronic_condition_remake = if_else(is.na(chronic_condition), NA_real_, chronic_condition_remake)) %>% 
  rename(chronic_condition_original = chronic_condition)

# M2mke_df %>%
#   select(BACAS11A:BACAS11DD) %>%
#   ncol() # 28

R1_df <- R1_df %>% 
  mutate(
    across(starts_with("RA1SA11"),
           ~ as.numeric(recode(.x,
                               "(1) YES" = 1,
                               "(2) NO" = 0
           )))
  ) %>% 
  mutate(chronic_condition_remake = rowSums(across(RA1SA11A:RA1SA11DD), na.rm = TRUE)) %>% 
  mutate(chronic_condition_remake = if_else(is.na(chronic_condition), NA_real_, chronic_condition_remake)) %>% 
  rename(chronic_condition_original = chronic_condition)

# R1_df %>%
#   select(RA1SA11A:RA1SA11DD) %>%
#   ncol() # 28

# R1_df %>% 
#   select(chronic_condition, chronic_condition_remake)

# Summary of conditions in R1 that are not in M2
# R1_df %>% 
#   distinct(id, .keep_all = TRUE) %>% 
#   select(RA1SA11EE:RA1SA11MM) %>%
#   summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>%
#   pivot_longer(cols = everything(),
#                names_to = "variable",
#                values_to = "with_condition") %>%
#   mutate(percent = (with_condition / nrow(R1_df))*100)

# ---- (0c) Clean race variable ----
levels(M2_df$race)
is.ordered(M2_df$race)
skimr::skim(M2mke_df$race) #numeric - needs to be changed to categorical
levels(R1_df$race)

M2mke_df <- M2mke_df %>% 
  mutate(
    race = case_when(
      race == 1 ~ "(1) White",
      race == 2 ~ "(2) Black and/or African American",
      race == 3 ~ "(3) Native American or Alaska Native Aleutian Islander/Eskimo",
      race == 4 ~ "(4) Asian",
      race == 5 ~ "(5) Native Hawaiian or Pacific Islander",
      race == 6 ~ "(6) Other (specify)",
      TRUE ~ NA_character_
    )
  ) %>% 
  mutate(
    race = as.factor(race)
  )

# levels(M2mke_df$race)

# ---- (1) Merge daily diary and main survey ----
# (1-) Stack MIDUS 2 and MKE main surveys ----
M2mke_df <- M2mke_df %>% 
  mutate(
    education = as.factor(education),
    sr_health = as.factor(sr_health)
  )

M2_df <- M2_df %>% 
  mutate(
    sr_health = as.factor(sr_health)
  )

M2_combined_df <- bind_rows(M2_df, M2mke_df)

# (1a) MIDUS 2 ----
M2_merge <- merge(M2_daily_df, M2_combined_df, by = "id", all.x = TRUE) %>% 
  select(id, day, pos_interaction, closeness, belonging, everything()) %>% 
  arrange(id, day)

length(unique(M2_merge$id)) # WOOO 2022!

na_main_M2 <- M2_merge %>%
  filter(is.na(life_satisfaction) & is.na(neg_affect) & is.na(pos_affect) & is.na(chronic_condition_remake)) 
# how many had missing global measures?
length(unique(na_main_M2$id)) # 67!


# PRODUCT: M2_merge is the M2 main and M2 daily diary project merged
# - 2022 total participants, 67 had missing global measures (see R file 2.2 explore missing, phone interview)

# write.csv(M2, "data/explore_M2-full.csv")

# (1b) Refresher 1 ----
R1_merge <- merge(R1_daily_df, R1_df, by = "id", all.x = TRUE) %>% 
  select(id, day, pos_interaction, closeness, belonging, everything()) %>% 
  arrange(id, day)

na_main_R1 <- R1_merge %>%
  filter(is.na(life_satisfaction) & is.na(neg_affect) & is.na(pos_affect) & is.na(chronic_condition_remake)) 
# how many had missing global measures?
length(unique(na_main_R1$id)) # 1 person
length(unique(R1_merge$id)) # total 782

# PRODUCT: R1_merge is the R1 main and R1 daily diary project merged
# - 782 total participants, only 1 missing all four global well-being measures

# Check chronic conditions in the R1 daily diary subset (that are not in M2)
R1_merge %>% 
  distinct(id, .keep_all = TRUE) %>% 
  select(RA1SA11EE:RA1SA11MM) %>%
  summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "with_condition") %>%
  mutate(percent = (with_condition / nrow(R1_df))*100)

# ---- (2) Combine M2 and R1 into one dataset ----

# create a study variable
M2_merge <- M2_merge %>% 
  mutate(
    study = "M2"
  ) %>% 
  select(
    id, study, everything()
  ) %>% 
  mutate(across(where(is.factor), ~ tolower(as.character(.x)))) # make into lower case

R1_merge <- R1_merge %>% 
  mutate(
    study = "R1"
  ) %>% 
  select(
    id, study, everything()
  ) %>% 
  mutate(across(where(is.factor), ~ tolower(as.character(.x))))

# stack the two datasets
full_M2_R1 <- bind_rows(M2_merge, R1_merge)

# ---- (3) Recode factors ----
unique(full_M2_R1$education)
unique(full_M2_R1$sr_health)

full_M2_R1_recode <- full_M2_R1 %>% 
  mutate(across(
    c(pos_interaction, stressor),
    ~ as.numeric(as.character(
      fct_recode(.,
                 "1" = "(1) yes",
                 "0" = "(2) no")
    )
  ))) %>% 
  
  mutate(across(
    c(closeness, belonging),
    ~ as.numeric(as.character(
      fct_recode(.,
                 "0" = "(0) none of the time",
                 "1" = "(1) a little of the time",
                 "2" = "(2) some of the time",
                 "3" = "(3) most of the time",
                 "4" = "(4) all of the time")
    ))
  )) %>% 
  
  mutate(
    day = as.numeric(as.character(
      fct_recode(day,
                 "1" = "(1) day 1",
                 "2" = "(2) day 2",
                 "3" = "(3) day 3",
                 "4" = "(4) day 4",
                 "5" = "(5) day 5",
                 "6" = "(6) day 6",
                 "7" = "(7) day 7",
                 "8" = "(8) day 8")
    )) 
  ) %>% 
  
  mutate(
    educ_num = as.numeric(str_extract(education, "\\d+")), # extract the number, \d+ = one or more digits
    education = case_when(
      educ_num >= 6 ~ 1,
      educ_num <= 5 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% 
  mutate(
    gender = recode(gender,
                    "(1) male" = 0,
                    "(2) female" = 1,
                    .default = NA_real_
                    )
  ) %>% 
  mutate(
    sr_health_original = as.numeric(str_extract(sr_health, "\\d+")), # extract the number, \d+ = one or more digits
    sr_health_reverse = 6 - sr_health_original
  ) %>% 
  mutate(
    func_limitation = (adl*3 + iadl*7)/10
  )

# Check: make sure did not introduce new NAs
full_M2_R1_recode %>%
  summarise(pos_interaction_na = sum(is.na(pos_interaction)),
            closeness_na = sum(is.na(closeness)),
            belonging_na = sum(is.na(belonging)),
            day_na = sum(is.na(day)),
            education_na = sum(is.na(education)),
            gender_na = sum(is.na(gender)),
            sr_health_na = sum(is.na(sr_health_reverse))
  )

full_M2_R1 %>%
  summarise(pos_interaction_na = sum(is.na(pos_interaction)),
            closeness_na = sum(is.na(closeness)),
            belonging_na = sum(is.na(belonging)),
            day_na = sum(is.na(day)),
            education_na = sum(is.na(education)),
            gender_na = sum(is.na(gender)),
            sr_health_na = sum(is.na(sr_health))
  )

# ---- (4) Make social connection variable ----
full_M2_R1_recode <- full_M2_R1_recode %>% 
  mutate(connection = (belonging + closeness)/2) %>% 
  select(id, study, day, pos_interaction, closeness, belonging, connection, everything())

# Check: How many NAs were introduced?
full_M2_R1_recode %>%
  filter(is.na(connection)) %>% 
  select(id, day, belonging, closeness, connection)
# 7 rows had only one score between belonging and closeness and now have an NA

# (5) Social responsivity variable ----
m <- lmer(
  connection ~ pos_interaction + (1 + pos_interaction | id),
  data = full_M2_R1_recode
)

# How much does connection increase for the average person on a positive interaction day?
fixed_slope <- fixef(m)["pos_interaction"]

# How does each person deviate from the fixed slope
random_slopes <- ranef(m)$id$pos_interaction

# Make person-level slope
responsivity_df <- ranef(m)$id %>%
  rownames_to_column("id") %>%
  mutate(
    social_respons = fixed_slope + pos_interaction
  ) %>%
  select(id, social_respons) %>% 
  mutate(id = as.numeric(id))

# Merge with dataset
df_final <- full_M2_R1_recode %>%
  left_join(responsivity_df, by = "id")

# (6) Mean positive interaction ----
df_final <- df_final %>% 
  group_by(id) %>% 
  mutate(connection_m = mean(connection, na.rm = TRUE),
         pos_int_m = mean(pos_interaction, na.rm = TRUE)) %>% 
  ungroup()


# how many participants? 
length(unique(full_M2_R1_recode$id)) # 2804
length(unique(M2_merge$id)) # 2022
length(unique(R1_merge$id)) # 782

# save ----
df_final <- df_final %>% 
  select(-starts_with("B1SA"),
         -starts_with("BAC"),
         -starts_with("RA1"))

write.csv(df_final, "data/M2-R1_2026_LF.csv")

# save version for mplus ----
df <- read.csv("data/M2-R1_2026_LF.csv")

df <- df[, -1] 

# make variables into numeric
df <- df %>% 
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

# rename variables to be under character limit
df <- df %>% 
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
    func_lim = func_limitation
  )

# make NA -999
df[is.na(df)] <- -999

write.table(df,
            file = "Mplus/M2-R1_06-26_LF.dat",
            sep = " ",
            row.names = FALSE,
            col.names = FALSE)

df_dat <- read.table("Mplus/M2-R1_06-26_LF.dat",
                     header = FALSE,
                     sep = "",
                     stringsAsFactors = FALSE)
  
# write.table(df,
#             file = "Mplus/M2-R1_2026_LF.dat",
#             sep = " ",
#             row.names = FALSE,
#             col.names = FALSE)
# 
# df_dat <- read.table("Mplus/M2-R1_2026_LF.dat",
#                      header = FALSE,
#                      sep = "",
#                      stringsAsFactors = FALSE)

cat(names(df), sep = " ")
# id m2 day pos_int closeness belonging connect stressor age gender life_satisfaction neg_aff pos_aff chro_or education white household adl iadl chro_new educ_num srh_or srh_rev func_lim respons connection_m pos_int_m