# load packages and data
library(tidyverse)
library(lme4)
df <- read.csv("data/M2-R1_2026_LF.csv")

df <- df %>% 
  mutate(
    chronic_condition_winsorize = if_else(chronic_condition_remake > 5, 5, chronic_condition_remake)
    ) %>%  # winsorize chronic condition so that max is 5
  group_by(id) %>% 
  mutate(
    connection_isd = sd(connection, na.rm = TRUE)
  )

df_plevel <- df %>% 
  select(id, age, gender, education, educ_num, race, social_respons, pos_int_m, connection_m,connection_isd, 
         life_satisfaction, life_sat_q1, life_sat_no_social, life_sat_social, neg_affect, pos_affect, 
         neg_affect_11_item, pos_affect_10_item, 
         sr_health_reverse, chronic_condition_remake,
         chronic_condition_winsorize, func_limitation) %>% 
  distinct(id, .keep_all = TRUE) # keeps only one row for each participant

df_plevel <- df_plevel %>% 
  mutate(
    social_respons_sq = social_respons^2 # make quadratic interaction term
  )


## ICC ----
# Random-intercept-only (empty) model
m0 <- lmer(connection ~ 1 + (1 | id), data = df, REML = TRUE)

# Extract variance components
vc <- as.data.frame(VarCorr(m0))
tau00  <- vc$vcov[vc$grp == "id"]          # between-person variance
sigma2 <- vc$vcov[vc$grp == "Residual"]    # within-person variance

# ICC
icc <- tau00 / (tau00 + sigma2)
icc

cat("Between-person variance (tau00):", round(tau00, 4), "\n")
cat("Within-person variance (sigma^2):", round(sigma2, 4), "\n")
cat("ICC:", round(icc, 3), "\n")


# (1) Social responsivity and outcome variables ----

# (a) Life satisfation ----
cor.test(df_plevel$social_respons, df_plevel$life_satisfaction)

# Graph
df_plevel %>% 
  ggplot(aes(x = social_respons, y = life_satisfaction)) +
  geom_point(alpha = 0.5) + 
  geom_smooth()

# Regression
quad_model <- lm(
  life_satisfaction ~ social_respons + social_respons_sq,
  data = df_plevel
)

summary(quad_model)

# With mean pos int
quad_model_cov <- lm(
  life_satisfaction ~ social_respons + social_respons_sq + pos_int_m,
  data = df_plevel
)

summary(quad_model_cov)


# With all covariates
quad_model_cov2 <- lm(
  life_satisfaction ~ social_respons + social_respons_sq + pos_int_m + 
    # connection_m +
   age + gender + education,
  data = df_plevel
)

summary(quad_model_cov2)

# 1-item Life satisfaction
quad_model_cov2 <- lm(
  life_sat_q1 ~ social_respons + social_respons_sq + pos_int_m + 
    # connection_m +
    age + gender + education,
  data = df_plevel
)

summary(quad_model_cov2)

# 3-item life satisfaction (no social)
quad_model_cov2 <- lm(
  life_sat_no_social ~ social_respons + social_respons_sq + pos_int_m + 
    connection_m +
    age + gender + education,
  data = df_plevel
)

summary(quad_model_cov2)


# 2-item life satisfaction (social only)
quad_model_cov2 <- lm(
  life_sat_social ~ social_respons + social_respons_sq + pos_int_m + 
    connection_m +
    age + gender + education,
  data = df_plevel
)

summary(quad_model_cov2)


# (b) Positive affect ----
cor.test(df_plevel$social_respons, df_plevel$pos_affect)

df_plevel %>% 
  ggplot(aes(x = social_respons, y = pos_affect)) +
  geom_point(alpha = 0.5) + 
  geom_smooth()

quad_model <- lm(
  pos_affect ~ social_respons + social_respons_sq,
  data = df_plevel
)

summary(quad_model)

quad_model_cov <- lm(
  pos_affect ~ social_respons + social_respons_sq + pos_int_m,
  data = df_plevel
)

summary(quad_model_cov)

# with all covariates
quad_model_cov2 <- lm(
  pos_affect ~ social_respons + social_respons_sq + pos_int_m + 
    connection_m +
    age + gender + education,
  data = df_plevel
)

summary(quad_model_cov2)

# PA with 10 items (incude PANAS)
quad_model_cov2 <- lm(
  pos_affect_10_item ~ social_respons + social_respons_sq + pos_int_m + 
    connection_m +
    age + gender + education,
  data = df_plevel
)

summary(quad_model_cov2)


# (c) Negative affect ----
cor.test(df_plevel$social_respons, df_plevel$neg_affect)

df_plevel %>% 
  ggplot(aes(x = social_respons, y = neg_affect)) +
  geom_point(alpha = 0.5) + 
  geom_smooth()

quad_model <- lm(
  neg_affect ~ social_respons + social_respons_sq,
  data = df_plevel
)

summary(quad_model)

quad_model_cov <- lm(
  neg_affect ~ social_respons + social_respons_sq + pos_int_m,
  data = df_plevel
)

summary(quad_model_cov)

# with all covariates
quad_model_cov2 <- lm(
  neg_affect ~ social_respons + social_respons_sq + pos_int_m + connection_m + 
    age + gender + education,
  data = df_plevel
)

summary(quad_model_cov2)

# NA with 11 items
quad_model_cov2 <- lm(
  neg_affect_11_item ~ social_respons + social_respons_sq + pos_int_m + 
    # connection_m +
    age + gender + education,
  data = df_plevel
)

summary(quad_model_cov2)


# (d) Self-rated health ----
cor.test(df_plevel$social_respons, df_plevel$sr_health_reverse)

df_plevel %>% 
  ggplot(aes(x = social_respons, y = sr_health_reverse)) +
  geom_point(position = "jitter", alpha = 0.5) + 
  geom_smooth()

quad_model <- lm(
  sr_health_reverse ~ social_respons + social_respons_sq,
  data = df_plevel
)

summary(quad_model)

quad_model_cov <- lm(
  sr_health_reverse ~ social_respons + social_respons_sq + pos_int_m,
  data = df_plevel
)

summary(quad_model_cov)

# with all covariates
quad_model_cov2 <- lm(
  sr_health_reverse ~ social_respons + social_respons_sq + pos_int_m + 
    # connection_m +
    age + gender + education,
  data = df_plevel
)

summary(quad_model_cov2)


# (e) Chronic conditions ----
cor.test(df_plevel$social_respons, df_plevel$chronic_condition_remake)

df_plevel %>% 
  ggplot(aes(x = social_respons, y = chronic_condition_remake)) +
  geom_point(position = "jitter", alpha = 0.5) + 
  geom_smooth()

quad_model <- lm(
  chronic_condition_winsorize ~ social_respons + social_respons_sq,
  data = df_plevel
)

summary(quad_model)

quad_model_cov <- lm(
  chronic_condition_winsorize ~ social_respons + social_respons_sq + pos_int_m,
  data = df_plevel
)

summary(quad_model_cov)

# with all covariates
quad_model_cov2 <- lm(
  chronic_condition_winsorize ~ social_respons + social_respons_sq + pos_int_m + 
    connection_m +
    age + gender + education,
  data = df_plevel
)

summary(quad_model_cov2)

quad_model_cov2 <- lm(
  chronic_condition_remake ~ social_respons + social_respons_sq + pos_int_m + 
    age + gender + education,
  data = df_plevel
)

summary(quad_model_cov2)

df_plevel %>% 
  ggplot(aes(chronic_condition_winsorize)) + 
  geom_bar()

# (f) Functional Limitations ----
cor.test(df_plevel$social_respons, df_plevel$func_limitation)

df_plevel %>% 
  ggplot(aes(x = social_respons, y = func_limitation)) +
  geom_point(position = "jitter", alpha = 0.5) + 
  geom_smooth()

quad_model <- lm(
  func_limitation ~ social_respons + social_respons_sq,
  data = df_plevel
)

summary(quad_model)

quad_model_cov <- lm(
  func_limitation ~ social_respons + social_respons_sq + pos_int_m,
  data = df_plevel
)

summary(quad_model_cov)

# with all covariates
quad_model_cov2 <- lm(
  func_limitation ~ social_respons + social_respons_sq + pos_int_m + 
    connection_m +
    age + gender + education,
  data = df_plevel
)

summary(quad_model_cov2)

# (3) Social responsivity and social connection ----
df <- df %>% 
  group_by(id) %>% 
  mutate(connection_m = mean(connection, na.rm = TRUE)) %>% 
  ungroup()

df_p <- df %>% 
  select(id, connection_m, pos_int_m, social_respons, life_satisfaction) %>% 
  distinct(id, .keep_all = TRUE)

cor.test(df_p$connection_m, df_p$social_respons)
cor.test(df_p$connection_m, df_p$life_satisfaction)

# (4) Mean positive interaction exploration ----
# (a) Correlation with social responsivity ----
cor.test(df$pos_int_m, df$social_respons) # Weakly negatively correlated social responsivity

# (b) Positive interaction and daily stressor ----
df %>% 
  group_by(pos_interaction) %>% 
  summarize(
    stressor_perc = mean(stressor, na.rm = TRUE)
  )

mean(df$stressor, na.rm = TRUE)

# (c) Difference in responsivity when including covariate pos_int mean ----
# EMPTY MODEL: Controlling for pos interaction mean:  
m2 <- lmer(
  connection ~ pos_interaction + pos_int_m + (1 + pos_interaction | id),
  data = df
)

# Fixed slope
fixed_slope2 <- fixef(m2)["pos_interaction"]

# Extract random slopes and compute person-level responsivity
responsivity_df2 <- ranef(m2)$id %>%
  rownames_to_column("id") %>%
  mutate(
    social_respons_v2 = fixed_slope2 + pos_interaction
  ) %>%
  select(id, social_respons_v2) %>%
  mutate(id = as.numeric(id))

# Merge back into the original dataframe
df <- df %>%
  left_join(responsivity_df2, by = "id")

# Examine the two different responsivity variables
ggplot(data = df) + 
  geom_density(aes(social_respons), 
               fill = "blue", color = "blue", alpha = 0.3) + 
  geom_density(aes(social_respons_v2), 
               fill = "red", color = "red", alpha = 0.3)

# Examine responsivity change with positive interaction mean
responsivity_examine <- df %>% 
  select(id, pos_int_m, social_respons, social_respons_v2) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  mutate(
    respons_change = social_respons_v2 - social_respons
  )

responsivity_examine %>% 
  ggplot(aes(respons_change)) + 
  geom_histogram()

skimr::skim(responsivity_examine$respons_change)

cor.test(responsivity_examine$pos_int_m, responsivity_examine$respons_change)

responsivity_examine %>% 
  ggplot(aes(pos_int_m, respons_change)) + 
  geom_point()

# (5) Social responsivity and demographics ----

# Gender
t.test(social_respons ~ gender, data = df_plevel)

# education
t.test(social_respons ~ education, data = df_plevel)

# age
cor.test(df_plevel$age, df_plevel$social_respons)

# Between variables ----
cor.test(df_plevel$life_satisfaction, df_plevel$pos_affect)
cor.test(df_plevel$chronic_condition_remake, df_plevel$sr_health_reverse)
cor.test(df_plevel$chronic_condition_remake, df_plevel$pos_affect)
cor.test(df_plevel$chronic_condition_remake, df_plevel$life_satisfaction)
cor.test(df$belonging, df$closeness)
