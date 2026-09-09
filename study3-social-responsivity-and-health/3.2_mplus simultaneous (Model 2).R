library(tidyverse)
library(apaTables)
df <- read.csv("data/M2-R1_2026_LF.csv")

#### SIMULTANEOUS MODEL (MODEL 2) ----
# 1. Chronic conditions ----
respons_chr <- read.table("Mplus/model2_chronic_respons.dat", 
                          header = FALSE, 
                          na.strings = "999")

colnames(respons_chr) <- c("chro_new", "connect", "pos_int", "connecti", 
                           "pos_int_", "age", "gender", "educatio", "m2", 
                           "respons2", "respons2_se", "respons", "respons_se", 
                           "b_connect", "b_connect_se", "id")

# Merge mplus output to main df
respons_chr <- respons_chr %>% 
  rename(respons_chr = respons)

df_merge <- df %>%
  left_join(
    respons_chr %>%
      select(id, respons_chr) %>%
      distinct(),
    by = "id"
  )

cor.test(df_merge$social_respons, df_merge$respons_chr)

# Plot
a <- 20.741  # quadratic coefficient
b <- -6.776 # linear coefficient
c <- 0.277 # intercept

skimr::skim(respons_chr$respons_chr)
respons <- seq(-0.611, 1.03, length.out = 100) # set range of graph to range of responsivity

chr <- a * respons^2 + b * respons + c

df_curve <- data.frame(respons, chr)

ggplot() +
  geom_point(data = df_merge,
             aes(x = respons_chr, y = chronic_condition_remake),
             alpha = 0.1, color = "grey") +
  geom_line(data = df_curve,
            aes(x = respons, y = chr),
            linewidth = 1.2) +
  scale_x_continuous(
    limits = c(-0.8, 1),
    breaks = seq(-0.8, 1, by = 0.2),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  labs(
    x = "Social Responsivity",
    y = "Chronic Conditions"
  ) +
  theme_minimal()

# 2. Self-rated health ----
respons_sr <- read.table("Mplus/model2_srhealth_respons.dat", 
                         header = FALSE, 
                         na.strings = "999")

colnames(respons_sr) <- c("srh_rev", "connect", "pos_int", "connecti", "pos_int_", 
                          "age", "gender", "educatio", "m2", "respons2", "respons2_se", 
                          "respons", "respons_se", "b_connect", "b_connect_se", "id")

# Merge mplus output to main df
respons_sr <- respons_sr %>% 
  rename(respons_sr = respons)

df_merge <- df %>%
  left_join(
    respons_sr %>%
      select(id, respons_sr) %>%
      distinct(),
    by = "id"
  )

cor.test(df_merge$social_respons, df_merge$respons_sr)

# Plot
a <- -8.284 # quadratic coefficient
b <- 1.435 # linear coefficient
c <- 3.133 # intercept

skimr::skim(respons_sr$respons_sr)
respons <- seq(-0.345, 0.634, length.out = 100) # set range of graph to range of responsivity

sr_health <- a * respons^2 + b * respons + c

df_curve <- data.frame(respons, sr_health)

ggplot() +
  geom_point(data = df_merge, aes(x = respons_sr, y = sr_health_reverse), 
             alpha = 0.1, color = "grey") +
  geom_line(data = df_curve, aes(x = respons, y = sr_health), linewidth = 1.2) +
  scale_x_continuous(
    limits = c(-0.8, 1),
    breaks = seq(-0.8, 1, by = 0.2),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  labs(
    x = "Social Responsivity",
    y = "Self-rated Health"
  ) +
  theme_minimal()


# 3. Functional Limitations ----
respons_func <- read.table("Mplus/model2_func_respons.dat", 
                          header = FALSE, 
                          na.strings = "999")

colnames(respons_func) <- c("func_lim", "connect", "pos_int", "connecti", "pos_int_", 
                            "age", "gender", "educatio", "m2", "respons2", "respons2_se", 
                            "respons", "respons_se", "b_connect", "b_connect_se", "id")

# Merge mplus output to main df
respons_func <- respons_func %>% 
  rename(respons_func = respons)

df_merge <- df %>%
  left_join(
    respons_func %>%
      select(id, respons_func) %>%
      distinct(),
    by = "id"
  )

cor.test(df_merge$social_respons, df_merge$respons_func)

# Plot
a <- 3.617  # quadratic coefficient
b <- -1.297 # linear coefficient
c <- 0.936 # intercept

skimr::skim(respons_func$respons_func)
respons <- seq(-0.669, 0.953, length.out = 100) # set range of graph to range of responsivity

func_lim <- a * respons^2 + b * respons + c

df_curve <- data.frame(respons, func_lim)

ggplot() +
  geom_point(data = df_merge, aes(x = respons_func, y = func_limitation), 
             alpha = 0.1, color = "grey") +
  geom_line(data = df_curve, aes(x = respons, y = func_lim), linewidth = 1.2) +
  scale_x_continuous(
    limits = c(-0.8, 1),
    breaks = seq(-0.8, 1, by = 0.2),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  labs(
    x = "Social Responsivity",
    y = "Functional Limitations"
  ) +
  theme_minimal()

### CORRELATION BETWEEN RESPONSIVITY ESTIMATES ----
## merge dfs
# put 6 datasets into a list
dfs <- list(respons_m2, respons_na, respons_pa, respons_sr, respons_chr, respons_func)

# keep only id + variables starting with "respons_"
dfs_selected <- dfs %>%
  map(~ select(.x, id, starts_with("respons_"), -respons_se)) %>% 
  map(~ distinct(.x, id, .keep_all = TRUE))

# merge all datasets by id
merged_respons <- reduce(dfs_selected, full_join, by = "id") %>% 
  rename(respons_lifesat = respons_m2)

# merge empty model responsivity
two_step <- read.csv("data/M2-R1_Mplus-respons.csv") %>% 
  distinct(id, .keep_all = TRUE) %>% 
  select(id, respons_mplus)

merged_respons <- merge(two_step, merged_respons, by = "id")

# keep only numeric variables for correlations
corr_data <- merged_respons %>%
  select(-id)

# create APA-style correlation table
apa.cor.table(
  corr_data,
  filename = "responsivity_corr_table.doc"
)

### ARCHIVE ----
# 1. Life satisfaction responsivity ----
respons_m2 <- read.table("Mplus/model2_respons_scores.dat", 
                         header = FALSE, 
                         na.strings = "999")

colnames(respons_m2) <- c("life_sat", "connect", "pos_int", "pos_int_m",
                          "age", "gender", "education", "m2",
                          "respons2", "respons2_se",
                          "respons", "respons_se",
                          "b_connect", "b_connect_se",
                          "id")

# Merge mplus output to main df
respons_m2 <- respons_m2 %>% 
  rename(respons_m2 = respons)

df_merge <- df %>%
  left_join(
    respons_m2 %>%
      select(id, respons_m2) %>%
      distinct(),
    by = "id"
  )

cor.test(df_merge$social_respons, df_merge$respons_m2)

df_merge %>% 
  ggplot(aes(social_respons)) + 
  geom_histogram() + 
  labs(title = "Two-Step Responsivity")

df_merge %>% 
  ggplot(aes(respons_m2)) + 
  geom_histogram() + 
  labs(title = "Simultaneous Responsivity")

# Plot
a <- -12.458 # quadratic coefficient
b <- 3.793 # linear coefficient
c <- 6.988 # intercept

skimr::skim(respons_m2$respons_m2)
respons <- seq(-0.547, 0.802, length.out = 100) # set range of graph to range of responsivity

life_satisfaction <- a * respons^2 + b * respons + c

df_curve <- data.frame(respons, life_satisfaction)

ggplot() +
  # geom_point(data = df_merge, aes(x = respons_m2, y = life_satisfaction), alpha = 0.3) +
  geom_line(data = df_curve, aes(x = respons, y = life_satisfaction), linewidth = 1.2) +
  labs(
    x = "Responsivity (Mplus)",
    y = "Life Satisfaction"
  ) +
  theme_minimal()

# 2. Positive affect responsivity ----
respons_pa <- read.table("Mplus/model2_PA_respons.dat", 
                         header = FALSE, 
                         na.strings = "999")

colnames(respons_pa) <- c("pos_aff", "connect", "pos_int", "age", "gender", "educatio", 
                          # "m2",
                          "respons2", "respons2_se", "respons", "respons_se", 
                          "b_connect", "b_connect_se", "id")

# Merge mplus output to main df
respons_pa <- respons_pa %>% 
  rename(respons_pa = respons)

df_merge <- df %>%
  left_join(
    respons_pa %>%
      select(id, respons_pa) %>%
      distinct(),
    by = "id"
  )

cor.test(df_merge$social_respons, df_merge$respons_pa)

# Merge with life satisfaction data (to check correlation of responsivity estimates)
m2_pa_merge <- respons_m2 %>% 
  left_join(
    respons_pa %>% 
      select(id, respons_pa) %>%
      distinct(),
    by = "id"
  )

cor.test(m2_pa_merge$respons_m2, m2_pa_merge$respons_pa) # .80 correlated with life satisfaction respons

# Plot
a <- -6.612 # quadratic coefficient
b <- 2.245 # linear coefficient
c <- 3.551 # intercept

skimr::skim(respons_pa$respons_pa)
respons <- seq(-0.24, 0.519, length.out = 100) # set range of graph to range of responsivity

pos_affect <- a * respons^2 + b * respons + c

df_curve <- data.frame(respons, life_satisfaction)

ggplot() +
  geom_point(data = df_merge, aes(x = respons_pa, y = pos_affect), alpha = 0.3) +
  geom_line(data = df_curve, aes(x = respons, y = pos_affect), linewidth = 1.2) +
  labs(
    x = "Responsivity (Mplus)",
    y = "Positive Affect"
  ) +
  theme_minimal()

# 3. Negative affect responsivity ----
respons_na <- read.table("Mplus/model2_NA_respons.dat", 
                         header = FALSE, 
                         na.strings = "999")

colnames(respons_na) <- c("neg_aff", "connect", "pos_int", "age", "gender", "educatio", "m2", 
                          "respons2", "respons2_se", "respons", "respons_se", 
                          "b_connect", "b_connect_se", "id")

# Merge mplus output to main df
respons_na <- respons_na %>% 
  rename(respons_na = respons)

df_merge <- df %>%
  left_join(
    respons_na %>%
      select(id, respons_na) %>%
      distinct(),
    by = "id"
  )

cor.test(df_merge$social_respons, df_merge$respons_na)

# Merge with life satisfaction data (to check correlation of responsivity estimates)
m2_pa_merge <- respons_m2 %>% 
  left_join(
    respons_na %>% 
      select(id, respons_na) %>%
      distinct(),
    by = "id"
  )

cor.test(m2_pa_merge$respons_m2, m2_pa_merge$respons_na) # .71 correlated with life satisfaction respons

# Plot
a <- 2.611 # quadratic coefficient
b <- -0.963 # linear coefficient
c <- 1.405 # intercept

skimr::skim(respons_na$respons_na)
respons <- seq(-0.971, 1.47, length.out = 100) # set range of graph to range of responsivity

neg_affect <- a * respons^2 + b * respons + c

df_curve <- data.frame(respons, neg_affect)

ggplot() +
  geom_point(data = df_merge, aes(x = respons_na, y = neg_affect), alpha = 0.3) +
  geom_line(data = df_curve, aes(x = respons, y = neg_affect), linewidth = 1.2) +
  labs(
    x = "Responsivity (Mplus)",
    y = "Negative Affect"
  ) +
  theme_minimal()

# Note: Below versions were used for debugging/investigating

# 2. Model 2 debug (V2 empty model) ----
# Change from Model 2: Group-mean centering pos-int

respons_debug <- read.table("Mplus/model2_debug_respons_scores.dat", 
                            header = FALSE, 
                            na.strings = "999")

colnames(respons_debug) <- c("CONNECT",
                             "POS_INT",
                             "RESPONS",
                             "B_CONNECT",
                             "ID"
)

skimr::skim(respons_debug$RESPONS)

respons_debug %>% 
  ggplot(aes(RESPONS)) + 
  geom_histogram()

# Merge mplus output to main df
respons_debug <- respons_debug %>% 
  rename(id = ID)

df_merge2 <- df %>%
  left_join(
    respons_debug %>%
      select(id, RESPONS) %>%
      distinct(),
    by = "id"
  )

cor.test(df_merge2$social_respons, df_merge2$RESPONS)
cor.test(df_merge$respons_m2, df_merge2$RESPONS)

# 3. Model 2 V2 (Life satisfaction) ----
# V2 = Group-mean centering pos int 

v2_df <- read.table("Mplus/model2_v2_respons.dat", 
                    header = FALSE, 
                    na.strings = "999")

colnames(v2_df) <- c("life_sat", "connect", "pos_int", "pos_int_",
                     "age", "gender", "educatio", "respons2",
                     "respons2_se", "respons", "respons_se", "b_connect",
                     "b_connect_se", "id")

skimr::skim(v2_df$respons)

df_plot <- df %>%
  left_join(
    v2_df %>%
      select(id, respons) %>%
      distinct(),
    by = "id"
  )

ggplot(df_plot, aes(x = respons, y = life_satisfaction)) +
  geom_point(alpha = 0.3) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = TRUE) + # “Best-fitting quadratic relationship in R using those estimates”
  labs(
    x = "Responsivity (FSCORES)",
    y = "Life Satisfaction"
  ) +
  theme_minimal()

# Plot 2: directly take coefficients
b1 <- 0.635 # linear coefficient
b2 <- -5.23 # quadratic coefficient


respons <- seq(-1, 1, length.out = 100)

life_sat <- b1 * respons + b2 * respons^2

df_curve <- data.frame(respons, life_sat)

ggplot() +
  geom_point(data = df_plot, aes(x = respons, y = life_satisfaction), alpha = 0.3) +
  geom_line(data = df_curve, aes(x = respons, y = life_sat), linewidth = 1.2) +
  labs(
    x = "Responsivity (Mplus)",
    y = "Life Satisfaction"
  ) +
  theme_minimal()

# 4. Model 2 V2 (PA) ----
v2_PA <- read.table("Mplus/model2_v2PA_respons.dat", 
                    header = FALSE, 
                    na.strings = "999")

colnames(v2_PA) <- c("pos_aff", "connect", "pos_int", "pos_int_",
                     "age", "gender", "educatio", "respons2",
                     "respons2_se", "respons", "respons_se", "b_connect",
                     "b_connect_se", "id")

skimr::skim(v2_PA$respons)

df_plot <- df %>%
  left_join(
    v2_PA %>%
      select(id, respons) %>%
      distinct(),
    by = "id"
  )

b1 <- 0.540 # linear coefficient
b2 <- -5.237 # quadratic coefficient


respons <- seq(-1, 1, length.out = 100)

pos_aff <- b1 * respons + b2 * respons^2

df_curve <- data.frame(respons, pos_aff)

ggplot() +
  geom_point(data = df_plot, aes(x = respons, y = pos_affect), alpha = 0.3) +
  geom_line(data = df_curve, aes(x = respons, y = pos_aff), linewidth = 1.2) +
  labs(
    x = "Responsivity (Mplus)",
    y = "Positive Affect"
  ) +
  theme_minimal()

cor.test(df_plot$respons, df_plot$social_respons)

# 5. Model 2 V3 (Life satisfaction) ----
# Note: V3 is with linear effect and no quadratic

v3 <- read.table("Mplus/model2_v3_respons.dat", 
                    header = FALSE, 
                    na.strings = "999")

colnames(v3) <- c("life_sat", "connect", "pos_int", "pos_int_", 
                     "age", "gender", "educatio", "respons", "b_connect", "id")

df_plot <- df %>%
  left_join(
    v3 %>%
      select(id, respons) %>%
      distinct(),
    by = "id"
  )

cor.test(df_plot$social_respons, df_plot$respons)
cor.test(df_plot$respons, df_plot$life_satisfaction)
skimr::skim(df_plot$respons)

ggplot(df_plot, aes(x = respons, y = life_satisfaction)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  # stat_smooth(method = "lm", formula = y ~ x + I(x^2), se = TRUE) + # “Best-fitting quadratic relationship in R using those estimates”
  labs(
    x = "Responsivity (FSCORES)",
    y = "Life Satisfaction"
  ) +
  theme_minimal()



