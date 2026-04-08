############################################################
# PROJECT: RaMMPS-CSH
# AUTHOR: Kassoum Dianou
# PURPOSE: Mortality estimation (MPS vs DHS)
############################################################


# ==============================
# 0. INITIALISATION
# ==============================
rm(list = ls())
set.seed(3)
here::i_am("code/TMIH_CSM_analysis_clean.R")

# ==============================
# 1. PACKAGES
# ==============================

# CRAN packages
cran_packages <- c(
  "htmltools", "foreign", "memisc", "tidyr", "readstata13",
  "rdhs", "dplyr", "purrr", "ggplot2", "khroma",
  "questionr", "testthat", "survival", "esquisse",
  "labelled", "data.table", "haven", "demogsurv",
  "gdata", "survey", "anesrake", "gtsummary",
  "tidyverse", "broom", "officer", "flextable",
  "writexl", "patchwork", "ggpubr", "cowplot",
  "forcats", "stringr", "gridExtra", "gtable",
  "tableone", "lubridate", "here"
)

# GitHub packages
github_packages <- c(
  "PPgp/wpp2022"
)

# ---------------------------------------------------------
# Install missing CRAN packages
# ---------------------------------------------------------
installed <- rownames(installed.packages())

for (pkg in cran_packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# ---------------------------------------------------------
# Install missing GitHub packages
# ---------------------------------------------------------
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

for (pkg in github_packages) {
  pkg_name <- basename(pkg)
  if (!(pkg_name %in% installed)) {
    devtools::install_github(pkg)
  }
}

# ---------------------------------------------------------
# Load all packages safely
# ---------------------------------------------------------
invisible(lapply(c(cran_packages, "wpp2022"), function(pkg) {
  library(pkg, character.only = TRUE)
}))


# ==============================
# 2. PATHS
# ==============================

paths <- list(
  data = here("data"),
  figures = here("figures"),
  tables = here("tables")
)

# ==============================
# 3. IMPORT DATA
# ==============================
child      <- read.dta13(file.path(paths$data, "Mps_data.dta"))
child_eds  <- read.dta13(file.path(paths$data, "DHS_IRData.dta"))


# here::here()
# list.files("data")
-------------------------------
  # 4. CLEANING MPS DATA
-------------------------------
  child <- child %>%
  mutate(
    source = as.factor(source),
    type = recode_factor(type, "1" = "EHCVM", "2" = "RDD"),
    b4_ = as.factor(b4_)
  )

child$migration[child$type == "RDD"] <- NA

child <- child %>%
  mutate(
    migration = recode_factor(migration,
                              "1" = "Migrant",
                              "0" = "Non migrant"),
    grappe = if_else(
      province %in% c("Komondjari (ou Komandjoari)", "Komandjoari",
                      "Kompienga", "Tapoa", "Yagha", "Oudalan"),
      1, 0
    ),
    grappe = factor(grappe, levels = c(1, 0),
                    labels = c("Excluded", "Included"))
  )

child$hhweight[child$type == "RDD"] <- 1

-------------------------------
  # 5. VARIABLE SELECTION
-------------------------------
  
child_mps_select <- child %>% 
  select(parent_key, round, v008, v021, v025, v005, v011, v024,
         dob, dod, death, type, wm_weight, st_trim_weight4_1,
         migration, grappe, age_3, nivins, stat_mat2,
         place_res, size, electricity, roofing, water) %>%
  rename(caseid = parent_key,
         weight = wm_weight)


child_mps_select <- child_mps_select %>%
  mutate(
    region = car::recode(v024, '1=1;4=1;5=1;8=1;10=1;12=1;2=2;3=2;6=2;7=2;9=2;11=2;13=2'),
    region2 = car::recode(v024, "1=1;4=1;5=1;8=0;10=0;12=0;2=2;3=2;6=2;7=2;9=2;11=2;13=2"),
    region3 = car::recode(v024, "1=1;4=1;5=0;8=0;10=1;12=0;2=2;3=2;6=2;7=2;9=2;11=2;13=2")
  )

-------------------------------
  # 6. DHS DATA PREPARATION
-------------------------------
  
child_eds <- child_eds %>% 
  mutate(
    grappe = factor("Included"),
    migration = factor("Non migrant"),
    weight = v005 / 1e6,
    type = factor("DHS"),
    v024 = as.numeric(v024),
    region3 = case_when(
      v024 %in% c(1, 4, 10)       ~ 1,
      v024 %in% c(5, 8, 12)       ~ 0,
      v024 %in% c(2, 3, 6, 7, 9, 11, 13) ~ 2,
      TRUE ~ NA_real_
    ),
    st_trim_weight4_1 = 1
  )

child_dhs_select <- child_eds %>% 
  select(caseid, round, v008, v021, v025, v005, v011, v024,
         dob, dod, death, type, weight, st_trim_weight4_1,
         migration, grappe, region, region3, region2,
         age_3, nivins, stat_mat2, place_res,
         size, electricity, roofing, water)

-------------------------------
  # 7. MERGE DATASETS
-------------------------------
  
vars_add <- c("age_3", "nivins", "stat_mat2", "place_res", "size", "electricity", "roofing", "water", "v025")

child_mps_select[vars_add] <- lapply(child_mps_select[vars_add], as.character)
child_dhs_select[vars_add] <- lapply(child_dhs_select[vars_add], as.character)

mps_dhs_select <- bind_rows(child_mps_select, child_dhs_select)

-------------------------------
  # 8. FORMATTING
-------------------------------
  
mps_dhs_select <- mps_dhs_select %>%
  mutate(
    region = factor(region, levels = c(1, 2),
                    labels = c("Conflict", "Non conflict")),
    place_res = factor(place_res, levels = c("urban", "rural")),
    electricity = factor(electricity, levels = c("No", "Yes"))
  )

-------------------------------
  # 8. TABLE SI.1 (EHCVM)
-------------------------------
# ehcvm <- read.dta13(file.path(paths$data, "ehcvm.dta")) need to request

ehcvm_h <- ehcvm %>%
  mutate(phone = ifelse(ownership == "Matched (3)", 1, 0))

tab1 <- CreateTableOne(vars = vars_add, strata = "phone",
                       data = ehcvm_h, test = TRUE)

tab1_df <- print(tab1, printToggle = FALSE) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable")

ft <- flextable(tab1_df)

doc <- read_docx() %>%
  body_add_par("Table 1. Characteristics by phone ownership",
               style = "heading 2") %>%
  body_add_flextable(ft)

print(doc, target = file.path(paths$tables, "Table1_EHCVM_phone.docx"))

-------------------------------
  # 10. PROPENSITY SCORE
-------------------------------
ehcvm_h$place = ehcvm_h$reg_conf

ps_model <- glm( phone ~ place + place_res + electricity + roofing + water +
      size + age_3,
    data = ehcvm_h,
    family = binomial(link = "logit")
  )

ehcvm_h$pscore <- predict(ps_model, type = "response")
summary(ps_model)


# Appliquer ce modèle à l’EDS et aux MPS

eds_h = mps_dhs_select[mps_dhs_select$type=="DHS",]


# Appliquer ce modèle à l’EDS
eds_h$place = eds_h$region
levels(ps_model$model$place)
eds_h$pscore <- predict(ps_model,
                        newdata = eds_h,
                        type = "response")

common_support <- range(ehcvm_h$pscore[ehcvm_h$phone == 1])

eds_cs <- eds_h %>%
  filter(pscore >= common_support[1],
         pscore <= common_support[2])

eds_cs$ipw = eds_cs$weight

# Appliquer ce modèle à MPS
mps_h = mps_dhs_select[mps_dhs_select$type!="DHS",]
mps_h$place = mps_h$region
mps_h$pscore <- predict(ps_model,
                        newdata = mps_h,
                        type = "response")

mps_h$ipw <- 1 / mps_h$pscore


# Création du plan de sondage

design_mps_ipw <- svydesign(
  ids = ~1,
  data = mps_h,
  weights = ~ipw
)


mps_dhs_h = rbind( mps_h, eds_cs)

mps_dhs_h$Pond = "IPW"

# Analyses de sensibilité et méthodes avancées : mise en œuvre complète
# Analyse de sensibilité 1 — Restriction à des sous-populations homogènes
# Fin R1: 2. Correction incomplète des biais de couverture télépho --------

mps_dhs_select$pscore = 1
mps_dhs_select$ipw = mps_dhs_select$weight
mps_dhs_select$Pond = "PS"
mps_dhs_h_ps = rbind(mps_dhs_h, mps_dhs_select)
mps_dhs_h_ps$Pond <- as.factor(mps_dhs_h_ps$Pond)

-------------------------------
  # 11. FIGURE 3
-------------------------------
  
# ==============================
# a. RECODE SURVEY TYPE
# ==============================
# Convert type variable to factor
mps_dhs_h_ps$type <- as.factor(mps_dhs_h_ps$type)

# Recode survey type into broader categories: MPS vs DHS
mps_dhs_h_ps <- mps_dhs_h_ps %>% 
  mutate(type_recode = case_when(
    type %in% c("EHCVM", "RDD") ~ "MPS",
    type == "DHS" ~ "DHS"
  )) %>%
  mutate(type_recode = factor(type_recode, levels = c("MPS", "DHS")))

mps_dhs_h <- mps_dhs_h %>%
  mutate(type_recode = case_when(
    type %in% c("EHCVM", "RDD") ~ "MPS",
    type == "DHS" ~ "DHS"
  )) %>%
  mutate(type_recode = factor(type_recode, levels = c("MPS", "DHS")))

# ==============================
# b. CALCULATE OVERALL MORTALITY (DHS vs MPS)
# ==============================
DHS_MPS <- calc_nqx(
  data = mps_dhs_h_ps,
  by = ~type_recode,
  strata = NULL,
  period = NULL,
  agegr = seq(0, 5),
  cohort = NULL,
  dob = "dob",
  dod = "dod",
  death = "death",
  weight = "weight",
  intv = "v008",
  tips = c(0, 4),
  origin = 1900,
  scale = 12
)

# ==============================
# c. DEFINE ANALYSIS PERIODS
# ==============================
data <- mps_dhs_h_ps

periods <- list(
  "2014–2017" = c(2014, 2018),
  "2018–2019" = c(2018, 2020),
  "2020–2022" = c(2020, 2023)
)

# ==============================
# d. CALCULATE MORTALITY BY PERIOD, SOURCE, REGION, AND WEIGHTING
# ==============================
results <- lapply(names(periods), function(period_name) {
  
  period <- periods[[period_name]]
  
  calc_nqx(
    data = data,
    by = ~Pond + type_recode + region,
    strata = NULL,
    agegr = seq(0, 5),
    period = period,
    cohort = NULL,
    dob = "dob",
    dod = "dod",
    death = "death",
    weight = "weight",
    intv = "v008",
    tips = NULL,
    origin = 1900,
    scale = 12
  ) %>%
    mutate(period = period_name)
  
}) %>%
  bind_rows() %>%
  mutate(
    period = factor(period, levels = names(periods)),
    Pond   = factor(Pond, levels = c("PS", "IPW")),
    region = factor(region, levels = c("Conflict", "Non conflict"))
  )

results_std_p1_t1_r1 <- results  # Save a copy of the standardized results

# ==============================
# e. FILTER FOR POST-STRATIFICATION WEIGHTS ONLY
# ==============================
results_ps <- results %>%
  filter(Pond == "PS") %>%
  mutate(
    region = factor(region, levels = c("Conflict", "Non conflict")),
    period = factor(period, levels = c("2014–2017", "2018–2019", "2020–2022"))
  )

# ==============================
# f. PLOT UNDER-FIVE MORTALITY
# ==============================
p_ps <- ggplot(
  results_ps,
  aes(x = period, y = est, fill = type_recode)
) +
  # Bar plot (dodge position for MPS vs DHS)
  geom_col(position = position_dodge(width = 0.8), width = 0.65) +
  
  # Error bars
  geom_errorbar(
    aes(ymin = ci_l, ymax = ci_u),
    position = position_dodge(width = 0.8),
    width = 0.2,
    linewidth = 0.6,
    color = "blue"
  ) +
  
  # Facet by region
  facet_wrap(~region, labeller = labeller(
    region = c(
      "Conflict" = "Conflict areas",
      "Non conflict" = "Non-conflict areas"
    )
  )) +
  
  # Color scale
  scale_fill_manual(
    values = c("MPS" = "gray60", "DHS" = "black"),
    name = "Data source"
  ) +
  
  # Y-axis settings
  scale_y_continuous(
    limits = c(0, 0.15),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  # Labels
  labs(
    x = "Period",
    y = "Under-five mortality (5q0)",
    caption = "Source: RaMMPS-2021/22 and DHS-2021"
  ) +
  
  # Theme
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p_ps)

# ==============================
# g. EXPORT FIGURE 
# ==============================
ggsave(
  filename = "Figure.3.png",
  plot = p_ps,
  width = 10,
  height = 6,
  dpi = 300
)


# Sous forme de tableau ---------------------------------------------------

# ==============================
# 12. Table SI3.2
# ==============================

# ==============================
# a. PREPARE RESULTS TABLE
# ==============================
# Combine estimates and confidence intervals in a single column
table_results <- results %>%
  mutate(
    estimate_ci = sprintf("%.3f (%.3f–%.3f)", est, ci_l, ci_u)
  ) %>%
  # Select relevant columns
  select(period, region, Pond, type_recode, estimate_ci) %>%
  # Pivot wider: one column per data source
  pivot_wider(
    names_from = type_recode,
    values_from = estimate_ci
  ) %>%
  # Arrange by adjustment, region, and period
  arrange(Pond, region, period)

# ==============================
# b. RENAME COLUMNS FOR PUBLICATION STYLE
# ==============================
table_results <- table_results %>%
  rename(
    Period = period,
    Region = region,
    Adjustment = Pond,
    `MPS (IPW/PS)` = MPS,
    `DHS (reference)` = DHS
  )

# ==============================
# c. CREATE FLEXTABLE
# ==============================
ft <- flextable(table_results) %>%
  autofit() %>%                      # Adjust column widths automatically
  theme_booktabs() %>%               # Clean publication-style theme
  align(align = "center", part = "all") %>% # Center align all cells
  bold(part = "header")              # Bold headers

# ==============================
# d. EXPORT TO WORD
# ==============================
doc <- read_docx() %>%
  body_add_par(
    "Table X. Under-five mortality (5q0) by period, region, data source, and adjustment method",
    style = "heading 2"
  ) %>%
  body_add_flextable(ft)

# Save Word document
print(doc, target = "Table_SI-3.2_results.docx")


# ==============================
# 13. Figure 4
# ==============================

# ==============================
# a. CALCULATE MORTALITY BY PERIOD, SOURCE, REGION, AND WEIGHTING
# ==============================
# Loop over each period and compute mortality
results <- lapply(names(periods), function(period_name) {
  
  period <- periods[[period_name]]
  
  calc_nqx(
    data = data,
    by = ~Pond + type + region,
    strata = NULL,
    agegr = seq(0, 5),
    period = period,
    cohort = NULL,
    dob = "dob",
    dod = "dod",
    death = "death",
    weight = "weight",
    intv = "v008",
    tips = NULL,
    origin = 1900,
    scale = 12
  ) %>%
    mutate(period = period_name)
  
}) %>%
  bind_rows() %>%
  mutate(
    period = factor(period, levels = names(periods)),
    Pond   = factor(Pond, levels = c("PS", "IPW")),
    region = factor(region, levels = c("Conflict", "Non conflict"))
  )

# Save standardized results
results_std_p1_t2_r1 <- results

# ==============================
# b. FILTER FOR POST-STRATIFICATION WEIGHTS ONLY
# ==============================
results_ps <- results %>%
  filter(Pond == "PS") %>%
  mutate(
    region = factor(region, levels = c("Conflict", "Non conflict")),
    period = factor(period, levels = names(periods)),
    type   = factor(type, levels = c("DHS", "EHCVM", "RDD"))
  )

# ==============================
# c. PLOT UNDER-FIVE MORTALITY (FIGURE 4)
# ==============================
p_ps <- ggplot(
  results_ps,
  aes(x = period, y = est, fill = type)
) +
  # Bar plot with dodged positions
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.65
  ) +
  # Error bars
  geom_errorbar(
    aes(ymin = ci_l, ymax = ci_u),
    position = position_dodge(width = 0.8),
    width = 0.2,
    linewidth = 0.6,
    color = "blue"
  ) +
  # Facet by region
  facet_wrap(~region, labeller = labeller(
    region = c(
      "Conflict" = "Conflict areas",
      "Non conflict" = "Non-conflict areas"
    )
  )) +
  # Manual fill colors for survey types
  scale_fill_manual(
    values = c(
      "DHS"   = "black",
      "EHCVM" = "gray50",
      "RDD"   = "gray75"
    ),
    name = "Data source"
  ) +
  # Y-axis settings
  scale_y_continuous(
    limits = c(0, 0.15),
    expand = expansion(mult = c(0, 0.05))
  ) +
  # Labels and caption
  labs(
    x = "Period",
    y = "Under-five mortality (5q0)",
    caption = "Source: RaMMPS-2021/22 and DHS-2021"
  ) +
  # Theme settings
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Print plot
print(p_ps)

# ==============================
# d. EXPORT FIGURE
# ==============================

ggsave(
  filename = "Figure4.png",
  plot = p_ps,
  width = 10,
  height = 6,
  dpi = 300
)

# ==============================
# 13. Figure SI3.1
# ==============================

# ==============================
# a. CALCULATE MORTALITY BY PERIOD, SOURCE, REGION, AND WEIGHTING
# ==============================
results <- lapply(names(periods), function(period_name) {
  
  period <- periods[[period_name]]
  
  calc_nqx(
    data = data,
    by = ~Pond + type_recode + region2,
    strata = NULL,
    agegr = seq(0, 5),
    period = period,
    cohort = NULL,
    dob = "dob",
    dod = "dod",
    death = "death",
    weight = "weight",
    intv = "v008",
    tips = NULL,
    origin = 1900,
    scale = 12
  ) %>%
    mutate(period = period_name)
  
}) %>%
  bind_rows() %>%
  mutate(
    period = factor(period, levels = names(periods)),
    Pond   = factor(Pond, levels = c("PS", "IPW")),
    region2 = factor(region2, levels = c("High conflict", "Moderate conflict", "Non conflict"))
  )

# Save standardized results
results_std_p1_t1_r2 <- results

# ==============================
# b. FILTER FOR POST-STRATIFICATION WEIGHTS ONLY
# ==============================
results_ps <- results %>%
  filter(Pond == "PS") %>%
  mutate(
    period = factor(period, levels = names(periods)),
    region2 = factor(region2, levels = c("High conflict", "Moderate conflict", "Non conflict")),
    type_recode = factor(type_recode, levels = c("DHS", "MPS"))
  )

# ==============================
# c. PLOT UNDER-FIVE MORTALITY (FIGURE SI.3)
# ==============================
p_ps <- ggplot(
  results_ps,
  aes(x = period, y = est, fill = type_recode)
) +
  # Bar plot
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.65
  ) +
  # Error bars
  geom_errorbar(
    aes(ymin = ci_l, ymax = ci_u),
    position = position_dodge(width = 0.8),
    width = 0.2,
    linewidth = 0.6,
    color = "blue"
  ) +
  # Facet by region2 (detailed conflict levels)
  facet_wrap(~region2, labeller = labeller(
    region2 = c(
      "High conflict" = "High conflict areas",
      "Moderate conflict" = "Moderate conflict areas",
      "Non conflict" = "Non-conflict areas"
    )
  )) +
  # Manual fill colors
  scale_fill_manual(
    values = c(
      "DHS" = "black",
      "MPS" = "gray60"
    ),
    name = "Data source"
  ) +
  # Y-axis settings
  scale_y_continuous(
    limits = c(0, 0.15),
    expand = expansion(mult = c(0, 0.05))
  ) +
  # Labels
  labs(
    x = "Period",
    y = "Under-five mortality (5q0)",
    caption = "Source: RaMMPS-2021/22 and DHS-2021"
  ) +
  # Theme
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Print plot
print(p_ps)

# ==============================
# d. EXPORT FIGURE 
# ==============================
ggsave(
  filename = "Figure-SI3_1.png",
  plot = p_ps,
  width = 11,
  height = 6,
  dpi = 300
)


# ==============================
# 14. Figure SI3.3
# ==============================

# ==============================
# a. DEFINE ANALYSIS PERIODS
# ==============================
periods <- list(
  "2016–2019" = c(2016, 2020),
  "2020–2022" = c(2020, 2023)
)

# ==============================
# b. CALCULATE MORTALITY BY PERIOD, SURVEY TYPE, REGION, AND WEIGHTING
# ==============================
results <- lapply(names(periods), function(period_name) {
  
  period <- periods[[period_name]]
  
  calc_nqx(
    data = data,
    by = ~Pond + type_recode + region2,
    strata = NULL,
    agegr = seq(0, 5),
    period = period,
    cohort = NULL,
    dob = "dob",
    dod = "dod",
    death = "death",
    weight = "weight",
    intv = "v008",
    tips = NULL,
    origin = 1900,
    scale = 12
  ) %>%
    mutate(period = period_name)
  
}) %>%
  bind_rows() %>%
  mutate(
    period = factor(period, levels = names(periods)),
    Pond   = factor(Pond, levels = c("PS", "IPW")),
    region2 = factor(region2, levels = c("High conflict", "Moderate conflict", "Non conflict"))
  )

# Save standardized results
results_std_p2_t1_r2 <- results

# ==============================
# c. FILTER FOR POST-STRATIFICATION WEIGHTS ONLY
# ==============================
results_ps <- results %>%
  filter(Pond == "PS") %>%
  mutate(
    period = factor(period, levels = names(periods)),
    region2 = factor(region2, levels = c("High conflict", "Moderate conflict", "Non conflict")),
    type_recode = factor(type_recode, levels = c("DHS", "MPS"))
  )

# ==============================
# d. PLOT UNDER-FIVE MORTALITY (FIGURE SI.3.2 / SI.3.3)
# ==============================
p_ps <- ggplot(
  results_ps,
  aes(x = period, y = est, fill = type_recode)
) +
  # Bar plot for PS only
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.65
  ) +
  # Error bars for confidence intervals
  geom_errorbar(
    aes(ymin = ci_l, ymax = ci_u),
    position = position_dodge(width = 0.8),
    width = 0.2,
    linewidth = 0.6,
    color = "blue"
  ) +
  # Facet by region with descriptive labels
  facet_wrap(~region2, labeller = labeller(
    region2 = c(
      "High conflict" = "High conflict areas",
      "Moderate conflict" = "Moderate conflict areas",
      "Non conflict" = "Non-conflict areas"
    )
  )) +
  # Manual fill colors for survey types
  scale_fill_manual(
    values = c(
      "DHS" = "black",
      "MPS" = "gray60"
    ),
    name = "Data source"
  ) +
  # Y-axis limits
  scale_y_continuous(
    limits = c(0, 0.15),
    expand = expansion(mult = c(0, 0.05))
  ) +
  # Axis labels and caption
  labs(
    x = "Period",
    y = "Under-five mortality (5q0)",
    caption = "Source: RaMMPS-2021/22 and DHS-2021"
  ) +
  # Minimal theme with customizations
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Print plot
print(p_ps)

# ==============================
# e. EXPORT FIGURE 
# ==============================
ggsave(
  filename = "Figure_SI_3_3.png",
  plot = p_ps,
  width = 11,
  height = 6,
  dpi = 300
)


# ==============================
# 15. Compare analytical vs bootstrap estimates of
#          under-five mortality (5q0) by survey type, region,
#          weighting method, and period
# ==============================

# ==============================
# a. DEFINE ANALYSIS PERIODS
# ==============================
periods <- list(
  "2014–2017" = c(2014, 2018),
  "2018–2019" = c(2018, 2020),
  "2020–2022" = c(2020, 2023)
)

# ==============================
# b. CALCULATE STANDARD MORTALITY ESTIMATES
# ==============================
results_std <- lapply(names(periods), function(period_name) {
  
  period <- periods[[period_name]]
  
  calc_nqx(
    data = data,
    by = ~Pond + type + region,
    strata = NULL,
    agegr = seq(0, 5),
    period = period,
    cohort = NULL,
    dob = "dob",
    dod = "dod",
    death = "death",
    weight = "weight",
    intv = "v008",
    tips = NULL,
    origin = 1900,
    scale = 12
  ) %>%
    mutate(period = period_name)
  
}) %>%
  bind_rows() %>%
  mutate(
    period = factor(period, levels = names(periods)),
    Pond   = factor(Pond, levels = c("PS", "IPW")),
    region = factor(region, levels = c("Conflict", "Non conflict"))
  )

# Save standard estimates
results_std_p1_t2_r1 <- results_std

# ==============================
# c. DEFINE BOOTSTRAP FUNCTION
# ==============================
bootstrap_nqx <- function(data, by, period, B = 1000, seed = 123, agegr = seq(0, 5)) {
  
  set.seed(seed)
  n <- nrow(data)
  
  # Run bootstrap
  boot_est <- replicate(B, {
    
    # Resample with replacement weighted by survey weight
    idx <- sample(seq_len(n), size = n, replace = TRUE, prob = data$weight)
    data_b <- data[idx, ]
    
    # Calculate mortality
    res <- calc_nqx(
      data_b,
      by = by,
      strata = NULL,
      period = period,
      agegr = agegr,
      cohort = NULL,
      dob = "dob",
      dod = "dod",
      death = "death",
      weight = "weight",
      intv = "v008",
      tips = NULL,
      origin = 1900,
      scale = 12
    )
    
    res$est
  })
  
  # Summarize bootstrap results
  tibble(
    est  = median(boot_est, na.rm = TRUE),
    se   = sd(boot_est, na.rm = TRUE),
    ci_l = quantile(boot_est, 0.025, na.rm = TRUE),
    ci_u = quantile(boot_est, 0.975, na.rm = TRUE)
  )
}

# ==============================
# d. RUN BOOTSTRAP ESTIMATES
# ==============================
results_boot <- lapply(names(periods), function(p_name) {
  
  p_range <- periods[[p_name]]
  
  data %>%
    group_by(Pond, type, region) %>%
    group_modify(~ bootstrap_nqx(
      data = .x,
      by = ~1,        # no additional stratification
      period = p_range,
      B = 1000
    )) %>%
    ungroup() %>%
    mutate(period = p_name)
  
}) %>%
  bind_rows() %>%
  rename(
    boot_est  = est,
    boot_se   = se,
    boot_ci_l = ci_l,
    boot_ci_u = ci_u
  ) %>%
  mutate(method = "bootstrap")

# ==============================
# e. MERGE STANDARD AND BOOTSTRAP ESTIMATES
# ==============================
results_final <- results_std %>%
  mutate(method = "Analytical") %>%
  left_join(results_boot, by = c("Pond", "type", "region", "period")) %>%
  mutate(
    period = factor(period, levels = names(periods)),
    Pond   = factor(Pond, levels = c("PS", "IPW")),
    region = factor(region, levels = c("Conflict", "Non conflict"))
  )

# ==============================
# f. PLOT ANALYTICAL ESTIMATES
# ==============================
p_std <- ggplot(results_final, aes(x = period, y = est, fill = type)) +
  geom_col(position = position_dodge(0.8), width = 0.65) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u),
                position = position_dodge(0.8),
                width = 0.2,
                color = "blue") +
  facet_grid(Pond ~ region) +
  scale_fill_manual(values = c("EHCVM" = "gray50", "RDD" = "gray80", "DHS" = "black"),
                    name = "Data source") +
  labs(y = "Under-five mortality (5q0)", x = "Period",
       caption = "Analytical confidence intervals") +
  theme_minimal()
print(p_std)

# ==============================
# g. PLOT BOOTSTRAP ESTIMATES
# ==============================
p_boot <- ggplot(results_final, aes(x = period, y = boot_est, fill = type)) +
  geom_col(position = position_dodge(0.8), width = 0.65) +
  geom_errorbar(aes(ymin = boot_ci_l, ymax = boot_ci_u),
                position = position_dodge(0.8),
                width = 0.2,
                color = "red3") +
  facet_grid(Pond ~ region) +
  scale_fill_manual(values = c("EHCVM" = "gray50", "RDD" = "gray80", "DHS" = "black"),
                    name = "Data source") +
  labs(y = "Under-five mortality (5q0)", x = "Period",
       caption = "Bootstrap confidence intervals (1,000 replications)") +
  theme_minimal()
print(p_boot)

# Save bootstrap plot
ggsave("Fig4_bootstrap_20260205.png", p_boot, width = 14, height = 8, dpi = 300)

# ==============================
# h. CREATE COMPARISON TABLE (ANALYTICAL vs BOOTSTRAP)
# ==============================
results_table <- results_final %>%
  mutate(
    analytical = sprintf("%.3f [%.3f; %.3f]", est, ci_l, ci_u),
    bootstrap  = sprintf("%.3f [%.3f; %.3f]", boot_est, boot_ci_l, boot_ci_u)
  ) %>%
  select(Pond, type, region, period, analytical, bootstrap) %>%
  arrange(Pond, region, type, period)

# ==============================
# i. EXPORT TABLE TO WORD
# ==============================
ft <- flextable(results_table) %>%
  merge_v(j = c("Pond", "type", "region")) %>%
  valign(j = c("Pond", "type", "region"), valign = "center") %>%
  align(j = c("Pond", "type", "region"), align = "center", part = "all") %>%
  align(j = "period", align = "left", part = "all") %>%
  align(j = c("analytical", "bootstrap"), align = "center", part = "all") %>%
  set_header_labels(
    Pond       = "Weighting",
    type       = "Source",
    region     = "Region",
    period     = "Period",
    analytical = "Analytical estimate (95% CI)",
    bootstrap  = "Bootstrap estimate (95% CI)"
  ) %>%
  bold(part = "header") %>%
  theme_booktabs() %>%
  autofit() %>%
  padding(padding = 6, part = "all") %>%
  set_caption(
    caption = "Comparison of analytical and bootstrap under-five mortality estimates by weighting method, data source, region, and period"
  )

# Export Word document
doc <- read_docx() %>%
  body_add_par(
    "Table X. Under-five mortality estimates by source, region, period, and weighting method",
    style = "heading 1"
  ) %>%
  body_add_flextable(ft)

print(doc, target = "Table_SI.1.1_bootstrap_improved.docx")


# Two periods ------------------------------------------------------------

# ==============================
# a. DEFINE ANALYSIS PERIODS
# ==============================
periods <- list(
  "2016–2019" = c(2016, 2020),
  "2020–2022" = c(2020, 2023)
)

# ==============================
# b. DEFINE BOOTSTRAP FUNCTION
# ==============================
bootstrap_nqx <- function(data, by, period, B = 1000, seed = 123, agegr = seq(0, 5)) {
  
  set.seed(seed)
  n <- nrow(data)
  
  # Run bootstrap
  boot_est <- replicate(B, {
    
    # Resample rows with replacement weighted by survey weight
    idx <- sample(seq_len(n), size = n, replace = TRUE, prob = data$weight)
    data_b <- data[idx, ]
    
    # Compute under-five mortality
    res <- calc_nqx(
      data_b,
      by = by,
      strata = NULL,
      period = period,
      agegr = agegr,
      cohort = NULL,
      dob = "dob",
      dod = "dod",
      death = "death",
      weight = "weight",
      intv = "v008",
      tips = NULL,
      origin = 1900,
      scale = 12
    )
    
    res$est
  })
  
  # Summarize bootstrap results
  tibble(
    est  = median(boot_est, na.rm = TRUE),
    se   = sd(boot_est, na.rm = TRUE),
    ci_l = quantile(boot_est, 0.025, na.rm = TRUE),
    ci_u = quantile(boot_est, 0.975, na.rm = TRUE)
  )
}

# ==============================
# c. LOAD STANDARD ESTIMATES
# ==============================
results_std <- results_std_p2_t2_r1  # previously computed analytical estimates

# ==============================
# d. RUN BOOTSTRAP ESTIMATES
# ==============================
results_boot <- lapply(names(periods), function(p_name) {
  
  p_range <- periods[[p_name]]
  
  data %>%
    group_by(Pond, type, region) %>%
    group_modify(~ bootstrap_nqx(
      data = .x,
      by = ~1,        # no internal stratification
      period = p_range,
      B = 1000
    )) %>%
    ungroup() %>%
    mutate(period = p_name)
  
}) %>%
  bind_rows() %>%
  rename(
    boot_est  = est,
    boot_se   = se,
    boot_ci_l = ci_l,
    boot_ci_u = ci_u
  ) %>%
  mutate(method = "bootstrap")

# ==============================
# e. MERGE STANDARD AND BOOTSTRAP ESTIMATES
# ==============================
results_final <- results_std %>%
  mutate(method = "Analytical") %>%
  left_join(results_boot, by = c("Pond", "type", "region", "period")) %>%
  mutate(
    period = factor(period, levels = names(periods)),
    Pond   = factor(Pond, levels = c("PS", "IPW")),
    region = factor(region, levels = c("Conflict", "Non conflict"))
  )

# ==============================
# f. CREATE COMPARISON TABLE
# ==============================
results_table <- results_final %>%
  mutate(
    analytical = sprintf("%.3f [%.3f; %.3f]", est, ci_l, ci_u),
    bootstrap  = sprintf("%.3f [%.3f; %.3f]", boot_est, boot_ci_l, boot_ci_u)
  ) %>%
  select(Pond, type, region, period, analytical, bootstrap) %>%
  arrange(Pond, region, type, period)

# ==============================
# g. EXPORT WORD TABLE
# ==============================
ft <- flextable(results_table) %>%
  merge_v(j = c("Pond", "type", "region")) %>%
  valign(j = c("Pond", "type", "region"), valign = "center") %>%
  align(j = c("Pond", "type", "region"), align = "center", part = "all") %>%
  align(j = "period", align = "left", part = "all") %>%
  align(j = c("analytical", "bootstrap"), align = "center", part = "all") %>%
  set_header_labels(
    Pond       = "Weighting",
    type       = "Source",
    region     = "Region",
    period     = "Period",
    analytical = "Analytical estimate (95% CI)",
    bootstrap  = "Bootstrap estimate (95% CI)"
  ) %>%
  bold(part = "header") %>%
  theme_booktabs() %>%
  autofit() %>%
  padding(padding = 6, part = "all") %>%
  set_caption(
    caption = "Comparison of analytical and bootstrap under-five mortality estimates by weighting method, data source, region, and period"
  )

# Export to Word
doc <- read_docx() %>%
  body_add_par(
    "Table X. Under-five mortality estimates by source, region, period and weighting method",
    style = "heading 1"
  ) %>%
  body_add_flextable(ft)

print(doc, target = "Table_SI1.2_bstrap.docx")
 

 # Region: High, Moderate vs Non-conflict ----------------------------------------
 
 # Three periods ----------------------------------------------------------
 
############################################################
# PURPOSE: Compare analytical vs bootstrap estimates of
#          under-five mortality (5q0) for periods
#          2014–2017, 2018–2019, 2020–2022 by survey type,
#          region2, and weighting
############################################################

# ==============================
# a. DEFINE ANALYSIS PERIODS
# ==============================
periods <- list(
  "2014–2017" = c(2014, 2018),
  "2018–2019" = c(2018, 2020),
  "2020–2022" = c(2020, 2023)
)

# ==============================
# b. DEFINE BOOTSTRAP FUNCTION
# ==============================
bootstrap_nqx <- function(data, by, period, B = 1000, seed = 123, agegr = seq(0, 5)) {
  
  set.seed(seed)
  n <- nrow(data)
  
  boot_est <- replicate(B, {
    # Resample rows with replacement, weighted by survey weight
    idx <- sample(seq_len(n), size = n, replace = TRUE, prob = data$weight)
    data_b <- data[idx, ]
    
    # Compute under-five mortality (5q0)
    res <- calc_nqx(
      data_b,
      by = by,
      strata = NULL,
      period = period,
      agegr = agegr,
      cohort = NULL,
      dob = "dob",
      dod = "dod",
      death = "death",
      weight = "weight",
      intv = "v008",
      tips = NULL,
      origin = 1900,
      scale = 12
    )
    
    res$est
  })
  
  # Return median, SD, and 95% CI of bootstrap estimates
  tibble(
    est  = median(boot_est, na.rm = TRUE),
    se   = sd(boot_est, na.rm = TRUE),
    ci_l = quantile(boot_est, 0.025, na.rm = TRUE),
    ci_u = quantile(boot_est, 0.975, na.rm = TRUE)
  )
}

# ==============================
# c. LOAD STANDARD ESTIMATES
# ==============================
results_std <- results_std_p1_t2_r2  # previously computed analytical estimates

# ==============================
# d. RUN BOOTSTRAP ESTIMATES
# ==============================
results_boot <- lapply(names(periods), function(p_name) {
  
  p_range <- periods[[p_name]]
  
  data %>%
    group_by(Pond, type, region2) %>%
    group_modify(~ bootstrap_nqx(
      data = .x,
      by = ~1,        # no internal stratification
      period = p_range,
      B = 1000
    )) %>%
    ungroup() %>%
    mutate(period = p_name)
  
}) %>%
  bind_rows() %>%
  rename(
    boot_est  = est,
    boot_se   = se,
    boot_ci_l = ci_l,
    boot_ci_u = ci_u
  ) %>%
  mutate(method = "bootstrap")

# ==============================
#e. MERGE STANDARD AND BOOTSTRAP ESTIMATES
# ==============================
results_final <- results_std %>%
  mutate(method = "Analytical") %>%
  left_join(results_boot, by = c("Pond", "type", "region2", "period")) %>%
  mutate(
    period = factor(period, levels = names(periods)),
    Pond   = factor(Pond, levels = c("PS", "IPW")),
    region2 = factor(region2, levels = c("High conflict", "Moderate conflict", "Non conflict"))
  )

# ==============================
# f. PREPARE FORMATTED TABLE
# ==============================
results_fmt <- results_final %>%
  mutate(
    analytical = sprintf("%.3f [%.3f; %.3f]", est, ci_l, ci_u),
    bootstrap  = sprintf("%.3f [%.3f; %.3f]", boot_est, boot_ci_l, boot_ci_u)
  ) %>%
  select(Pond, type, region2, period, analytical, bootstrap) %>%
  arrange(Pond, type, region2, period)

# ==============================
# g. CREATE WORD TABLE
# ==============================
ft <- flextable(results_fmt) %>%
  merge_v(j = c("Pond", "type", "region2")) %>%
  valign(j = c("Pond", "type", "region2"), valign = "center") %>%
  align(j = c("Pond", "type", "region2"), align = "center", part = "all") %>%
  align(j = "period", align = "left", part = "all") %>%
  align(j = c("analytical", "bootstrap"), align = "center", part = "all") %>%
  set_header_labels(
    Pond       = "Weighting",
    type       = "Source",
    region2    = "Region",
    period     = "Period",
    analytical = "Analytical estimate (95% CI)",
    bootstrap  = "Bootstrap estimate (95% CI)"
  ) %>%
  bold(part = "header") %>%
  theme_booktabs() %>%
  autofit() %>%
  padding(padding = 6, part = "all") %>%
  set_caption(
    caption = "Comparison of analytical and bootstrap under-five mortality estimates by weighting method, data source, region2, and period"
  )

# ==============================
# h. EXPORT TO WORD
# ==============================
doc <- read_docx() %>%
  body_add_par(
    "Table X. Under-five mortality estimates by source, region2, period and weighting method",
    style = "heading 1"
  ) %>%
  body_add_flextable(ft)

print(doc, target = "Table_SI1.3.docx")

 
 # Two périods ------------------------------------------------------------

############################################################
# PURPOSE: Compare analytical vs bootstrap estimates of
#          under-five mortality (5q0) for periods
#          2016–2019, 2020–2022 by survey type, region2,
#          and weighting
############################################################

# ==============================
# a. DEFINE ANALYSIS PERIODS
# ==============================
periods <- list(
  "2016–2019" = c(2016, 2020),
  "2020–2022" = c(2020, 2023)
)

# ==============================
# b. DEFINE BOOTSTRAP FUNCTION
# ==============================
bootstrap_nqx <- function(data, by, period, B = 1000, seed = 123, agegr = seq(0, 5)) {
  
  set.seed(seed)
  n <- nrow(data)
  
  # Replicate bootstrap B times
  boot_est <- replicate(B, {
    # Weighted resampling of rows
    idx <- sample(seq_len(n), size = n, replace = TRUE, prob = data$weight)
    data_b <- data[idx, ]
    
    # Compute under-five mortality (5q0)
    res <- calc_nqx(
      data_b,
      by = by,
      strata = NULL,
      period = period,
      agegr = agegr,
      cohort = NULL,
      dob = "dob",
      dod = "dod",
      death = "death",
      weight = "weight",
      intv = "v008",
      tips = NULL,
      origin = 1900,
      scale = 12
    )
    
    res$est
  })
  
  # Return median, SD, and 95% CI
  tibble(
    est  = median(boot_est, na.rm = TRUE),
    se   = sd(boot_est, na.rm = TRUE),
    ci_l = quantile(boot_est, 0.025, na.rm = TRUE),
    ci_u = quantile(boot_est, 0.975, na.rm = TRUE)
  )
}

# ==============================
# c. LOAD STANDARD ESTIMATES
# ==============================
results_std <- results_std_p2_t2_r2  # previously computed analytical estimates

# ==============================
# d. RUN BOOTSTRAP ESTIMATES
# ==============================
results_boot <- lapply(names(periods), function(p_name) {
  
  p_range <- periods[[p_name]]
  
  data %>%
    group_by(Pond, type, region2) %>%
    group_modify(~ bootstrap_nqx(
      data = .x,
      by = ~1,        # no internal stratification
      period = p_range,
      B = 1000
    )) %>%
    ungroup() %>%
    mutate(period = p_name)
  
}) %>%
  bind_rows() %>%
  rename(
    boot_est  = est,
    boot_se   = se,
    boot_ci_l = ci_l,
    boot_ci_u = ci_u
  ) %>%
  mutate(method = "bootstrap")

# ==============================
# e. MERGE STANDARD AND BOOTSTRAP ESTIMATES
# ==============================
results_final <- results_std %>%
  mutate(method = "Analytical") %>%
  left_join(results_boot, by = c("Pond", "type", "region2", "period")) %>%
  mutate(
    period = factor(period, levels = names(periods)),
    Pond   = factor(Pond, levels = c("PS", "IPW")),
    region2 = factor(region2, levels = c("High conflict", "Moderate conflict", "Non conflict"))
  )

# ==============================
# f. PREPARE FORMATTED TABLE
# ==============================
results_fmt <- results_final %>%
  mutate(
    analytical = sprintf("%.3f [%.3f; %.3f]", est, ci_l, ci_u),
    bootstrap  = sprintf("%.3f [%.3f; %.3f]", boot_est, boot_ci_l, boot_ci_u)
  ) %>%
  select(Pond, type, region2, period, analytical, bootstrap) %>%
  arrange(Pond, type, region2, period)

# ==============================
# g. CREATE WORD TABLE
# ==============================
ft <- flextable(results_fmt) %>%
  merge_v(j = c("Pond", "type", "region2")) %>%
  valign(j = c("Pond", "type", "region2"), valign = "center") %>%
  align(j = c("Pond", "type", "region2"), align = "center", part = "all") %>%
  align(j = "period", align = "left", part = "all") %>%
  align(j = c("analytical", "bootstrap"), align = "center", part = "all") %>%
  set_header_labels(
    Pond       = "Weighting",
    type       = "Source",
    region2    = "Region",
    period     = "Period",
    analytical = "Analytical estimate (95% CI)",
    bootstrap  = "Bootstrap estimate (95% CI)"
  ) %>%
  bold(part = "header") %>%
  theme_booktabs() %>%
  autofit() %>%
  padding(padding = 6, part = "all") %>%
  set_caption(
    caption = "Comparison of analytical and bootstrap under-five mortality estimates by weighting method, data source, region2, and period"
  )

# ==============================
# h. EXPORT TO WORD
# ==============================
doc <- read_docx() %>%
  body_add_par(
    "Table X. Under-five mortality estimates by source, region2, period and weighting method",
    style = "heading 1"
  ) %>%
  body_add_flextable(ft)

print(doc, target = "Table_SI1.4.docx")


############################################################
# PURPOSE: Robustness check - MPS 5q0 with/without excluded provinces
#          Regions: Sahel and East, period: 2020–2022
############################################################

# ==============================
# 1. SELECT AND RECODE REGIONS
# ==============================
sahel_est <- mps_dhs_select %>%
  filter(v024 %in% c(8, 12)) %>%
  mutate(v024 = recode_factor(v024,
                              `8` = "East",
                              `12` = "Sahel"),
         v024 = factor(v024))

# ==============================
# 2. CREATE MPS / DHS INDICATOR
# ==============================
sahel_est <- sahel_est %>%
  mutate(
    mps = if_else(type %in% c("RDD", "EHCVM"), 1, 0),
    mps = factor(mps, levels = c(1, 0), labels = c("MPS", "DHS"))
  )

# ==============================
# 3. CALCULATE 5q0
# ==============================
# Only MPS, separate clusters
result3 <- calc_nqx(
  sahel_est %>% filter(mps == "MPS"),
  by = ~v024 + mps + grappe,
  strata = NULL,
  period = c(2020, 2022),
  agegr = seq(0,5),
  dob = "dob", dod = "dod", death = "death",
  weight = "weight", intv = "v008", origin = 1900, scale = 12
)

# MPS all combined clusters (for comparison)
result_inc <- calc_nqx(
  sahel_est,
  by = ~v024 + mps,
  strata = NULL,
  period = c(2020, 2022),
  agegr = seq(0,5),
  dob = "dob", dod = "dod", death = "death",
  weight = "weight", intv = "v008", origin = 1900, scale = 12
)
result_inc$grappe <- "Both"
result3 <- bind_rows(result3, result_inc)

# Combine MPS/DHS label
result3 <- result3 %>%
  mutate(source = paste(mps, grappe, sep = "-"),
         source = factor(source,
                         levels = c("MPS-Excluded", "MPS-Both", "MPS-Included", "DHS-Both"),
                         labels = c("MPS-Excluded", "MPS", "MPS-Included", "DHS")))

# ==============================
# 4. PLOT 5q0 (manual colors)
# ==============================
manual_colors <- c(
  "DHS"          = "black",
  "MPS-Excluded" = "#b0b0b0",  # light gray
  "MPS-Included" = "#4f4f4f"   # dark gray
)

# Exclude intermediate "MPS" if needed
result3_filtered <- result3 %>% filter(source != "MPS")

combined_plot <- ggplot(result3_filtered, aes(x = v024, y = est, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.65) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.15,
                linewidth = 0.6,
                position = position_dodge(width = 0.8),
                color = "blue2") +
  labs(
    x = "Region",
    y = "Probability of dying between age 0 and 5 (5q0)",
    fill = "Data source"
  ) +
  scale_fill_manual(values = manual_colors) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 8, hjust = 1),
    axis.text.y = element_text(angle = 8, hjust = 1, size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold", size = 10)
  )

ggsave("Figure5.png", combined_plot, width = 14, height = 7, dpi = 300)

# ==============================
# 5. CALCULATE PERSON-YEARS
# ==============================
data_py <- sahel_est %>%
  filter(mps == "MPS") %>%
  mutate(
    end = ifelse(!is.na(dod), dod, v008),
    start = dob,
    end = pmin(end, dob + 60),   # limit to 5 years
    py = pmax((end - start) / 12, 0),  # exposure in years
    death_u5 = ifelse(!is.na(dod) & (dod - dob) <= 60, 1, 0)
  )

results_py <- data_py %>%
  group_by(mps, grappe) %>%
  summarise(
    deaths = sum(death_u5 * weight, na.rm = TRUE),
    person_years = sum(py * weight, na.rm = TRUE),
    rate = deaths / person_years,
    .groups = "drop"
  ) %>%
  mutate(
    rate_1000 = round(rate * 1000, 2),
    deaths = round(deaths, 1),
    person_years = round(person_years, 1)
  )

# Approximate 5q0 from rate
results_py <- results_py %>%
  mutate(
    q5_approx = round(1 - exp(-5 * rate), 3)
  )

# ==============================
# 6. CREATE WORD TABLE
# ==============================
table_py <- results_py %>%
  rename(
    Survey = mps,
    Cluster = grappe,
    Deaths = deaths,
    `Person-years` = person_years,
    `Mortality rate (per 1,000)` = rate_1000
  )

ft <- flextable(table_py) %>%
  autofit() %>%
  theme_booktabs() %>%
  align(align = "center", part = "all") %>%
  bold(part = "header")

doc <- read_docx() %>%
  body_add_par(
    "Table SI-X. Under-five mortality based on person-years of exposure (MPS sample, 2020–2022)",
    style = "heading 2"
  ) %>%
  body_add_flextable(ft) %>%
  body_add_par(
    "Deaths and person-years are weighted. Mortality rates are expressed per 1,000 person-years.",
    style = "Normal"
  )

print(doc, target = "Table_person_years_MPS_both.docx")


############################################################
# PURPOSE: Age pattern of under-five mortality (1q4 / 4q1)
#          Figures SI4.1 and SI4.2
############################################################

# ==============================
# 1. CALCUL DES ESTIMATIONS PAR PÉRIODE, SOURCE ET RÉGION
# ==============================
results <- lapply(names(periods), function(period_name) {
  period <- periods[[period_name]]
  calc_nqx(
    data,
    by = ~type_recode + region,  # Figure SI4.1: type recodé
    strata = NULL,
    agegr = seq(1, 5),           # 1q4 = mortality between 1-5 years
    period = period,
    cohort = NULL,
    dob = "dob", dod = "dod", death = "death",
    weight = "weight", intv = "v008", tips = NULL,
    origin = 1900, scale = 12
  ) %>% mutate(period = period_name)
}) %>% bind_rows()

# ==============================
# 2. AJOUTER LES DIFFÉRENCES RELATIVES (RD)
# ==============================
RD_df <- tribble(
  ~region,        ~period,       ~RD,
  "Conflict",     "2014-2017",   -31.8,
  "Non conflict", "2014-2017",   -75.2,
  "Conflict",     "2018-2019",   -33.9,
  "Non conflict", "2018-2019",   -11.7,
  "Conflict",     "2020-2022",   124.6,
  "Non conflict", "2020-2022",   26.3
)

resultsRD <- results %>%
  left_join(RD_df, by = c("region", "period"))

# ==============================
# 3. FONCTION DE PLOTTING PAR RÉGION
# ==============================
plot_by_region <- function(region_name) {
  
  df_plot <- resultsRD %>% filter(region == region_name)
  
  ggplot(df_plot, aes(x = period, y = est, fill = type_recode)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
    geom_errorbar(
      aes(ymin = ci_lower, ymax = ci_upper),
      width = 0.2,
      position = position_dodge(width = 0.9),
      color = "blue2"
    ) +
    # Ajouter les RD
    geom_text(
      data = distinct(df_plot, region, period, RD),
      inherit.aes = FALSE,
      aes(
        x = period,
        y = max(df_plot$ci_upper) * 1.05,
        label = paste0("RD = ", RD)
      ),
      color = "black",
      fontface = "bold",
      size = 4
    ) +
    scale_fill_manual(values = c("MPS" = "gray50", "DHS" = "black"), name = "Data source") +
    ylim(0.0, 0.12) +
    labs(x = "Period", y = "4q1", title = paste(region_name)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      legend.position = "bottom"
    )
}

# ==============================
# 4. GENERERTE GRAPHIC SI4.1
# ==============================
conflict_plot <- plot_by_region("Conflict")
non_conflict_plot <- plot_by_region("Non conflict")

combined_plot_SI4.1 <- conflict_plot + non_conflict_plot +
  plot_annotation(caption = "Source: RaMMPS-2021/22 and DHS-2021") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("Figure_SI4.1.png", combined_plot_SI4.1, width = 14, height = 7, dpi = 300)

# ==============================
# 5. FIGURE SI4.2 
# ==============================
results2 <- lapply(names(periods), function(period_name) {
  period <- periods[[period_name]]
  calc_nqx(
    data,
    by = ~type + region,
    strata = NULL,
    agegr = seq(1, 5),
    period = period,
    cohort = NULL,
    dob = "dob", dod = "dod", death = "death",
    weight = "weight", intv = "v008", tips = NULL,
    origin = 1900, scale = 12
  ) %>% mutate(period = period_name)
}) %>% bind_rows()

plot_by_region2 <- function(region_name) {
  ggplot(results2 %>% filter(region == region_name),
         aes(x = period, y = est, fill = type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 0.2, position = position_dodge(width = 0.9),
                  color = "blue2") +
    scale_fill_manual(values = c("EHCVM" = "gray50", "RDD" = "gray80", "DHS" = "black"),
                      name = "Data source") +
    ylim(0.0, 0.10) +
    labs(x = "Period", y = "4q1", title = paste(region_name)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          legend.position = "bottom")
}

conflict_plot2 <- plot_by_region2("Conflict")
non_conflict_plot2 <- plot_by_region2("Non conflict")

combined_plot_SI4.2 <- conflict_plot2 + non_conflict_plot2 +
  plot_annotation(caption = "Source: RaMMPS-2021/22 and DHS-2021") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("Figure_SI4.2.png", combined_plot_SI4.2, width = 14, height = 7, dpi = 300)

############################################################
# PURPOSE: Infant mortality (1q0) by period, data source, and security context
#          Figures SI4.3 and SI4.4
############################################################

# ==============================
# 1. CALCULTE 1q0 
# ==============================
results <- lapply(names(periods), function(period_name) {
  period <- periods[[period_name]]
  calc_nqx(
    data,
    by = ~type_recode + region,  # type recodé pour SI4.3
    strata = NULL,
    agegr = seq(0, 1),           # 1q0 = mortality between 0-1 year
    period = period,
    cohort = NULL,
    dob = "dob", dod = "dod", death = "death",
    weight = "weight", intv = "v008", tips = NULL,
    origin = 1900, scale = 12
  ) %>% mutate(period = period_name)
}) %>% bind_rows()

# ==============================
# 2. ADD RELATIVES DIFFERENCES (RD)
# ==============================
RD_table <- tribble(
  ~region,        ~period,        ~RD,
  "Conflict",     "2014-2017",    32.6,
  "Non conflict", "2014-2017",    31.7,
  "Conflict",     "2018-2019",    6.9,
  "Non conflict", "2018-2019",    6.6,
  "Conflict",     "2020-2022",    60.3,
  "Non conflict", "2020-2022",    33.7
)

resultsRD <- results %>%
  left_join(RD_table, by = c("region", "period"))

# ==============================
# 3. FUNCTION OF PLOTTING BY REGION (SI4.3)
# ==============================
plot_by_region <- function(region_name) {
  
  df_plot <- resultsRD %>% filter(region == region_name)
  
  ggplot(df_plot, aes(x = period, y = est, fill = type_recode)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 0.2,
                  position = position_dodge(width = 0.9),
                  color = "blue2") +
    geom_text(data = distinct(df_plot, region, period, RD),
              inherit.aes = FALSE,
              aes(x = period, y = max(df_plot$ci_upper) * 1.05, label = paste0("RD = ", RD)),
              color = "black", fontface = "bold", size = 4) +
    scale_fill_manual(values = c("MPS" = "gray50", "DHS" = "black"),
                      name = "Data source") +
    ylim(0.0, 0.12) +
    labs(x = "Period", y = "1q0", title = paste(region_name)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          legend.position = "bottom")
}

# Générer les graphiques SI4.3
conflict_plot <- plot_by_region("Conflict")
non_conflict_plot <- plot_by_region("Non conflict")

combined_plot_SI4.3 <- conflict_plot + non_conflict_plot +
  plot_annotation(caption = "Source: RaMMPS-2021/22 and DHS-2021") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("Figure_SI4.3.png", combined_plot_SI4.3, width = 14, height = 7, dpi = 300)

# ==============================
# 4. CALCULATE 1q0 (type original)
# ==============================
results2 <- lapply(names(periods), function(period_name) {
  period <- periods[[period_name]]
  calc_nqx(
    data,
    by = ~type + region,         # type original pour SI4.4
    strata = NULL,
    agegr = seq(0, 1),
    period = period,
    cohort = NULL,
    dob = "dob", dod = "dod", death = "death",
    weight = "weight", intv = "v008", tips = NULL,
    origin = 1900, scale = 12
  ) %>% mutate(period = period_name)
}) %>% bind_rows()

# ==============================
# 5. FUNCTION OF PLOTTING BY REGION (SI4.4)
# ==============================
plot_by_region2 <- function(region_name) {
  ggplot(results2 %>% filter(region == region_name),
         aes(x = period, y = est, fill = type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 0.2, position = position_dodge(width = 0.9),
                  color = "blue2") +
    scale_fill_manual(values = c("EHCVM" = "gray50", "RDD" = "gray80", "DHS" = "black"),
                      name = "Data source") +
    ylim(0.0, 0.10) +
    labs(x = "Period", y = "1q0", title = paste(region_name)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          legend.position = "bottom")
}

# Générer les graphiques SI4.4
conflict_plot2 <- plot_by_region2("Conflict")
non_conflict_plot2 <- plot_by_region2("Non conflict")

combined_plot_SI4.4 <- conflict_plot2 + non_conflict_plot2 +
  plot_annotation(caption = "Source: RaMMPS-2021/22 and DHS-2021") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("Figure_SI4.4.png", combined_plot_SI4.4, width = 14, height = 7, dpi = 300)
