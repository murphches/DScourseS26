library(jsonlite)
library(readxl)
library(httr)
library(tidyverse)
library(dplyr)
library(rvest)
library(here)
library(janitor)
library(ggplot2)
library(fixest)

ghg_data <- read_excel(
                path = "/Users/murphychesley/Desktop/Data Science for Economists/Final Project/Data and R Files/AllStateGHGData90-22_v082924.xlsx",
                sheet = "Data by Economic Sectors",
                col_names = TRUE
)

power_sector <- ghg_data %>%
  filter(econ_sector == "Electric Power Industry")

rggi_core <- c("CT", "DE", "ME", "MD", "MA", "NH", "NY", "RI", "VT")

power_sector_nj <- power_sector %>%
  filter(geo_ref %in% c(rggi_core, "NJ"))

#Build totals with all relevant years through 2022
power_sector_nj_totals <- power_sector_nj %>%
  select(geo_ref, Y2000, Y2003, Y2006, Y2009, Y2012, Y2015, Y2018, Y2020, Y2022) %>%
  group_by(geo_ref) %>%
  summarise(
    total_2000 = sum(Y2000, na.rm = TRUE),
    total_2003 = sum(Y2003, na.rm = TRUE),
    total_2006 = sum(Y2006, na.rm = TRUE),
    total_2009 = sum(Y2009, na.rm = TRUE),
    total_2012 = sum(Y2012, na.rm = TRUE),
    total_2015 = sum(Y2015, na.rm = TRUE),
    total_2018 = sum(Y2018, na.rm = TRUE),
    total_2020 = sum(Y2020, na.rm = TRUE),
    total_2022 = sum(Y2022, na.rm = TRUE)
  )

#Pivot long and build treatment indicators
power_sector_nj_long <- power_sector_nj_totals %>%
  pivot_longer(
    cols      = starts_with("total_"),
    names_to  = "year",
    values_to = "emissions_tg"
  ) %>%
  mutate(
    year = as.integer(str_remove(year, "total_")),
    NJ_exit = case_when(
      geo_ref == "NJ" & year >= 2012 ~ 1,
      TRUE                           ~ 0
    ),
    NJ_reentry = case_when(
      geo_ref == "NJ" & year >= 2020 ~ 1,
      TRUE                           ~ 0
    ),
    is_NJ = ifelse(geo_ref == "NJ", 1, 0)
  )

# Step 3: Rerun regressions
did2_nj <- feols(
  emissions_tg ~ NJ_exit | geo_ref + year,
  data    = power_sector_nj_long %>% filter(year >= 2009),
  cluster = ~geo_ref
)

did3_nj <- feols(
  emissions_tg ~ NJ_reentry | geo_ref + year,
  data    = power_sector_nj_long %>% filter(year >= 2012),
  cluster = ~geo_ref
)

did_unified_emissions <- feols(
  emissions_tg ~ NJ_exit + NJ_reentry | geo_ref + year,
  data    = power_sector_nj_long %>% filter(year >= 2009),
  cluster = ~geo_ref
)

etable(did2_nj, did3_nj, did_unified_emissions,
       headers  = c("DiD 2: NJ Withdrawal",
                    "DiD 3: NJ Reentry",
                    "Unified"),
       notes    = "Control group: core RGGI states (CT, DE, ME, MD, MA,
                   NH, NY, RI, VT). Clustered standard errors at state
                   level. Emissions in teragrams CO2 equivalent.",
       se.below = TRUE)

#Plot through 2022
power_sector_nj_long %>%
  mutate(group = ifelse(geo_ref == "NJ", "New Jersey", "RGGI Core")) %>%
  group_by(group, year) %>%
  summarise(mean_emissions = mean(emissions_tg), .groups = "drop") %>%
  ggplot(aes(x = year, y = mean_emissions, color = group, group = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2009, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "blue") +
  annotate("text", x = 2009, y = Inf, label = "RGGI launch",
           hjust = 1.1, vjust = 1.1, size = 3.5, color = "gray40") +
  annotate("text", x = 2012, y = Inf, label = "NJ exits",
           hjust = -0.1, vjust = 3.5, size = 3.5, color = "red") +
  annotate("text", x = 2020, y = Inf, label = "NJ reentry",
           hjust = -0.1, vjust = 1.5, size = 3.5, color = "blue") +
  scale_x_continuous(breaks = c(2000, 2003, 2006, 2009, 
                                2012, 2015, 2018, 2020, 2022)) +
  labs(
    title    = "Power Sector CO2 Emissions: NJ vs. Core RGGI States",
    subtitle = "Dashed lines mark RGGI policy events (2000–2022)",
    x        = "Year",
    y        = "Mean Emissions (Tg CO2)",
    color    = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

-------------------------------------------------------------------------------
library(haven)

load_and_collapse_brfss <- function(xpt_path, year) {
    
    cat("Loading", year, "...\n")
    
    df <- read_xpt(xpt_path)
    
    if ("_CASTHM1" %in% names(df)) {
      df <- df %>%
        filter(`_CASTHM1` %in% c(1, 2)) %>%
        mutate(current_asthma = ifelse(`_CASTHM1` == 2, 1, 0))
      
    } else if ("_CASTHMA" %in% names(df)) {
      df <- df %>%
        filter(`_CASTHMA` %in% c(1, 2)) %>%
        mutate(current_asthma = ifelse(`_CASTHMA` == 2, 1, 0))
      
    } else {
      df <- df %>%
        filter(ASTHMA %in% c(1, 2)) %>%
        mutate(current_asthma = ifelse(ASTHMA == 1 & ASTHNOW == 1, 1, 0))
    }
    
    df %>%
      mutate(
        state_fips = as.character(`_STATE`),
        year       = year
      ) %>%
      group_by(state_fips, year) %>%
      summarise(
        current_asthma_pct = mean(current_asthma, na.rm = TRUE) * 100,
        n_adults           = n(),
        .groups            = "drop"
      )
  }
# Run across all years
brfss_panel <- bind_rows(
  load_and_collapse_brfss("BRFS Data/LLCP2000.XPT", 2000),
  load_and_collapse_brfss("BRFS Data/LLCP2003.XPT", 2003),
  load_and_collapse_brfss("BRFS Data/LLCP2006.XPT", 2006),
  load_and_collapse_brfss("BRFS Data/LLCP2009.XPT", 2009),
  load_and_collapse_brfss("BRFS Data/LLCP2012.XPT", 2012),
  load_and_collapse_brfss("BRFS Data/LLCP2015.XPT", 2015),
  load_and_collapse_brfss("BRFS Data/LLCP2018.XPT", 2018),
  load_and_collapse_brfss("BRFS Data/LLCP2020.XPT", 2020),
  load_and_collapse_brfss("BRFS Data/LLCP2022.XPT", 2022)
)

# Filter to RGGI states and pivot wide
fips_crosswalk <- tibble(
  state_fips = c("9", "10", "23", "24", "25", "33", "34", "36", "44", "50"),
  geo_ref    = c("CT", "DE", "ME", "MD", "MA", "NH", "NJ", "NY", "RI", "VT")
)

brfss_rggi <- brfss_panel %>%
  left_join(fips_crosswalk, by = "state_fips") %>%
  filter(!is.na(geo_ref))

brfss_wide <- brfss_rggi %>%
  select(geo_ref, year, current_asthma_pct) %>%
  pivot_wider(
    names_from   = year,
    values_from  = current_asthma_pct,
    names_prefix = "asthma_"
  ) %>%
  arrange(geo_ref)

did_unified_asthma <- feols(
  current_asthma_pct ~ NJ_exit + NJ_reentry | geo_ref + year,
  data    = analysis_panel %>% filter(year >= 2009),
  cluster = ~geo_ref
)

etable(did_unified_emissions, did_unified_asthma,
       headers  = c("CO2 Emissions (Tg)", "Asthma Prevalence (%)"),
       notes    = "Control group: core RGGI states (CT, DE, ME, MD, MA,
                   NH, NY, RI, VT). Clustered SEs at state level.
                   Sample restricted to 2009 onwards.",
       se.below = TRUE)

#Symmetry test
library(car)

cat("Symmetry test — Emissions:\n")
linearHypothesis(did_unified_emissions, "NJ_exit + NJ_reentry = 0")

cat("Symmetry test — Asthma:\n")
linearHypothesis(did_unified_asthma, "NJ_exit + NJ_reentry = 0")
-------------------------------------------------------------------------------
#Create the merged panel
analysis_panel <- power_sector_nj_long %>%
  left_join(brfss_rggi, by = c("geo_ref", "year"))

write_csv(analysis_panel,       "analysis_panel.csv")
write_csv(brfss_rggi,           "brfss_asthma_rggi_panel.csv")
write_csv(power_sector_nj_long, "emissions_panel.csv")

#Plot analysis panel
analysis_panel %>%
  mutate(group = ifelse(geo_ref == "NJ", "New Jersey", "RGGI Core")) %>%
  group_by(group, year) %>%
  summarise(
    mean_asthma = mean(current_asthma_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = year, y = mean_asthma, color = group, group = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2009, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = 2012, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "blue") +
  annotate("text", x = 2009, y = Inf, label = "RGGI launch",
           hjust = 1.1, vjust = 1.5, size = 3.5, color = "gray40") +
  annotate("text", x = 2012, y = Inf, label = "NJ exits",
           hjust = -0.1, vjust = 3.5, size = 3.5, color = "red") +
  annotate("text", x = 2020, y = Inf, label = "NJ reentry",
           hjust = -0.1, vjust = 1.5, size = 3.5, color = "blue") +
  scale_x_continuous(breaks = c(2000, 2003, 2006, 2009,
                                2012, 2015, 2018, 2020, 2022)) +
  scale_y_continuous(limits = c(6, 13),
                     breaks = seq(6, 13, by = 1),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Adult Asthma Prevalence: NJ vs. Core RGGI States",
    subtitle = "Dashed lines: RGGI launch (gray), NJ exit (red), NJ reentry (blue)",
    x        = "Year",
    y        = "Mean Current Asthma Prevalence",
    color    = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )