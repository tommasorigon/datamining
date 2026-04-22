# ----------------------------------------
# Title: LAB 1 (Ames Housing, pre-processing)
# Author: Tommaso Rigon
# ----------------------------------------

rm(list = ls())

library(tidyverse) # forcats is included in tidyverse

# Load data
# Official paper: https://jse.amstat.org/v19n3/decock.pdf
# Variable description: https://tommasorigon.github.io/datamining/data/ames_documentation.txt
ames <- read.csv("../data/AmesHousing.csv")

glimpse(ames)

# ----------------------------------------
# 1. Basic filtering
# ----------------------------------------

# Keep only residential zones and normal sale conditions
ames <- ames %>%
  filter(
    !MS.Zoning %in% c("C (all)", "I (all)", "A (agr)", "FV"),
    Sale.Condition == "Normal"
  ) %>%
  select(-Sale.Condition, -Sale.Type, -Order, -PID)

# ----------------------------------------
# 2. Response variable
# ----------------------------------------

summary(ames$SalePrice)

par(mfrow = c(1, 2))
hist(ames$SalePrice, main = "SalePrice")
hist(log(ames$SalePrice), main = "log(SalePrice)")

# ----------------------------------------
# 3. Missing values
# ----------------------------------------

# Count NAs per column; show only columns with at least one missing value
ames %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  select(where(~ .x > 0))

# In the documentation, NA for Alley means "no alley access" — not a true missing value
table(ames$Alley, useNA = "always")

# Categorical variables: NA encodes absence of a feature
ames <- ames %>%
  mutate(
    Alley         = replace_na(Alley, "No alley"),
    Fence         = replace_na(Fence, "No fence"),
    Fireplace.Qu  = replace_na(Fireplace.Qu, "No fireplace"),
    Bsmt.Exposure = ifelse(Bsmt.Exposure == "" | is.na(Bsmt.Exposure), "No basement", Bsmt.Exposure),
    Bsmt.Cond     = replace_na(Bsmt.Cond, "No basement"),
    Bsmt.Qual     = replace_na(Bsmt.Qual, "No basement")
  )

# Drop detailed basement finish variables to reduce complexity
ames <- ames %>% select(-starts_with("BsmtFin"))

# Garage: if any garage variable is missing, the house has no garage
garage_vars <- names(ames)[startsWith(names(ames), "Garage")]
no_garage_rows <- ames %>%
  select(all_of(garage_vars)) %>%
  apply(1, anyNA)

ames <- ames %>%
  mutate(across(all_of(garage_vars), ~ ifelse(no_garage_rows, "No garage", .x)))

# Garage.Yr.Blt is dropped: numeric but structurally missing for houses without a garage
ames <- ames %>%
  select(-Garage.Yr.Blt)

# Numeric variables: NA encodes zero (no feature present)
ames <- ames %>%
  mutate(
    Bsmt.Full.Bath = replace_na(Bsmt.Full.Bath, 0),
    Bsmt.Half.Bath = replace_na(Bsmt.Half.Bath, 0),
    Lot.Frontage   = replace_na(Lot.Frontage, 0),
    Mas.Vnr.Area   = replace_na(Mas.Vnr.Area, 0)
  )

# Remaining fixes: empty strings treated as the dominant / "none" category
ames <- ames %>%
  mutate(
    Electrical   = ifelse(Electrical == "", "SBrkr", Electrical),
    Mas.Vnr.Type = ifelse(Mas.Vnr.Type == "", "None", Mas.Vnr.Type),
    Misc.Feature = replace_na(Misc.Feature, "None"),
    Pool.QC      = ifelse(is.na(Pool.QC), "No", "Yes")
  )

# ----------------------------------------
# 4. Reduce categorical complexity
# ----------------------------------------

# MS.SubClass is a numeric code representing a dwelling type — treat as character
ames <- ames %>%
  mutate(MS.SubClass = as.character(MS.SubClass))

# Lump neighbourhoods with fewer than 20 observations into "Other"
ames <- ames %>%
  mutate(Neighborhood = fct_lump_min(Neighborhood, 20))

# For all remaining character columns, lump infrequent levels into "Other"
ames <- ames %>%
  mutate(across(where(is.character), ~ fct_lump_lowfreq(.x)))

# ----------------------------------------
# 5. Feature engineering
# ----------------------------------------

ames <- ames %>%
  mutate(
    Porch.SF  = Open.Porch.SF + Enclosed.Porch + X3Ssn.Porch + Screen.Porch,
    Tot.Bath  = Full.Bath + 0.5 * Half.Bath + Bsmt.Full.Bath + 0.5 * Bsmt.Half.Bath,
    House.Age = Yr.Sold - Year.Remod.Add
  )

# Remove the original variables that were combined or are no longer needed
ames <- ames %>%
  select(
    -c(Open.Porch.SF, Enclosed.Porch, X3Ssn.Porch, Screen.Porch),
    -c(Full.Bath, Half.Bath, Bsmt.Full.Bath, Bsmt.Half.Bath),
    -c(Mo.Sold, Yr.Sold, Year.Remod.Add, Year.Built),
    -c(Garage.Area)
  )

# ----------------------------------------
# 6. Save cleaned dataset
# ----------------------------------------

write.csv(ames, "../data/ames.csv")
