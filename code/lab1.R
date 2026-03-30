#' ---
#' title: "LAB 1 (Ames Housing, pre-processing)"
#' author: "Tommaso Rigon"
#' ---

rm(list = ls())

library(tidyverse)
library(forcats)

# Load data
ames <- read.csv("../data/AmesHousing.csv")

# Official paper: https://jse.amstat.org/v19n3/decock.pdf
# Variable description: https://tommasorigon.github.io/datamining/data/ames_documentation.txt

# Brief look at the dataset
str(ames)

# The skimr package is requires here; use "summary" as an alternative
skimr::skim(ames)

# ----------------------------------------
# 1. Basic filtering
# ----------------------------------------

# Keep only residential, normal sales
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

par(mfrow = c(1,2))
hist(ames$SalePrice, main = "SalePrice")
hist(log(ames$SalePrice), main = "log(SalePrice)")

# ----------------------------------------
# 3. Missing values
# ----------------------------------------

# Compute the frequency of the missing values for each variable
freq_missing <- apply(ames, 2, function(x) sum(is.na(x))) # Number of missing values
freq_missing[freq_missing > 0]

# It turns out (see the documentation) that NA here means "no alley"
table(ames$Alley, useNA = "always")

# Categorical: "absence = meaningful"
ames <- ames %>%
  mutate(
    Alley = replace_na(Alley, "No alley"),
    Fence = replace_na(Fence, "No fence"),
    Fireplace.Qu = replace_na(Fireplace.Qu, "No fireplace"),
    Bsmt.Exposure = ifelse(Bsmt.Exposure == "" | is.na(Bsmt.Exposure), "No basement", Bsmt.Exposure),
    Bsmt.Cond = replace_na(Bsmt.Cond, "No basement"),
    Bsmt.Qual = replace_na(Bsmt.Qual, "No basement")
  )

# Drop complex basement vars (simplify)
ames <- ames %>% select(-starts_with("BsmtFin"))

# Garage: if any missing → no garage
no_garage <- apply(ames[, c("Garage.Cond","Garage.Finish","Garage.Qual","Garage.Type")], 1, function(x) any(is.na(x)))
ames[no_garage, c("Garage.Cond","Garage.Finish","Garage.Qual","Garage.Type")] <- "No garage"

# Drop problematic variable
ames <- ames %>% 
  select(-Garage.Yr.Blt)

# Numeric imputations
ames <- ames %>%
  mutate(
    Bsmt.Full.Bath = replace_na(Bsmt.Full.Bath, 0),
    Bsmt.Half.Bath = replace_na(Bsmt.Half.Bath, 0),
    Lot.Frontage   = replace_na(Lot.Frontage, 0),
    Mas.Vnr.Area   = replace_na(Mas.Vnr.Area, 0)
  )

# Other fixes
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

# Lump rare levels (min frequency = 20)
ames <- ames %>%
  mutate(across(where(is.character), ~ fct_lump_min(as.factor(.), min = 20)))

# Neighborhood grouping
freq <- table(ames$Neighborhood)
ames$Neighborhood[freq[ames$Neighborhood] < 20] <- "Small"

# ----------------------------------------
# 5. Feature engineering
# ----------------------------------------

ames <- ames %>%
  mutate(
    Porch.SF = Open.Porch.SF + Enclosed.Porch + X3Ssn.Porch + Screen.Porch,
    Tot.Bath = Full.Bath + 0.5*Half.Bath + Bsmt.Full.Bath + 0.5*Bsmt.Half.Bath,
    House.Age = Yr.Sold - Year.Remod.Add
  )

# Remove redundant variables
ames <- ames %>%
  select(
    -c(Open.Porch.SF, Enclosed.Porch, X3Ssn.Porch, Screen.Porch),
    -c(Full.Bath, Half.Bath, Bsmt.Full.Bath, Bsmt.Half.Bath),
    -c(Mo.Sold, Yr.Sold, Year.Remod.Add, Year.Built)
  )

# ----------------------------------------
# 6. Near-zero variance features
# ----------------------------------------

library(caret)
nzv <- nearZeroVar(ames)
ames <- ames[, -nzv]

# ----------------------------------------
# 7. Save cleaned dataset
# ----------------------------------------

write.csv(ames, "../data/ames_clean.csv", row.names = FALSE)