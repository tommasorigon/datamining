# -----------------------------------
# LAB 2 (Ames Housing)
# Course: Data Mining
# Author: Tommaso Rigon
# --------------------------------

rm(list = ls())

# The dataset can be also found online here: https://tommasorigon.github.io/datamining/data/AmesHousing.csv
ames <- read.csv("../data/AmesHousing.csv", header = TRUE)

# Official paper: https://jse.amstat.org/v19n3/decock.pdf
# Variable description: https://tommasorigon.github.io/datamining/data/ames_documentation.txt

# Brief look at the dataset
str(ames)

# The skimr package is requires here; use "summary" as an alternative
skimr::skim(ames)

# Our aim is to predict the variable Sale.Price as a function of everything else.
# Use the documentation to get more insight about this variable and its meaning:
table(ames$MS.Zoning)

# From the documentation:
# "C (all)" = Commercial sales
# "I (all)" = Industrial sales
# "A (agr)" = Agricultural sales
# "FV" = Floating village sales
ames <- subset(ames, !(MS.Zoning %in% c("C (all)", "I (all)", "A (agr)", "FV")))

# The sale type is also relevant
table(ames$Sale.Condition)

# Only normal sales
ames <- subset(ames, Sale.Condition == "Normal")
# Then, the variables can be dropped
ames <- subset(ames, select = -c(Sale.Condition, Sale.Type))

# Drop the variables PID and Order as well
# PID is actually interesting for other reasons; see the website: https://www.cityofames.org/government/departments-divisions-a-h/city-assessor
ames <- subset(ames, select = -c(Order, PID))

# The response variable ---------------------------------------------------------------------

# Let us have a look at the main variable (not too much though!)
summary(ames$SalePrice)

par(mfrow = c(1, 2))
hist(ames$SalePrice, xlab = "Price", main = "SalePrice")
hist(log(ames$SalePrice), xlab = "Price", main = "Logarithm of SalePrice")

# Missing values -----------------------------------------------------------------------------

# Compute the frequency of the missing values for each variable
freq_missing <- apply(ames, 2, function(x) sum(is.na(x))) # Number of missing values
freq_missing[freq_missing > 0]

# It turns out (see the documentation) that NA here means "no alley"
table(ames$Alley, useNA = "always")
# We therefore just re-code the NA as appropriate
ames$Alley[is.na(ames$Alley)] <- "No alley access"

# A similar story holds for the Fence, the Fireplace and the basement
table(ames$Fence, useNA = "always")
ames$Fence[is.na(ames$Fence)] <- "No fence"

table(ames$Fireplace.Qu, useNA = "always")
ames$Fireplace.Qu[is.na(ames$Fireplace.Qu)] <- "No fireplace"

# Basement
table(ames$Bsmt.Exposure, ames$Bsmt.Cond, useNA = "always")

ames$Bsmt.Exposure[is.na(ames$Bsmt.Exposure)] <- "No basement"
ames$Bsmt.Cond[is.na(ames$Bsmt.Cond)] <- "No basement"
ames$Bsmt.Qual[is.na(ames$Bsmt.Qual)] <- "No basement"
ames$BsmtFin.Type.1[is.na(ames$BsmtFin.Type.1)] <- "No basement"
ames$BsmtFin.Type.2[is.na(ames$BsmtFin.Type.2)] <- "No basement"

# The "No basement" values are occuring at the same rows. Can you spot why this is going to be problematic?

# There are also some "blank" lines, which are problably some "true" missing values. We impute these values with the most common one
table(ames$Bsmt.Exposure, useNA = "always")
ames$Bsmt.Exposure[ames$Bsmt.Exposure == ""] <- "No"

table(ames$BsmtFin.Type.2, useNA = "always")
ames$BsmtFin.Type.2[ames$BsmtFin.Type.2 == ""] <- "Unf"

# For the garage, there is a slighly different situation because there are some inconsistencies:
table(ames$Garage.Cond, ames$Garage.Type, useNA = "always")

# We assume that if ANY variable is denoted as "no garage" then it means that there is no garage
# This might be a strong assumption, but (i) it simplifies the modelling and (ii) it impacts a small number of data points (1 or 2 at most)
id_no_garage <- apply(cbind(
  is.na(ames$Garage.Cond), is.na(ames$Garage.Finish),
  is.na(ames$Garage.Qual), is.na(ames$Garage.Type)
), 1, any)

ames$Garage.Cond[id_no_garage] <- "No garage"
ames$Garage.Finish[id_no_garage] <- "No garage"
ames$Garage.Qual[id_no_garage] <- "No garage"
ames$Garage.Type[id_no_garage] <- "No garage"

# There is finally the "Garage Yr Blt" for which the situation is more complex (why?).
# For the sake of simplicity, we can remove it
table(ames$Garage.Yr.Blt, useNA = "always")
ames <- subset(ames, select = -Garage.Yr.Blt)

# For the bathrooms, we can use numbers to denote the absence of the feature
ames$Bsmt.Full.Bath[is.na(ames$Bsmt.Full.Bath)] <- 0
ames$Bsmt.Half.Bath[is.na(ames$Bsmt.Half.Bath)] <- 0

# It's unlikely that the a house is lacking the electrical system. We therefore impute the value with the mode
table(ames$Electrical, useNA = "always")
ames$Electrical[ames$Electrical == ""] <- "SBrkr"

table(ames$Lot.Frontage, useNA = "always")
ames$Lot.Frontage[is.na(ames$Lot.Frontage)] <- 0

# The values of  Mas.Vnr are left black
table(ames$Mas.Vnr.Type, useNA = "always")
# It is not clear from the documentation what these 9 values represent, but it make sense to use "None"
ames$Mas.Vnr.Type[ames$Mas.Vnr.Type == ""] <- "None"
table(ames$Mas.Vnr.Area, useNA = "always")
ames$Mas.Vnr.Area[is.na(ames$Mas.Vnr.Area)] <- 0

# Two fixes are necessary here
table(ames$Misc.Feature, useNA = "always")
ames$Misc.Feature[is.na(ames$Misc.Feature)] <- "No additional feature"

# We regroup the levels, because otherwise the estimation would be impossible
ames$Misc.Feature[ames$Misc.Feature == "TenC"] <- "Othr"

table(ames$Pool.QC, useNA = "always")
ames$Pool.QC[is.na(ames$Pool.QC)] <- "No"
ames$Pool.QC[ames$Pool.QC %in% c("TA", "Ex", "Gd", "Fa")] <- "Yes"


# Neighborhood ------------------------------------------------------------------------------------------------
freq_neigh <- table(ames$Neighborhood)
freq_neigh

ames$Neighborhood[ames$Neighborhood %in% names(freq_neigh)[freq_neigh < 20]] <- "Small neighborhood"
table(ames$Neighborhood)

# There are smartest way of doing this grouping, taking into account the location of each 

# Grouping the levels -----------------------------------------------------------------------------------------
table(ames$Exterior.1st)

# library forcats
ames$Exterior.1st <- forcats::fct_lump_lowfreq(ames$Exterior.1st)
ames$Exterior.2nd <- forcats::fct_lump_lowfreq(ames$Exterior.2nd)
ames$Garage.Qual <- forcats::fct_lump_lowfreq(ames$Garage.Qual)
table(ames$Exterior.1st)

# Even better, we can do it (after carefully checking this), on every single character column (dplyr needs to be installed)
ames <- dplyr::mutate(ames, dplyr::across(is.character, function(x) forcats::fct_lump_lowfreq(x)))


# Feature engineering -----------------------------------------------------------------------------------------

ames$Porch.Sq.Feet <- ames$Open.Porch.SF + ames$Enclosed.Porch + ames$X3Ssn.Porch + ames$Screen.Porch
ames$Tot.Bathrooms <- ames$Full.Bath + 0.5 * ames$Half.Bath + ames$Bsmt.Full.Bath + 0.5 * ames$Bsmt.Half.Bath
ames$House.Age <- ames$Yr.Sold - ames$Year.Remod.Add

# Some simplifications ----------------------------------------------------------------------------------------

# Most of the information is already included in House Age
ames <- subset(ames, select = -c(Mo.Sold, Yr.Sold, Year.Remod.Add, Year.Built))
# Most of the information is already included in Porch Sq Feet
ames <- subset(ames, select = -c(Open.Porch.SF, Enclosed.Porch, X3Ssn.Porch, Screen.Porch))
# Most of the information is already included in Tot Bathrooms
ames <- subset(ames, select = -c(Full.Bath, Half.Bath, Bsmt.Full.Bath, Bsmt.Half.Bath))

# Near constant variables -------------------------------------------------------------------------------------

# Caret package need to be installed
colnames(ames)[caret::nearZeroVar(ames)]

table(ames$Street)
table(ames$Utilities)
table(ames$Condition.2)
table(ames$Pool.Area)
table(ames$Roof.Matl)

# Almost no information is present in these variables
ames <- subset(ames, select = -c(Pool.Area, Utilities, Street, Condition.2, Roof.Matl))

# Training, validation and test set ----------------------------------------------------------------------------

# Manual subdivision in training / test
set.seed(123)
# Randomly allocate the id of the variables into training and test
id_train <- sort(sample(1:nrow(ames), size = floor(0.5 * nrow(ames)), replace = FALSE))
id_validation_test <- setdiff(1:nrow(ames), id_train)
# Now we allocate the validation test
id_validation <- sort(sample(id_validation_test, size = floor(0.5 * length(id_validation_test)), replace = FALSE))
# And finally the test set
id_test <- setdiff(id_validation_test, id_validation)

# Create two different datasets
ames_train <- ames[id_train, ]
ames_validation <- ames[id_validation, ]
ames_test <- ames[id_test, ]

write.csv(data.frame(ames_train), "../data/ames_train.csv", row.names = FALSE)
write.csv(data.frame(ames_validation), "../data/ames_validation.csv", row.names = FALSE)
write.csv(data.frame(ames_test), "../data/ames_test.csv", row.names = FALSE)
