# This file is part of Fresno County HIA.
#
# Fresno County HIA is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this script.  If not, see <http://www.gnu.org/licenses/>.

# Author: Alex Karner
# File: process_CHTS_data_01.R
# Purpose: Process the raw data from the 2010-2012 California Statewide Travel Survey.
# Impute some missing values.
# Create smaller files to be used as input for the Fresno County HIA. 

# Library definitions
library(survey)
library(StatMatch)

# Scientific notation is no fun 
options(scipen = 100)

# Function definitions
rbind.data.frame.NA <- function(...) {
# This function allows rbind to function when columns are not equivalent.
# Missing columns are filled with NAs.
# Courtesy of: http://marc.info/?l=r-help&m=108333217907421
	N <- unique(unlist(lapply(list(...), names)))
  result <- NULL
  for (DF in list(...)) {
    x <- as.data.frame(lapply(N, function(x) 
    	if (x %in% names(DF)) DF[,x] 
			else NA))
    names(x) <- N
    result <- rbind(result,x)
  }
  result
}

# Set your working directory
setwd("E:/data_CHTS 2013/original")

# -----------------------------------
# Data preparation
# -----------------------------------

# Read in the full data files and prepare smaller versions for easier HIA analysis
# Note that an account with NREL is necessary to access the data
# See instructions here: http://www.nrel.gov/vehiclesandfuels/secure_transportation_data.html
# Unzip and place all files in your working directory

place <- read.csv("place.csv")
persons <- read.csv("persons.csv")
hhs <- read.csv("households.csv")

# Hot deck imputation of missing ages
# According to the 2010-2012 California Household Travel Survey Final Report, hot deck imputation was used on 
# respondents that refused to provide their age before calculating final weights (p. 91). Unfortunately, the imputed
# ages are not included here. Fortunately, the report states that average values by education level (educa: 1-6 =
# relevant, 7 = other, 8/9 = DK/refused), work status (emply: 1 = Yes, 2 = No, Other = NA/Refused) and student 
# status (wkstat: 6 = Student, Other = Not student, 98/99 = DK/refused) were used as the basis for imputation. 
# Further, "If education level was refused or missing, a mean age of relevant work status and student status 
# category was applied. If all the variables used for imputation are refusals, and the overall average age was 
# applied."

# Create a unique person ID
persons$ID <- paste0(persons$sampn, persons$perno)

# Recode missing variables
persons$age[persons$age > 900] <- NA
persons$educa[persons$educa > 7] <- NA
persons$emply[persons$emply > 2] <- NA
persons$wkstat[persons$wkstat > 97] <- NA

# Note: Students may not be employed
# Create a new categorical variable, employ_stat, that takes on four values:
# 1 = student, 2 = employed, 3 = non-student, not employed, 4 = DK/refused
employ_stat <- ifelse(persons$emply == 1, 1, NA)  # Employed
employ_stat[persons$wkstat != 6 & persons$emply == 2] <- 3  # Not student and not employed
employ_stat[is.na(persons$wkstat) & persons$emply != 1] <- 4  # employed people have a NA wkstat
employ_stat[is.na(persons$emply)] <- 4  # Some students have an NA for emply
employ_stat[persons$wkstat == 6] <- 2  # Student
persons$employ_stat <- employ_stat

# Hot deck imputation of missing ages

# Split the data into donor and recipient 
# There are 201 records with age but no education or work status
# These are not useful as donors, but will be merged into the final dataset
to.merge <- persons[!is.na(persons$age) & persons$employ_stat == 4 & is.na(persons$educa), 
	c("ID", "age", "employ_stat", "educa", "expperwgt")]

# Donors with both education and work status
donor.1 <- persons[!is.na(persons$age) & persons$employ_stat < 4 & !is.na(persons$educa), 
	c("ID", "age", "employ_stat", "educa", "expperwgt")]

# Donors with work status but no education
donor.2 <- persons[!is.na(persons$age) & persons$employ_stat < 4 & is.na(persons$educa), 
	c("ID", "age", "employ_stat", "expperwgt")]

# Donors with education but no work status
donor.3 <- persons[!is.na(persons$age) & persons$employ_stat > 3 & !is.na(persons$educa), 
	c("ID", "age", "educa", "expperwgt")]

# Recipients with no education or work status - will have overall mean imputed
recip.0 <- persons[is.na(persons$age) & persons$employ_stat > 3 & is.na(persons$educa), 
	c("ID", "age", "employ_stat", "educa", "expperwgt")]

# Recipients with both education and work status
recip.1 <- persons[is.na(persons$age) & persons$employ_stat < 4 & !is.na(persons$educa), 
	c("ID", "employ_stat", "educa", "expperwgt")]

# Recipients with work status but no education
recip.2 <- persons[is.na(persons$age) & persons$employ_stat < 4 & is.na(persons$educa), 
	c("ID", "employ_stat", "expperwgt")]

# Recipients with education but no work status
recip.3 <- persons[is.na(persons$age) & persons$employ_stat > 3 & !is.na(persons$educa), 
	c("ID", "educa", "expperwgt")]

# Total number of to.merge, donor, and recipient records equals the total number in the persons file
stopifnot(
	nrow(to.merge) + nrow(donor.1) + nrow(donor.2) + nrow(donor.3) + 
	nrow(recip.0) + nrow(recip.1) + nrow(recip.2) + nrow(recip.3) ==
	nrow(persons)
)

# All variables
# Use population weights since we're solving backwards from an unpublished method in which ages were
# imputed before weights were calculated.
imp.NND.1 <- RANDwNND.hotdeck(data.rec = recip.1, data.don = donor.1,	
	match.vars = NULL, don.class = c("employ_stat", "educa"), weight.don = "expperwgt")
rec.imp.1 <- create.fused(data.rec = recip.1, data.don = donor.1, mtc.ids = imp.NND.1$mtc.ids, z.vars="age")

# Work status but no education
imp.NND.2 <- RANDwNND.hotdeck(data.rec = recip.2, data.don = rbind(donor.1[, -4], donor.2),	
	match.vars = NULL, don.class = c("employ_stat"), weight.don = "expperwgt")
rec.imp.2 <- create.fused(data.rec = recip.2, data.don = rbind(donor.1[, -4], donor.2), 
	mtc.ids = imp.NND.2$mtc.ids, z.vars="age")

# Education but no work status
imp.NND.3 <- RANDwNND.hotdeck(data.rec = recip.3, data.don = rbind(donor.1[, -3], donor.3),	
	match.vars = NULL, don.class = c("educa"), weight.don = "expperwgt")
rec.imp.3 <- create.fused(data.rec = recip.3, data.don = rbind(donor.1[, -3], donor.3), 
	mtc.ids = imp.NND.3$mtc.ids, z.vars="age")

# Rebuild the person records
final <- rbind.data.frame.NA(to.merge, donor.1, donor.2, donor.3, recip.0, rec.imp.1, rec.imp.2, rec.imp.3)

# Finally, impute the mean age for records with no education or work status
final$age[is.na(final$age)] <- 
	weighted.mean(subset(final, !is.na(age))$age, final$expperwgt[!is.na(final$age)])

# Merge the imputed age with the person records
persons <- merge(persons, final[, c("ID", "age")], by.x = "ID", by.y = "ID")

names(persons)[152] <- "age_imputed"
names(persons)[6] <- "age"

# Fields needed for the HIA:
# HH: home county, tract, zip
# Person: age, sex, race
# Trips: trip distance
# All: weights

# Subset to create files containing only the needed columns
with(hhs, write.csv(cbind(sampno, ctfip, hzip, hctract, incom, htrips, hhwgt, exphhwgt), "hh_sm.csv", row.names = FALSE))
with(persons, write.csv(cbind(sampn, perno, ID, gend, age, age_imputed, ptrips, perwgt, expperwgt), "person_sm.csv", row.names = FALSE))
with(place, write.csv(cbind(sampn, plano, perno, mode, tripdistance, tripdur, tcf, tcfperwgt, exptcfperwgt), "trip_sm.csv", row.names = FALSE))