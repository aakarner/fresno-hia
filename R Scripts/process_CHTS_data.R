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
# along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

# Author: Alex Karner
# File: process_CHTS_data.R
# Purpose: Process data from the 2010-2012 California Statewide Travel Survey to be used as input
# for the Fresno County HIA. 

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

# place <- read.csv("place.csv")
# persons <- read.csv("persons.csv")
# hhs <- read.csv("households.csv")

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
# with(hhs, write.csv(cbind(sampno, ctfip, hzip, hctract, incom, htrips, hhwgt, exphhwgt), "hh_sm.csv", row.names = FALSE))
# with(persons, write.csv(cbind(sampn, perno, ID, gend, age, age_imputed, ptrips, perwgt, expperwgt), "person_sm.csv", row.names = FALSE))
# with(place, write.csv(cbind(sampn, plano, perno, mode, tripdistance, tripdur, tcf, tcfperwgt, exptcfperwgt), "trip_sm.csv", row.names = FALSE))

# The place file includes a record for every place visited on the travel day, including a home location anchor
place <- read.csv("trip_sm.csv")

# Remove the home anchor location	
place <- place[place$plano > 1, ] 

# Remove missing cases
place <- place[!is.na(place$exptcfperwgt), ] # This shouldn't be necessary, but one record has no weight

# Create an ID variable for matching to person records
place$ID <- paste0(place$sampn, place$perno)

# Recode modes into categories relevant for the HIA
place$mode_recode <-
	ifelse(place$mode == 1, "Walk",
	ifelse(place$mode == 2, "Bike",
	ifelse(place$mode %in% c(3, 4), "Other non-motorized",
	ifelse(place$mode == 5, "Auto driver",
	ifelse(place$mode %in% c(6, 7), "Auto passenger",
	ifelse(place$mode %in% 8:14, "Private transit",
	ifelse(place$mode %in% 15:23, "Bus and express bus",
	ifelse(place$mode == 21, "Paratransit",
	ifelse(place$mode == 24, "Heavy rail",
	ifelse(place$mode == 25, "Commuter rail",
	ifelse(place$mode %in% 26:28, "Light rail", "Ferry")))))))))))

# Full mode categories and from the technical appendix pp. 263-4
# 1=Walk
# 2=Bike
# 
# Other
# 3=Wheelchair / Mobility Scooter
# 4=Other Non-Motorized
#
# Auto driver
# 5=Auto / Van / Truck Driver
# Auto passenger
# 6=Auto / Van / Truck Passenger
# 7=Carpool / Vanpool
# 
# Private transit
# 8=Motorcycle / Scooter / Moped
# 9=Taxi / Hired Car / Limo
# 10=Rental Car/Vehicle
# 11=Private shuttle (SuperShuttle, employer, hotel, etc.)
# 12=Greyhound Bus
# 13=Plane
# 14=Other Private Transit

# Bus/express bus
# 15=Local Bus, Rapid Bus
# 16=Express Bus / Commuter Bus (AC Transbay, Golden Gate Transit, etc)
# 17=Premium Bus ( Metro Orange / Silver Line )
# 18=School Bus
# 19=Public Transit Shuttle (DASH, Emery Go Round, etc.)
# 20=AirBART / LAX FlyAway
# 22=Amtrak Bus
# 23=Other Bus

# Paratransit
# 21=Dial-a-Ride / Paratransit (Access Services, etc.)

# Heavy rail
# 24=BART, Metro Red / Purple Line

# Commuter rail
# 25=ACE, Amtrak, Caltrain, Coaster, Metrolink

# Light rail/other
# 26=Metro Blue / Green / Gold Line, Muni Metro, Sacramento Light Rail, San Diego Sprinter / Trolley / Orange/Blue/Green, VTA Light Rail
# 27=Street Car / Cable Car
# 28=Other Rail

# Ferry
# 29=Ferry / Boat

# The person file contains one record for every person in every sampled household including those
# that did not travel on the travel day
persons <- read.csv("person_sm.csv")

# Recode age into eight categories, consistent with the HIA
persons$age8cat <-
	ifelse(persons$age_imputed <= 4, 1,
	ifelse(persons$age_imputed <= 14, 2,
	ifelse(persons$age_imputed <= 29, 3,
	ifelse(persons$age_imputed <= 44, 4,
	ifelse(persons$age_imputed <= 59, 5,
	ifelse(persons$age_imputed <= 69, 6,
	ifelse(persons$age_imputed <= 79, 7, 8)))))))

# Add a column of ones
persons$ones <- rep(1, 109113)

# The household file contains one record for every sampled household
hhs <- read.csv("hh_sm.csv")

# Create a data frame containing merged trip, household, and person data
CA.trips <- merge(persons, hhs, by.x = "sampn", by.y = "sampno")
CA.trips <- merge(CA.trips, place, by.x = "ID", by.y = "ID", all = TRUE)

# Create a data frame containing merged household and person data
persons <- merge(persons, hhs, by.x = "sampn", by.y = "sampno")

# We're only interested in trips
# Remove table elements that don't represent trips
sum(is.na(CA.trips$mode))
CA.trips <- CA.trips[!is.na(CA.trips$exptcfperwgt), ]

# -----------------------------------
# Analysis
# -----------------------------------

# Create requisite complex survey objects
CA.trips.svy <- svydesign(id = ~ID, weights = ~exptcfperwgt, data = CA.trips)
CA.person.svy <- svydesign(id = ~ID, weights = ~expperwgt, data = persons)
CA.hh.svy <- svydesign(id = ~sampno, weights = ~exphhwgt, data = hhs)

# Total trips

# All three of these should match, but don't
# It seems that the person/HH/trip weights were not developed to match
# For trip analysis, use the trip weights
svytotal(~ones, CA.trips.svy) # 131,781,481
svytotal(~ptrips, CA.person.svy, na.rm = TRUE) # 116,486,704
svytotal(~htrips, CA.hh.svy, na.rm = TRUE) # 107,739,833

# Time spent in active modes by age/sex category

# gend == 1, male
# gend == 2, female
# mode == 1, walk
# mode == 2, bike

# Total trip duration by mode

# 2 genders, 11 mode categories, 8 age categories
travel.times <- matrix(nrow = 88, ncol = 2)

# California 
# uncomment for statewide figures

# for(i in 1:2) { # gender
# 	print(paste0("i is ", i))
# 	
# 	for (j in 1:11) { # mode category
# 		print(paste0("j is ", j))
# 		
# 		for (k in 1:8) { # age category
# 			
# 			travel.times[k + 8 * (j - 1), i] <- 
# 			# if there are no trips in this category, return 0, otherwise return the total trip duration by
# 			# age-sex category
# 				ifelse(sum(CA.trips$gend == i & CA.trips$age8cat == k
# 					& CA.trips$mode_recode == levels(factor(CA.trips$mode_recode))[j]) > 0,
# 					{
# 						coef(svytotal(~tripdur, 
# 							subset(CA.trips.svy, 
# 							gend == i & age8cat == k & mode_recode == levels(factor(CA.trips$mode_recode))[j]))) 
# 					}, 0)
#
# 		}
# 		
# 	}
# 	
# }

# Fresno County only

for(i in 1:2) { # gender
	print(paste0("i is ", i))
	for (j in 1:11) { # mode category
		print(paste0("j is ", j))
		for (k in 1:8) { # age category
			travel.times[k + 8 * (j - 1), i] <- 
			# if there are no trips in this category, return 0, otherwise return the total trip duration by
			# age-sex category
				ifelse(sum(CA.trips$gend == i & CA.trips$age8cat == k & CA.trips$ctfip == 6019
					& CA.trips$mode_recode == levels(factor(CA.trips$mode_recode))[j], na.rm = TRUE) > 0,
					{
						coef(svytotal(~tripdur, 
							subset(CA.trips.svy, 
							gend == i & age8cat == k & ctfip == 6019 & mode_recode == levels(factor(CA.trips$mode_recode))[j]), 
							na.rm = TRUE))
					}, 0)
		}	
	}
}

# Check that all travel time has been accounted for
# Only consider trips made by respondents with reported gender
stopifnot(round(sum(travel.times)) == round(coef(svytotal(~tripdur, subset(CA.trips.svy, gend %in% c(1,2) & 
		ctfip == 6019)))))

# Total trip distance by mode

# 2 genders, 11 mode categories, 8 age categories
travel.distance <- matrix(nrow = 88, ncol = 2)

# California
# uncomment for statewide figures

# for(i in 1:2) { # gender
# 	print(paste0("i is ", i))
# 	for (j in 1:11) { # mode category
# 		print(paste0("j is ", j))	
# 		for (k in 1:8) { # age category		
# 			travel.distance[k + 8 * (j - 1), i] <- 
# 			# if there are no trips in this category, return 0, otherwise return the total trip duration by
# 			# age-sex category
# 				ifelse(sum(CA.trips$tripdistance[CA.trips$gend == i & CA.trips$age8cat == k 
# 					& CA.trips$mode_recode == levels(factor(CA.trips$mode_recode))[j]], na.rm = TRUE) > 0,
# 					{
# 						coef(svytotal(~tripdistance, 
# 							subset(CA.trips.svy, 
# 							gend == i & age8cat == k & mode_recode == levels(factor(CA.trips$mode_recode))[j]), na.rm = TRUE)) 
# 					}, 0)		
# 		}
# 	}
# }


# Fresno County 
for(i in 1:2) { # gender
	print(paste0("i is ", i))	
	for (j in 1:11) { # mode category
		print(paste0("j is ", j))		
		for (k in 1:8) { # age category			
			travel.distance[k + 8 * (j - 1), i] <- 
			# if there are no trips in this category, return 0, otherwise return the total trip duration by
			# age-sex category
				ifelse(sum(CA.trips$tripdistance[CA.trips$gend == i & CA.trips$age8cat == k & CA.trips$ctfip == 6019
					& CA.trips$mode_recode == levels(factor(CA.trips$mode_recode))[j]], na.rm = TRUE) > 0,
					{
						coef(svytotal(~tripdistance, 
							subset(CA.trips.svy, 
							gend == i & age8cat == k & ctfip == 6019 & 
									mode_recode == levels(factor(CA.trips$mode_recode))[j]), na.rm = TRUE)) 
					}, 0)		
		}
	}
}


# Check that all travel distance has been accounted for
# Only consider trips made by respondents with reported gender
stopifnot(round(sum(travel.distance, na.rm = TRUE)) == 
		round(coef(svytotal(~tripdistance, subset(CA.trips.svy, gend %in% c(1,2) & ctfip == 6019), na.rm = TRUE))))

# Average cycling speed for Fresno county
coef(svytotal(~tripdistance, subset(CA.trips.svy, mode == 1 & ctfip == 6019 & gend %in% c(1,2)), na.rm = TRUE)) / 
	(coef(svytotal(~tripdur, subset(CA.trips.svy, mode == 1 & ctfip == 6019 & gend %in% c(1,2))), na.rm = TRUE) / 60)

# Write the output and copy into the ITHIM sheet
write.table(travel.times, "travelTimebyModeGender_fresno.csv", sep = ",", row.names = FALSE, col.names = FALSE)
write.table(travel.distance, "travelDistancebyModeGender_fresno.csv", sep = ",", row.names = FALSE, col.names = FALSE)

# Create a table of population counts
pop.age.gender <- svytable(~age8cat+gend, CA.person.svy)
pop.age.gender <- svytable(~age8cat+gend, subset(CA.person.svy, ctfip == 6019))

# Write the output and copt into the ITHIM sheet
write.csv(pop.age.gender, "popAgeGender_fresno.csv", row.names = FALSE)