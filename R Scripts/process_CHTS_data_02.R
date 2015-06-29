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
# File: process_CHTS_data_02.R
# Purpose: Use previously prepared input data and conduct further processing for the Fresno County HIA. 

# Library definitions
library(survey)

# Scientific notation is no fun 
options(scipen = 100)

# Set your working directory
setwd("E:/data_CHTS 2013/original")

# -----------------------------------
# Data preparation
# -----------------------------------

# The place (trips) file includes a record for every place visited on the travel day, including a home location anchor
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

# Remove improbably slow and fast walking trips
place$speed <- place$tripdist / (place$tripdur / 60)
sum((place$speed < 0.1 | place$speed > 10) & place$mode == 1, na.rm = TRUE)
place$exptcfperwgt[(place$speed < 0.1 | place$speed > 10) & place$mode == 1] <- NA

# Remove improbably slow and fast cycling trips
sum((place$speed < 0.1 | place$speed > 24) & place$mode == 2, na.rm = TRUE)
place$exptcfperwgt[(place$speed < 0.1 | place$speed > 24) & place$mode == 2] <- NA

# Person file

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

# Create active transport variables
CA.trips$minday_walk <- 0
CA.trips$minday_walk[CA.trips$mode == 1] <- CA.trips$tripdur[CA.trips$mode == 1]

CA.trips$minday_bike <- 0
CA.trips$minday_bike[CA.trips$mode == 2] <- CA.trips$tripdur[CA.trips$mode == 2]

# Calculate metabolic equivalents for transport-related physical activity
# Source: Table C4 in ITHIM Technical Appendices
CA.trips$MET_trans_hrs_day <- with(CA.trips,
	ifelse(age8cat < 6, 3.8 * (minday_walk / 60) + 3.8 * (minday_bike / 60),
	ifelse(age8cat == 6, 3.0 * (minday_walk / 60) + 3.8 * (minday_bike / 60),
	ifelse(age8cat == 7, 2.5 * (minday_walk / 60) + 3.8 * (minday_bike / 60),
	ifelse(age8cat == 8, 2.5 * (minday_walk / 60) + 3.8 * (minday_bike / 60), 0)))))

# Transport-Related Hours per week
CA.trips$trans_hrs_day <- with(CA.trips,
	ifelse(age8cat < 6, (minday_walk / 60) + (minday_bike / 60),
	ifelse(age8cat == 6, (minday_walk / 60) + (minday_bike / 60),
	ifelse(age8cat == 7, (minday_walk / 60) + (minday_bike / 60),
	ifelse(age8cat == 8, (minday_walk / 60) + (minday_bike / 60), 0)))))

# Sum active transport variables at the person level and merge with the person table
active.travel <- aggregate( 
	1.0 * CA.trips[, c("MET_trans_hrs_day", "trans_hrs_day", "minday_walk", "minday_bike")], 
	by = list(CA.trips$ID), sum)
names(active.travel)[1] <- "ID"

persons <- merge(persons, active.travel, all = TRUE)

# Enter zeroes for those with no travel on the travel day
persons$MET_trans_hrs_day[is.na(persons$MET_trans_hrs_day)] <- 0
persons$trans_hrs_day[is.na(persons$trans_hrs_day)] <- 0
persons$minday_walk[is.na(persons$minday_walk)] <- 0
persons$minday_bike[is.na(persons$minday_bike)] <- 0

# -----------------------------------
# Analysis
# -----------------------------------

# Create requisite complex survey objects
CA.trips.svy <- svydesign(id = ~ID, weights = ~exptcfperwgt, data = CA.trips)
CA.person.svy <- svydesign(id = ~ID, weights = ~expperwgt, data = persons)
CA.hh.svy <- svydesign(id = ~sampno, weights = ~exphhwgt, data = hhs)

# Save the complex survey objects to an .RData file
# Just load this in the future instead of running the data preparation section of the script
save(CA.trips.svy, CA.person.svy, CA.hh.svy, file = "CHTS_2013.RData")

# Total trips

# All three of these should match, but don't
# It seems that the person/HH/trip weights were not developed to match
# For trip analysis, use the trip weights
svytotal(~ones, CA.trips.svy) # 130,116,195
svytotal(~ptrips, CA.person.svy, na.rm = TRUE) # 116,486,704
svytotal(~htrips, CA.hh.svy, na.rm = TRUE) # 107,739,833

# Define geographic identifiers
SJV.counties <- c(6107, 6047, 6039, 6019, 6077, 6031, 6029, 6099)
# SJV.3.counties <- c(6019, 6029, 6077)
# bay.area.counties <- c(6041, 6075, 6013, 6081, 6095, 6097, 6001, 6085, 6055)
# fresno.cty <- 6019

# Total trip duration by mode

# 2 genders, 11 mode categories, 8 age categories
# gend == 1, male
# gend == 2, female
# mode == 1, walk
# mode == 2, bike

travel.times <- matrix(nrow = 88, ncol = 2)
travel.times.err <- matrix(nrow = 88, ncol = 2)

# Eight county San Joaquin Valley
for(i in 1:2) { # gender
	print(paste0("i is ", i))
	for (j in 1:11) { # mode category
		print(paste0("j is ", j))
		for (k in 1:8) { # age category
			time <- try(svytotal(~tripdur, subset(CA.trips.svy, 
							gend == i & age8cat == k & ctfip %in% SJV.counties
							& mode_recode == levels(factor(CA.trips$mode_recode))[j]), 
							na.rm = TRUE), silent = TRUE)
			# if there are no trips in this category, return 0, otherwise return the total trip duration by
			# age-sex category
			travel.times[k + 8 * (j - 1), i] <- 
				ifelse(class(time) == "try-error", 0, coef(time))
			travel.times.err[k + 8 * (j - 1), i] <-
				ifelse(class(time) == "try-error", 0, SE(time))
		}
	}
}

# Check that all travel time has been accounted for
# Only consider trips made by respondents with reported gender
stopifnot(round(sum(travel.times)) == round(coef(svytotal(~tripdur, subset(CA.trips.svy, gend %in% c(1,2) & 
		ctfip %in% SJV.counties)))))

# Total trip distance by mode
travel.distance <- matrix(nrow = 88, ncol = 2)
travel.distance.err <- matrix(nrow = 88, ncol = 2)

# Eight county San Joaquin Valley
for(i in 1:2) { # gender
	print(paste0("i is ", i))	
	for (j in 1:11) { # mode category
		print(paste0("j is ", j))		
		for (k in 1:8) { # age category			
			dist <- try(svytotal(~tripdistance, subset(CA.trips.svy, 
									gend == i & age8cat == k & ctfip %in% SJV.counties & 
									mode_recode == levels(factor(CA.trips$mode_recode))[j]), 
									na.rm = TRUE), silent = TRUE) 
			# if there are no trips in this category, return 0, otherwise return the total trip duration by
			# age-sex category
			travel.distance[k + 8 * (j - 1), i] <- 
				ifelse(class(dist) == "try-error", 0, coef(dist))
			travel.distance.err[k + 8 * (j - 1), i] <-
				ifelse(class(dist) == "try-error", 0, SE(dist))
		}
	}
}

# Check that all travel distance has been accounted for
# Only consider trips made by respondents with reported gender
stopifnot(round(sum(travel.distance, na.rm = TRUE)) == 
		round(coef(svytotal(~tripdistance, subset(CA.trips.svy, gend %in% c(1,2) & ctfip %in% SJV.counties), 
		na.rm = TRUE))))

# -----------------------------------
# Output
# -----------------------------------

# Write the output for use in the SJV HIA spreadsheet
write.table(travel.times, "output/travelTimebyModeGender_SJV.csv", sep = ",", row.names = FALSE, col.names = FALSE)
write.table(travel.times.err, "output/travelTimebyModeGenderSE_SJV.csv", sep = ",", row.names = FALSE, 
	col.names = FALSE)

write.table(travel.distance, "output/travelDistancebyModeGender_SJV.csv", sep = ",", 
	row.names = FALSE, col.names = FALSE)
write.table(travel.distance.err, "output/travelDistancebyModeGenderSE_SJV.csv", sep = ",", 
	row.names = FALSE, col.names = FALSE)

# Create a table of population counts for the 8 county SJV
pop.age.gender <- svytable(~age8cat + gend, subset(CA.person.svy, ctfip %in% SJV.counties))

# Write the output
write.csv(pop.age.gender, "output/popAgeGender_SJV.csv", row.names = FALSE)

# Calculate the baseline coefficient of variation of active transportation time 

# Use 'person' as the unit of analysis and consider their total daily travel time 
sqrt(coef(svyvar(~I(minday_walk + minday_bike), subset(CA.person.svy, ctfip %in% SJV.counties), na.rm = TRUE)))/
coef(svymean(~I(minday_walk + minday_bike), subset(CA.person.svy, ctfip %in% SJV.counties), na.rm = TRUE))