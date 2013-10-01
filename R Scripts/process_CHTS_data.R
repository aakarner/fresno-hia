library(survey)
options(scipen = 100)

setwd("E:/data_CHTS 2013/original")

# -----------------------------------
# Data preparation
# -----------------------------------

# Read in the full data files and prepare smaller versions for easier HIA analysis
# place <- read.csv("place.csv")
# persons <- read.csv("persons.csv")
# hhs <- read.csv("households.csv")
# 
# # home county, tract, zip
# # person age, sex, race
# # trip distance
# # weights
# 

# Subset to create files containing only the needed columns
# with(hhs, 
#		write.csv(cbind(sampno, ctfip, hzip, hctract, incom, htrips, hhwgt, exphhwgt), "hh_sm.csv", row.names = FALSE))
# with(persons, 
#		write.csv(cbind(sampn, perno, gend, age, ptrips, perwgt, expperwgt), "person_sm.csv", row.names = FALSE))
# with(place,
#		write.csv(cbind(sampn, plano, perno, mode, tripdistance, tripdur, tcf, tcfperwgt, exptcfperwgt), "trip_sm.csv", row.names = FALSE))

# The place file includes a record for every place visited on the travel day, including a home location anchor
place <- read.csv("trip_sm.csv")

# Remove the home anchor location	
place <- place[place$plano > 1, ] 

# Remove missing cases
place <- place[!is.na(place$exptcfperwgt), ] # This shouldn't be the case, but one record has no weight

# Create a unique ID variable
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
	ifelse(persons$age <= 4, 1,
	ifelse(persons$age <= 14, 2,
	ifelse(persons$age <= 29, 3,
	ifelse(persons$age <= 44, 4,
	ifelse(persons$age <= 59, 5,
	ifelse(persons$age <= 69, 6,
	ifelse(persons$age <= 79, 7, 8)))))))

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

# Average non-motorized trip duration per person in Fresno County

# 2 genders, 11 mode categories, 8 age categories
travel.times <- matrix(nrow = 88, ncol = 2)

for(i in 1:2) { # gender
	print(paste0("i is ", i))
	
	for (j in 1:11) { # mode category
		print(paste0("j is", j))
		
		for (k in 1:8) { # age category
			
			travel.times[j * k, i] <- 
			ifelse(sum(CA.trips$gend == i & CA.trips$age8cat == k & CA.trips$mode_recode == levels(factor(CA.trips$mode_recode))[j]) > 0, {
				coef(svytotal(~tripdur,
					subset(CA.trips.svy, 
						gend == i & age8cat == k & mode_recode == levels(factor(CA.trips$mode_recode))[j]))) }, 0)
			
			#svytotal(~ones.x, subset(CA.person.svy, gend == i & age8cat == k & ctfip == 6019)))
		}
		
	}
	
}

try(svytotal(~tripdur, subset(CA.trips.svy, gend == i & age8cat == k & mode_recode == levels(factor(CA.trips$mode_recode))[j])))
			


svytotal(~tripdur, subset(CA.trips.svy, gend == 2 & age8cat == 4 & mode == 2 & ctfip == 6019))

# average non-motorized trip distance per person

# Mode share
svytotal(~factor(mode), CA.trips.svy)/131781481