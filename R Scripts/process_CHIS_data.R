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
# File: process_CHIS_data.R
# Purpose: Process data from the 2005 California Heath Interview Survey
# for the Fresno County HIA. 
# NB: 
# * This script was ported to R based on SAS code by Neil Maizlish available in
# Appendix F of Maizlish, N., Woodcock, J., Co, S., Ostro, B., Fanai, A., Fairley, D., 2012. 
# Health co-benefits and transportation-related reductions in greenhouse gas emissions in the bay area: 
# Technical report. California Department of Public Health.
# * The 2005 data must be used because they contain an occupation type variable that was later dropped.
# * The statewide data are available publicly here: http://healthpolicy.ucla.edu/chis/data/Pages/public-use-data.aspx
# It is possible to request sub-state data, but code must be provided and the request approved.

# TODO: Make the statewide data more representative of SJV/Fresno conditions by adjusting 
# the assumed distribution of employment using recent ACS PUMS data.

# Library definitions
library(survey)
library(reshape)

# Set working drectory
setwd("D:/Dropbox/Work/02 postdoc/SJV HIA/data/CHIS/public use files/chis05_adult_spss")

# -----------------------------------
# Data preparation
# -----------------------------------

# Convert ADULT.SAV to ADULT.csv prior to running this line
# Otherwise use read.spss() in package 'foreign' 
chis.2005 <- read.csv("ADULT.csv")
names(chis.2005) <- tolower(names(chis.2005))

# Create age categories;
chis.2005$age8cat =
	ifelse(chis.2005$srage_p <= 4, 1,
	ifelse(chis.2005$srage_p <= 14, 2,
	ifelse(chis.2005$srage_p <= 29, 3,
	ifelse(chis.2005$srage_p <= 44, 4,
	ifelse(chis.2005$srage_p <= 59, 5,
	ifelse(chis.2005$srage_p <= 69, 6,
	ifelse(chis.2005$srage_p <= 79, 7, 8)))))))

# Create walk for fun, job-related, moderate and vigorous physical activity

# Walk for fun
chis.2005$minwk_walk4fun <- chis.2005$ad42
chis.2005$minwk_walk4fun[chis.2005$ad40 %in% c(-1, 2)] <- 0
chis.2005$minwk_walk4fun[chis.2005$ad42unt == 2] <- chis.2005$minwk_walk4fun[chis.2005$ad42unt == 2] * 60
chis.2005$minday_walk4fun <- chis.2005$minwk_walk4fun
chis.2005$minwk_walk4fun <- chis.2005$minwk_walk4fun * chis.2005$ad41
chis.2005$minday_walk4fun <- chis.2005$minwk_walk4fun / 7

# Percent of the population walking for transport
chis.2005$walk4transport <- chis.2005$ad37
chis.2005$minwk_walk4transport <- chis.2005$ad39
chis.2005$minwk_walk4transport[chis.2005$ad37 %in% c(2, 3)] <- 0
chis.2005$minwk_walk4transport[chis.2005$ad39unt == 2] <- chis.2005$minwk_walk4transport[chis.2005$ad39unt == 2] * 60
chis.2005$minday_walk4transport <- chis.2005$minwk_walk4transport
chis.2005$minwk_walk4transport <- chis.2005$minwk_walk4transport * chis.2005$ad38

# Here Maizlish includes calculations for total active transport time based on the CHIS data.
# We will use only the CHTS data to calculate active travel properties.
# TODO: Acquire Fresno County data from the CHIS for comparison.

# Moderate Physical Activity
chis.2005$minwk_mod <- chis.2005$ae27a
chis.2005$minwk_mod[chis.2005$ae26 == 1] <- 0
chis.2005$minwk_mod[chis.2005$ae27 <= 0] <- 0
chis.2005$minwk_mod[chis.2005$ae27unt == 2] <- chis.2005$minwk_mod[chis.2005$ae27unt == 2] * 60
chis.2005$minwk_mod <- chis.2005$minwk_mod * chis.2005$ae27

# Vigorous Activity
chis.2005$minwk_vig <- chis.2005$ae25a;
chis.2005$minwk_vig[chis.2005$ae24 == -1] <- 0
chis.2005$minwk_vig[chis.2005$ae25 <= 0] <- 0
chis.2005$minwk_vig[chis.2005$ae25aunt == 2] <- chis.2005$minwk_vig[chis.2005$ae25aunt == 2] * 60
chis.2005$minwk_vig <- chis.2005$minwk_vig * chis.2005$ae25

# Make MET assignments
# Walking for fun
chis.2005$MET_walk_hrs_wk <-
	ifelse(chis.2005$age8cat < 6, 3.8 * chis.2005$minwk_walk4fun / 60,
	ifelse(chis.2005$age8cat == 6, 3.0 * chis.2005$minwk_walk4fun / 60,
	ifelse(chis.2005$age8cat == 7, 2.5 * chis.2005$minwk_walk4fun / 60,
	2.5 * chis.2005$minwk_walk4fun / 60)))

# Moderate exercise
chis.2005$MET_mod_hrs_wk <-
	ifelse(chis.2005$age8cat < 6, 3.8 * chis.2005$minwk_mod / 60,
	ifelse(chis.2005$age8cat == 6, 3.0 * chis.2005$minwk_mod / 60,
	ifelse(chis.2005$age8cat == 7, 2.5 * chis.2005$minwk_mod / 60,
	2.5 * chis.2005$minwk_mod / 60)))

# Vigorous exercise
chis.2005$MET_vig_hrs_wk <-
	ifelse(chis.2005$age8cat < 6, 9 * chis.2005$minwk_vig / 60,
	ifelse(chis.2005$age8cat == 6, 8 * chis.2005$minwk_vig / 60,
	ifelse(chis.2005$age8cat == 7, 7 * chis.2005$minwk_vig / 60,
	7 * chis.2005$minwk_vig / 60)))

# Occupational physical activity for people < 60 years of age;

# Asterisk out next line to set minimum day time activity hours
# On-job physical activity
# AK3 is hours worked. Assumption that non-job PA is 0 METS

# Initialize hours worked and non-job activity for unemployed
chis.2005$hours_worked <- 0
chis.2005$hours_worked[chis.2005$ak3 > 0] <- chis.2005$ak3[chis.2005$ak3 > 0]

# Use Woodcock's rule that 5 of 8 hours/day of work are at the
# nominal MET rating

chis.2005$hours_worked <- 5 / 8 * chis.2005$hours_worked

chis.2005$MET_occ_hrs_wk = 0

chis.2005$MET_occ_hrs_wk <-
	 # management, professional, office
	ifelse(chis.2005$occmain %in% c(1, 2, 5), 1.5 * chis.2005$hours_worked,
	# Service and sales
	ifelse(chis.2005$occmain %in% c(3, 4), 2.3 * chis.2005$hours_worked,
	# Farming, forestry
	ifelse(chis.2005$occmain == 6, 5.8 * chis.2005$hours_worked,
	# Construction
	ifelse(chis.2005$occmain == 7, 5.5 * chis.2005$hours_worked,
	# Installation and repair
	ifelse(chis.2005$occmain == 8, 3.5 * chis.2005$hours_worked,
	# Production
	ifelse(chis.2005$occmain == 9, 3.0 * chis.2005$hours_worked,
	# Transportation and material moving
	ifelse(chis.2005$occmain == 10, 6.5 * chis.2005$hours_worked,
	#Military
	ifelse(chis.2005$occmain == 11, 4.0 * chis.2005$hours_worked,
	ifelse((chis.2005$occmain < 0 | chis.2005$occmain == 99) & chis.2005$hours_worked > 0,
	2.5 * chis.2005$hours_worked0, 0)))))))))

# If they're not unemployed (ak3 > 0) assign 2.5 METs per hour worked
chis.2005$MET_occ_hrs_wk
		
# Total - non transport related physical activity;
# Inclusion of walking is double counting;
# MET hours/week of non-transport physical activity
chis.2005$MET_hrwk_nt_pa <- chis.2005$MET_mod_hrs_wk + chis.2005$MET_vig_hrs_wk + chis.2005$MET_occ_hrs_wk

# Create a table of hours/week of non-work physical activity METS by age-sex-occupation categories
# Use as.data.frame.table() to coerce the 'by' object to a data frame
# Ref: http://tolstoy.newcastle.edu.au/R/e8/help/09/11/6227.html
a.s.o.table <- as.data.frame.table(
	by(chis.2005, list(chis.2005$age8cat, chis.2005$srsex, chis.2005$occmain), 
	function(x) weighted.mean(x$MET_hrwk_nt_pa, x$rakedw0), simplify = TRUE))
	
names(a.s.o.table) <- c("age8cat", "gender", "occupation", "METS")
a.s.o.table$METS <- a.s.o.table$METS / 7  # Convert to daily values

# Write the output
write.csv(a.s.o.table, "CHIS_2005_nonwork_PA.csv", row.names = FALSE)