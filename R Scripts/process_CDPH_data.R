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
# File: process_CDPH_data.R
# Purpose: Calculate age-sex specific deathrates by county for traffic deaths and diseases related to
# physical activity and air pollution.
# NB: The input files specified here must be obtained for a fee from CDPH and are confidential. 
# Additionally, we have heavily processed them to combine diseases into relevant categories from the 
# Global Burden of Disease Study. 

# Library definitions
library(lattice)
library(R2HTML)

# -----------------------------------
# Data preparation
# -----------------------------------

# Convert TEMPD.SAV to TEMPD.csv prior to running this line
# Otherwise use read.spss() in package 'foreign' 
cdph.death <- read.csv("D:/Dropbox/Work/02 postdoc/SJV HIA/CDPH - vital statistics/TEMPD.csv")
# cdph.death$county3 <- as.character(cdph.death$county3)

years <- c(2008, 2009, 2010)
diseases <- paste0("GBDGRP", 1:13)
r.names <- c(paste0("GBDGRP", 1:13, "_08"), paste0("GBDGRP", 1:13, "_09"), paste0("GBDGRP", 1:13, "_10"))

# These are not regular FIPS codes - see appropriate data dictionary for details
sjv.ctys <- c("010", "015", "016", "020", "024", "039", "050", "054")
ca.ctys <- c("001","002","003","004","005","006","007","008","009","010","011","012","013","014","015","016","017","018","019","020","021","022","023","024","025","026","027","028","029","030","031","032","033","034","035","036","037","038","039","040","041","042","043","044","045","046","047","048","049","050","051","052","053","054","055","056","057","058")

# Retain only California counties (those with place of residence outside of CA are also recorded)
cdph.death <- cdph.death[cdph.death$county3 %in% ca.ctys, ]
cdph.death$county3 <- cdph.death$county3[, drop = TRUE]

# Calculate county-specific death rates
# Create a dataframe containing counts of deaths by disease, by year, population row by row 
agg.deaths <- matrix(nrow = 39, ncol = 58, dimnames = list(r.names, ca.ctys))

count <- 1

for(i in years) {
	for(j in diseases) {
		agg.deaths[count, ] <- with(cdph.death[cdph.death$yod == i,], tapply(get(j), county3, sum, na.rm = TRUE))	
		count <- count + 1
	}
}

# Paste agg.deaths to the clipboard
HTML(agg.deaths, file("clipboard-128","w"), append=FALSE)

# Now paste the clipboard into Excel