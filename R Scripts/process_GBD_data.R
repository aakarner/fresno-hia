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
# File: process_GBD_data.R
# Purpose: Process data from the Global Burden of Disease study to be used as input
# for the Fresno County HIA. 
# NB: This script uses a pre-processed data file as input that's availabel on github:
# https://github.com/aakarner/fresno-hia/tree/master/Data/WHO%20Global%20Burden%20of%20Disease

# Set working directory
setwd("D:/Dropbox/Work/02 postdoc/SJV HIA/Data/WHO global burden of disease")

# -----------------------------------
# Data preparation
# -----------------------------------
gbd.2010 <- read.table("IHME_USA_GBD_2010_ITHIM diseases, non-overlapping ages.csv", header = TRUE, 
	sep = ",", stringsAsFactors = TRUE)
	
# Recode age levels in the age factor
# levels(gbd.2010$age_name)
#  [1] "15-19 years"   "20-24 years"   "25-29 years"   "30-34 years"   "35-39 years"   "40-44 years"   "45-49 years"  
#  [8] "5-14 years"    "50-54 years"   "55-59 years"   "60-64 years"   "65-69 years"   "70-74 years"   "75-79 years"  
# [15] "80+ years"     "All ages"      "Under 5 years"

# The categories we want are:
# 0-4
# 5-14
# 15-29
# 30-44
# 45-59
# 60-69
# 70-79
# 80+

levels(gbd.2010$age_name) <- 
	c("15-29", "15-29", "15-29", "30-44", "30-44", "30-44", "45-59", "5-14", "45-59", "45-59", 
		"60-69", "60-69", "70-79", "70-79", "80+", "All ages", "0-4")

# [1] "15-29"    "30-44"    "45-59"    "5-14"     "60-69"    "70-79"    "80+"      "All ages" "0-4"     

# Recode disease levels in the disease factor
# levels(gbd.2010$cause_medium)
#  [1] "Alzheimers disease"            "Asthma"                        "Breast cancer"                
#  [4] "Colorectal cancer"             "COPD"                          "Diabetes"                     
#  [7] "Hemorrhagic stroke"            "Hypertensive heart disease"    "Ischemic heart disease"       
# [10] "Ischemic stroke"               "Lower respiratory infections"  "Lung cancer"                  
# [13] "Other respiratory diseases"    "Other road injury"             "Pedestrian road injury"       
# [16] "Rheumatic heart disease"       "Road injury"                   "Stroke"                       
# [19] "Unipolar depressive disorders" "Upper respiratory infections"  

levels(gbd.2010$cause_medium) <- c("Alzheimers disease", "Asthma and COPD", "Breast cancer", "Colorectal cancer", "Asthma and COPD", "Diabetes", "Isch Stroke", "Hypertensive HD", "IHD", "Hemor Stroke", "Upper and lower respiratory", "Lung cancer", "Asthma and COPD", "Road injury", "Road injury", "Rheumatic HD", "Road injury", "Stroke", "Depression", "Upper and lower respiratory")

#  [1] "Alzheimers disease"          "Asthma and COPD"             "Breast cancer"               "Colorectal cancer"          
#  [5] "Diabetes"                    "Isch Stroke"                 "Hypertensive HD"             "IHD"                        
#  [9] "Hemor Stroke"                "Upper and lower respiratory" "Lung cancer"                 "Road injury"                
# [13] "Rheumatic HD"                "Stroke"                      "Depression"   

# Create a new table using the updated categories to paste into excel
agg.results <- with(gbd.2010, tapply(nm_mean, list(cause_medium, age_name, sex, measure), sum, na.rm = TRUE))

# Write the output to a csv file
write.csv(as.data.frame(agg.results), "GBD2010_clean.csv", row.names = FALSE)