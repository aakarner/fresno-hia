Step-by-step instructions for HIA completion
============================================

This guide provides a schematic of the data preparation required to assess the physical activity-related health impacts of regional transportation plans in your jurisdiction. The implementation contained in this GitHub repository relies on several R scripts and an Excel sheet. The procedure is currently limited to the US, but in principle if similar data are available in another country or jurisdiction the procedure could be easily adapted. Because the specific data for each jurisdiction must be located and processed differently, these instructions focus on the generalities of data preparation rather than the specifics of this particular implementation.  

Prepare public health data
--------------------------
* [Global Burden of Disease](http://ghdx.healthmetricsandevaluation.org/global-burden-disease-study-2010-gbd-2010-data-downloads)

The Global Burden of Disease Study estimated rates of morbidity and mortality by disease type and age-sex category for individual countries. It includes estimates of premature mortality (years of life lost) and years living with disability. The sum of these is known as disability adjusted life years (DALYs). 

Use these data to prepare death rates per 100,000 residents and DALYs in each age-sex category for each disease of interest.

* [California Department of Public Health Vital Statistics Data](http://www.cdph.ca.gov/data/statistics/Pages/DeathStatisticalDataTables.aspx)

The CDPH has datasets available for purchase that record every death in the state including cause and demographic information. 

Crosswalk diseases from the GBD to match the CDPH's disease categories. Calculate death rates per 100,000 individuals in your jurisdiction (region, county, city). Calculate a relative risk (RR) of death for your jurisdiction relative to the US totals from the GBD study. Use the calculated RR to adjust the GBD's DALYs. A baseline estimate of DALYs for Fresno County is contained in the Excel sheet "GBDUS"

* [California Health Interview Survey](http://healthpolicy.ucla.edu/chis/Pages/default.aspx)

The California Health Interview Survey is used to estimate the amount of non-transport physical activity undertaken by the population. Calculate median values of non-transport (occupational and exercise) physical activity in units of MET hours/week.

Prepare transport physical activity data
----------------------
* [California Household Travel Survey](http://www.dot.ca.gov/hq/tsip/otfa/tab/chts_travelsurvey.html)

The CHTS is used to establish baseline, age-sex specific measures of active travel behavior. The excel sheet "Active transport" contains overall and relative measures of walking and cycling times and distances for the baseline and all scenarios. You must make assumptions regarding the change in distribution of active transport time with increasing physical activity and embed those assumptions in the relative spread for each scenario. Typically one would assume that the relative rates converge as active travel increases.

The relative distributions are used as input to the Excel worksheets "Baseline" and "Scenario." These sheets use the population distribution for your jurisdiction and overall mean values of active transportation time to develop lognormal distributions of active travel time in each age sex category. The quintile of each distribution is assigned a MET value for active travel. 

Calculate incidence of disease
------------------------------

Each disease of interest has its own green-colored worksheet. Within each sheet, the baseline (rows 4-21) and scenario (rows 23-39) conditions are compared for each age-sex-physical activity quintile category. 

In the baseline condition, the overall rates of morbidity and mortality are reproduced and distributed between the age-sex quintiles. In the scenario condition, overall physical activity is increased, and the relative risk of disease is reduced assuming a linear relationship between the log relative risk and the square root of MET hours per week. This approach is consistent with guidance in Woodcock et al. (2009). 

An attributable fraction and corresponding new burden of disease is calculated from the new relative risk. This is applied to each age-sex quintile, reducing morbidity and mortality accordingly. Differences between the baseline and scenario are calculated and displayed. 

All health benefits are summed and displayed on the "Health results" worksheet.

If the user wishe to add additional diseases, they must create a new disease worksheet by copying one of the existing ones, update the links for RR of 1 MET, deaths, YLLs, and YLD per group, and the denominator for the proportion (row 41).

Conduct analysis
----------------

See the "Instructions" worksheet for further details on extracting quantified improvements in health outcomes from the spreadsheet model.

References
----------
Woodcock, J., Edwards, P., Tonne, C., Armstrong, B.G., Ashiru, O., Banister, D., Beevers, S., Chalabi, Z., Chowdhury, Z., Cohen, A., Franco, O.H., Haines, A., Hickman, R., Lindsay, G., Mittal, I., Mohan, D., Tiwari, G., Woodward, A., Roberts, I., 2009. Public health benefits of strategies to reduce greenhouse-gas emissions: Urban land transport. The Lancet 374(9705), 1930-1943.