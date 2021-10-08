# EU Electricity Market Panel

This project was done for my Applied Econometrics class at the Toulouse School of Economics in collaboration with Moongyeom Kim.

## Overview

This project uses a panel approach to analyze the effect of renewable electricity penetration on balancing market outcomes in 20 EU member states. This data was scraped from the ENTSO-e transparency platform in the form of unstructured XML data. Regular expression and xml techniques were used to parse the data into a structured format. 5 years of hourly/sub-hourly data for over 20 countries was scraped, parsed, and aggregated. Renewable electricity penetration is measured through the real time percentage of renewable power in the supply stack, and the main balancing market outcome studied is balancing volume (Refer to the project paper for a overview on electricity balancing markets). The panel model includes several other relevant exogenous variables including forecast and real time load (demand), and imports/exports among others. 

## Data Sources
The data is sourced from the European Network of Transmission System Operators for Electricity (ENTSO-e) transparency database. The data is downloaded with a HTTP based API, which returns XML files containing the data. The database can be found here.

## Tools
The paper was built using Latex. The coding was done in R using the tidyverse, XML, stringr, plm, stargazer, sandwich, stats, forecast, and tseries packages.
