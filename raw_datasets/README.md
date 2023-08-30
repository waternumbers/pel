This folder contains script which set up the data sets used within the vignette.

simulation.R - generates data for the simulation study
who.R - packages the WHO data
WHO-COVID-19-global-data.csv - WHO global data downloaded from https://covid19.who.int/WHO-COVID-19-global-data.csv on 2023-08-30 15:30 UTC



Download link: https://covid19.who.int/WHO-COVID-19-global-data.csv
Field name	Type	Description
Date_reported	Date	Date of reporting to WHO
Country_code	String	ISO Alpha-2 country code
Country	String	Country, territory, area
WHO_region	String	WHO regional offices: WHO Member States are grouped into six WHO regions -- Regional Office for Africa (AFRO), Regional Office for the Americas (AMRO), Regional Office for South-East Asia (SEARO), Regional Office for Europe (EURO), Regional Office for the Eastern Mediterranean (EMRO), and Regional Office for the Western Pacific (WPRO).
New_cases	Integer	New confirmed cases. Calculated by subtracting previous cumulative case count from current cumulative cases count.*
Cumulative_cases	Integer	Cumulative confirmed cases reported to WHO to date.
New_deaths	Integer	New confirmed deaths. Calculated by subtracting previous cumulative deaths from current cumulative deaths.*
Cumulative_deaths	Integer	Cumulative confirmed deaths reported to WHO to date.
* Users should note that, in addition to capturing new cases and deaths reported on any given day, updates are made retrospectively to correct counts on previous days as needed based on subsequent information received. See "Daily aggregate case and death count data" above for further details on the calculation of new cases/deaths.
