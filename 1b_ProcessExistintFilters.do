clear
clear matrix
cd "C:\Users\Anna\AnnaDesktop\centroid-route"

	* PREPARE DISTANCE VARIABLES
		import delimited "02_DataCreated\1_route-existing-filters\2_routedistance_to_filter.csv", varnames(1) clear 
		gen within_500m=(routedist<0.5)
		gen within_1km=(routedist<1)
		gen within_2km=(routedist<2)
		foreach x in 500m 1km 2km {
		bysort startid: egen num_`x'=sum(within_`x')
		}
		
		bysort startid: egen mindist=min(routedist)
		recode mindist 2/max=2
		
		keep startid num_500m num_1km num_2km mindist
		rename startid lsoa11cd
		duplicates drop
		save "02_DataCreated\x-temp\lsoa_vars.dta", replace
		
	* MERGE
		import delimited "01_DataInput\london_lsoa_cents\London_lsoa_list.csv", varnames(1) clear
		rename ïlsoa11cd lsoa11cd
		merge 1:1 lsoa11cd using "02_DataCreated\x-temp\lsoa_vars.dta", nogen
		foreach x in 500m 1km 2km {
		recode num_`x' .=0
		}
		recode mindist .=2
		export delimited using "02_DataCreated\1_route-existing-filters\3_lsoa_distance_existing.csv", replace
