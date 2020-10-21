clear
clear matrix
cd "I:\Github_Maxtor\ata-london-infra\"

	* PREPARE DISTANCE VARIABLES
		import delimited "02_DataCreated\1-route-mf-2020\2_routedistance_to_filter.csv", varnames(1) clear 
		gen within_500m=(routedist<0.5)
		gen within_1km=(routedist<1)
		foreach x in 500m 1km {
		bysort startid: egen num_`x'=sum(within_`x')
		}
		
		bysort startid: egen mindist=min(routedist)
		recode mindist 1/max=1
		
		keep startid num_500m num_1km mindist
		rename startid oa11cd
		duplicates drop
		save "02_DataCreated\x-temp\oa_vars.dta", replace
		
	* MERGE
		import delimited "01_DataInput\cents_london_oa\London_OA_list.csv", varnames(1) clear 	
		merge 1:1 oa11cd using "02_DataCreated\x-temp\oa_vars.dta", nogen // check no _m==2
		foreach x in 500m 1km {
		recode num_`x' .=0
		}
		recode mindist .=1
		export delimited using "02_DataCreated\1-route-mf-2020\3_num_filters_from_centroid.csv", replace
