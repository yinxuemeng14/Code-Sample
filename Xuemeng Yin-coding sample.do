*************************************************************************************
* Coding sample: Here shows some coding part  in the project
* Program: sample.do
* Purpose: Replicate from R and IV regression
* Date: 03/17/2023
*
* Sections(Shown some lines in each part, not whole):
*     1. Clean FOCUS Files: Carriage (Channel Position) & Geography
*     2. Merge FOCUS Files: Carriage (Channel Position) & Geography
*     3. Merge with Viewership (NLTV)
*     4. Reshape data
*     5. Merge with demographic controls
*     6. First-stage regression specifications varying the FE
*     7. First-stage regression specifications varying the set of demographic controls
*     8. IV
*  
*
* Original Files Used(Not shown):
*     1. Custom - Carriage 2012/5/7-12-31.csv
*     2. Custom - Geography Served 2012/5/7.csv
*     3. 2012_2015_2017 County-Level Data.cs f
*     4. demographic.csv
*     5. zipcodepopulation.csv
*
* Files Created:
*     1. 2012/5/7 & channelp.dta
*     2. 01view.dta
*     3. 02reshape.dta
*     4. 03demographic.dta
*     5. 04firststage.dta
*************************************************************************************

clear all
set more off
cap log close
ssc install estout, replace
ssc install winsor, replace
cap log close
ssc install unique, replace
ssc install reghdfe
ssc install ivreghdfe

if "`c(username)'"=="Branda"  {
	global basedir "/Users/Branda/Dropbox/Fox News Effect on Policing Attitudes (RA)"
	global raw = "$basedir/Data/Raw/Nielsen/"
	global clean = "$basedir/Data/Clean/Nielsen/Ash/"
	global clean_cces = "$basedir/Data/Clean/"
	global replication = "$basedir/Data/Clean/Replication/Nielsen/Ash/"
	global tables = "$basedir/Output/Tables/Nielsen/"
	global figures = "$basedir/Output/Figures/Nielsen/Ash/"
}

log using "Nielsen.log", replace
local date 02132023

********************************************************************************
*  2. Merge FOCUS Files: Carriage (Channel Position) & Geography
********************************************************************************
					
gl list 2 5 7

gl news2		"CNN"
gl news5		"FXNC"	
gl news7		"MNBC"							
gl label 2 5 7

scalar counter=0

foreach i of num $list{
    foreach j of num $label{	

        gl newstype  news`j'
        use "${clean}201`i'Carriage.dta",clear
        
        replace heid=subinstr(heid," ","",.)
        replace netstncallletters=subinstr(netstncallletters," ","",.)
        rename netstncallletters news
		replace devicename=subinstr(devicename ," ","",.)
		rename lineupchannelposition channelp
        drop stationpureaffiliationcode
		
		* Following Martin 2017 to get Ordinal Channel Position Variable:
		* 1. Drop HD channels (last two letters are HD), PPV (pay per view), BLACKOUT, Split 
		*  channels (this has no influence for keeping the main news channel at one time)
		* 2. Drop "Community Specific" and "Unique Situation" devices
		drop if devicename == "DIGITALCOMMUNITYSPECIFIC"| devicename=="DIGITALUNIQUESITUATION" 
		drop if strpos(netstnname, "HD")>0
		drop if strpos(news, "PPV") >0 | strpos(news, "BLACKOUT") >0
		
		* Following Martin 2017, to get a unique HEID / Channel Position take the following steps:
		* 1. The same New Channel may show up multiple times with equal channel position if it's 
		*    on different broadcast devices. In that case, keep the "digital device" one.
		* 2. If the channel positions are different across the duplicates, then keep the smallest
		*    channel number. 
        gsort heid news channelp -devicename
        bys heid news: gen dgroup = _n
        keep if dgroup==1
        drop dgroup
		
		* Following Martin 2017, create a variable denoting if the HEID has both FXNC & MSNBC(= 4), 
		*   just MSNBC (=3), just FXNC (=2), or neither (=1).
		gen ttemchan_config=.
		replace ttemchan_config=0
		bys  heid: replace ttemchan_config=2 if news=="FXNC"
		bys  heid: replace ttemchan_config=3 if news=="MNBC"
		bys  heid: egen tchan_config=sum(ttemchan_config)
		replace tchan_config=4 if tchan_config==5
		replace tchan_config=1 if tchan_config==0
		
		* Generate an indicator variables denoting if the HEID has FXNC, MSNBC, and CNN
		for X in any FXNC MNBC CNN: gen temp_X = 1 if news=="X"
		for X in any FXNC MNBC CNN: bys heid: egen has_X = max(temp_X)
		for X in any FXNC MNBC CNN: replace has_X = 0 if has_X==.
		drop temp_FXNC temp_MNBC temp_CNN
		
		* Construct an Ordinal Channel Position (e.g. if the first channel on the HEID is 9701,
		*   and Fox News is on 9721 with no gaps between them, then it becomes channel 21).
		bysort heid (channelp): gen ochannelp = _n      
		bysort heid: gen nchannel = _N
		
		* Reduce dataset to just the News Network of Focus (CNN, FXNC, MNBC)
		keep if news=="$$newstype"
       

		* Now Merge with the Geography Served Data. Notes:
        *  1. For the FXNC 2012 sample, there are 63,066 matches.
		*     There are 1,856 cases where there a geography but not channel (i.e. no Fox there)
		*     And 3 cases where we have channel position by no geography served (HEIDs: 21994/6/8)
		merge 1:m heid using "${clean}201`i'Geography.dta"
		
		
		* Adjust the names of 3 counties here, as they have both names and will be repeated later
		replace county="RICHMOND,VA" if county== "RICHMOND IND,VA"
        replace county="ST LOUIS,MO" if county== "ST LOUIS IND,MO"
		replace county="BALTIMORE,MD" if county == "BALTIMORE IND,MD"
        keep if _merge==3
        drop _merge
		
        * Merge with Census data on population by Zip Code (ACS 5-year estimates)
        merge m:1 zipcode county using "${clean}zip_to_county.dta"

		
		* Now we'll investigate the cases that don't merge. 
		*  1. First we drop areas with pop but no data
		*  2. Then we look at _merge = 1 (6,676 cases in the first loop). We'll drop both too.
		*     a) Cases where Census registers the area as having pop = 0. 
		*     b) There are 297 cases where Nielsen gave a placeholder Zip (000-1).
		drop if _merge!=3
        drop _merge
        
        * Since Martin 2017 had zip-code level viewership data, we need to follow Ash 2022 to 
		*   aggregate to the county level. The steps include:
		*  1. If there are multiple channel positions within a zipcode (implying it's served by
		*     multiple HEIDs), we take the unweighted average across these positions.
		*  2. We can then drop collapse to the zip-code level (since channel position is now
		*     the same across the zip code).
		*  3. To decide which line to keep, we'll keep the HEID that serves the highest pop (across
		*     all zipcodes it serves) since that was the rule used in Martin to decide which HEID 
		*     to preserve (rather than average across HEIDs in a zip code). This allows us to 
		*     carry around the ordinal Channel Position variable (and number of channels on the
		*     HEID) to match the rules by Martin used for that variable. 
	    bys county zipcode: egen mchannelp = mean(channelp)
		drop channelp
		rename mchannelp channelp
		
		bys heid: egen heidtotal = total(zip_pop)
        gsort county zipcode -heidtotal
        bys county zipcode : gen zipgroup = _n
        keep if zipgroup==1
        drop zipgroup 
        
		* To aggregate to the county level, we follow Ash 2022 weighted (by zip code population) avg
		bys county: egen countytotal = total(zip_pop)
		gen p=zip_pop/countytotal
		
		* Ash 2022: Unadjusted Channel Position
		gen wchannelp=channelp*p
        bys county (zipcode): egen avechannelp = total(wchannelp)
	
	    * Martin 2017: Ordinal Channel Position
		gen wochannelp=ochannelp*p
        bys county (zipcode): egen aveochannelp = total(wochannelp)
		
		* Keep one line per county (use the largest Zip in that county for this step)
		gsort county -zip_pop
        bys county : gen cgroup = _n
        keep if cgroup==1
        drop cgroup		
		
		* Finally, let's construct the share of the County with access to this channel
		gen share = countytotal/county_pop
		disp `$newstype'
		sum share
		
        save "${clean}201`i'channelp$$newstype.dta", replace
    }
    use "${clean}201`i'channelpCNN.dta"
    append using "${clean}201`i'channelpFXNC.dta" ///
                 "${clean}201`i'channelpMNBC.dta", force
    gen year=201`i'
    save "${clean}201`i'channelp.dta", replace
}

use "${clean}2012channelp.dta", clear
append using "${clean}2015channelp.dta" ///
             "${clean}2017channelp.dta", force
replace news="MSNBC" if news=="MNBC"


save "${clean}channelp.dta", replace


********************************************************************************
*   4. Reshape Data
********************************************************************************
		   
gl reshape total countytotal* zipcode tchan_config diaryonly nchannel rtg shr imp hutput ///
  intab sow share avechannelp aveochannelp
		   
use "${clean}01view.dta", clear
*fre newsn

keep astate county ucounty newsn year total countytotal* zipcode ///
     rtg shr imp hutput intab sow share avechannelp aveochannelp ///
     chan_config cchan_config tchan_config diaryonly nchannel county_pop

preserve
keep if year==2012
reshape wide $reshape , i(county)  j(newsn)
save "${clean}2012reshape.dta", replace
restore

preserve
keep if year==2015
reshape wide $reshape , i(county)  j(newsn)
save "${clean}2015reshape.dta", replace
restore

keep if year==2017
reshape wide $reshape , i(county)  j(newsn)
append using "${clean}2015reshape.dta" "${clean}2012reshape.dta", force	
save "${clean}02reshape.dta",replace

********************************************************************************
*   5. Demographics
********************************************************************************
import delimited "${raw}original_demo_zips/state.csv", clear 
rename v1 statename
save "${clean}demo/state.dta",replace

ssc install getcensus, replace
ssc install jsonio, replace
getcensus catalog, product(ST) clear


* These will all use ACS 5-year estimates from 2010. 
global key = "1758f65294f0e76b37676ea66827527207d04b90"


*%foodstamps 
*S2201_C02_001 total hh total hh, S2201_C01_001 recieving foodstamps
*https://data.census.gov/table?q=foodstamp&g=010XX00US$0500000&y=2010&tid=ACSST5Y2010.S2201
getcensus  S2201_C02_001 S2201_C01_001, geography(county) years(2010) sample(5) key($key) clear
gen foodstamps = s2201_c02_001e / s2201_c01_001e
keep state county name foodstamps
save "${clean}demo/05foodstamps.dta", replace


*%education
*nohs:no high school education
*somecollege:<1year, no degree, associate degree
*college:enter college
*S1501_C01_002E no high school, S1501_C01_003E highschool
*S1501_C01_005E bachelor+, S1501_C01_004E college or associate degree
*S1501_C01_006E total 25+, S1501_C01_011E associate degree
*S1501_C01_012E bachelor, S1501_C01_013E graduate or professional
*https://data.census.gov/table?q=education&g=010XX00US$0500000&y=2010&tid=ACSST5Y2010.S1501
getcensus S1501_C01_002 S1501_C01_005 S1501_C01_011 S1501_C01_004 S1501_C01_003 ///
S1501_C01_012 S1501_C01_013, geography(county) years(2010) sample(5) key($key) clear
gen nohs= s1501_c01_002e/100
gen college=(s1501_c01_005e + s1501_c01_011e)/100
gen somecollege= s1501_c01_004e/100
gen hsgrad= s1501_c01_003e/100
gen bach=s1501_c01_012e/100
gen postgrad=s1501_c01_013e/100
keep state county name nohs college hsgrad bach postgrad somecollege
save "${clean}demo/06education.dta", replace



* Merge all of demographic variables

use  "${clean}demo/06education.dta", clear
merge 1:1 state county using "${clean}demo/05foodstamps.dta"
drop _merge

replace state=substr(state,2,.) if substr(state,1,1) =="0"
rename countyname ucounty


* There are duplicates on the merging variable in this master dataset.
* 185 demographic data not matched, 0 viewership not matched, matched: 8977
merge 1:m astate ucounty using "${clean}02reshape.dta"
keep if _merge==3
drop _merge


save "${clean}03demographic.dta", replace

********************************************************************************
* 6. First-stage regression specifications varying the FE
********************************************************************************/
use "${clean}03demographic.dta", clear

* Winsorizing the top and bottom decile for each network. 
foreach X in cnn fox msnbc {
	winsor avechannelp_`X', p(.1) gen (navechannelp_`X') 
	drop avechannelp_`X'
	rename navechannelp_`X' avechannelp_`X'
}

* Normalize RTG Normalize rtg and shr, channel position
for X in any cnn fox msnbc: egen nrtg_X = std(rtg_X)
for Y in any cnn fox msnbc: for X in any ave aveo: egen std_Xchannelp_Y = std(Xchannelp_Y)
for Y in any cnn fox msnbc: for X in any ave aveo: gen neg_std_Xchannelp_Y = -1 * std_Xchannelp_Y
sum std_avechannelp* std_aveochannelp* nrtg*

*demographic controls for Martin 2017
gl martinbasic lpopdensity urban suburban /// 
               asian black hispanic other male age* income ///
               bach somecollege hsgrad postgrad
*adding income, house, food, status, tax, religious and politics
*repubcontribte96 fedral campaign contribute			   
gl martinextensive own medvalue veteran medssa_inc medroom built2005 aggtax ///
             family married unmarried same_sex_couple foodstamps repubvote 		 

* State * Year fixed effects
egen stateyear = group(year astate)
rename county_pop pop
save "${clean}04firststage.dta", replace

* Construct a regression table showing how First Stage varies with controls
* Will construct one using the Standardized Ordinal one and the Std Adjusted one
* Time-varying controls: Ratings for CNN (mins_cnn) and MSNBC (mins_msnbc)+ Share with Access to Fox

eststo M1: reghdfe nrtg_fox neg_std_aveochannelp_fox nrtg_msnbc nrtg_cnn missing_msnbc ///
  missing_cnn share_fox share_cnn share_msnbc [aweight = pop], absorb(stateyear) vce(cl county)
 test neg_std_avechannelp_fox
 estadd scalar FStat = r(F)
 sum nrtg_fox if e(sample)
 estadd scalar DepVarMean=r(mean)
 sum neg_std_avechannelp_fox if e(sample)
 estadd scalar IVarMean=r(mean)

eststo M8: reghdfe nrtg_fox neg_std_aveochannelp_fox nrtg_msnbc nrtg_cnn missing_msnbc ///
  missing_cnn share_fox share_cnn share_msnbc $martinbasic $martinextensive [aweight = pop], ///
  absorb(stateyear) vce(cl county)
 test neg_std_aveochannelp_fox
 estadd scalar FStat = r(F)
 sum nrtg_fox if e(sample)
 estadd scalar DepVarMean=r(mean)
 sum neg_std_aveochannelp_fox if e(sample)
 estadd scalar IVarMean=r(mean)

#d ;
esttab M1 M2 using "${tables}fox_first_stage.tex", replace 
  keep(neg_std_aveochannelp_fox)    
  coeflabel(neg_std_aveochannelp_fox "Fox Channel (Martin 2017)")
  cells(b(star fmt(3)) se(par fmt(3))) legend sty(fixed) nomtitles
  star(* 0.10 ** 0.05 *** 0.01) stat(N r2 FStat DepVarMean IVarMean, 
  fmt(%12.0gc 2 2 2 2) label("N" "\$R^2$"))  
  postfoot(`"State-Year FE? &X&X\\"'
  `"Demographics? &&X \\"'
  `"\hline\hline"' `"\bottomrule"' `"\multicolumn{3}{l}{\footnotesize \sym{*} \(p<0.10\),
  \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)} \\"' `"\end{tabular}"' `"}"') label noabbrev ;
#d cr

* Binscatter of Regression 7 above 
binscatter nrtg_fox neg_std_aveochannelp_fox [aweight = pop], ///
  controls(nrtg_msnbc nrtg_cnn missing_msnbc share_fox share_cnn share_msnbc) absorb(stateyear)
  
********************************************************************************
*   7. Merge with CCES and run pseudo-replication (effect on Rep Identity)
********************************************************************************

* Construct a Reduced-Form table


eststo M1: reghdfe foxnews_watcher neg_std_aveochannelp_fox nrtg_msnbc nrtg_cnn missing_msnbc ///
  missing_cnn share_fox share_cnn share_msnbc $martinbasic $martinextensive ///
  [aweight = pop], absorb(state) vce(cl county)
 test neg_std_aveochannelp_fox
 estadd scalar FStat = r(F)
 sum foxnews_watcher if e(sample)
 estadd scalar DepVarMean=r(mean)
 sum neg_std_aveochannelp_fox if e(sample)
 estadd scalar IVarMean=r(mean)

eststo M2: reghdfe id_repub neg_std_aveochannelp_fox nrtg_msnbc nrtg_cnn missing_msnbc ///
  missing_cnn share_fox share_cnn share_msnbc $martinbasic $martinextensive ///
  [aweight = pop], absorb(state) vce(cl county)
 test neg_std_aveochannelp_fox
 estadd scalar FStat = r(F)
 sum id_repub if e(sample)
 estadd scalar DepVarMean=r(mean)
 sum neg_std_aveochannelp_fox if e(sample)
 estadd scalar IVarMean=r(mean)

eststo M3: reghdfe policing_index neg_std_aveochannelp_fox nrtg_msnbc nrtg_cnn missing_msnbc ///
  missing_cnn share_fox share_cnn share_msnbc num_media faminc_new educ marstat gender id_repub ///
  $martinbasic $martinextensive [aweight = pop], absorb(state) vce(cl county)
 test neg_std_aveochannelp_fox
 estadd scalar FStat = r(F)
 sum policing_index if e(sample)
 estadd scalar DepVarMean=r(mean)
 sum neg_std_aveochannelp_fox if e(sample)
 estadd scalar IVarMean=r(mean)

#d ;
esttab M1 M2 M3 using "${tables}fox_reduced_form.tex", replace 
  keep(neg_std_aveochannelp_fox)    
  coeflabel(neg_std_aveochannelp_fox "Fox Channel Position")
  cells(b(star fmt(3)) se(par fmt(3))) legend sty(fixed) nomtitles
  star(* 0.10 ** 0.05 *** 0.01) stat(N r2 FStat DepVarMean IVarMean, 
  fmt(%12.0gc 2 2 2 2) label("N" "\$R^2$"))    
  mgroups("Fox Watcher (CCES)" "Republican ID" "Policing Index", pattern(1  1  1 )
  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
  postfoot(`"State FE? &X&X&X&X \\"' 
   `"CCES Demographics? &X&X&X&X \\"' 
   `"Martin Demographics? &X&X&X&X\\"' 
   `"Republican ID Control? &&&&X\\"' 
  `"\hline\hline"' `"\bottomrule"' `"\multicolumn{4}{l}{\footnotesize \sym{*} \(p<0.10\),
  \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)} \\"' `"\end{tabular}"' `"}"') label noabbrev ;
#d cr
********************************************************************************
*   8. IV Specification
********************************************************************************

eststo M1: ivreghdfe policing_index (nrtg_fox = neg_std_aveochannelp_fox) nrtg_msnbc ///
  nrtg_cnn missing_msnbc missing_cnn share_fox share_cnn share_msnbc [aweight = pop], ///
  absorb(state) cl(county)
 sum foxnews_watcher if e(sample)
 estadd scalar DepVarMean=r(mean)
 sum nrtg_fox if e(sample)
 estadd scalar IVarMean=r(mean)


eststo M2: ivreghdfe policing_index (nrtg_fox = neg_std_aveochannelp_fox) ///
  nrtg_msnbc nrtg_cnn missing_msnbc ///
  missing_cnn share_fox share_cnn share_msnbc $martinbasic $martinextensive ///
  [aweight = pop], absorb(state) cl(county)
 sum foxnews_watcher if e(sample)
 estadd scalar DepVarMean=r(mean)
 sum nrtg_fox if e(sample)
 estadd scalar IVarMean=r(mean)


#d ;
esttab M1 M2 using "${tables}fox_IV.tex", replace 
  keep(nrtg_fox)   
  coeflabel(nrtg_fox "Fox Viewership (Std)")
  cells(b(star fmt(3)) se(par fmt(3))) legend sty(fixed) nomtitles
  star(* 0.10 ** 0.05 *** 0.01) stat(N r2 DepVarMean IVarMean, 
  fmt(%12.0gc 2 2 2) label("N" "\$R^2$"))  
  mgroups("Fox Watcher (CCES)" "Republican ID" "Policing Index", pattern(1 0 )
  prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
  postfoot(`"State FE? &X&X \\"' 
   `"CCES Demographics? &&X \\"' 
   `"Martin Demographics? &&&X \\"' 
   `"Republican ID Control? &&& \\"' 
  `"\hline\hline"' `"\bottomrule"' `"\multicolumn{3}{l}{\footnotesize \sym{*} \(p<0.10\),
  \sym{**} \(p<0.05\), \sym{***} \(p<0.01\)} \\"' `"\end{tabular}"' `"}"') label noabbrev ;
#d cr
*/

