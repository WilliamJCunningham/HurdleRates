********************************************************************************
* Preamble
********************************************************************************
* Set Base Directory
cd "C:\Users\willi\Desktop\Research\Data and Processing\ReformattedDatai"

* Import Data
clear
import delimited ".\Prepped_Entrepreneur_Data.csv", encoding(ISO-8859-2)
********************************************************************************
* Data Processing
********************************************************************************
* Create Variables
* ID
destring subject_unique_id, generate(SubjectID) ignore(`"_"') percent

* Periods
gen period=set-1
drop if period==0

* Hurdle Indicators
gen HighHurdle=0
replace HighHurdle=1 if hurdle_rate==125
gen MidHurdle=0
replace MidHurdle=1 if hurdle_rate==95

* Search Indicators
gen HighSearch=0
replace HighSearch=1 if search_cost==1.5
gen MidSearch=0
replace MidSearch=1 if search_cost==.5

* Outcome Variables
* Lottery Choice Features
destring mean, generate(ExProfit) ignore(`"NA"') percent
destring var, generate(ExVar) ignore(`"NA"') percent

* Outside Option
gen OutOpt=0
replace OutOpt=1 if option==0
sort hurdle_rate search_cost period

* Average Searches by Period
egen AvgSearch=mean(number_of_searches), by(hurdle_rate search_cost period)

* Average Choice of Outside Option by Period
egen AvgOutOpt=mean(OutOpt), by(hurdle_rate search_cost period)

* Tag
egen newval=tag(hurdle_rate search_cost period)

* Late Period Indicator
gen last5=0
replace last5=1 if period>10

* Second Half Indicator
gen late = 0
replace late = 1 if period > 7

* Payoffs Manager and Investor and Metrics Based on Actual Selection vs. Random Choice, vs. Max Expected Value
foreach var of varlist payoff_investor_no_h-payoff_manager_high_h_diff {
    gen temp = real(`var')
    drop `var'
    rename temp `var'
}
********************************************************************************
* Create Tables and Graphs
********************************************************************************
* Create Summary Stats Tables
* Table of Key Outcome Variables Excluding Outside Option Choices by Hurdle Rate and Search Cost
table (hurdle_rate search_cost) if option > 0, ///
	nototals ///
    statistic(mean number_of_searches) ///
    statistic(mean profit) ///
    statistic(mean ExProfit) ///
    statistic(mean ExVar)
	
collect export Table5.tex, replace
* Table of Outside Option Choices by Hurdle Rate and Search Cost
table (hurdle_rate search_cost), statistic(mean OutOpt)

collect export Table6.tex, replace

*first vs. last half :

table (hurdle_rate search_cost) if option > 0 & period < 7, ///
	nototals ///
    statistic(mean number_of_searches) ///
    statistic(mean profit) ///
    statistic(mean ExProfit) ///
    statistic(mean ExVar)
	
	table (hurdle_rate search_cost) if option > 0 & period > 7, ///
	nototals ///
    statistic(mean number_of_searches) ///
    statistic(mean profit) ///
    statistic(mean ExProfit) ///
    statistic(mean ExVar)
* Create Graphs

*****************
* Search Behavior
*****************

* No Hurdle Rate, Low v. High Search Cost
twoway  (line AvgSearch period if newval==1 & hurdle_rate==0 & search_cost==.5, lcolor(blue)) (line AvgSearch period if newval==1 & hurdle_rate==0 & search_cost==1.5), ytitle(Number of Searches) xtitle(Period) xscale(range(1 15)) xmtick(minmax) title(No Hurdle Rate) legend(on order(1 "Low Search Cost" 2 "High Search Cost")) scheme(s1color)

graph export ".\SearchNoHurd.eps", replace as(eps) name("Graph") preview(off)

* Medium Hurdle Rate, Low v. High Search Cost
twoway  (line AvgSearch period if newval==1 & hurdle_rate==95 & search_cost==.5, lcolor(blue))(line AvgSearch period if newval==1 & hurdle_rate==95 & search_cost==1.5), ytitle(Number of Searches) xtitle(Period) xscale(range(1 15)) xmtick(minmax) title(Medium Hurdle Rate) legend(on order(1 "Low Search Cost" 2 "High Search Cost")) scheme(s1color)

graph export ".\SearchMedHurd.eps", replace as(eps) name("Graph") preview(off)

* High Hurdle Rate, Low v. High Search Cost
twoway  (line AvgSearch period if newval==1 & hurdle_rate==125 & search_cost==.5, lcolor(blue)) (line AvgSearch period if newval==1 & hurdle_rate==125 & search_cost==1.5), ytitle(Number of Searches) xtitle(Period) xscale(range(1 15)) xmtick(minmax) title(High Hurdle Rate) legend(on order(1 "Low Search Cost" 2 "High Search Cost")) scheme(s1color)

graph export ".\SearchHighHurd.eps", replace as(eps) name("Graph") preview(off)

******************
* Opt Out Behavior
******************

* Low Hurdle Rate, Average Choice of Outside Option
twoway  (line AvgOutOpt period if newval==1 & hurdle_rate==0 & search_cost==.5, lcolor(blue)) (line AvgOutOpt period if newval==1 & hurdle_rate==0 & search_cost==1.5), ytitle(Number of Out Opt) xtitle(Period) xscale(range(1 15)) xmtick(minmax) title(No Hurdle Rate) legend(on order(1 "Low Search Cost" 2 "High Search Cost")) scheme(s1color)

graph export ".\OONoHurd.eps", replace as(eps) name("Graph") preview(off)

* Medium Hurdle Rate, Average Choice of Outside Option
twoway  (line AvgOutOpt period if newval==1 & hurdle_rate==95 & search_cost==.5, lcolor(blue)) (line AvgOutOpt period if newval==1 & hurdle_rate==95 & search_cost==1.5), ytitle(Number of Out Opt) xtitle(Period) xscale(range(1 15)) xmtick(minmax) title(Medium Hurdle Rate) legend(on order(1 "Low Search Cost" 2 "High Search Cost")) scheme(s1color)

graph export ".\OOMedHurd.eps", replace as(eps) name("Graph") preview(off)

* High Hurdle Rate, Average Choice of Outside Option
twoway  (line AvgOutOpt period if newval==1 & hurdle_rate==125 & search_cost==.5, lcolor(blue)) (line AvgOutOpt period if newval==1 & hurdle_rate==125 & search_cost==1.5), ytitle(Number of Out Opt) xtitle(Period) xscale(range(1 15)) xmtick(minmax) title(High Hurdle Rate) legend(on order(1 "Low Search Cost" 2 "High Search Cost")) scheme(s1color)

graph export ".\OOHighHurd.eps", replace as(eps) name("Graph") preview(off)

********************************************************************************
* Regressions
********************************************************************************
* Set ID and Period Variables
xtset SubjectID period

*****************
* Search Behavior
*****************

* Search With Choice of Outside Option Excluded

* High Search Cost Impact Among No Hurdle Rate Treatments
xtreg  number_of_searches HighSearch last5 if hurdle_rate==0 & option>0, re vce(cluster SubjectID) 
outreg2 using Table1, tex(pr) dec(3) replace

* High Search Cost Impact Among Medium Hurdle Rate Treatments
xtreg  number_of_searches HighSearch last5 if hurdle_rate==95 & option>0, re vce(cluster SubjectID)
outreg2 using Table1, tex(pr) dec(3)

* High Search Cost Impact Among High Hurdle Rate Treatments
xtreg  number_of_searches HighSearch last5 if hurdle_rate==125 & option>0, re vce(cluster SubjectID)
outreg2 using Table1, tex(pr) dec(3)

* Search With Choice of Outside Option Included

* High Search Cost Impact Among No Hurdle Rate Treatments
xtreg  number_of_searches HighSearch last5 if hurdle_rate==0 , re vce(cluster SubjectID)
outreg2 using Table1, tex(pr) dec(3) 

* High Search Cost Impact Among Medium Hurdle Rate Treatments
xtreg  number_of_searches HighSearch last5 if hurdle_rate==95, re vce(cluster SubjectID) 
outreg2 using Table1, tex(pr) dec(3)

* High Search Cost Impact Among High Hurdle Rate Treatments
xtreg  number_of_searches HighSearch last5 if hurdle_rate==125, re vce(cluster SubjectID) 
outreg2 using Table1, tex(pr) dec(3)

* Search With Only Choice of Outside Option Included

* No Hurdle Rate Saw No Choices of Outside Option and is Omitted 

* High Search Cost Impact Among Medium Hurdle Rate Treatments
xtreg  number_of_searches HighSearch last5 if hurdle_rate==95 & option==0, re vce(cluster SubjectID)
outreg2 using Table1, tex(pr) dec(3) 

* High Search Cost Impact Among High Hurdle Rate Treatments
xtreg  number_of_searches HighSearch last5 if hurdle_rate==125 & option==0, re vce(cluster SubjectID)
outreg2 using Table1, tex(pr) dec(3)

******************
* Opt Out Behavior
******************

* No Hurdle Rate Saw No Choices of Outside Option and is Omitted 

* High Search Cost Impact Among Medium Hurdle Rate Treatments
xtreg  OutOpt HighSearch last5 if hurdle_rate==95 & search_cost!=0, re vce(cluster SubjectID) 
outreg2 using Table2, tex(pr) dec(3) replace

* High Search Cost Impact Among High Hurdle Rate Treatments
xtreg  OutOpt HighSearch last5 if hurdle_rate==125& search_cost!=0, re vce(cluster SubjectID) 
outreg2 using Table2, tex(pr) dec(3)

* High Hurdle Rate Impact Among Low Search Cost Treatments
xtreg  OutOpt HighHurdle last5 if search_cost==.5 & hurdle_rate!=0, re vce(cluster SubjectID) 
outreg2 using Table2, tex(pr) dec(3) replace

* High Hurdle Rate Impact Among Low Search Cost Treatments
xtreg  OutOpt HighHurdle last5 if search_cost==1.5 & hurdle_rate!=0, re vce(cluster SubjectID) 
outreg2 using Table2, tex(pr) dec(3)

*****************
* Choice Outcomes
*****************

*Hurdle Rates

* Impact of Hurdle Rates On Expected Outcome Among Low Search Cost Sets, No Outside Option Choices
xtreg  ExProfit HighHurdle MidHurdle last5  if search_cost== .5 & option>0, re vce(cluster SubjectID)
outreg2 using Table3, tex(pr) dec(3) replace

* Impact of Hurdle Rates On Expected Outcome Among High Search Cost Sets, No Outside Option Choices
xtreg  ExProfit HighHurdle MidHurdle last5  if search_cost== 1.5 & option>0, re vce(cluster SubjectID)
outreg2 using Table3, tex(pr) dec(3)

* Impact of Hurdle Rates On Varience Outcome Among Low Search Cost Sets, No Outside Option Choices
xtreg  ExVar HighHurdle MidHurdle last5 if search_cost== .5 & option>0, re vce(cluster SubjectID)
outreg2 using Table3, tex(pr) dec(3)

* Impact of Hurdle Rates On Varience Outcome Among High Search Cost Sets, No Outside Option Choices
xtreg  ExVar HighHurdle MidHurdle last5 if search_cost== 1.5 & option>0, re vce(cluster SubjectID)
outreg2 using Table3, tex(pr) dec(3)

* Impact of High Search Cost On Expected Outcome Among Low Search Cost Sets, No Outside Option Choices
xtreg  ExProfit HighSearch MidSearch last5 if hurdle_rate==0 & option>0, re vce(cluster SubjectID) 
outreg2 using Table3, tex(pr) dec(3)

*Search Costs

* Impact of Search Costs On Expected Outcome Among No Hurdle Rate Treatments, No Outside Option Choices
xtreg  ExProfit HighSearch MidSearch last5 if hurdle_rate==0 & option>0, re vce(cluster SubjectID)
outreg2 using Table4, tex(pr) dec(3) replace

* Impact of Search Costs On Expected Outcome Among Medium Hurdle Rate Treatments, No Outside Option Choices
xtreg  ExProfit HighSearch MidSearch last5 if hurdle_rate==95 & option>0, re vce(cluster SubjectID)
outreg2 using Table4, tex(pr) dec(3)

* Impact of Search Costs On Expected Outcome Among High Hurdle Rate Treatments, No Outside Option Choices
xtreg  ExProfit HighSearch MidSearch last5 if hurdle_rate==125 & option>0, re vce(cluster SubjectID)
outreg2 using Table4, tex(pr) dec(3)

* Impact of Search Costs On Varience Outcome Among No Hurdle Rate Treatments, No Outside Option Choices
xtreg  ExVar HighSearch MidSearch last5 if hurdle_rate==0 & option>0, re vce(cluster SubjectID) 
outreg2 using Table4, tex(pr) dec(3) 

* Impact of Search Costs On Varience Outcome Among Medium Hurdle Rate Treatments, No Outside Option Choices
xtreg  ExVar HighSearch MidSearch last5 if hurdle_rate==95 & option>0, re vce(cluster SubjectID)
outreg2 using Table4, tex(pr) dec(3)

* Impact of Search Costs On Varience Outcome Among High Hurdle Rate Treatments, No Outside Option Choices
xtreg  ExVar HighSearch MidSearch last5 if hurdle_rate==125 & option>0, re vce(cluster SubjectID)
outreg2 using Table4, tex(pr) dec(3)

********************************************************************************

*Direct Questions Of 2 17:

* How often do subjects select the highest expected value option?

* Expected outcome
gen MaxEVChosen = (ExProfit == mean_max) if OutOpt != 1

* By Hurdle Rate
tabstat MaxEVChosen if OutOpt != 1, by(hurdle_rate) stat(mean n)

* By Search Cost
tabstat MaxEVChosen if OutOpt != 1, by(search_cost) stat(mean n)

* Number Of Searches
tabstat MaxEVChosen if OutOpt != 1, by(number_of_searches) stat(mean n)

* As a Regression
xtprobit MaxEVChosen i.hurdle_rate##HighSearch i.hurdle_rate##MidSearch if option>0, re vce(cluster SubjectID)


* How often do subjects select the option that is best for them?
*Expected Value

* No Hurdle
gen MaxPayoffChosenNoH = (payoff_manager_no_h == payoff_manager_no_h_max) if OutOpt != 1 & hurdle_rate==0

* By Search Cost
tabstat MaxPayoffChosenNoH if OutOpt != 1, by(search_cost) stat(mean n)

* Number Of Searches
tabstat MaxPayoffChosenNoH if OutOpt != 1, by(number_of_searches) stat(mean n)

* As a Regression
xtprobit MaxPayoffChosenNoH HighSearch MidSearch if option>0, re vce(cluster SubjectID)


* Mid Hurdle
gen MaxPayoffChosenLowH = (payoff_manager_low_h == payoff_manager_low_h_max) if OutOpt != 1 & hurdle_rate==95

* By Search Cost
tabstat MaxPayoffChosenLowH if OutOpt != 1, by(search_cost) stat(mean n)

* Number Of Searches
tabstat MaxPayoffChosenLowH if OutOpt != 1, by(number_of_searches) stat(mean n)

* As a Regression
xtprobit MaxPayoffChosenLowH HighSearch MidSearch if option>0, re vce(cluster SubjectID)

* High Hurdle

gen MaxPayoffChosenHighH = (payoff_manager_high_h == payoff_manager_high_h_max) if OutOpt != 1 & hurdle_rate==125

* By Search Cost
tabstat MaxPayoffChosenHighH if OutOpt != 1, by(search_cost) stat(mean n)

* Number Of Searches
tabstat MaxPayoffChosenHighH if OutOpt != 1, by(number_of_searches) stat(mean n)

* As a Regression
xtprobit MaxPayoffChosenHighH HighSearch if option>0, re vce(cluster SubjectID)

* For all!
*collapse best choice to one column
egen MaxPayoffChosen = rowfirst(MaxPayoffChosenNoH MaxPayoffChosenLowH MaxEVChosen)

xtprobit MaxPayoffChosen i.hurdle_rate##HighSearch i.hurdle_rate##MidSearch if option>0, re vce(cluster SubjectID)


* How often do individuals opt out?

 * By Hurdle Rate
tabstat OutOpt, by(hurdle_rate) stat(mean n)

* By Search Cost
tabstat OutOpt, by(search_cost) stat(mean n)

* Number Of Searches
tabstat OutOpt, by(number_of_searches) stat(mean n)

* As a Regression
xtprobit OutOpt i.hurdle_rate##HighSearch, re vce(cluster SubjectID)


* Late period effects?

xtprobit MaxEVChosen i.hurdle_rate##HighSearch##late i.hurdle_rate##MidSearch if option>0, re vce(cluster SubjectID)

xtprobit MaxPayoffChosen i.hurdle_rate##HighSearch##late i.hurdle_rate##MidSearch if option>0, re vce(cluster SubjectID)

xtprobit OutOpt i.hurdle_rate##HighSearch##late, re vce(cluster SubjectID)



