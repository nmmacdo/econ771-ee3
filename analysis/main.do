* Project: ECON 771 Empirical Exercise 3
* Date Created: November 3, 2022
* Last Edited: November 6, 2022
* Name: Noah MacDonald

* This script requires four external packages. Uncomment below to install.
// ssc install estout, replace
// ssc install rdrobust, replace
// ssc install rddensity, replace
// ssc install lpdensity, replace

* Preamble statements
clear
// eststo clear
set more off
set varabbrev off

* Setting working directory
cd "C:\Users\noahm\OneDrive - Emory University\Grad\5 Emory Fall 2022\ECON 771 Health Economics II\assignment-3"

**********************************************
* Subsidy data prep following Ericson (2014) *
**********************************************

***Prepare some supplemental data
		use "data/Data_subsidyinfo.dta", replace
			reshape long s, i(PDPregion) j(year)
			sort PDPregion year
			tempfile theSubsidies
		save `theSubsidies', replace

**********************************************
* General data prep following Ericson (2014) *
**********************************************

	use "data/Data_main.dta", clear
	
	***The var "isin" will tell me whether that plan existed in 200x
		gen isin = 1
	
	***Now, create information about companies
		***Create a numeric version of the text string; useful for stata
		egen firmID = group(orgParentCode)
		egen uniqueIDNum = group(uniqueID)
		
			***Create variables that show when a firm began offering a plan
				egen firstYrExist_F0 = min(year),by(firmID)
				label var firstYrExist_F0 "The first year this plan's company offered a plan"
						
				gen thisPlansExist_F0 = 1 if year >firstYrExist_F0 
				replace thisPlansExist_F0 =0 if thisPlansExist_F0 !=1
				label var thisPlansExist_F0  "=1 if company previously offerred a plan in any state,=0 if else"
				
				***Want to know the first year that company offered a plan in that state.
				egen firstYrExistState_F0 = min(year),by(firmID state)
				label var firstYrExistState_F0 "The first year this plan's company offered a plan, firm def 0"
					
				gen thisPlansExistState_F0 = 1 if year >firstYrExistState_F0 
				replace thisPlansExistState_F0 =0 if thisPlansExistState_F0 !=1
				label var thisPlansExistState_F0  "=1 if company (def 0) previously offerred a plan in any state,=0 if else"
		
	***Generate descriptive cohort variables
		egen minYear = min(year),by(uniqueID)
		
		foreach yr in 2006 2007 2008 2009 2010{
			gen cohort`yr' = 1 if minYear == `yr'
		}
	
		gen cohort = .
		foreach x in 2006 2007 2008 2009 2010{
			replace cohort = `x' if cohort`x'==1
		}
		
		replace cohort2006=0 if cohort2006 !=1
		replace cohort2007=0 if cohort2007 !=1
		replace cohort2008=0 if cohort2008 !=1
		replace cohort2009=0 if cohort2009 !=1
		replace cohort2010=0 if cohort2010 !=1
	
	
		***Create the year of plan's existence
		gen yearOfPlan = year-cohort+1
		replace yearOfPlan = . if yearOfPlan<1 
	
		tab year, generate(Dyear)
		tab yearOfPlan, generate(DOfPlan)
		rename DOfPlan1 _DOfPlan1
		rename Dyear1 _Dyear1
		egen maxYear = max(year),by(uniqueID)
	
		***Indicator for if a plan is basic every year until YR
		gen isBasic = 1 if benefit=="B"
		replace isBasic = 0 if benefit!="B"
		egen minIsBasic = min(isBasic),by(uniqueID)
	
		foreach yr in 2007 2008 2009 2010{
			gen isBasic`yr'= isBasic
			replace isBasic`yr'=1 if year >`yr'
			egen minIsBasic`yr' = min(isBasic`yr'),by(uniqueID)
			drop isBasic`yr'
		} 
	
	***Handle Premium
		label var premium "Monthly Premium"
		gen lnPremium = ln(premium)
	
	***Create Variables Describing plan detail
		***Various types of basic plans
			gen DS = 1 if btypedetail =="DS"
			replace DS = 0 if btypedetail !="DS"
			gen AE = 1 if btypedetail =="AE"
			replace AE = 0 if btypedetail !="AE"
		
		foreach y in 2006 2007 2008 2009 2010{ 
			gen AE`y' = 1 if year == `y' & btypedetail =="AE"
			replace AE`y'= 0 if AE`y'!=1
			gen DS`y' = 1 if year == `y' & btypedetail =="DS"
			replace DS`y'= 0 if DS`y'!=1
		}	
	
		***Create the interaction between BA and deductible amount
		***AE and DS always have the same deductible (varies by year). 
		****But BA may have varying deductibles
		
		***Generate Deductible Groups
			gen cDeduct0 = 1 if deductible ==0
			gen cDeduct1_50 = 1 if deductible >0 & deductible<=50
			gen cDeduct51_100 = 1 if deductible >50 & deductible<=100
			gen cDeduct101_150 = 1 if deductible >100 & deductible<=150
			gen cDeduct151_200 = 1 if deductible >150 & deductible<=200
			gen cDeduct201_250 = 1 if deductible >200 & deductible<=250
			gen cDeduct251_300 = 1 if deductible >250 & deductible<=300
			gen cDeduct301_ = 1 if deductible >300 & deductible !=.
		
			foreach x in cDeduct0  cDeduct1_50   cDeduct51_100  cDeduct101_150 cDeduct151_200 cDeduct201_250 cDeduct251_300 cDeduct301_{
				replace `x' = 0 if `x' ==.
			}
			
			foreach x in cDeduct0  cDeduct1_50   cDeduct51_100  cDeduct101_150 cDeduct151_200 cDeduct201_250 cDeduct251_300 cDeduct301_{
				foreach y in 2006 2007 2008 2009 2010{ 
					gen `x'`y' = `x' if year == `y'
					replace `x'`y'= 0 if  year != `y'
				}
			}
			assert cDeduct0  + cDeduct1_50 +   cDeduct51_100   + cDeduct101_150  + cDeduct151_200 +  cDeduct201_250 + cDeduct251_300+cDeduct301_==1 if deductible !=.
		
			***And focus only on the BA plans
			foreach x in cDeduct0  cDeduct1_50   cDeduct51_100  cDeduct101_150 cDeduct151_200 cDeduct201_250 cDeduct251_300 cDeduct301_{
				foreach y in 2006 2007 2008 2009 2010{ 
					gen BA`x'`y' = `x' if year == `y' & btypedetail=="BA"
					replace BA`x'`y'= 0 if  BA`x'`y'==.
				}
			}
	
			
	****For lagged data analysis, create lagged plan type/deductible groups
		xtset uniqueIDNum year, yearly
		sort uniqueIDNum year
		***Stata lags can't handle string variables lagged. Must do manually
			gen L4btypedetail=btypedetail[_n-4] if uniqueID==uniqueID[_n-4] & year == 2010 
			gen L3btypedetail=btypedetail[_n-3] if uniqueID==uniqueID[_n-3] & (year == 2010 | year == 2009) 
			gen L2btypedetail=btypedetail[_n-2] if uniqueID==uniqueID[_n-2] & (year == 2010 | year == 2009 | year ==2008)
			gen L1btypedetail=btypedetail[_n-1] if uniqueID==uniqueID[_n-1] & (year == 2010 | year == 2009 | year ==2008 | year==2007)
			gen L0btypedetail=btypedetail
			
			gen L4benefit=benefit[_n-4] if uniqueID==uniqueID[_n-4] & year == 2010 
			gen L3benefit=benefit[_n-3] if uniqueID==uniqueID[_n-3] & (year == 2010 | year == 2009) 
			gen L2benefit=benefit[_n-2] if uniqueID==uniqueID[_n-2] & (year == 2010 | year == 2009 | year ==2008)
			gen L1benefit=benefit[_n-1] if uniqueID==uniqueID[_n-1] & (year == 2010 | year == 2009 | year ==2008 | year==2007)
			gen L0benefit=benefit
	
		
		foreach L in 0 1 2 3 4{
			***For basic types only
			gen L`L'DS = 1 if L`L'btypedetail =="DS"
			replace L`L'DS = 0 if L`L'btypedetail !="DS"
			gen L`L'AE = 1 if L`L'btypedetail =="AE"
			replace L`L'AE = 0 if L`L'btypedetail !="AE"
		
			****BUT BA may have varying deductibles
			gen  L`L'BA_0 = 1 if  L`L'.deductible ==0 & L`L'btypedetail =="BA"
			gen  L`L'BA_1_99 = 1 if L`L'.deductible >0 & L`L'.deductible<100 & L`L'btypedetail =="BA"
			gen  L`L'BA_100 = 1 if L`L'.deductible==100 & L`L'btypedetail =="BA"
			gen  L`L'BA_101_99 = 1 if L`L'.deductible >100 & L`L'.deductible<200 & L`L'btypedetail =="BA"
			gen  L`L'BA_200_49 = 1 if L`L'.deductible >=200 & L`L'.deductible<250 & L`L'btypedetail =="BA"
			gen  L`L'BA_250Up = 1 if L`L'.deductible >=250 & L`L'.deductible!=. & L`L'btypedetail =="BA"
	
			foreach x in L`L'BA_0 L`L'BA_1_99 L`L'BA_100 L`L'BA_101_99 L`L'BA_200_49 L`L'BA_250Up{
				replace `x' = 0 if `x' !=1
			}
		
			***Make sure everyone is in a category
			egen x = rowtotal(L`L'DS-L`L'BA_250Up)
			assert x==1 if L`L'benefit =="B" & L`L'btypedetail!=""
			drop x
		}
	
	***Work with Enrollment Data
		egen double stateYrEnroll =sum(enrollment),by(state year)
		gen share = enrollment/stateYrEnroll
		gen double lnS = log(share)
		
		gen enrollmentNonLIS = enrollment-enrollmentLIS
		egen double stateYrEnrollNonLIS =sum(enrollmentNonLIS),by(state year)
		gen shareNonLIS = enrollmentNonLIS/stateYrEnrollNonLIS
		gen double lnSNonLIS = log(shareNonLIS)
		
		***When LIS enrollment is missing b/c <10, it is imputed to be 5
		gen enrollmentNonLISimpute = enrollment-enrollmentLISimpute
		egen double stateYrEnrollNonLISimpute =sum(enrollmentNonLISimpute),by(state year)
		
		gen shareNonLISimpute = enrollmentNonLISimpute/stateYrEnrollNonLISimpute
		gen double lnS_std = log(shareNonLISimpute)
		
*****************************************
* RD data prep following Ericson (2014) *
*****************************************

	sort PDPregion year
	merge PDPregion year using `theSubsidies'
	assert _merge==3
	drop _merge
	rename s LISsubsidy

	gen LISPremium =  premium - LISsubsidy
	***Not all proposed plans are actually such
	gen proposedBenchmarkPlan = 1 if LISPremium <= 0
	replace proposedBenchmarkPlan = 0 if  proposedBenchmarkPlan != 1
		
	sum LISPremium, detail
	sum LISPremium if LIS==0, detail
	sum LISPremium if LIS==1, detail
	****SOME MISCATEGORIZATION 
	generate ProblemObs =1 if LISPremium < 0 & LIS == 0
	replace ProblemObs =2 if LISPremium > 0 & LIS == 1
	tab ProblemObs
	***Why positive premiums when LIS == 1? DEMINIMUM PROVISION: note that this problem only occurs in 2007+
		sum LISPremium if ProblemObs==2, detail 
		tab ProblemObs year
	***Why Negative premiums when seemingly not eligible? 
		sum LISPremium if ProblemObs==1, detail 
		tab benefit if  ProblemObs==1
		tab btypedetail if  ProblemObs==1
	****FIX: Not eligible for LIS if benefit == E
		replace LISPremium = . if benefit == "E"
		replace proposedBenchmarkPlan =.  if benefit == "E" 
		
	***Polynomials
		gen LISPremiumSq = LISPremium*LISPremium
		gen LISPremiumCub= LISPremium*LISPremium*LISPremium
		gen LISPremiumQuart= LISPremium*LISPremium*LISPremium*LISPremium
		***Interacted with Status
		gen LISPremiumSq_IS = LISPremium*LISPremium*LIS
		gen LISPremiumCub_IS= LISPremium*LISPremium*LISPremium*LIS
		gen LISPremiumQuart_IS= LISPremium*LISPremium*LISPremium*LISPremium*LIS
		
		gen premiumSq= premium*premium
		gen premiumCub= premium*premium*premium
		gen premiumQuart =premium*premium*premium*premium

************************************
* Question 2: Replicating Figure 3 *
************************************

rdplot lnS LISPremium if LISPremium >= -10 & LISPremium <= 10 & year==2006, ///
c(0) nbins(20 20) support(-10 10) graph_options(graphregion(color(white)) ///
legend(off) xtitle("Monthly premium − LIS subsidy, 2006") ///
ytitle("log enrollment share, 2006"))

graph export "results/question2.png", replace

*******************************************************
* Question 3: Replicating Figure 3 with J=10 and J=40 *
*******************************************************

rdplot lnS LISPremium if LISPremium >= -10 & LISPremium <= 10 & year==2006, ///
c(0) nbins(10 10) support(-10 10) graph_options(graphregion(color(white)) ///
legend(off) xtitle("Monthly premium − LIS subsidy, 2006") ///
ytitle("log enrollment share, 2006"))

graph export "results/question3a.png", replace

rdplot lnS LISPremium if LISPremium >= -10 & LISPremium <= 10 & year==2006, ///
c(0) nbins(40 40) support(-10 10) graph_options(graphregion(color(white)) ///
legend(off) xtitle("Monthly premium − LIS subsidy, 2006") ///
ytitle("log enrollment share, 2006"))

graph export "results/question3b.png", replace

*************************************
* Question 4: Evenly-spaced RD Plot *
*************************************

rdplot lnS LISPremium if LISPremium >= -10 & LISPremium <= 10 & year==2006, ///
c(0) binselect(es) support(-10 10) graph_options(graphregion(color(white)) ///
legend(off) xtitle("Monthly premium − LIS subsidy, 2006") ///
ytitle("log enrollment share, 2006"))

graph export "results/question4.png", replace

**********************************
* Question 5: Manipulation Tests *
**********************************

// help rddensity
rddensity LISPremium, plot

graph export "results/question5.png", replace

***********************************
* Question 6: Table 3 Replication *
***********************************

/* Note: rdrobust doesn't seem to give us all the values we want. Uncomment
below to see what we get out of the box. */ 
// rdrobust lnS LISPremium if year==2006, c(0) p(1) h(4) vce(cluster firmID)

*** Step 1: Constructing variables for manual RD estimates

* Only looking at observations that are at most 4 away from the cutoff
gen RDwindow = 1 if LISPremium >= -4 & LISPremium <= 4
replace RDwindow = 0 if RDwindow != 1
* Marking observations below the cutoff
gen belowBench = 1 if LISPremium <=0 & RDwindow == 1
replace belowBench = 0 if LISPremium >0 & RDwindow == 1
* Focusing in on 2006 status
gen belowBench2006Temp = 1 if belowBench==1 & year ==2006
	replace belowBench2006Temp =0 if belowBench2006Temp!=1
gen RDwindow2006Temp = 1 if RDwindow==1 & year ==2006
	replace RDwindow2006Temp =0 if RDwindow2006Temp!=1
* Making clean variables per uniqueid
egen belowBench2006=max(belowBench2006Temp), by(uniqueID)
egen RDwindow2006=max(RDwindow2006Temp), by(uniqueID)
* Creating LISPremium mirror variables above/below the cutoff
gen LISPremiumNeg =LISPremium if LISPremium<=0
	replace LISPremiumNeg = 0 if LISPremium>0
gen LISPremiumPos =LISPremium if LISPremium>=0
	replace LISPremiumPos = 0 if LISPremium<0
* And their squared values
foreach x in Neg Pos{
	gen LISPremium`x'Sq = LISPremium`x'*LISPremium`x'
}
* And setting as a panel so we can use Stata's lag syntax
xtset uniqueIDNum year, yearly

*** Step 2: Running regressions for Panel A
eststo clear
eststo: quietly regress lnS belowBench2006 LISPremiumNeg LISPremiumPos ///
if year == 2006 & RDwindow2006 == 1, vce(cluster firmID)
eststo: quietly regress lnS belowBench2006 L1.LISPremiumNeg L1.LISPremiumPos ///
if year == 2007 & RDwindow2006 == 1, vce(cluster firmID)
eststo: quietly regress lnS belowBench2006 L2.LISPremiumNeg L2.LISPremiumPos ///
if year == 2008 & RDwindow2006 == 1, vce(cluster firmID)
eststo: quietly regress lnS belowBench2006 L3.LISPremiumNeg L3.LISPremiumPos ///
if year == 2009 & RDwindow2006 == 1, vce(cluster firmID)
eststo: quietly regress lnS belowBench2006 L4.LISPremiumNeg L4.LISPremiumPos ///
if year == 2010 & RDwindow2006 == 1, vce(cluster firmID)
* and displaying them
esttab, b(3) se(3) r2 nomtitles compress star(* 0.10 ** 0.05 *** 0.01)
eststo clear

*** Step 3: Running regressions for Panel B
eststo clear
eststo: quietly regress lnS belowBench2006 LISPremiumNeg LISPremiumPos ///
LISPremiumNegSq LISPremiumPosSq if year == 2006 & RDwindow2006 == 1, ///
vce(cluster firmID)
eststo: quietly regress lnS belowBench2006 L1.LISPremiumNeg L1.LISPremiumPos ///
L1.LISPremiumNegSq L1.LISPremiumPosSq if year == 2007 & RDwindow2006 == 1, ///
vce(cluster firmID)
eststo: quietly regress lnS belowBench2006 L2.LISPremiumNeg L2.LISPremiumPos ///
L2.LISPremiumNegSq L2.LISPremiumPosSq if year == 2008 & RDwindow2006 == 1, ///
vce(cluster firmID)
eststo: quietly regress lnS belowBench2006 L3.LISPremiumNeg L3.LISPremiumPos ///
L3.LISPremiumNegSq L3.LISPremiumPosSq if year == 2009 & RDwindow2006 == 1, ///
vce(cluster firmID)
eststo: quietly regress lnS belowBench2006 L4.LISPremiumNeg L4.LISPremiumPos ///
L4.LISPremiumNegSq L4.LISPremiumPosSq if year == 2010 & RDwindow2006 == 1, ///
vce(cluster firmID)
* and displaying them
esttab, b(3) se(3) r2 nomtitles compress star(* 0.10 ** 0.05 *** 0.01) ///
keep(belowBench2006)
eststo clear

********************************
* Question 7: Table 3 w/ ES BW *
********************************

*** Step 1: Getting CE-optimal BWs for linear/quadratic models with clustered SE
rdbwselect lnS LISPremium, c(0) p(1) bwselect(cerrd) vce(cluster firmID)
* 3.685 on either side of the cutoff for the linear model
rdbwselect lnS LISPremium, c(0) p(2) bwselect(cerrd) vce(cluster firmID)
* 5.393 on either side of the cutoff for the quadratic model

*** Step 2: Creating new RDwindow variabes for these optimal bandwidths
* For linear model
gen RDwindow7a = 1 if LISPremium >= -3.685 & LISPremium <= 3.685
replace RDwindow7a = 0 if RDwindow != 1
gen RDwindow7a2006Temp = 1 if RDwindow7a==1 & year ==2006
	replace RDwindow7a2006Temp =0 if RDwindow7a2006Temp!=1
egen RDwindow7a2006=max(RDwindow7a2006Temp), by(uniqueID)
* For quadratic model
gen RDwindow7b = 1 if LISPremium >= -5.393 & LISPremium <= 5.393
replace RDwindow7b = 0 if RDwindow != 1
gen RDwindow7b2006Temp = 1 if RDwindow7b==1 & year ==2006
	replace RDwindow7b2006Temp =0 if RDwindow7b2006Temp!=1
egen RDwindow7b2006=max(RDwindow7b2006Temp), by(uniqueID)

*** Step 3: Running regressions for Panel A
eststo clear
eststo: quietly regress lnS belowBench2006 LISPremiumNeg LISPremiumPos ///
if year == 2006 & RDwindow7a2006 == 1, vce(cluster firmID)
eststo: quietly regress lnS belowBench2006 L1.LISPremiumNeg L1.LISPremiumPos ///
if year == 2007 & RDwindow7a2006 == 1, vce(cluster firmID)
eststo: quietly regress lnS belowBench2006 L2.LISPremiumNeg L2.LISPremiumPos ///
if year == 2008 & RDwindow7a2006 == 1, vce(cluster firmID)
eststo: quietly regress lnS belowBench2006 L3.LISPremiumNeg L3.LISPremiumPos ///
if year == 2009 & RDwindow7a2006 == 1, vce(cluster firmID)
eststo: quietly regress lnS belowBench2006 L4.LISPremiumNeg L4.LISPremiumPos ///
if year == 2010 & RDwindow7a2006 == 1, vce(cluster firmID)
* and displaying them
esttab, b(3) se(3) r2 nomtitles compress star(* 0.10 ** 0.05 *** 0.01)
esttab using "results/table7a.tex", replace booktabs compress nogaps ///
b(3) se(3) r2 star(* 0.10 ** 0.05 *** 0.01) nonumber nomtitles 
eststo clear

*** Step 4: Running regressions for Panel B
eststo clear
eststo: quietly regress lnS belowBench2006 LISPremiumNeg LISPremiumPos ///
LISPremiumNegSq LISPremiumPosSq if year == 2006 & RDwindow7b2006 == 1, ///
vce(cluster firmID)
eststo: quietly regress lnS belowBench2006 L1.LISPremiumNeg L1.LISPremiumPos ///
L1.LISPremiumNegSq L1.LISPremiumPosSq if year == 2007 & RDwindow7b2006 == 1, ///
vce(cluster firmID)
eststo: quietly regress lnS belowBench2006 L2.LISPremiumNeg L2.LISPremiumPos ///
L2.LISPremiumNegSq L2.LISPremiumPosSq if year == 2008 & RDwindow7b2006 == 1, ///
vce(cluster firmID)
eststo: quietly regress lnS belowBench2006 L3.LISPremiumNeg L3.LISPremiumPos ///
L3.LISPremiumNegSq L3.LISPremiumPosSq if year == 2009 & RDwindow7b2006 == 1, ///
vce(cluster firmID)
eststo: quietly regress lnS belowBench2006 L4.LISPremiumNeg L4.LISPremiumPos ///
L4.LISPremiumNegSq L4.LISPremiumPosSq if year == 2010 & RDwindow7b2006 == 1, ///
vce(cluster firmID)
* and displaying them
esttab, b(3) se(3) r2 nomtitles compress star(* 0.10 ** 0.05 *** 0.01) ///
keep(belowBench2006)
esttab using "results/table7b.tex", replace booktabs compress nogaps b(3) ///
se(3) r2 star(* 0.10 ** 0.05 *** 0.01) nonumber nomtitles keep(belowBench2006)
eststo clear