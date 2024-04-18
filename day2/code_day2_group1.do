* Stata code for Group 1. Odds Ratios 

*net install xbrcsplinei, from(http://stats4life.se/stata) replace

clear all
use http://www.stats4life.se/data/drm/ad_group_1.dta

* Common effect multivariate dose-response model using a quadratic function 

gen dose2 = dose^2 
mvmeta_make ,  saving(_mvmeta_est_ad) replace by(id)  names(b V) : drmeta e_b dose dose2 , data(n cases) id(id)  type(type) se(e_se) 
 
preserve
use _mvmeta_est_ad, clear 
meta mvregress b* ,  wcovvariables(V*) common
estimates save _mvmeta_est_ad, replace
restore 

estimates use _mvmeta_est_ad

* pick up the values of the two transformations at the chosen reference of 25 kg/m^2
scalar ref1 = 25
scalar ref2 = 25^2

* this is just to satisfy an internal check of predictnl
gen bdose = dose
gen bdose2 = dose2

* Obtain predicted dose-response function with a confidence interval for all observed values of the dose
* using 25 (median) kg/m^2 as referent 
predictnl e_md_q = _b[bdose:_cons]*(dose-ref1) + _b[bdose2:_cons]*(dose2-ref2), se(e_se_md_q)

gen mdq = exp(e_md_q)
gen lbq = exp(e_md_q-1.96*e_se_md_q)
gen ubq = exp(e_md_q+1.96*e_se_md_q)

sort dose 
list dose mdq lbq ubq 

twoway (line mdq dose , sort lc(black)) ///
	(line lbq ubq dose, sort lp(- -) lc(black black)) ///
		 , ///
		legend(off)   ysize(4) xsize(4) ///
		xlabel(#10) ///
		ylabel(1 2 4 8) ///
		ytitle("Odds Ratio") ///
		xtitle("Body Mass Index, kg/m^2") name(f_dr_ipd_q_ce, replace) yscale(log) ///
		title("Common effect DRM " "Quadratic function") yscale(range(0.5 10))

* Common effect dose-response model using restricted cubic splines 

mkspline doses = dose , nk(3) cubic displayknots 
mat knots = r(knots)
mvmeta_make ,  saving(_mvmeta_est_ad) replace by(id)  names(b V) : drmeta e_b doses1 doses2 , data(n cases) id(id)  type(type) se(e_se) 
 
preserve
use _mvmeta_est_ad, clear 
meta mvregress b* ,  wcovvariables(V*) common
estimates save _mvmeta_est_ad, replace
restore 

estimates use _mvmeta_est_ad

* pick up the values of the two splines at the chosen reference of 25 kg/m^2
xbrcsplinei, matknots(knots) values(25)  
scalar ref1 = 25
scalar ref2 = .5010287

* this is just to satisfy an internal check of predictnl
gen bdoses1 = doses1 
gen bdoses2 = doses2

* Obtain predicted dose-response function with a confidence interval for all observed values of the dose
* using 25 (median) kg/m^2 as referent 
predictnl e_md_s = _b[bdoses1:_cons]*(doses1-ref1) + _b[bdoses2:_cons]*(doses2-ref2), se(e_se_md_s)

gen mds = exp(e_md_s)
gen lbs = exp(e_md_s-1.96*e_se_md_s)
gen ubs = exp(e_md_s+1.96*e_se_md_s)

sort dose 
list dose mds lbs ubs 

twoway (line mds dose , sort lc(black)) ///
	(line lbs ubs dose, sort lp(- -) lc(black black)) ///
		 , ///
		legend(off)   ysize(4) xsize(4) ///
		xlabel(#10) ///
		ylabel(1 2 4 8) ///
		ytitle("Odds Ratio") ///
		xtitle("Body Mass Index, kg/m^2") name(f_dr_ipd_ce, replace) yscale(log) ///
		title("Common effect DRM" "Restricted Cubic Spline") yscale(range(0.5 10))

* Random-effects dose-response model using restricted cubic splines 

preserve
use _mvmeta_est_ad, clear 
meta mvregress b* ,  wcovvariables(V*) random(reml)
estimates save _mvmeta_est_ad, replace
restore 

estimates use _mvmeta_est_ad

* pick up the values of the two splines at the chosen reference of 25 kg/m^2
xbrcsplinei, matknots(knots) values(25)  
scalar ref1 = 25
scalar ref2 = .5010287

predictnl e_md_s_r = _b[bdoses1:_cons]*(doses1-ref1) + _b[bdoses2:_cons]*(doses2-ref2), se(e_se_md_s_r)

gen mds_r = exp(e_md_s_r)
gen lbs_r = exp(e_md_s_r-1.96*e_se_md_s_r)
gen ubs_r = exp(e_md_s_r+1.96*e_se_md_s_r)

sort dose 
list dose mds_r lbs_r ubs_r

twoway (line mds_r dose , sort lc(black)) ///
	(line lbs_r ubs_r dose, sort lp(- -) lc(black black)) ///
		 , ///
		legend(off)   ysize(4) xsize(4) ///
		xlabel(#10) ///
		ylabel(1 2 4 8) ///
		ytitle("Odds Ratio") ///
		xtitle("Body Mass Index, kg/m^2") name(f_dr_ipd_re, replace) yscale(log) ///
		title("Random-effects DRM" "Restricted Cubic Spline") yscale(range(0.5 10))
		
		