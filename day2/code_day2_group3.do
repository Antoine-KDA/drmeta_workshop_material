* Stata code for Group 3. Hazard Ratio

*net install xbrcsplinei, from(http://stats4life.se/stata) replace

clear all
use http://www.stats4life.se/data/drm/ad_group_3.dta

* Common effect multivariate dose-response model using a quadratic function 

* Quick way using drmeta and its post-estimation command drmeta_graph 
gen dose2 = dose^2 
drmeta e_b dose dose2, data(PT cases) id(id)  type(type) se(e_se)  fixed

drmeta_graph , dose(0(.5)7) ref(3) equation(d d^2) gls  eform ///
		legend(off)   ysize(4) xsize(4) ///
		xlabel(#10) ///
		ylabel(#10) ///
		ytitle("Mean Difference") ///
		xtitle("Alcohol Consumption, drinks/week") name(f_dr_ipd_q_ce, replace)  ///
		title("Common effect DRM " "Quadratic function") yscale(log)

mkspline doses = dose , nk(3) cubic displayknots 
mat knots = r(knots)
drmeta e_b doses1 doses2, data(PT cases) id(id)  type(type) se(e_se) reml

drmeta_graph , dose(0(.5)7) ref(3) matknots(knots)  noci eform ///
		legend(off)   ysize(4) xsize(4) ///
		xlabel(#10) ///
		ylabel(#10) ///
		ytitle("Hazard Ratio") ///
	    xtitle("Alcohol Consumption, drinks/week")    ///
		title("Random-effects DRM" "Restricted Cubic Spline")  yscale(log) blup
		
* Longer way of doing the two-stage approach
mvmeta_make ,  saving(_mvmeta_est_ad) replace by(id)  names(b V) : drmeta e_b dose dose2, data(PT case) id(id)  type(type) se(e_se)  
 
preserve
use _mvmeta_est_ad, clear 
meta mvregress b* ,  wcovvariables(V*) common
estimates save _mvmeta_est_ad, replace
restore 

estimates use _mvmeta_est_ad

* pick up the values of the two transformations at the chosen reference of 7 units
scalar ref1 = 3
scalar ref2 = 3^2

* this is just to satisfy an internal check of predictnl
gen bdose = dose
gen bdose2 = dose2

* Obtain predicted dose-response function with a confidence interval for all observed values of the dose
* using 3 (median) units as referent 
predictnl e_lnhr_q = _b[bdose:_cons]*(dose-ref1) + _b[bdose2:_cons]*(dose2-ref2), se(e_se_md_q)

gen hrq = exp(e_lnhr_q)
gen lbq = exp(e_lnhr_q-1.96*e_se_md_q)
gen ubq = exp(e_lnhr_q+1.96*e_se_md_q)

twoway (line hrq dose , sort lc(black)) ///
	(line lbq ubq dose, sort lp(- -) lc(black black)) ///
		 , ///
		legend(off)   ysize(4) xsize(4) ///
		xlabel(#10) ///
		ylabel(#10) ///
		ytitle("Hazard Ratio") ///
		xtitle("Alcohol Consumption, drinks/week") name(f_dr_ipd_q_ce, replace)  ///
		title("Common effect DRM " "Quadratic function")  yscale(log)


* Common effect dose-response model using restricted cubic splines 

mkspline doses = dose , nk(3) cubic displayknots 
mat knots = r(knots)
mvmeta_make ,  saving(_mvmeta_est_ad) replace by(id)  names(b V) :  drmeta e_b doses1 doses2, data(PT case) id(id)  type(type) se(e_se)  
 
preserve
use _mvmeta_est_ad, clear 
meta mvregress b* ,  wcovvariables(V*) common
estimates save _mvmeta_est_ad, replace
restore 

estimates use _mvmeta_est_ad

xbrcsplinei  doses , matknots(knots) values(0(1)7)  

* pick up the values of the two splines at the chosen reference of 3 units
xbrcsplinei, matknots(knots) values(3)  

scalar ref1 = 3
scalar ref2 =  .149266  

* this is just to satisfy an internal check of predictnl
capture drop bdoses1 bdoses2
gen bdoses1 = doses1 
gen bdoses2 = doses2

* Obtain predicted dose-response function with a confidence interval for all observed values of the dose
* using 3 (median) unit as referent 
predictnl e_lnhr_s = _b[bdoses1:_cons]*(doses1-ref1) + _b[bdoses2:_cons]*(doses2-ref2), se(e_se_md_s)

gen hrs = exp(e_lnhr_s )
gen lbs = exp(e_lnhr_s-1.96*e_se_md_s)
gen ubs = exp(e_lnhr_s+1.96*e_se_md_s)

sort dose 

twoway (line hrs dose , sort lc(black)) ///
	(line lbs ubs dose, sort lp(- -) lc(black black)) ///
		 , ///
		legend(off)   ysize(4) xsize(4) ///
		xlabel(#10) ///
		ylabel(#10) ///
		ytitle("Hazard Ratio") ///
	    xtitle("Alcohol Consumption, drinks/week") name(f_dr_ipd_ce, replace) ///
		title("Common effect DRM" "Restricted Cubic Spline")   
		
* Random-effects dose-response model using restricted cubic splines 

preserve
use _mvmeta_est_ad, clear 
meta mvregress b* ,  wcovvariables(V*) random(reml)
estimates save _mvmeta_est_ad, replace
restore 

estimates use _mvmeta_est_ad

* pick up the values of the two splines at the chosen reference of 25 kg/m^2
scalar ref1 = 3
scalar ref2 = .149266   
predictnl e_lnhr_s_r = _b[bdoses1:_cons]*(doses1-ref1) + _b[bdoses2:_cons]*(doses2-ref2), se(e_se_md_s_r)

gen hrs_r = exp(e_lnhr_s_r)
gen lbs_r = exp(e_lnhr_s_r -1.96*e_se_md_s_r)
gen ubs_r = exp(e_lnhr_s_r+1.96*e_se_md_s_r)

twoway (line hrs_r dose , sort lc(black)) ///
	(line lbs_r ubs_r dose, sort lp(- -) lc(black black)) ///
		 , ///
		legend(off)   ysize(4) xsize(4) ///
		xlabel(#10) ///
		ylabel(#10) ///
		ytitle("Hazard Ratio") ///
	    xtitle("Alcohol Consumption, drinks/week")  name(f_dr_ipd_re, replace)  ///
		title("Random-effects DRM" "Restricted Cubic Spline")  yscale(log)
		
		
