* Stata code for Group 1. Mean Difference

*net install xbrcsplinei, from(http://stats4life.se/stata) replace

clear all
use http://www.stats4life.se/data/drm/ad_group_2.dta
 gen dose2 = dose^2 
drmeta e_b dose dose2, data(n sd_y) id(id)  type(type) se(e_se)  fixed
drmeta_graph , dose(4(.5)9) ref(7) equation(d d^2) gls  ///
		legend(off)   ysize(4) xsize(4) ///
		xlabel(#10) ///
		ylabel(#10) ///
		ytitle("Mean Difference") ///
		xtitle("Omega-3 Index") name(f_dr_ipd_q_ce, replace)  ///
		title("Common effect DRM " "Quadratic function")  

* Common effect multivariate dose-response model using a quadratic function 

gen dose2 = dose^2 
mvmeta_make ,  saving(_mvmeta_est_ad) replace by(id)  names(b V) : drmeta e_b dose dose2, data(n sd_y) id(id)  type(type) se(e_se)  
 
preserve
use _mvmeta_est_ad, clear 
meta mvregress b* ,  wcovvariables(V*) common
estimates save _mvmeta_est_ad, replace
restore 

estimates use _mvmeta_est_ad

* pick up the values of the two transformations at the chosen reference of 7 units
scalar ref1 = 7
scalar ref2 = 7^2

* this is just to satisfy an internal check of predictnl
gen bdose = dose
gen bdose2 = dose2

* Obtain predicted dose-response function with a confidence interval for all observed values of the dose
* using 7 (median) units as referent 
predictnl e_md_q = _b[bdose:_cons]*(dose-ref1) + _b[bdose2:_cons]*(dose2-ref2), se(e_se_md_q)

gen lbq = e_md_q-1.96*e_se_md_q
gen ubq = e_md_q+1.96*e_se_md_q

twoway (line e_md_q dose , sort lc(black)) ///
	(line lbq ubq dose, sort lp(- -) lc(black black)) ///
		 , ///
		legend(off)   ysize(4) xsize(4) ///
		xlabel(#10) ///
		ylabel(#10) ///
		ytitle("Mean Difference") ///
		xtitle("Omega-3 Index") name(f_dr_ipd_q_ce, replace)  ///
		title("Common effect DRM " "Quadratic function")  

* Common effect dose-response model using restricted cubic splines 

mkspline doses = dose , nk(3) cubic displayknots 
mat knots = r(knots)
mvmeta_make ,  saving(_mvmeta_est_ad) replace by(id)  names(b V) :  drmeta e_b doses1 doses2, data(n sd_y) id(id)  type(type) se(e_se)  
 
preserve
use _mvmeta_est_ad, clear 
meta mvregress b* ,  wcovvariables(V*) common
estimates save _mvmeta_est_ad, replace
restore 

estimates use _mvmeta_est_ad

*quietly xbrcsplinei , matknots(knots) values(0(1)70)  gen(alc)
xbrcsplinei  doses , matknots(knots) values(4(.5)9)  

* pick up the values of the two splines at the chosen reference of 7 units
xbrcsplinei, matknots(knots) values(7)  
scalar ref1 = 7
scalar ref2 = .3247274  

* this is just to satisfy an internal check of predictnl
gen bdoses1 = doses1 
gen bdoses2 = doses2

* Obtain predicted dose-response function with a confidence interval for all observed values of the dose
* using 7 (median) unit as referent 
predictnl e_md_s = _b[bdoses1:_cons]*(doses1-ref1) + _b[bdoses2:_cons]*(doses2-ref2), se(e_se_md_s)

gen lbs =  e_md_s-1.96*e_se_md_s
gen ubs =  e_md_s+1.96*e_se_md_s

sort dose 

twoway (line e_md_s dose , sort lc(black)) ///
	(line lbs ubs dose, sort lp(- -) lc(black black)) ///
		 , ///
		legend(off)   ysize(4) xsize(4) ///
		xlabel(#10) ///
		ylabel(#10) ///
		ytitle("Mean Difference") ///
		xtitle("Omega-3 Index") name(f_dr_ipd_ce, replace) ///
		title("Common effect DRM" "Restricted Cubic Spline")  
		
* Random-effects dose-response model using restricted cubic splines 

preserve
use _mvmeta_est_ad, clear 
meta mvregress b* ,  wcovvariables(V*) random(reml)
estimates save _mvmeta_est_ad, replace
restore 

estimates use _mvmeta_est_ad

* pick up the values of the two splines at the chosen reference of 25 kg/m^2
scalar ref1 = 7
scalar ref2 = .3247274  
predictnl e_md_s_r = _b[bdoses1:_cons]*(doses1-ref1) + _b[bdoses2:_cons]*(doses2-ref2), se(e_se_md_s_r)

gen lbs_r = e_md_s_r-1.96*e_se_md_s_r
gen ubs_r = e_md_s_r+1.96*e_se_md_s_r

twoway (line e_md_s_r dose , sort lc(black)) ///
	(line lbs_r ubs_r dose, sort lp(- -) lc(black black)) ///
		 , ///
		legend(off)   ysize(4) xsize(4) ///
		xlabel(#10) ///
		ylabel(#10) ///
		ytitle("Mean Difference") ///
		xtitle("Omega-3 Index") name(f_dr_ipd_re, replace)  ///
		title("Random-effects DRM" "Restricted Cubic Spline")  
		
		