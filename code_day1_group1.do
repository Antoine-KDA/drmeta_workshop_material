* Stata code for Group 1. Odds Ratios 

clear all
use http://www.stats4life.se/data/drm/ipd_group_1.dta

* Part 1
tabstat x y , by(id)
*twoway (histogram x), by(id)

* Study-specific adjusted estimated regression coefficients 
statsby e_b = _b[x] e_se = _se[x] , by(id): logit y x c
list id e_b e_se

* Meta-analysis 
meta set e_b e_se , common
meta summarize , eform 
scalar e_beta = r(theta)
scalar e_se_beta = r(se)

* Forest plot 
meta forestplot , eform name(f_cm_ipd, replace) 

* Dose-response plot of the common odds ratio using 25 as referent

capture drop x 
range x 20 35 
gen q_025 = exp(e_beta*(x-25)+invnormal(.025)*sqrt(e_se_beta^2*(x-25)^2))
gen q_500 = exp(e_beta*(x-25)+invnormal(.500)*sqrt(e_se_beta^2*(x-25)^2))
gen q_975 = exp(e_beta*(x-25)+invnormal(.975)*sqrt(e_se_beta^2*(x-25)^2))

* Graph the 3 quantiles of confidence on the log scale 
twoway ///
(line q_500 x , sort lc(blue%50) lp(l)) ///
(line q_025 x , sort lc(blue%50) lp(-)) ///
(line q_975 x , sort lc(blue%50) lp(-)) ///
, legend(off) xtitle("Dose") ytitle("Odds Ratio") ylabel(.5 1 2 4 8)  ///
	xlabel(19(2)35) aspect(1) name(f_dr_cm_ipd, replace) yscale(log)

* You can repeat the above code and choose the option random

use http://www.stats4life.se/data/drm/ipd_group_1.dta, clear
statsby e_b = _b[x] e_se = _se[x] , by(id): logit y x c
meta set e_b e_se , random(dlaird)
meta summarize , eform 
scalar e_beta = r(theta)
scalar e_se_beta = r(se)
scalar tau2 = r(tau2)
* Forest plot 
meta forestplot , eform name(f_re_ipd, replace) 
* Dose-response plot of the common odds ratio using 25 as referent
set obs 17 // increase number of rows in the current dataset
capture drop x 
egen x = seq() , from(19) to(35) // generate a sequence 
gen q_025 = exp(e_beta*(x-25)+invnormal(.025)*sqrt( (e_se_beta^2)*(x-25)^2))
gen q_500 = exp(e_beta*(x-25)+invnormal(.500)*sqrt( (e_se_beta^2)*(x-25)^2))
gen q_975 = exp(e_beta*(x-25)+invnormal(.975)*sqrt( (e_se_beta^2)*(x-25)^2))
* Graph the 3 quantiles of confidence on the log scale 
twoway ///
(line q_500 x , sort lc(blue%50) lp(l)) ///
(line q_025 x , sort lc(blue%50) lp(-)) ///
(line q_975 x , sort lc(blue%50) lp(-)) ///
, legend(off) xtitle("Dose") ytitle("Odds Ratio") ylabel(.5 1 2 4 8)  ///
	xlabel(19(2)35) aspect(1) name(f_dr_re_ipd, replace) yscale(log)

* Part 2 

use http://www.stats4life.se/data/drm/ad_group_1.dta, clear
list id xcat dose n cases e_b e_se e_lb e_ub , sepby(id)

* Study-specific adjusted estimated regression coefficients 
statsby e_b = _b[dose] e_se = _se[dose] , by(id) : drmeta e_b dose , data(n cases) id(id)  type(type) se(e_se) 
list id e_b e_se

* Common effect Meta-analysis 
meta set e_b e_se , common studylabel(id) 
meta summarize, eform
meta forestplot, eform name(f_cm_ad, replace)  

* Random-effects Meta-analysis
meta set e_b e_se , random(dlaird)
meta summarize , eform 
scalar e_beta = r(theta)
scalar e_se_beta = r(se)
scalar tau2 = r(tau2)
* Forest plot 
meta forestplot , eform name(f_re_ad, replace) 
* Dose-response plot of the common odds ratio using 25 as referent
set obs 17 // increase number of rows in the current dataset
capture drop x 
egen x = seq() , from(19) to(35) // generate a sequence 
gen q_025 = exp(e_beta*(x-25)+invnormal(.025)*sqrt( (e_se_beta^2)*(x-25)^2))
gen q_500 = exp(e_beta*(x-25)+invnormal(.500)*sqrt( (e_se_beta^2)*(x-25)^2))
gen q_975 = exp(e_beta*(x-25)+invnormal(.975)*sqrt( (e_se_beta^2)*(x-25)^2))
* Graph the 3 quantiles of confidence on the log scale 
twoway ///
(line q_500 x , sort lc(blue%50) lp(l)) ///
(line q_025 x , sort lc(blue%50) lp(-)) ///
(line q_975 x , sort lc(blue%50) lp(-)) ///
, legend(off) xtitle("Dose") ytitle("Odds Ratio") ylabel(.5 1 2 4 8)  ///
	xlabel(19(2)35) aspect(1) name(f_dr_re_ad, replace) yscale(log)
 