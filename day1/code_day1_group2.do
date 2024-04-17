* Stata code for Group 2. Mean Difference

clear all
use http://www.stats4life.se/data/drm/ipd_group_2.dta

* Part 1
tabstat x y , by(id)
*twoway (histogram x), by(id)

* Study-specific adjusted estimated regression coefficients 
statsby e_b = _b[x] e_se = _se[x] , by(id): regress y x c
list id e_b e_se

* Meta-analysis 
meta set e_b e_se , common
meta summarize  
scalar e_beta = r(theta)
scalar e_se_beta = r(se)

* Forest plot 
meta forestplot , name(f_cm_ipd, replace) 

* Dose-response plot of the common odds ratio using 7 as referent

capture drop x 
range x 2 11 
gen q_025 = e_beta*(x-7)+invnormal(.025)*sqrt(e_se_beta^2*(x-7)^2)
gen q_500 = e_beta*(x-7)+invnormal(.500)*sqrt(e_se_beta^2*(x-7)^2)
gen q_975 = e_beta*(x-7)+invnormal(.975)*sqrt(e_se_beta^2*(x-7)^2)

* Graph the 3 quantiles of confidence  
twoway ///
(line q_500 x , sort lc(blue%50) lp(l)) ///
(line q_025 x , sort lc(blue%50) lp(-)) ///
(line q_975 x , sort lc(blue%50) lp(-)) ///
, legend(off) xtitle("Dose") ytitle("Mean Difference") ylabel(#10)  ///
	xlabel(#10) aspect(1) name(f_dr_cm_ipd, replace)  

* You can repeat the above code and choose the option random

use http://www.stats4life.se/data/drm/ipd_group_2.dta, clear
statsby e_b = _b[x] e_se = _se[x] , by(id): regress y x c
meta set e_b e_se , random(dlaird)
meta summarize  
scalar e_beta = r(theta)
scalar e_se_beta = r(se)
scalar tau2 = r(tau2)
* Forest plot 
meta forestplot ,  name(f_re_ipd, replace) 
* Dose-response plot of the common odds ratio using 7 as referent
capture drop x 
egen x = seq() , from(2) to(11) // generate a sequence 
gen q_025 = e_beta*(x-7)+invnormal(.025)*sqrt(e_se_beta^2*(x-7)^2)
gen q_500 = e_beta*(x-7)+invnormal(.500)*sqrt(e_se_beta^2*(x-7)^2)
gen q_975 = e_beta*(x-7)+invnormal(.975)*sqrt(e_se_beta^2*(x-7)^2)
* Graph the 3 quantiles of confidence on the log scale 
twoway ///
(line q_500 x , sort lc(blue%50) lp(l)) ///
(line q_025 x , sort lc(blue%50) lp(-)) ///
(line q_975 x , sort lc(blue%50) lp(-)) ///
, legend(off) xtitle("Dose") ytitle("Mean Difference") ylabel(#10)  ///
	xlabel(#10) aspect(1) name(f_dr_re_ipd, replace)  

* Part 2 

use http://www.stats4life.se/data/drm/ad_group_2.dta, clear

* Study-specific adjusted estimated regression coefficients 
statsby e_b = _b[dose] e_se = _se[dose] , by(id) : drmeta e_b dose , data(n sd_y) id(id)  type(type) se(e_se) 
list id e_b e_se

* Common effect Meta-analysis 
meta set e_b e_se , common studylabel(id)  eslabel(MD)
meta summarize
meta forestplot,  name(f_cm_ad, replace)  

* Random-effects Meta-analysis
meta set e_b e_se , random(dlaird)  eslabel(MD)
meta summarize 
scalar e_beta = r(theta)
scalar e_se_beta = r(se)
scalar tau2 = r(tau2)

* Forest plot 
meta forestplot ,  name(f_re_ad, replace) 
* Dose-response plot of the common Mean Difference using 7 as referent
capture drop x 
egen x = seq() , from(2) to(11) // generate a sequence 
gen q_025 = e_beta*(x-7)+invnormal(.025)*sqrt(e_se_beta^2*(x-7)^2)
gen q_500 = e_beta*(x-7)+invnormal(.500)*sqrt(e_se_beta^2*(x-7)^2)
gen q_975 = e_beta*(x-7)+invnormal(.975)*sqrt(e_se_beta^2*(x-7)^2)

* Graph the 3 quantiles of confidence  
twoway ///
(line q_500 x , sort lc(blue%50) lp(l)) ///
(line q_025 x , sort lc(blue%50) lp(-)) ///
(line q_975 x , sort lc(blue%50) lp(-)) ///
, legend(off) xtitle("Dose") ytitle("Mean Difference") ylabel(#10)  ///
	xlabel(#10) aspect(1) name(f_dr_re_ad, replace)  
 
