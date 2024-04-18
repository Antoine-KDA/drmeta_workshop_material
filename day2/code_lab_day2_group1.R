#' Solution lab Day 2, group 1
#' Written by AC, 240609

# Load packages
library(tidyverse)
library(rms)
library(Epi)
# remotes::install_github("alecri/dosresmeta")
library(dosresmeta)

# theme for ggplot
theme_set(theme_light())


# Part 1  ----

# load data
load("data/ad_group_1.RData")
ad


#' 1.	Quadratic function. In a dose-response model, use a quadratic function to model the 
#' dose-response association between the quantitative exposure and the outcome
fit_sq_ad <- dosresmeta(formula = e_b ~ dose + I(dose^2), id = id, type = type, se = e_se,
                        cases = cases, n = n, data = ad, proc = "2stage")
summary(fit_sq_ad)

#' 2.	Based on the estimated meta-analytical model, is there any strong indication against a 
#' simpler linear trend?  What is the result of the test for the overall dose-response association? 
waldtest(Sigma = vcov(fit_sq_ad), b = coef(fit_sq_ad), Terms = 1:2)
waldtest(Sigma = vcov(fit_sq_ad), b = coef(fit_sq_ad), Terms = 2)


#' 3.	Independenlty of the test result, visualize the estimated dose-response curve
#'  with a 95% confidence interval. Chose a referent value. 

# referent value for prediction
xref <- 25  # median(ad$dose) 
newd <- data.frame(dose = c(xref, seq(min(ad$dose), max(ad$dose), length.out = 100)))

# mean prediction
pred_sq <- predict(fit_sq_ad, newdata = newd, expo = TRUE)
ggplot(pred_sq, aes(dose, pred)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = .2) +
  scale_y_continuous(transform = "log", breaks = c(.5, .75, 1, 2, 4, 8, 16))


#' 4.	Restricted cubic spline function. In a common-effect dose-response model, 
#' use restricted cubic splines with 3 knots at fixed percentiles (10th, 50th, 90th) of the overall 
#' exposure distribution to model the dose-response function.
k <- quantile(ad$dose, c(.1, .5, .9))
fit_rcs_ad_fe <- dosresmeta(formula = e_b ~ rcs(dose, k), id = id, type = type, 
                         cases = cases, n = n, data = ad, se = e_se, proc = "2stage", method = "fixed")
summary(fit_rcs_ad_fe)


#' 5.	Based on the estimated meta-analytical model, is there any strong indication against a simpler 
#' linear trend?  What is the result of the test for the overall dose-response association? 
waldtest(Sigma = vcov(fit_rcs_ad_fe), b = coef(fit_rcs_ad_fe), Terms = 1:2)
waldtest(Sigma = vcov(fit_rcs_ad_fe), b = coef(fit_rcs_ad_fe), Terms = 2)



#' 6.	Independenlty of the test result, visualize the estimated dose-response function with a 
#' 95% confidence interval. Chose a the same referent value as before.

# mean prediction
pred_rcs_fe <- cbind(newd, predict(fit_rcs_ad_fe, newdata = newd, expo = TRUE))
ggplot(pred_rcs_fe, aes(dose, pred)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub), alpha = .2) +
  scale_y_continuous(transform = "log", breaks = c(.5, .75, 1, 2, 4, 8, 16))



# Part 2  ----


#' 1.	Compute and interpret common measures of heterogeneity, such as the Q statistic 
#' and Q test, estimate of between-study heterogeneity, and I2, for the meta-analysis of linear trend.

# linear trend on aggregated data
fit_lin_ad <- dosresmeta(formula = e_b ~ dose, id = id, type = type, se = e_se,
                         cases = cases, n = n, data = ad, proc = "2stage")
summary(fit_lin_ad)


#' 2.	Present graphically the observed heterogeneity in linear trend and its 
#' implication in terms of mean predicted linear trend, as well as predicted 
#' linear trend for a new study.

# prediction for linear regression coefficients
pred_bi_lin <- predict(fit_lin_ad, delta = 1) %>% 
  mutate(
    # prediction intervals for the linear trend of a new study
    pi.lb = coef(fit_lin_ad) - qt(.975, fit_lin_ad$df.residual - 2) * sqrt(diag(fit_lin_ad$Psi + vcov(fit_lin_ad))),
    pi.ub = coef(fit_lin_ad) + qt(.975, fit_lin_ad$df.residual - 2) * sqrt(diag(fit_lin_ad$Psi + vcov(fit_lin_ad)))
)
pred_bi_lin


# study-specific predictions
predi_lin <- data.frame(dose = newd) %>% 
  cbind(sapply(fit_lin_ad$bi, function(b) exp(b * (newd$dose - newd$dose[1])))) %>% 
  pivot_longer(cols = -dose, values_to = "pred", names_to = "id")
# prediction for the average, and a new study
predi_newlin <- newd %>% 
  cbind(lapply(pred_bi_lin, function(x) exp(x * (newd$dose - newd$dose[1]))) %>% 
          bind_cols())

ppredi_newlin <- ggplot(predi_lin, aes(dose, pred)) +
  geom_line(aes(group = id), col = "grey") +
  geom_line(data = predi_newlin, linewidth = 1.5) +
  geom_ribbon(data = predi_newlin, aes(ymin = pi.lb, ymax = pi.ub), alpha = 0.3) +
  geom_ribbon(data = predi_newlin, aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.5) +
  scale_y_continuous(transform = "log", breaks = c(.5, .75, 1, 2, 4, 8, 16))
ppredi_newlin



#' 3.	Repeat Questions 1-2 for meta-analysis of non-linear dose-response. Contrast the findings.

# restriced cubic splines on aggregated data
fit_rcs_ad <- dosresmeta(formula = e_b ~ rcs(dose, k), id = id, type = type, 
                            cases = cases, n = n, data = ad, se = e_se, proc = "2stage")
summary(fit_rcs_ad)


# prediction for linear regression coefficients
pred_bi_rcs <- data.frame(
  pred = coef(fit_rcs_ad), 
  # prediction intervals for the mean rcs coefficient
  ci.lb = coef(fit_rcs_ad) - qnorm(.975) * sqrt(diag(vcov(fit_rcs_ad))),
  ci.ub = coef(fit_rcs_ad) + qnorm(.975) * sqrt(diag(vcov(fit_rcs_ad))),
  # prediction intervals for the rcs coefficients of a new study
  pi.lb = coef(fit_rcs_ad) - qt(.975, fit_rcs_ad$df.residual - 2) * sqrt(diag(fit_rcs_ad$Psi + vcov(fit_rcs_ad))),
  pi.ub = coef(fit_rcs_ad) + qt(.975, fit_rcs_ad$df.residual - 2) * sqrt(diag(fit_rcs_ad$Psi + vcov(fit_rcs_ad)))
)
pred_bi_rcs

# study-specific predictions
predi_rcs <- data.frame(dose = newd) %>% 
  cbind(apply(fit_rcs_ad$bi, 1, function(b){
    exp(b[1] * (newd$dose - newd$dose[1]) + 
          b[2] * (rcspline.eval(newd$dose, k) - c(rcspline.eval(newd$dose[1], k))))
  })) %>% 
  pivot_longer(cols = -dose, values_to = "pred", names_to = "id")
# prediction for the average, and a new study
predi_newrcs <- newd %>% 
  cbind(lapply(pred_bi_rcs, function(b){
    exp(b[1] * (newd$dose - newd$dose[1]) + 
          b[2] * (rcspline.eval(newd$dose, k) - c(rcspline.eval(newd$dose[1], k))))
  }))

ppredi_newrcs <- ggplot(predi_rcs, aes(dose, pred)) +
  geom_line(aes(group = id), col = "grey") +
  geom_line(data = predi_newrcs, linewidth = 1.5) +
  geom_ribbon(data = predi_newrcs, aes(ymin = pi.lb, ymax = pi.ub), alpha = 0.3) +
  geom_ribbon(data = predi_newrcs, aes(ymin = ci.lb, ymax = ci.ub), alpha = 0.5) +
  scale_y_continuous(transform = "log", breaks = c(.5, 1, 2, 4, 8, 16, 32))
ppredi_newrcs


#' 4.	Consider the study-level covariate z. Is there any differential dose-response curve 
#' depending on the level of z? Perform a subgroup or meta-regression analysis, and 
#' present the predicted mean curves conditional on the levels of z.

# distribution of study-level covariate
filter(ad, e_se == 0) %>% count(z)

# subgroup analysis: separate dose-response curves by levels of z
fiti_subg <- by(ad, ad$z, function(dat) {
  dosresmeta(formula = e_b ~ rcs(dose, k), id = id, type = type, se = e_se,
             cases = cases, n = n, data = dat, proc = "2stage")
})

# prediction
pred_subg <- lapply(fiti_subg, predict, newdata = data.frame(dose = newd), expo = TRUE) %>% 
  bind_rows(.id = "z") %>% 
  rename(dose = "rcs(dose, k)dose")

ggplot(pred_subg, aes(dose, pred)) +
  geom_line(aes(col = z)) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub, fill = z), alpha = .2) +
  scale_y_continuous(transform = "log", breaks = c(.5, .75, 1, 2, 4, 8, 16))


# meta-regression
fit_mr <- dosresmeta(formula = e_b ~ rcs(dose, k), id = id, type = type, se = e_se,
                     cases = cases, n = n, data = ad, proc = "2stage", mod = ~ z)
summary(fit_mr)
# test for differential dose-response associations
waldtest(vcov(fit_mr), coef(fit_mr), Terms = 3:6)

# prediction
newd_mr <- expand.grid(dose = newd$dose, z = factor(unique(ad$z)))
pred_mr <- tapply(newd_mr, newd_mr$z, function(n) {
  cbind(n, predict(fit_mr, newdata = n, expo = TRUE))
}) %>% 
  bind_rows()

ggplot(pred_mr, aes(dose, pred)) +
  geom_line(aes(col = z)) +
  geom_ribbon(aes(ymin = ci.lb, ymax = ci.ub, fill = z), alpha = .2) +
  scale_y_continuous(transform = "log", breaks = c(.5, .75, 1, 2, 4, 8, 16))
