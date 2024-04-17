#' Solution lab Day 1, group 2
#' Written by AC, 240609

# Load packages
library(tidyverse)
library(rms)
library(Epi)
library(metafor)
# remotes::install_github("alecri/dosresmeta")
library(dosresmeta)

# theme for ggplot
theme_set(theme_light())


# Part 1  ----

#' 1.	Describe the main features of the individual data, in particular the exposure and outcome distributions.

# load data (locally or from url)
load("data/ipd_group_2.RData")
# load(url("http://www.stats4life.se/data/drm/ipd_group_2.RData"))

# continuous outcome
by(ipd$y, ipd$id, summary)
ggplot(ipd, aes(x = y, after_stat(density), fill = factor(id))) +
  geom_histogram() +
  geom_line(stat = "density") +
  facet_wrap(~ id)

# exposure distribution
by(ipd$x, ipd$id, summary)
ggplot(ipd, aes(x = x, after_stat(density), fill = factor(id))) +
  geom_histogram() +
  geom_line(stat = "density") +
  facet_wrap(~ id)

# correlation
ggplot(ipd, aes(x = x, y = y)) +
  geom_point(size = 1) +
  geom_smooth(se = FALSE) +
  facet_wrap(~ id)


#' 2.	Create a table with the study-specific linear trend coefficients and standard error for 
#' the exposure-outcome association suitable for conducting a meta-analysis.

# linear trend
fiti_lin_ipd <- by(ipd, ipd$id, function(dat) glm(y ~ x + c, data = dat, family = gaussian))

# data for meta-analysis
bi_rma_ipd <- data.frame(
  yi = sapply(fiti_lin_ipd, function(x) coef(x)["x"]),
  vi = sapply(fiti_lin_ipd, function(x) vcov(x)["x", "x"])
)
bi_rma_ipd


#' 3.	Present the study-specific estimates of the linear trends using a forest plot and/or 
#' the estimated summary dose-response function. Write the findings for the result 
#' section of an hypothetical review paper. 

# univariate meta-analysis
res_ipd <- rma(yi = yi, vi = vi, data = bi_rma_ipd)
res_ipd

# forest plot
forest(res_ipd)

# estimated summary dose-response function
xref <- 7 # median(ipd$x)
newdata <- data.frame(dose = seq(5, 9, length.out = 100)) 
pred_long_ipd <- newdata %>% 
  mutate(mean_pred = coef(res_ipd) * (dose - xref)) %>% 
  cbind(sapply(bi_rma_ipd$yi, function(b) b * (newdata$dose - xref))) %>% 
  pivot_longer(cols = -c(dose, mean_pred), names_to = "id", values_to = "pred")

ppred_long_ipd <- ggplot(pred_long_ipd, aes(dose, pred, group = id)) +
  geom_line(col = "grey") +
  geom_line(aes(y = mean_pred), linewidth = 1.5)
ppred_long_ipd



# Part 2 ----


#' 1.	Describe the main features of the aggregated data, in particular th exposure and outcome distribution.

# load data (locally or from url)
load("data/ad_group_2.RData")
# load(url("http://www.stats4life.se/data/drm/ad_group_2.RData"))
ad

# how many exposure categories by study?
table(ad$id)
# which is the referent category in each study?
# what is the outcome measure in the referent category?
filter(ad, e_se == 0)


#'2.	Create a table with study-specific estimated regression coefficients and 
#' estimated standard error suitable for conducting a meta-analysis.

# linear trend on aggregated data
fit_lin_ad <- dosresmeta(formula = e_b ~ dose, id = id, se = e_se, covariance = "md",
                         sd = sd_y, n = n, data = ad, proc = "2stage")

# study-speci§fic coefficients
bi_rma_ad <- data.frame(
  yi = unname(fit_lin_ad$bi),
  vi = unlist(fit_lin_ad$Si)
)
bi_rma_ad


#' 3.	Present the study-specific estimates of the linear trends using a forest plot 
#' and/or the estimated summary dose-response function. 
#' Write the findings for the result section of an hypothetical review paper. 

# univariate meta-analysis
res_ad <- rma(yi = yi, vi = vi, data = bi_rma_ad)
res_ad
# same as the result from dosresmeta
# coef(fit_lin_ad)

# forest plot
forest(res_ad)

# estimated summary dose-response function
pred_long_ad <- newdata %>% 
  mutate(mean_pred = coef(res_ad) * (dose - xref)) %>% 
  cbind(sapply(bi_rma_ad$yi, function(b) b * (newdata$dose - xref))) %>% 
  pivot_longer(cols = -c(dose, mean_pred), names_to = "id", values_to = "pred")

ppred_long_ad <- ggplot(pred_long_ad, aes(dose, pred, group = id)) +
  geom_line(col = "grey") +
  geom_line(aes(y = mean_pred), linewidth = 1.5)
ppred_long_ad


#' 3.	Present the study-specific estimates of the linear trends using a forest plot 
#' and/or the estimated summary dose-response function. 
#' Write the findings for the result section of an hypothetical review paper. 

# comparison between mean estimates from ipd and ad
cbind("ipd" = ci.lin(res_ipd)[, c("Estimate", "2.5%", "97.5%")], 
      "ad" = ci.lin(res_ad)[, c("Estimate", "2.5%", "97.5%")])

# comparison between study-specific estimates from ipd and ad
bind_rows(
  mutate(pred_long_ipd, data = "ipd"),
  mutate(pred_long_ad, data = "ad")
) %>% 
  ggplot(aes(dose, pred, col = data)) +
  geom_line() +
  facet_wrap(~ id)
