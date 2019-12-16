Models
================
Laura Cosgrove
11/29/2019

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(geepack)
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

# Read data

``` r
alc_dat <- readxl::read_xls("./data/ALCDEP.xls")

alc_dat <- janitor::clean_names(alc_dat) %>% 
  mutate(treatment = factor(treatment)) %>% 
  mutate(gender = factor(gender, levels = c(0,1), labels = c("Male", "Female")))

alc_dat_long <- alc_dat %>% 
  pivot_longer(nd0:nd60, names_to = "days", values_to = "drinks", names_prefix = "nd")
```

# Fit GEE models

``` r
alc_dat_long <- alc_dat_long %>% 
  mutate(exposure = 30)

pois_gee <- geeglm(drinks ~ treatment*days + gender,
       family = poisson,
       data = alc_dat_long,
       id = sid,
       corstr = "ar1")

summary(pois_gee)
```

    ## 
    ## Call:
    ## geeglm(formula = drinks ~ treatment * days + gender, family = poisson, 
    ##     data = alc_dat_long, id = sid, corstr = "ar1")
    ## 
    ##  Coefficients:
    ##                   Estimate  Std.err      Wald Pr(>|W|)    
    ## (Intercept)        5.22610  0.01571 1.106e+05   <2e-16 ***
    ## treatment2        -0.01593  0.01907 6.980e-01    0.403    
    ## treatment3        -0.02585  0.02040 1.606e+00    0.205    
    ## days30            -0.39386  0.01425 7.642e+02   <2e-16 ***
    ## days60            -0.41353  0.01253 1.089e+03   <2e-16 ***
    ## genderFemale      -1.01273  0.01493 4.602e+03   <2e-16 ***
    ## treatment2:days30 -0.01293  0.01901 4.620e-01    0.497    
    ## treatment3:days30 -0.39227  0.02087 3.534e+02   <2e-16 ***
    ## treatment2:days60 -0.38624  0.01892 4.165e+02   <2e-16 ***
    ## treatment3:days60 -0.38748  0.01928 4.038e+02   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Estimated Scale Parameters:
    ##             Estimate Std.err
    ## (Intercept)    1.901  0.1119
    ## 
    ## Correlation: Structure = ar1  Link = identity 
    ## 
    ## Estimated Correlation Parameters:
    ##       Estimate Std.err
    ## alpha   0.5486 0.03297
    ## Number of clusters:   314   Maximum cluster size: 3

``` r
pois_gee_int <- geeglm(drinks ~ treatment*days + treatment*gender,
       family = poisson,
       data = alc_dat_long,
       id = sid,
       corstr = "ar1")

summary(pois_gee_int) #ns
```

    ## 
    ## Call:
    ## geeglm(formula = drinks ~ treatment * days + treatment * gender, 
    ##     family = poisson, data = alc_dat_long, id = sid, corstr = "ar1")
    ## 
    ##  Coefficients:
    ##                         Estimate Std.err     Wald Pr(>|W|)    
    ## (Intercept)               5.2287  0.0178 85835.74   <2e-16 ***
    ## treatment2               -0.0197  0.0225     0.76     0.38    
    ## treatment3               -0.0298  0.0247     1.47     0.23    
    ## days30                   -0.3939  0.0142   764.23   <2e-16 ***
    ## days60                   -0.4135  0.0125  1089.27   <2e-16 ***
    ## genderFemale             -1.0222  0.0235  1884.16   <2e-16 ***
    ## treatment2:days30        -0.0129  0.0190     0.46     0.50    
    ## treatment3:days30        -0.3923  0.0209   353.36   <2e-16 ***
    ## treatment2:days60        -0.3862  0.0189   416.54   <2e-16 ***
    ## treatment3:days60        -0.3875  0.0193   403.80   <2e-16 ***
    ## treatment2:genderFemale   0.0150  0.0351     0.18     0.67    
    ## treatment3:genderFemale   0.0159  0.0370     0.18     0.67    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Estimated Scale Parameters:
    ##             Estimate Std.err
    ## (Intercept)      1.9   0.112
    ## 
    ## Correlation: Structure = ar1  Link = identity 
    ## 
    ## Estimated Correlation Parameters:
    ##       Estimate Std.err
    ## alpha    0.549   0.033
    ## Number of clusters:   314   Maximum cluster size: 3

``` r
##Trajectories##
#get contrast of treatment 3 days 60 to treatment 3 days 30
#get contrast of treatment 1 days 60 to treatment 1 days 30
#get contrast of treatment 3 days 60 to treatment 3 days 30

## Treatment effect ##
#get contrast of treatment 3 days 60 to treatment 2 days 60
#get contrast of treatment 3 days 60 to treatment 2 days 20

colnames(pois_gee$geese$vbeta) = names(coef(pois_gee))
rownames(pois_gee$geese$vbeta) = names(coef(pois_gee))
var_b0 = pois_gee$geese$vbeta["(Intercept)","(Intercept)"]
var_b1 = pois_gee$geese$vbeta["treatment2","treatment2"]
var_b2 = pois_gee$geese$vbeta["(Intercept)","(Intercept)"]
var_b3 = pois_gee$geese$vbeta["(Intercept)","(Intercept)"]
var_b4 = pois_gee$geese$vbeta["(Intercept)","(Intercept)"]

var_b6 = pois_gee$geese$vbeta["genderFemale","genderFemale"]
cov_b0b6 = pois_gee$geese$vbeta["genderFemale","(Intercept)"]
women_count_baseline = exp(coef(pois_gee)["(Intercept)"] + coef(pois_gee)["genderFemale"])
women_var_baseline = var_b0 + var_b6 + 2*cov_b0b6
women_ci_baseline = c(exp(coef(pois_gee)["(Intercept)"] + coef(pois_gee)["genderFemale"] - qchisq(0.975, 1)*sqrt(women_var_baseline)), exp(coef(pois_gee)["(Intercept)"] + coef(pois_gee)["genderFemale"] + qchisq(0.975, 1)*sqrt(women_var_baseline)))

var_b4 = pois_gee$geese$vbeta["days30","days30"]
var_b5 = pois_gee$geese$vbeta["days60","days60"]
cov_b4b5 = pois_gee$geese$vbeta["days30","days60"]
d30_to_d60_rr = exp(coef(pois_gee)["days60"] - coef(pois_gee)["days30"])
d30_d60_var = var_b4 + var_b5 - 2*cov_b4b5
d30_to_d60_ci = c(exp(coef(pois_gee)["days60"] - coef(pois_gee)["days30"] - qchisq(0.975, 1)*sqrt(d30_d60_var)), exp(coef(pois_gee)["days60"] - coef(pois_gee)["days30"] + qchisq(0.975, 1)*sqrt(d30_d60_var)))

#treatment 2 to treatment 1
var_trt2 = pois_gee$geese$vbeta["treatment2","treatment2"]
var_trt2d30 = pois_gee$geese$vbeta["treatment2:days30","treatment2:days30"]
cov_day30trt2 = pois_gee$geese$vbeta["treatment2:days30","treatment2"]
trt2day30totrt1day30_rr = exp(coef(pois_gee)["treatment2"] + coef(pois_gee)["treatment2:days30"])
trt2day30totrt1day30_var = var_trt2 + var_trt2d30 - 2*cov_day30trt2
trt2day30totrt1day30_ci = c(exp(log(trt2day30totrt1day30_rr) - qchisq(0.975, 1)*sqrt(trt2day30totrt1day30_var)), exp(log(trt2day30totrt1day30_rr) + qchisq(0.975, 1)*sqrt(trt2day30totrt1day30_var)))

var_trt2d60 = pois_gee$geese$vbeta["treatment2:days60","treatment2:days60"]
cov_day60trt2 = pois_gee$geese$vbeta["treatment2:days60","treatment2"]
trt2day60totrt1day60_rr = exp(coef(pois_gee)["treatment2"] + coef(pois_gee)["treatment2:days60"])
trt2day60totrt1day60_var = var_trt2 + var_trt2d60 + 2*cov_day60trt2
trt2day60totrt1day60_ci = c(exp(log(trt2day60totrt1day60_rr) - qchisq(0.975, 1)*sqrt(trt2day60totrt1day60_var)), exp(log(trt2day60totrt1day60_rr) + qchisq(0.975, 1)*sqrt(trt2day60totrt1day60_var)))

cov_day60day30trt2 = pois_gee$geese$vbeta["treatment2:days60","treatment2:days30"]
trt2trt1_day60today30_r_rr = exp(coef(pois_gee)["treatment2:days60"] - coef(pois_gee)["treatment2:days30"])
trt2trt1_day60today30_r_var = var_trt2d30 + var_trt2d60 - 2*cov_day60day30trt2
trt2trt1_day60today30_r_ci = c(exp(log(trt2trt1_day60today30_r_rr) - qchisq(0.975, 1)*sqrt(trt2trt1_day60today30_r_var)), exp(log(trt2trt1_day60today30_r_rr) + qchisq(0.975, 1)*sqrt(trt2trt1_day60today30_r_var)))

#treatment 3 to treatment 1
var_trt3 = pois_gee$geese$vbeta["treatment3","treatment3"]
var_trt3d30 = pois_gee$geese$vbeta["treatment3:days30","treatment3:days30"]
cov_day30trt3 = pois_gee$geese$vbeta["treatment3:days30","treatment3"]
trt3day30totrt1day30_rr = exp(coef(pois_gee)["treatment3"] + coef(pois_gee)["treatment3:days30"])
trt3day30totrt1day30_var = var_trt3 + var_trt3d30 - 2*cov_day30trt3
trt3day30totrt1day30_ci = c(exp(log(trt3day30totrt1day30_rr) - qchisq(0.975, 1)*sqrt(trt3day30totrt1day30_var)), exp(log(trt3day30totrt1day30_rr) + qchisq(0.975, 1)*sqrt(trt3day30totrt1day30_var)))

var_trt3d60 = pois_gee$geese$vbeta["treatment3:days60","treatment3:days60"]
cov_day60trt3 = pois_gee$geese$vbeta["treatment3:days60","treatment3"]
trt3day60totrt1day60_rr = exp(coef(pois_gee)["treatment3"] + coef(pois_gee)["treatment3:days60"])
trt3day60totrt1day60_var = var_trt3 + var_trt3d60 + 2*cov_day60trt3
trt3day60totrt1day60_ci = c(exp(log(trt3day60totrt1day60_rr) - qchisq(0.975, 1)*sqrt(trt3day60totrt1day60_var)), exp(log(trt3day60totrt1day60_rr) + qchisq(0.975, 1)*sqrt(trt3day60totrt1day60_var)))

cov_day60day30trt3 = pois_gee$geese$vbeta["treatment3:days60","treatment3:days30"]
trt3trt1_day60today30_r_rr = exp(coef(pois_gee)["treatment3:days60"] - coef(pois_gee)["treatment3:days30"])
trt3trt1_day60today30_r_var = var_trt3d30 + var_trt3d60 - 2*cov_day60day30trt3
trt3trt1_day60today30_r_ci = c(exp(log(trt3trt1_day60today30_r_rr) - qchisq(0.975, 1)*sqrt(trt3trt1_day60today30_r_var)), exp(log(trt3trt1_day60today30_r_rr) + qchisq(0.975, 1)*sqrt(trt3trt1_day60today30_r_var)))


broom::tidy(pois_gee, exponentiate = TRUE, conf.int = TRUE)
```

    ## # A tibble: 10 x 7
    ##    term             estimate std.error statistic p.value conf.low conf.high
    ##    <chr>               <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl>
    ##  1 (Intercept)       186.       0.0157   1.11e+5   0      180.      192.   
    ##  2 treatment2          0.984    0.0191   6.98e-1   0.403    0.948     1.02 
    ##  3 treatment3          0.974    0.0204   1.61e+0   0.205    0.936     1.01 
    ##  4 days30              0.674    0.0142   7.64e+2   0        0.656     0.694
    ##  5 days60              0.661    0.0125   1.09e+3   0        0.645     0.678
    ##  6 genderFemale        0.363    0.0149   4.60e+3   0        0.353     0.374
    ##  7 treatment2:days…    0.987    0.0190   4.62e-1   0.497    0.951     1.02 
    ##  8 treatment3:days…    0.676    0.0209   3.53e+2   0        0.648     0.704
    ##  9 treatment2:days…    0.680    0.0189   4.17e+2   0        0.655     0.705
    ## 10 treatment3:days…    0.679    0.0193   4.04e+2   0        0.654     0.705

1.  Treatment effect question

At baseline, the expected count of past-30 day alcoholic drinks for men
is 186 (95% Wald CI (180, 192)), and for women the expected count is
(95% Wald CI 62.576 , 72.993), in treatment 1. These estimates of the
baseline count by gender is not significantly different between
treatments, indicating that randomization was successful.

A treatment by gender interaction term was considered, but it was not
significant, which is unsurprising examining the plot trajectory.

The ratio of the expected count of past-30 day alcoholic drinks for
those in treatment 1 30 days after baseline compared to baseline is
0.674 (95% CI 0.656 - 0.694), adjusted for gender. The ratio of the
expected count of past-30 day alcoholic drinks for those in treatment 1
60 days after baseline compared to baseline is 0.661 (95% CI 0.645 -
0.678). From day 30 to day 60, the ratio of the expected count of
past-30 day alcoholic drinks for those in treatment 1 is 0.981and the
95% CI is (0.906, 1.062). This means that the expected count of past-30
day alcoholic drinks is not significantly different from day 30 to day
60 for the population of patients in treatment 1.

The ratio of the expected count of past-30 day alcoholic drinks for
those in treatment 2 at 30 days compared to those in treatment 1 at 30
days is 0.972 (95% CI 0.83, 1.137), adjusted for gender. The ratio of
the expected count of past-30 day alcoholic drinks for those in
treatment 2 at 60 days compared to those in treatment 1 at 60 days is is
0.669 (95% CI 0.599, 0.747). And the difference-in-difference, that is,
the ratio of the ratio of expected count of past-30 day alcoholic drinks
for those in treatment 2 at 60 days compared to at 30 days to the ratio
of expected count of past-30 day alcoholic drinks for those in treatment
1 at 60 days compared to at 30 days is 0.688 (95% CI 0.613, 0.773). This
means that while treatment 1 and 2 do not differ at days 30 in terms of
the expected counts of alcoholic drinks, they do differ from the days 30
to days 60 period, where those in treatment 2 have a ratio of expected
drinks from days 30 to days 60 that is 0.688 times that of treatment 1
(which is not significantly different from 1, as shown above). These
patterns apply to both gender subpopulations.

The ratio of the expected count of past-30 day alcoholic drinks for
those in treatment 3 at 30 days compared to those in treatment 1 at 30
days is 0.658 (95% CI 0.555, 0.781), adjusted for gender. The ratio of
the expected count of past-30 day alcoholic drinks for those in
treatment 3 at 60 days compared to those in treatment 1 at 60 days is is
0.661 (95% CI 0.589, 0.743). And the difference-in-difference, that is,
the ratio of the ratio of expected count of past-30 day alcoholic drinks
for those in treatment 2 at 60 days compared to at 30 days to the ratio
of expected count of past-30 day alcoholic drinks for those in treatment
1 at 60 days compared to at 30 days is 1.005 (95% CI 0.896, 1.127). This
means treatment 1 and 3 differ at days 30 and at days 60 in terms of the
expected counts of alcoholic drinks, with treatment 3 being more
efficacious at reducing counts of alcoholic drinks. But they do not
differ in terms of their respective change in expected count of
alcoholic drinks from the days 30 to days 60 period, indicating that
treatment 3, similar to treatment 1, has most of its reduction effects
in the first block of time between day 0 and day 30. These patterns
apply to both gender
subpopulations.

# fit mixed effect model to use negative binomial? nope - overdispersion is not important

``` r
pois_glmm <- lme4::lmer(drinks ~ treatment*days + gender + (1|sid), 
       family = poisson,
       data = alc_dat_long)
```

    ## Warning in lme4::lmer(drinks ~ treatment * days + gender + (1 | sid),
    ## family = poisson, : calling lmer with 'family' is deprecated; please use
    ## glmer() instead

``` r
summary(pois_glmm)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: poisson  ( log )
    ## Formula: drinks ~ treatment * days + gender + (1 | sid)
    ##    Data: alc_dat_long
    ## Control: 
    ## structure(list(optimizer = c("bobyqa", "Nelder_Mead"), calc.derivs = TRUE,  
    ##     use.last.params = FALSE, restart_edge = FALSE, boundary.tol = 1e-05,  
    ##     tolPwrss = 1e-07, compDev = TRUE, nAGQ0initStep = TRUE, checkControl = list( 
    ##         check.nobs.vs.rankZ = "ignore", check.nobs.vs.nlev = "stop",  
    ##         check.nlev.gtreq.5 = "ignore", check.nlev.gtr.1 = "stop",  
    ##         check.nobs.vs.nRE = "stop", check.rankX = "message+drop.cols",  
    ##         check.scaleX = "warning", check.formula.LHS = "stop",  
    ##         check.response.not.const = "stop"), checkConv = list( 
    ##         check.conv.grad = list(action = "warning", tol = 0.001,  
    ##             relTol = NULL), check.conv.singular = list(action = "message",  
    ##             tol = 1e-04), check.conv.hess = list(action = "warning",  
    ##             tol = 1e-06)), optCtrl = list()), class = c("glmerControl",  
    ## "merControl"))
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##     7154     7207    -3566     7132      931 
    ## 
    ## Scaled residuals: 
    ##    Min     1Q Median     3Q    Max 
    ## -2.644 -0.581 -0.062  0.558  2.674 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  sid    (Intercept) 0.00978  0.0989  
    ## Number of obs: 942, groups:  sid, 314
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         5.2208     0.0145  359.82   <2e-16 ***
    ## treatment2         -0.0158     0.0185   -0.85     0.39    
    ## treatment3         -0.0241     0.0188   -1.28     0.20    
    ## days30             -0.3939     0.0136  -28.87   <2e-16 ***
    ## days60             -0.4135     0.0137  -30.13   <2e-16 ***
    ## genderFemale       -1.0142     0.0139  -73.22   <2e-16 ***
    ## treatment2:days30  -0.0129     0.0190   -0.68     0.50    
    ## treatment3:days30  -0.3923     0.0207  -18.97   <2e-16 ***
    ## treatment2:days60  -0.3862     0.0203  -19.01   <2e-16 ***
    ## treatment3:days60  -0.3875     0.0208  -18.64   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) trtmn2 trtmn3 days30 days60 gndrFm tr2:30 tr3:30 tr2:60
    ## treatment2  -0.676                                                        
    ## treatment3  -0.661  0.503                                                 
    ## days30      -0.379  0.297  0.292                                          
    ## days60      -0.377  0.295  0.290  0.400                                   
    ## genderFemal -0.417  0.068  0.060  0.000  0.000                            
    ## trtmnt2:d30  0.272 -0.412 -0.210 -0.719 -0.288  0.000                     
    ## trtmnt3:d30  0.250 -0.196 -0.387 -0.660 -0.264  0.000  0.474              
    ## trtmnt2:d60  0.254 -0.385 -0.196 -0.270 -0.675  0.000  0.375  0.178       
    ## trtmnt3:d60  0.249 -0.195 -0.385 -0.264 -0.660  0.000  0.190  0.350  0.446

``` r
library(glmmTMB)
nb_glmm <- glmmTMB(drinks ~ treatment*days + gender + (1|sid), 
       family = nbinom1,
       data = alc_dat_long)
summary(nb_glmm)
```

    ##  Family: nbinom1  ( log )
    ## Formula:          drinks ~ treatment * days + gender + (1 | sid)
    ## Data: alc_dat_long
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##     7156     7214    -3566     7132      930 
    ## 
    ## Random effects:
    ## 
    ## Conditional model:
    ##  Groups Name        Variance Std.Dev.
    ##  sid    (Intercept) 0.00978  0.0989  
    ## Number of obs: 942, groups:  sid, 314
    ## 
    ## Overdispersion parameter for nbinom1 family (): 7.82e-07 
    ## 
    ## Conditional model:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)         5.2208     0.0145     360   <2e-16 ***
    ## treatment2         -0.0158     0.0185      -1     0.39    
    ## treatment3         -0.0241     0.0188      -1     0.20    
    ## days30             -0.3939     0.0136     -29   <2e-16 ***
    ## days60             -0.4135     0.0137     -30   <2e-16 ***
    ## genderFemale       -1.0142     0.0139     -73   <2e-16 ***
    ## treatment2:days30  -0.0129     0.0190      -1     0.50    
    ## treatment3:days30  -0.3923     0.0207     -19   <2e-16 ***
    ## treatment2:days60  -0.3862     0.0203     -19   <2e-16 ***
    ## treatment3:days60  -0.3875     0.0208     -19   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
