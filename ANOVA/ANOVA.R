library(tidyverse)
library(dplyr)
library(lsr)
library(emmeans)
library(car)


data <- read.csv("storm_ada_use.csv")

colnames(data)

######## Ada

## 2x4 ANOVA
res.aov3 <- aov(sim_ada ~  class + comparaison_ada + class:comparaison_ada, data = data)
summary(res.aov3)
#                        Df  Sum Sq  Mean Sq F value   Pr(>F)    
# class                   3 0.02252 0.007508  10.667 1.49e-06 ***
# comparaison_ada         1 0.00774 0.007738  10.993  0.00108 ** 
# class:comparaison_ada   3 0.00565 0.001882   2.674  0.04831 *  
# Residuals             208 0.14641 0.000704                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## Assumptions
plot(res.aov3, 1)
plot(res.aov3, 2)

leveneTest(sim_ada ~ comparaison_ada*class, data = data)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value   Pr(>F)   
# group   7  3.0941 0.004001 **
#       208          

# Shapiro-Wilk test
aov_residuals <- residuals(object = res.aov3)
shapiro.test(x = aov_residuals )

# Shapiro-Wilk normality test
# 
# data:  aov_residuals
# W = 0.98198, p-value = 0.007347

## R-squared
ss_total <- sum((data$sim_ada - mean(data$sim_ada))^2)
ss_residual <- sum(residuals(res.aov3)^2)
r_squared <- 1 - (ss_residual / ss_total)
r_squared # 0.1969663

## Effect Size
etaSquared(res.aov3)
#                           eta.sq eta.sq.part
# class                 0.12354826  0.13333765
# comparaison_ada       0.04244184  0.05019879
# class:comparaison_ada 0.03097624  0.03714134

## Post Hoc

## Class:#########################
emmeans(res.aov3, pairwise ~ class, adjust = "tukey")
# $emmeans
# class         emmean      SE  df lower.CL upper.CL
# Controversial  0.853 0.00409 208    0.845    0.861
# High Priority  0.861 0.00308 208    0.855    0.867
# Low Priority   0.835 0.00361 208    0.827    0.842
# Neutral        0.847 0.00391 208    0.840    0.855
# 
# Results are averaged over the levels of: comparaison_ada 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast                      estimate      SE  df t.ratio p.value
# Controversial - High Priority -0.00829 0.00513 208  -1.617  0.3712
# Controversial - Low Priority   0.01818 0.00546 208   3.330  0.0056*
# Controversial - Neutral        0.00546 0.00566 208   0.965  0.7695
# High Priority - Low Priority   0.02647 0.00475 208   5.574  <.0001*
# High Priority - Neutral        0.01375 0.00498 208   2.761  0.0317*
# Low Priority - Neutral        -0.01272 0.00532 208  -2.389  0.0824
# 
# Results are averaged over the levels of: comparaison_ada 
# P value adjustment: tukey method for comparing a family of 4 estimates 


## Group:#########################
emmeans(res.aov3, pairwise ~ comparaison_ada, adjust = "tukey")
# $emmeans
# comparaison_ada emmean      SE  df lower.CL upper.CL
# Other            0.854 0.00261 208    0.849    0.860
# Own              0.843 0.00261 208    0.838    0.849
# 
# Results are averaged over the levels of: class 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast    estimate      SE  df t.ratio p.value
# Other - Own    0.011 0.00369 208   2.978  0.0032*
# 
# Results are averaged over the levels of: class 

## Interaction:#########################
emmeans(res.aov3, pairwise ~ comparaison_ada | class, adjust = "tukey")
# $emmeans
# class = controversial:
# comparison emmean      SE  df lower.CL upper.CL
# Other       0.861 0.00579 208    0.849    0.872
# Own         0.845 0.00579 208    0.833    0.856
# 
# class = highPriority:
# comparison emmean      SE  df lower.CL upper.CL
# Other       0.873 0.00436 208    0.864    0.881
# Own         0.849 0.00436 208    0.841    0.858
# 
# class = lowPriority:
# comparison emmean      SE  df lower.CL upper.CL
# Other       0.833 0.00511 208    0.823    0.843
# Own         0.836 0.00511 208    0.826    0.846
# 
# class = neutral:
# comparison emmean      SE  df lower.CL upper.CL
# Other       0.851 0.00553 208    0.840    0.862
# Own         0.843 0.00553 208    0.833    0.854
# 
# Confidence level used: 0.95 
# 
# $contrasts
# class = controversial:
# contrast    estimate      SE  df t.ratio p.value
# Other - Own   0.0161 0.00819 208   1.965  0.0508
# 
# class = highPriority:
# contrast    estimate      SE  df t.ratio p.value
# Other - Own   0.0231 0.00617 208   3.750  0.0002*
# 
# class = lowPriority:
# contrast    estimate      SE  df t.ratio p.value
# Other - Own  -0.0028 0.00722 208  -0.388  0.6985
# 
# class = neutral:
# contrast    estimate      SE  df t.ratio p.value
# Other - Own   0.0076 0.00782 208   0.971  0.3325

######## USE

## 2x4 ANOVA
res.aov3 <- aov(sim_use ~ class + comparaison_ada + comparaison_ada:class, data = data)
summary(res.aov3)
#                        Df Sum Sq Mean Sq F value  Pr(>F)   
# class                   3 0.1566 0.05220   5.220 0.00171 **
# comparaison_ada         1 0.0408 0.04077   4.077 0.04475 * 
# class:comparaison_ada   3 0.0798 0.02660   2.660 0.04923 * 
# Residuals             208 2.0802 0.01000                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## Assumptions: Homogeneity of variance / Normal Q-Q
plot(res.aov3, 1)
plot(res.aov3, 2)

#Anova(res.aov3, type = "III")

## R-squared
ss_total <- sum((data$sim_use - mean(data$sim_use))^2)
ss_residual <- sum(residuals(res.aov3)^2)
r_squared <- 1 - (ss_residual / ss_total)
r_squared # 0.1175843

## Effect Size
etaSquared(res.aov3)
#                           eta.sq eta.sq.part
# class                 0.06643442  0.07001572
# comparaison_ada       0.01729623  0.01922419
# class:comparaison_ada 0.03385367  0.03694729

## Post Hoc

## Class:#########################
emmeans(res.aov3, pairwise ~ class, adjust = "tukey")
# $emmeans
# class         emmean     SE  df lower.CL upper.CL
# Controversial  0.459 0.0154 208    0.429    0.490
# High Priority  0.467 0.0116 208    0.444    0.490
# Low Priority   0.405 0.0136 208    0.379    0.432
# Neutral        0.418 0.0147 208    0.389    0.448
# 
# Results are averaged over the levels of: comparaison_ada 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast                      estimate     SE  df t.ratio p.value
# Controversial - High Priority -0.00781 0.0193 208  -0.404  0.9776
# Controversial - Low Priority   0.05402 0.0206 208   2.626  0.0455
# Controversial - Neutral        0.04091 0.0213 208   1.917  0.2241
# High Priority - Low Priority   0.06183 0.0179 208   3.455  0.0037
# High Priority - Neutral        0.04872 0.0188 208   2.595  0.0494
# Low Priority - Neutral        -0.01311 0.0201 208  -0.653  0.9143
# 
# Results are averaged over the levels of: comparaison_ada 
# P value adjustment: tukey method for comparing a family of 4 estimates 


## Group:#########################
emmeans(res.aov3, pairwise ~ comparaison_ada, adjust = "tukey")
# $emmeans
# comparaison_ada emmean      SE  df lower.CL upper.CL
# Other            0.449 0.00985 208    0.429    0.468
# Own              0.427 0.00985 208    0.407    0.446
# 
# Results are averaged over the levels of: class 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast    estimate     SE  df t.ratio p.value
# Other - Own   0.0221 0.0139 208   1.588  0.1138
# 
# Results are averaged over the levels of: class

## Interaction:#########################
emmeans(res.aov3, pairwise ~ comparaison_ada | class, adjust = "tukey")
# $emmeans
# class = Controversial:
# comparaison_ada emmean     SE  df lower.CL upper.CL
# Other            0.467 0.0218 208    0.424    0.510
# Own              0.452 0.0218 208    0.409    0.495
# 
# class = High Priority:
# comparaison_ada emmean     SE  df lower.CL upper.CL
# Other            0.505 0.0164 208    0.473    0.537
# Own              0.429 0.0164 208    0.397    0.462
# 
# class = Low Priority:
# comparaison_ada emmean     SE  df lower.CL upper.CL
# Other            0.394 0.0192 208    0.356    0.432
# Own              0.417 0.0192 208    0.379    0.455
# 
# class = Neutral:
# comparaison_ada emmean     SE  df lower.CL upper.CL
# Other            0.429 0.0209 208    0.388    0.470
# Own              0.408 0.0209 208    0.367    0.449
# 
# Confidence level used: 0.95 
# 
# $contrasts
# class = Controversial:
# contrast    estimate     SE  df t.ratio p.value
# Other - Own   0.0157 0.0309 208   0.508  0.6119
# 
# class = High Priority:
# contrast    estimate     SE  df t.ratio p.value
# Other - Own   0.0756 0.0233 208   3.251  0.0013
# 
# class = Low Priority:
# contrast    estimate     SE  df t.ratio p.value
# Other - Own  -0.0234 0.0272 208  -0.861  0.3900
# 
# class = Neutral:
# contrast    estimate     SE  df t.ratio p.value
# Other - Own   0.0207 0.0295 208   0.700  0.4845
