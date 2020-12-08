# Linear Model without Random Effects

> summary(mort_linear)

Response DaysAlive :

Call:
lm(formula = DaysAlive ~ Spp + PopID + Trt, data = mort_B23)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.92896 -0.36229  0.08902  0.36334  1.36334 

Coefficients:
                       Estimate Std. Error t value Pr(>|t|)    
(Intercept)             0.86326    0.07798  11.070  < 2e-16 ***
Spplittorina_obtusata   0.03219    0.07830   0.411 0.681221    
Spplittorina_saxatilis -0.21238    0.07847  -2.706 0.007131 ** 
PopIDRI                -0.22660    0.06407  -3.537 0.000459 ***
TrtHS                   0.69344    0.07847   8.837  < 2e-16 ***
TrtNT                   1.26010    0.07847  16.059  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.6078 on 354 degrees of freedom
Multiple R-squared:  0.4435,	Adjusted R-squared:  0.4356 
F-statistic: 56.42 on 5 and 354 DF,  p-value: < 2.2e-16


Response DaysDead :

Call:
lm(formula = DaysDead ~ Spp + PopID + Trt, data = mort_B23)

Residuals:
| Min | 1Q | Median | 3Q | Max |
| --- | --- | --- | --- | --- |
| -1.36334 | -0.36334 | -0.08902 | 0.36229 | 1.92896 |

Coefficients:
                       Estimate Std. Error t value Pr(>|t|)    
(Intercept)             1.13674    0.07798  14.577  < 2e-16 ***
Spplittorina_obtusata  -0.03219    0.07830  -0.411 0.681221    
Spplittorina_saxatilis  0.21238    0.07847   2.706 0.007131 ** 
PopIDRI                 0.22660    0.06407   3.537 0.000459 ***
TrtHS                  -0.69344    0.07847  -8.837  < 2e-16 ***
TrtNT                  -1.26010    0.07847 -16.059  < 2e-16 ***
---

Residual standard error: 0.6078 on 354 degrees of freedom  
Multiple R-squared:  0.4435,	Adjusted R-squared:  0.4356  
F-statistic: 56.42 on 5 and 354 DF,  p-value: < 2.2e-16