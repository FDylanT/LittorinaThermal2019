# Model Results after Modifying Data

YAY!! It converged!!

I restructured the data frame in LittorinaStatistics.Rmd so that each individual appeared in only one line, with new columns representing the number of exposure days each snail was alive and dead (0-2 of 2 days total). I ran a new generalized linear mixed-effects model on this data (assigned to mort_prop), which converged and provided the results below.

> summary(mort_prop)

Generalized linear mixed model  
  fit by maximum likelihood  
  (Laplace Approximation) [glmerMod]  
 Family: binomial  ( logit )  
Formula: cbind(DaysAlive, DaysDead) ~ Spp + PopID + Trt + (1 | Block) +  
    (1 | Seatable)  
   Data: mort_B23  
Control: glmerControl(optimizer = "bobyqa")  

| AIC | BIC | logLik | deviance | df.resid |
|---|---|---|---|---|
|475.1 | 506.2 | -229.6 | 459.1 | 352 |

Scaled residuals: 
| Min | 1Q | Median | 3Q | Max |
| --- | --- | --- | --- | --- |
| -7.1132 | -0.4908 | 0.1330 | 0.4477 | 3.4226 |

Random effects:
| Groups | Name | Variance | Std.Dev. |
| --- | --- | --- | --- |
| Seatable | (Intercept) | 0.0000 | 0.0000 |
| Block | (Intercept) | 0.7136 | 0.8448 |

Number of obs: 360, groups: Seatable, 3; Block, 2

Fixed effects:
| | Estimate | Std. Error | z value | Pr(>|z|) |
| --- | --- | --- | --- | --- |
| (Intercept) | -0.1485 | 0.6392 | -0.232 | 0.81625 |
| Spplittorina_obtusata | 0.1427 | 0.2560 | 0.557 | 0.57722 |
| Spplittorina_saxatilis | -0.7877 | 0.2565 | -3.071 | 0.00213 ** |
| PopIDRI | -0.8708 | 0.2123 | -4.102 | 4.09e-05 *** |
| TrtHS | 1.7937 | 0.2273 | 7.890 | 3.03e-15 *** |
| TrtNT | 4.9388 | 0.4657 | 10.605 | < 2e-16 *** |

Correlation of Fixed Effects:
| | (Intr) | Spplttrn_b | Spplttrn_s | PpIDRI | TrtHS |
| --- | --- | --- | --- | --- | --- |
| Spplttrn_bt | -0.197 | | | | |
| Spplttrn_sx | -0.183 | 0.487 | | | |
| PopIDRI | -0.147 | -0.016 | 0.040 | | |
| TrtHS | -0.136 | 0.017 | -0.113 | -0.141 | |
| TrtNT | -0.054 | 0.017 | -0.122 | -0.143 | 0.308 |

convergence code: 0  
boundary (singular) fit: see ?isSingular