glht Output - Pairwise Contrasts
================
2/10/2021

``` r
mort_B23$int <- with(mort_B23, interaction(Spp, PopID, Block))
m1 <- lm(DaysAlive ~ Trt + int, data = mort_B23)
l2 <- glht(m1, linfct = mcp(int = "Tukey"))
summary(l2)
```

    ## 
    ##   Simultaneous Tests for General Linear Hypotheses
    ## 
    ## Multiple Comparisons of Means: Tukey Contrasts
    ## 
    ## 
    ## Fit: lm(formula = DaysAlive ~ Trt + int, data = mort_B23)
    ## 
    ## Linear Hypotheses:
    ##                                                            Estimate Std. Error
    ## littorina_obtusata.MA.2 - littorina_littorea.MA.2 == 0    1.000e-01  1.442e-01
    ## littorina_saxatilis.MA.2 - littorina_littorea.MA.2 == 0   1.000e-01  1.442e-01
    ## littorina_littorea.RI.2 - littorina_littorea.MA.2 == 0    3.333e-02  1.442e-01
    ## littorina_obtusata.RI.2 - littorina_littorea.MA.2 == 0   -1.000e-01  1.442e-01
    ## littorina_saxatilis.RI.2 - littorina_littorea.MA.2 == 0  -4.667e-01  1.442e-01
    ## littorina_littorea.MA.3 - littorina_littorea.MA.2 == 0    4.672e-01  1.431e-01
    ## littorina_obtusata.MA.3 - littorina_littorea.MA.2 == 0    6.667e-01  1.442e-01
    ## littorina_saxatilis.MA.3 - littorina_littorea.MA.2 == 0   3.971e-01  1.455e-01
    ## littorina_littorea.RI.3 - littorina_littorea.MA.2 == 0    4.333e-01  1.442e-01
    ## littorina_obtusata.RI.3 - littorina_littorea.MA.2 == 0    4.000e-01  1.442e-01
    ## littorina_saxatilis.RI.3 - littorina_littorea.MA.2 == 0   6.667e-02  1.442e-01
    ## littorina_saxatilis.MA.2 - littorina_obtusata.MA.2 == 0  -4.732e-15  1.442e-01
    ## littorina_littorea.RI.2 - littorina_obtusata.MA.2 == 0   -6.667e-02  1.442e-01
    ## littorina_obtusata.RI.2 - littorina_obtusata.MA.2 == 0   -2.000e-01  1.442e-01
    ## littorina_saxatilis.RI.2 - littorina_obtusata.MA.2 == 0  -5.667e-01  1.442e-01
    ## littorina_littorea.MA.3 - littorina_obtusata.MA.2 == 0    3.672e-01  1.431e-01
    ## littorina_obtusata.MA.3 - littorina_obtusata.MA.2 == 0    5.667e-01  1.442e-01
    ## littorina_saxatilis.MA.3 - littorina_obtusata.MA.2 == 0   2.971e-01  1.455e-01
    ## littorina_littorea.RI.3 - littorina_obtusata.MA.2 == 0    3.333e-01  1.442e-01
    ## littorina_obtusata.RI.3 - littorina_obtusata.MA.2 == 0    3.000e-01  1.442e-01
    ## littorina_saxatilis.RI.3 - littorina_obtusata.MA.2 == 0  -3.333e-02  1.442e-01
    ## littorina_littorea.RI.2 - littorina_saxatilis.MA.2 == 0  -6.667e-02  1.442e-01
    ## littorina_obtusata.RI.2 - littorina_saxatilis.MA.2 == 0  -2.000e-01  1.442e-01
    ## littorina_saxatilis.RI.2 - littorina_saxatilis.MA.2 == 0 -5.667e-01  1.442e-01
    ## littorina_littorea.MA.3 - littorina_saxatilis.MA.2 == 0   3.672e-01  1.431e-01
    ## littorina_obtusata.MA.3 - littorina_saxatilis.MA.2 == 0   5.667e-01  1.442e-01
    ## littorina_saxatilis.MA.3 - littorina_saxatilis.MA.2 == 0  2.971e-01  1.455e-01
    ## littorina_littorea.RI.3 - littorina_saxatilis.MA.2 == 0   3.333e-01  1.442e-01
    ## littorina_obtusata.RI.3 - littorina_saxatilis.MA.2 == 0   3.000e-01  1.442e-01
    ## littorina_saxatilis.RI.3 - littorina_saxatilis.MA.2 == 0 -3.333e-02  1.442e-01
    ## littorina_obtusata.RI.2 - littorina_littorea.RI.2 == 0   -1.333e-01  1.442e-01
    ## littorina_saxatilis.RI.2 - littorina_littorea.RI.2 == 0  -5.000e-01  1.442e-01
    ## littorina_littorea.MA.3 - littorina_littorea.RI.2 == 0    4.339e-01  1.431e-01
    ## littorina_obtusata.MA.3 - littorina_littorea.RI.2 == 0    6.333e-01  1.442e-01
    ## littorina_saxatilis.MA.3 - littorina_littorea.RI.2 == 0   3.638e-01  1.455e-01
    ## littorina_littorea.RI.3 - littorina_littorea.RI.2 == 0    4.000e-01  1.442e-01
    ## littorina_obtusata.RI.3 - littorina_littorea.RI.2 == 0    3.667e-01  1.442e-01
    ## littorina_saxatilis.RI.3 - littorina_littorea.RI.2 == 0   3.333e-02  1.442e-01
    ## littorina_saxatilis.RI.2 - littorina_obtusata.RI.2 == 0  -3.667e-01  1.442e-01
    ## littorina_littorea.MA.3 - littorina_obtusata.RI.2 == 0    5.672e-01  1.431e-01
    ## littorina_obtusata.MA.3 - littorina_obtusata.RI.2 == 0    7.667e-01  1.442e-01
    ## littorina_saxatilis.MA.3 - littorina_obtusata.RI.2 == 0   4.971e-01  1.455e-01
    ## littorina_littorea.RI.3 - littorina_obtusata.RI.2 == 0    5.333e-01  1.442e-01
    ## littorina_obtusata.RI.3 - littorina_obtusata.RI.2 == 0    5.000e-01  1.442e-01
    ## littorina_saxatilis.RI.3 - littorina_obtusata.RI.2 == 0   1.667e-01  1.442e-01
    ## littorina_littorea.MA.3 - littorina_saxatilis.RI.2 == 0   9.339e-01  1.431e-01
    ## littorina_obtusata.MA.3 - littorina_saxatilis.RI.2 == 0   1.133e+00  1.442e-01
    ## littorina_saxatilis.MA.3 - littorina_saxatilis.RI.2 == 0  8.638e-01  1.455e-01
    ## littorina_littorea.RI.3 - littorina_saxatilis.RI.2 == 0   9.000e-01  1.442e-01
    ## littorina_obtusata.RI.3 - littorina_saxatilis.RI.2 == 0   8.667e-01  1.442e-01
    ## littorina_saxatilis.RI.3 - littorina_saxatilis.RI.2 == 0  5.333e-01  1.442e-01
    ## littorina_obtusata.MA.3 - littorina_littorea.MA.3 == 0    1.994e-01  1.431e-01
    ## littorina_saxatilis.MA.3 - littorina_littorea.MA.3 == 0  -7.010e-02  1.443e-01
    ## littorina_littorea.RI.3 - littorina_littorea.MA.3 == 0   -3.388e-02  1.431e-01
    ## littorina_obtusata.RI.3 - littorina_littorea.MA.3 == 0   -6.722e-02  1.431e-01
    ## littorina_saxatilis.RI.3 - littorina_littorea.MA.3 == 0  -4.006e-01  1.431e-01
    ## littorina_saxatilis.MA.3 - littorina_obtusata.MA.3 == 0  -2.696e-01  1.455e-01
    ## littorina_littorea.RI.3 - littorina_obtusata.MA.3 == 0   -2.333e-01  1.442e-01
    ## littorina_obtusata.RI.3 - littorina_obtusata.MA.3 == 0   -2.667e-01  1.442e-01
    ## littorina_saxatilis.RI.3 - littorina_obtusata.MA.3 == 0  -6.000e-01  1.442e-01
    ## littorina_littorea.RI.3 - littorina_saxatilis.MA.3 == 0   3.622e-02  1.455e-01
    ## littorina_obtusata.RI.3 - littorina_saxatilis.MA.3 == 0   2.887e-03  1.455e-01
    ## littorina_saxatilis.RI.3 - littorina_saxatilis.MA.3 == 0 -3.304e-01  1.455e-01
    ## littorina_obtusata.RI.3 - littorina_littorea.RI.3 == 0   -3.333e-02  1.442e-01
    ## littorina_saxatilis.RI.3 - littorina_littorea.RI.3 == 0  -3.667e-01  1.442e-01
    ## littorina_saxatilis.RI.3 - littorina_obtusata.RI.3 == 0  -3.333e-01  1.442e-01
    ##                                                          t value Pr(>|t|)    
    ## littorina_obtusata.MA.2 - littorina_littorea.MA.2 == 0     0.693   0.9999    
    ## littorina_saxatilis.MA.2 - littorina_littorea.MA.2 == 0    0.693   0.9999    
    ## littorina_littorea.RI.2 - littorina_littorea.MA.2 == 0     0.231   1.0000    
    ## littorina_obtusata.RI.2 - littorina_littorea.MA.2 == 0    -0.693   0.9999    
    ## littorina_saxatilis.RI.2 - littorina_littorea.MA.2 == 0   -3.235   0.0591 .  
    ## littorina_littorea.MA.3 - littorina_littorea.MA.2 == 0     3.265   0.0542 .  
    ## littorina_obtusata.MA.3 - littorina_littorea.MA.2 == 0     4.622    <0.01 ***
    ## littorina_saxatilis.MA.3 - littorina_littorea.MA.2 == 0    2.730   0.2167    
    ## littorina_littorea.RI.3 - littorina_littorea.MA.2 == 0     3.004   0.1116    
    ## littorina_obtusata.RI.3 - littorina_littorea.MA.2 == 0     2.773   0.1949    
    ## littorina_saxatilis.RI.3 - littorina_littorea.MA.2 == 0    0.462   1.0000    
    ## littorina_saxatilis.MA.2 - littorina_obtusata.MA.2 == 0    0.000   1.0000    
    ## littorina_littorea.RI.2 - littorina_obtusata.MA.2 == 0    -0.462   1.0000    
    ## littorina_obtusata.RI.2 - littorina_obtusata.MA.2 == 0    -1.387   0.9655    
    ## littorina_saxatilis.RI.2 - littorina_obtusata.MA.2 == 0   -3.929    <0.01 ** 
    ## littorina_littorea.MA.3 - littorina_obtusata.MA.2 == 0     2.567   0.3033    
    ## littorina_obtusata.MA.3 - littorina_obtusata.MA.2 == 0     3.929    <0.01 ** 
    ## littorina_saxatilis.MA.3 - littorina_obtusata.MA.2 == 0    2.042   0.6639    
    ## littorina_littorea.RI.3 - littorina_obtusata.MA.2 == 0     2.311   0.4705    
    ## littorina_obtusata.RI.3 - littorina_obtusata.MA.2 == 0     2.080   0.6368    
    ## littorina_saxatilis.RI.3 - littorina_obtusata.MA.2 == 0   -0.231   1.0000    
    ## littorina_littorea.RI.2 - littorina_saxatilis.MA.2 == 0   -0.462   1.0000    
    ## littorina_obtusata.RI.2 - littorina_saxatilis.MA.2 == 0   -1.387   0.9655    
    ## littorina_saxatilis.RI.2 - littorina_saxatilis.MA.2 == 0  -3.929    <0.01 ** 
    ## littorina_littorea.MA.3 - littorina_saxatilis.MA.2 == 0    2.567   0.3028    
    ## littorina_obtusata.MA.3 - littorina_saxatilis.MA.2 == 0    3.929    <0.01 ** 
    ## littorina_saxatilis.MA.3 - littorina_saxatilis.MA.2 == 0   2.042   0.6637    
    ## littorina_littorea.RI.3 - littorina_saxatilis.MA.2 == 0    2.311   0.4704    
    ## littorina_obtusata.RI.3 - littorina_saxatilis.MA.2 == 0    2.080   0.6374    
    ## littorina_saxatilis.RI.3 - littorina_saxatilis.MA.2 == 0  -0.231   1.0000    
    ## littorina_obtusata.RI.2 - littorina_littorea.RI.2 == 0    -0.924   0.9988    
    ## littorina_saxatilis.RI.2 - littorina_littorea.RI.2 == 0   -3.466   0.0285 *  
    ## littorina_littorea.MA.3 - littorina_littorea.RI.2 == 0     3.033   0.1038    
    ## littorina_obtusata.MA.3 - littorina_littorea.RI.2 == 0     4.391    <0.01 ** 
    ## littorina_saxatilis.MA.3 - littorina_littorea.RI.2 == 0    2.500   0.3426    
    ## littorina_littorea.RI.3 - littorina_littorea.RI.2 == 0     2.773   0.1963    
    ## littorina_obtusata.RI.3 - littorina_littorea.RI.2 == 0     2.542   0.3172    
    ## littorina_saxatilis.RI.3 - littorina_littorea.RI.2 == 0    0.231   1.0000    
    ## littorina_saxatilis.RI.2 - littorina_obtusata.RI.2 == 0   -2.542   0.3182    
    ## littorina_littorea.MA.3 - littorina_obtusata.RI.2 == 0     3.964    <0.01 ** 
    ## littorina_obtusata.MA.3 - littorina_obtusata.RI.2 == 0     5.315    <0.01 ***
    ## littorina_saxatilis.MA.3 - littorina_obtusata.RI.2 == 0    3.417   0.0336 *  
    ## littorina_littorea.RI.3 - littorina_obtusata.RI.2 == 0     3.698   0.0131 *  
    ## littorina_obtusata.RI.3 - littorina_obtusata.RI.2 == 0     3.466   0.0285 *  
    ## littorina_saxatilis.RI.3 - littorina_obtusata.RI.2 == 0    1.155   0.9917    
    ## littorina_littorea.MA.3 - littorina_saxatilis.RI.2 == 0    6.527    <0.01 ***
    ## littorina_obtusata.MA.3 - littorina_saxatilis.RI.2 == 0    7.857    <0.01 ***
    ## littorina_saxatilis.MA.3 - littorina_saxatilis.RI.2 == 0   5.937    <0.01 ***
    ## littorina_littorea.RI.3 - littorina_saxatilis.RI.2 == 0    6.240    <0.01 ***
    ## littorina_obtusata.RI.3 - littorina_saxatilis.RI.2 == 0    6.009    <0.01 ***
    ## littorina_saxatilis.RI.3 - littorina_saxatilis.RI.2 == 0   3.698   0.0135 *  
    ## littorina_obtusata.MA.3 - littorina_littorea.MA.3 == 0     1.394   0.9641    
    ## littorina_saxatilis.MA.3 - littorina_littorea.MA.3 == 0   -0.486   1.0000    
    ## littorina_littorea.RI.3 - littorina_littorea.MA.3 == 0    -0.237   1.0000    
    ## littorina_obtusata.RI.3 - littorina_littorea.MA.3 == 0    -0.470   1.0000    
    ## littorina_saxatilis.RI.3 - littorina_littorea.MA.3 == 0   -2.800   0.1848    
    ## littorina_saxatilis.MA.3 - littorina_obtusata.MA.3 == 0   -1.853   0.7881    
    ## littorina_littorea.RI.3 - littorina_obtusata.MA.3 == 0    -1.618   0.9018    
    ## littorina_obtusata.RI.3 - littorina_obtusata.MA.3 == 0    -1.849   0.7896    
    ## littorina_saxatilis.RI.3 - littorina_obtusata.MA.3 == 0   -4.160    <0.01 ** 
    ## littorina_littorea.RI.3 - littorina_saxatilis.MA.3 == 0    0.249   1.0000    
    ## littorina_obtusata.RI.3 - littorina_saxatilis.MA.3 == 0    0.020   1.0000    
    ## littorina_saxatilis.RI.3 - littorina_saxatilis.MA.3 == 0  -2.271   0.4986    
    ## littorina_obtusata.RI.3 - littorina_littorea.RI.3 == 0    -0.231   1.0000    
    ## littorina_saxatilis.RI.3 - littorina_littorea.RI.3 == 0   -2.542   0.3175    
    ## littorina_saxatilis.RI.3 - littorina_obtusata.RI.3 == 0   -2.311   0.4710    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## (Adjusted p values reported -- single-step method)
