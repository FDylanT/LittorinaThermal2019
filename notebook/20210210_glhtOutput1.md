# glht Output - Pairwise Comparisons

Simultaneous Tests for General Linear Hypotheses

Multiple Comparisons of Means: Tukey Contrasts

Fit: lm(formula = DaysAlive ~ Trt + int, data = mort_B23)

Linear Hypotheses:  

|                                                           | Estimate | Std. Error | t value | Pr(>|t|) |
|---|---|---|---|---|
|littorina_obtusata.MA.2 - littorina_littorea.MA.2 == 0   | 1.000e-01  1.442e-01   0.693   0.9999    |
|littorina_saxatilis.MA.2 - littorina_littorea.MA.2 == 0  | 1.000e-01  1.442e-01   0.693   0.9999    |
|littorina_littorea.RI.2 - littorina_littorea.MA.2 == 0   | 3.333e-02  1.442e-01   0.231   1.0000    |
|littorina_obtusata.RI.2 - littorina_littorea.MA.2 == 0   |-1.000e-01  1.442e-01  -0.693   0.9999    |
|littorina_saxatilis.RI.2 - littorina_littorea.MA.2 == 0  |-4.667e-01  1.442e-01  -3.235   0.0593 .  |
|littorina_littorea.MA.3 - littorina_littorea.MA.2 == 0   | 4.672e-01  1.431e-01   3.265   0.0538 .  |
|littorina_obtusata.MA.3 - littorina_littorea.MA.2 == 0   | 6.667e-01  1.442e-01   4.622    <0.01 ***|
|littorina_saxatilis.MA.3 - littorina_littorea.MA.2 == 0  | 3.971e-01  1.455e-01   2.730   0.2159    |
|littorina_littorea.RI.3 - littorina_littorea.MA.2 == 0   | 4.333e-01  1.442e-01   3.004   0.1113    |
|littorina_obtusata.RI.3 - littorina_littorea.MA.2 == 0   | 4.000e-01  1.442e-01   2.773   0.1965    |
|littorina_saxatilis.RI.3 - littorina_littorea.MA.2 == 0  | 6.667e-02  1.442e-01   0.462   1.0000    |
|littorina_saxatilis.MA.2 - littorina_obtusata.MA.2 == 0  |-4.732e-15  1.442e-01   0.000   1.0000    |
|littorina_littorea.RI.2 - littorina_obtusata.MA.2 == 0   |-6.667e-02  1.442e-01  -0.462   1.0000    |
|littorina_obtusata.RI.2 - littorina_obtusata.MA.2 == 0   |-2.000e-01  1.442e-01  -1.387   0.9655    |
|littorina_saxatilis.RI.2 - littorina_obtusata.MA.2 == 0  |-5.667e-01  1.442e-01  -3.929    <0.01 ** |
|littorina_littorea.MA.3 - littorina_obtusata.MA.2 == 0   | 3.672e-01  1.431e-01   2.567   0.3039    |
|littorina_obtusata.MA.3 - littorina_obtusata.MA.2 == 0   | 5.667e-01  1.442e-01   3.929    <0.01 ** |
|littorina_saxatilis.MA.3 - littorina_obtusata.MA.2 == 0  | 2.971e-01  1.455e-01   2.042   0.6639    |
|littorina_littorea.RI.3 - littorina_obtusata.MA.2 == 0   | 3.333e-01  1.442e-01   2.311   0.4709    |
|littorina_obtusata.RI.3 - littorina_obtusata.MA.2 == 0   | 3.000e-01  1.442e-01   2.080   0.6373    |
|littorina_saxatilis.RI.3 - littorina_obtusata.MA.2 == 0  |-3.333e-02  1.442e-01  -0.231   1.0000    |
|littorina_littorea.RI.2 - littorina_saxatilis.MA.2 == 0  |-6.667e-02  1.442e-01  -0.462   1.0000    |
|littorina_obtusata.RI.2 - littorina_saxatilis.MA.2 == 0  |-2.000e-01  1.442e-01  -1.387   0.9655    |
|littorina_saxatilis.RI.2 - littorina_saxatilis.MA.2 == 0 |-5.667e-01  1.442e-01  -3.929    <0.01 ** |
|littorina_littorea.MA.3 - littorina_saxatilis.MA.2 == 0  | 3.672e-01  1.431e-01   2.567   0.3026    |
|littorina_obtusata.MA.3 - littorina_saxatilis.MA.2 == 0  | 5.667e-01  1.442e-01   3.929    <0.01 ** |
|littorina_saxatilis.MA.3 - littorina_saxatilis.MA.2 == 0 | 2.971e-01  1.455e-01   2.042   0.6639    |
|littorina_littorea.RI.3 - littorina_saxatilis.MA.2 == 0  | 3.333e-01  1.442e-01   2.311   0.4704    |
|littorina_obtusata.RI.3 - littorina_saxatilis.MA.2 == 0  | 3.000e-01  1.442e-01   2.080   0.6370    |
|littorina_saxatilis.RI.3 - littorina_saxatilis.MA.2 == 0 |-3.333e-02  1.442e-01  -0.231   1.0000    |
|littorina_obtusata.RI.2 - littorina_littorea.RI.2 == 0   |-1.333e-01  1.442e-01  -0.924   0.9988    |
|littorina_saxatilis.RI.2 - littorina_littorea.RI.2 == 0  |-5.000e-01  1.442e-01  -3.466   0.0292 *  |
|littorina_littorea.MA.3 - littorina_littorea.RI.2 == 0   | 4.339e-01  1.431e-01   3.033   0.1036    |
|littorina_obtusata.MA.3 - littorina_littorea.RI.2 == 0   | 6.333e-01  1.442e-01   4.391    <0.01 ***|
|littorina_saxatilis.MA.3 - littorina_littorea.RI.2 == 0  | 3.638e-01  1.455e-01   2.500   0.3425    |
|littorina_littorea.RI.3 - littorina_littorea.RI.2 == 0   | 4.000e-01  1.442e-01   2.773   0.1954    |
|littorina_obtusata.RI.3 - littorina_littorea.RI.2 == 0   | 3.667e-01  1.442e-01   2.542   0.3164    |
|littorina_saxatilis.RI.3 - littorina_littorea.RI.2 == 0  | 3.333e-02  1.442e-01   0.231   1.0000    |
|littorina_saxatilis.RI.2 - littorina_obtusata.RI.2 == 0  |-3.667e-01  1.442e-01  -2.542   0.3177    |
|littorina_littorea.MA.3 - littorina_obtusata.RI.2 == 0   | 5.672e-01  1.431e-01   3.964    <0.01 ** |
|littorina_obtusata.MA.3 - littorina_obtusata.RI.2 == 0   | 7.667e-01  1.442e-01   5.315    <0.01 ***|
|littorina_saxatilis.MA.3 - littorina_obtusata.RI.2 == 0  | 4.971e-01  1.455e-01   3.417   0.0339 *  |
|littorina_littorea.RI.3 - littorina_obtusata.RI.2 == 0   | 5.333e-01  1.442e-01   3.698   0.0130 *  |
|littorina_obtusata.RI.3 - littorina_obtusata.RI.2 == 0   | 5.000e-01  1.442e-01   3.466   0.0291 *  |
|littorina_saxatilis.RI.3 - littorina_obtusata.RI.2 == 0  | 1.667e-01  1.442e-01   1.155   0.9918    |
|littorina_littorea.MA.3 - littorina_saxatilis.RI.2 == 0  | 9.339e-01  1.431e-01   6.527    <0.01 ***|
|littorina_obtusata.MA.3 - littorina_saxatilis.RI.2 == 0  | 1.133e+00  1.442e-01   7.857    <0.01 ***|
|littorina_saxatilis.MA.3 - littorina_saxatilis.RI.2 == 0 | 8.638e-01  1.455e-01   5.937    <0.01 ***|
|littorina_littorea.RI.3 - littorina_saxatilis.RI.2 == 0  | 9.000e-01  1.442e-01   6.240    <0.01 ***|
|littorina_obtusata.RI.3 - littorina_saxatilis.RI.2 == 0  | 8.667e-01  1.442e-01   6.009    <0.01 ***|
|littorina_saxatilis.RI.3 - littorina_saxatilis.RI.2 == 0 | 5.333e-01  1.442e-01   3.698   0.0133 *  |
|littorina_obtusata.MA.3 - littorina_littorea.MA.3 == 0   | 1.994e-01  1.431e-01   1.394   0.9642    |
|littorina_saxatilis.MA.3 - littorina_littorea.MA.3 == 0  |-7.010e-02  1.443e-01  -0.486   1.0000    |
|littorina_littorea.RI.3 - littorina_littorea.MA.3 == 0   |-3.388e-02  1.431e-01  -0.237   1.0000    |
|littorina_obtusata.RI.3 - littorina_littorea.MA.3 == 0   |-6.722e-02  1.431e-01  -0.470   1.0000    |
|littorina_saxatilis.RI.3 - littorina_littorea.MA.3 == 0  |-4.006e-01  1.431e-01  -2.800   0.1848    |
|littorina_saxatilis.MA.3 - littorina_obtusata.MA.3 == 0  |-2.696e-01  1.455e-01  -1.853   0.7870    |
|littorina_littorea.RI.3 - littorina_obtusata.MA.3 == 0   |-2.333e-01  1.442e-01  -1.618   0.9017    |
|littorina_obtusata.RI.3 - littorina_obtusata.MA.3 == 0   |-2.667e-01  1.442e-01  -1.849   0.7891    |
|littorina_saxatilis.RI.3 - littorina_obtusata.MA.3 == 0  |-6.000e-01  1.442e-01  -4.160    <0.01 ** |
|littorina_littorea.RI.3 - littorina_saxatilis.MA.3 == 0  | 3.622e-02  1.455e-01   0.249   1.0000    |
|littorina_obtusata.RI.3 - littorina_saxatilis.MA.3 == 0  | 2.887e-03  1.455e-01   0.020   1.0000    |
|littorina_saxatilis.RI.3 - littorina_saxatilis.MA.3 == 0 |-3.304e-01  1.455e-01  -2.271   0.4993    |
|littorina_obtusata.RI.3 - littorina_littorea.RI.3 == 0   |-3.333e-02  1.442e-01  -0.231   1.0000    |
|littorina_saxatilis.RI.3 - littorina_littorea.RI.3 == 0  |-3.667e-01  1.442e-01  -2.542   0.3177    |
|littorina_saxatilis.RI.3 - littorina_obtusata.RI.3 == 0  |-3.333e-01  1.442e-01  -2.311   0.4700    |
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
(Adjusted p values reported -- single-step method)