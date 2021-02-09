# Final Linear Model as of 12/07/2020

#### mort17

brglm(cbind(DaysAlive, DaysDead) ~ Spp * PopID * Trt * Block - Spp:PopID:Trt:Block - Spp:Trt:Block - Spp:PopID:Trt - Spp:PopID:Block - PopID:Trt:Block - PopID:Block - Spp:Block, data = mort_B23, family = binomial(link = "logit"))

> summary(mort17)

![image](https://github.com/FfionT/LittorinaThermal2019/blob/master/notebook/notebook_figures/mort17_summary.png){width=50%}

> plot(mort17)

| | |
|---|---|
| ![image](https://github.com/FfionT/LittorinaThermal2019/blob/master/notebook/notebook_figures/mort17_plot1.png) | ![image](https://github.com/FfionT/LittorinaThermal2019/blob/master/notebook/notebook_figures/mort17_plot2.png) |
| ![image](https://github.com/FfionT/LittorinaThermal2019/blob/master/notebook/notebook_figures/mort17_plot3.png) | ![image](https://github.com/FfionT/LittorinaThermal2019/blob/master/notebook/notebook_figures/mort17_plot4.png) |