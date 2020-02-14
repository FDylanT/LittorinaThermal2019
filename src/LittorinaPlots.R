# Load libraries
library(tidyverse)

# Load data
Litt <- read.csv("~/-Lab/Datasheets/LittorinaData.csv")
Resp <- read.csv("~/-Lab/Datasheets/RespirationData.csv")

mortData_all <- Litt %>%
  rename(Spp = Genus_Species) %>%
  mutate(Spp = recode(Spp, littorina_littorea = "LL", littorina_obtusata = "LO", littorina_saxatilis = "LS")) %>%
  mutate(SampleID = as.character(SampleID))

mortData <- mortData_all %>%
  filter(PreTrtMortNotes == "")

# Plot overall weight variation

### Histogram
ggplot(data = mortData, aes(x = WetWeight, fill = Spp)) +
  geom_histogram(binwidth = 0.1) +
  labs(x = "Wet Weight (g)", y = "Number of Individuals", title = "Size Distribution of Samples") +
  scale_fill_discrete(name = "Species", labels = c("L. littorea", "L. obtusata", "L. saxatilis")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.text = element_text(face = "italic"))

### Boxplots
ggplot(data = mortData, aes(x = Spp, y = WetWeight, colour = Spp, linetype = PopID)) +
  geom_boxplot() +
  labs(x = "Species", y = "Wet Weight (g)", title = "Size Distribution of Samples") +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5)) +
  scale_x_discrete(labels = c("L. littorea", "L. obtusata", "L. saxatilis")) +
  scale_linetype_discrete(name = "Population", labels = c("Northern", "Southern")) +
  guides(colour = FALSE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(face = "italic"))

# Plot weight variation by block
mortB1 <- mortData %>%
  filter(Block == 1)

ggplot(data = mortB1, aes(x = Spp, y = WetWeight, colour = Spp, linetype = PopID)) +
  geom_boxplot() +
  labs(x = "Species", y = "Wet Weight (g)", title = "Size Distribution of Block 1 Samples") +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5)) +
  scale_x_discrete(labels = c("L. littorea", "L. obtusata", "L. saxatilis")) +
  scale_linetype_discrete(name = "Population", labels = c("Northern", "Southern")) +
  guides(colour = FALSE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(face = "italic"))

mortB2 <- mortData %>%
  filter(Block == 2)

ggplot(data = mortB2, aes(x = Spp, y = WetWeight, colour = Spp, linetype = PopID)) +
  geom_boxplot() +
  labs(x = "Species", y = "Wet Weight (g)", title = "Size Distribution of Block 2 Samples") +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5)) +
  scale_x_discrete(labels = c("L. littorea", "L. obtusata", "L. saxatilis")) +
  scale_linetype_discrete(name = "Population", labels = c("Northern", "Southern")) +
  guides(colour = FALSE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(face = "italic"))

mortB3 <- mortData %>%
  filter(Block == 3)

ggplot(data = mortB3, aes(x = Spp, y = WetWeight, colour = Spp, linetype = PopID)) +
  geom_boxplot() +
  labs(x = "Species", y = "Wet Weight (g)", title = "Size Distribution of Block 3 Samples") +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5)) +
  scale_x_discrete(labels = c("L. littorea", "L. obtusata", "L. saxatilis")) +
  scale_linetype_discrete(name = "Population", labels = c("Northern", "Southern")) +
  guides(colour = FALSE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title.x = element_text(face = "italic"))

# Plot mortality

### Block 1
mortB1_grp <- mortB1 %>%
  rename("1" = TrtDay1_Survived, "1.2" = Day2_Survived, "3" = TrtDay3_Survived) %>%
  gather("1", "1.2", "3", key = "TrtDay", value = "Survived") %>%
  mutate(Surv_Int = as.integer(Survived), Tot_Int = 1) %>%
  group_by(Spp, PopID, Trt, TrtDay) %>%
  summarise(Surv = sum(Surv_Int), Tot = sum(Tot_Int)) %>%
  mutate(Percent_Surv = Surv/Tot)

ggplot(mortB1_grp, aes(x = TrtDay, y = Percent_Surv, group = interaction(PopID, Trt), shape = PopID, linetype = PopID, colour = Trt)) +
  geom_line() +
  geom_point() +
  labs(x = "Days of Treatment", y = "Percent Survival", title = "Block 1 Mortality") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +
  scale_shape_discrete(name = "Population", labels = c("Northern", "Southern")) +
  scale_linetype_discrete(name = "Population", labels = c("Northern", "Southern")) +
  scale_colour_manual(name = "Treatment", labels = c("0C, 0C", "50C, 45C", "19C, 19C"), values = c("dodgerblue", "orangered", "black")) +
  guides(shape = guide_legend(order = 1), linetype = guide_legend(order = 1)) +
  facet_wrap( ~ Spp, labeller = labeller(Spp = c(LL = "L. littorea", LO = "L. obtusata", LS = "L. saxatilis"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), strip.text = element_text(face = "italic"))

### Block 2
mort_grp23 <- mortData %>%
  filter(Block == 2 | Block == 3) %>%
  rename("1" = TrtDay1_Survived, "2" = TrtDay2_Survived) %>%
  gather("1", "2", key = "TrtDay", value = "Survived") %>%
  mutate(Surv_Int = as.integer(Survived), Tot_Int = 1) %>%
  group_by(Block, Spp, PopID, Trt, TrtDay) %>%
  summarise(Surv = sum(Surv_Int), Tot = sum(Tot_Int)) %>%
  mutate(Percent_Surv = Surv/Tot)

mortB2_grp <- mort_grp23 %>%
  filter(Block == 2)

ggplot(mortB2_grp, aes(x = TrtDay, y = Percent_Surv, group = interaction(PopID, Trt), shape = PopID, linetype = PopID, colour = Trt)) +
  geom_line() +
  geom_point() +
  labs(x = "Days of Treatment", y = "Percent Survival", title = "Block 2 Mortality") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +
  scale_shape_discrete(name = "Population", labels = c("Northern", "Southern")) +
  scale_linetype_discrete(name = "Population", labels = c("Northern", "Southern")) +
  scale_colour_manual(name = "Treatment", labels = c("-13C, -12C", "47C, 44C", "20C, 19C"), values = c("dodgerblue", "orangered", "black")) +
  guides(shape = guide_legend(order = 1), linetype = guide_legend(order = 1)) +
  facet_wrap( ~ Spp, labeller = labeller(Spp = c(LL = "L. littorea", LO = "L. obtusata", LS = "L. saxatilis"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), strip.text = element_text(face = "italic"))

### Block 3
mortB3_grp <- mort_grp23 %>%
  filter(Block == 3)

ggplot(mortB3_grp, aes(x = TrtDay, y = Percent_Surv, group = interaction(PopID, Trt), shape = PopID, linetype = PopID, colour = Trt)) +
  geom_line() +
  geom_point() +
  labs(x = "Days of Treatment", y = "Percent Survival", title = "Block 3 Mortality") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +
  scale_shape_discrete(name = "Population", labels = c("Northern", "Southern")) +
  scale_linetype_discrete(name = "Population", labels = c("Northern", "Southern")) +
  scale_colour_manual(name = "Treatment", labels = c("-12C, -12C", "42C, 46C", "21C, 20C"), values = c("dodgerblue", "orangered", "black")) +
  guides(shape = guide_legend(order = 1), linetype = guide_legend(order = 1)) +
  facet_wrap( ~ Spp, labeller = labeller(Spp = c(LL = "L. littorea", LO = "L. obtusata", LS = "L. saxatilis"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), strip.text = element_text(face = "italic"))

### Blocks 2 & 3
mortB23_grp <- mortData %>%
  filter(Block != 1) %>%
  rename("1" = TrtDay1_Survived, "2" = TrtDay2_Survived) %>%
  gather("1", "2", key = "TrtDay", value = "Survived") %>%
  mutate(Surv_Int = as.integer(Survived), Tot_Int = 1) %>%
  group_by(Spp, PopID, Trt, TrtDay) %>%
  summarise(Surv = sum(Surv_Int), Tot = sum(Tot_Int)) %>%
  mutate(Percent_Surv = Surv/Tot)

ggplot(mortB23_grp, aes(x = TrtDay, y = Percent_Surv, group = interaction(PopID, Trt), shape = PopID, linetype = PopID, colour = Trt)) +
  geom_line() +
  geom_point() +
  labs(x = "Days of Treatment", y = "Percent Survival", title = "Mortality, Blocks 2 & 3") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +
  scale_shape_discrete(name = "Population", labels = c("Northern", "Southern")) +
  scale_linetype_discrete(name = "Population", labels = c("Northern", "Southern")) +
  scale_colour_manual(name = "Treatment", labels = c("Cold Shock", "Heat Shock", "Control"), values = c("dodgerblue", "orangered", "black")) +
  guides(shape = guide_legend(order = 1), linetype = guide_legend(order = 1)) +
  facet_wrap( ~ Spp, labeller = labeller(Spp = c(LL = "L. littorea", LO = "L. obtusata", LS = "L. saxatilis"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), strip.text = element_text(face = "italic"))

# Plot respiration variation (B3)
respData <- Resp %>%
  filter(!is.na(Block & VialN)) %>%
  rename(MaxRate = MaxRespRate_umolO2.hr) %>%
  mutate(Spp = as.factor(str_extract(SampleID, "L[LOS]"))) %>%
  mutate(SampleN = as.integer(as.character(SampleN))) %>%
  mutate(SampleID = as.character(SampleID))

mort_resp_all <- right_join(mortData_all, respData)

ggplot(mort_resp_all, aes(x = Spp, y = MaxRate, colour = Spp, linetype = PopID)) +
  geom_boxplot() +
  labs(x = "Species", y = "Respiration Rate (umol O2 / hr)", title = "Respiration Rate Distribution of Block 3 Samples") +
  scale_x_discrete(labels = c("L. littorea", "L. obtusata", "L. saxatilis")) +
  scale_linetype_discrete(name = "Population", labels = c("Northern", "Southern")) +
  guides(colour = FALSE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(face = "italic"))

# Plot respiration vs weight
mort_resp <- mort_resp_all %>%
  filter(PreTrtMortNotes == "" | PreTrtMortNotes == "wrong spp, orig. labeled littorina_saxatilis")

ggplot(mort_resp, aes(x = WetWeight, y = MaxRate, colour = Spp, shape = PopID, linetype = PopID)) +
  geom_point() +
  labs(x = "Wet Weight (g)", y = "Maximum Respiration Rate (umol O2 / hr)", title = "Block 3 Samples") +
  scale_colour_discrete(name = "Species", labels = c("L. littorea", "L. obtusata", "L. saxatilis"),
                        guide = guide_legend(label.theme = element_text(size = 10, face = "italic"))) +
  scale_shape_discrete(name = "Population", labels = c("Northern", "Southern")) +
  scale_linetype_discrete(name = "Population", labels = c("Northern", "Southern")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
