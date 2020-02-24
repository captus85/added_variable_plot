library(tidyverse)
library(readxl)
library(reshape2)
library(ggplot2)
library(lm.beta)
library(devtools)
require(ggiraph)
require(ggiraphExtra)
require(plyr)
library(GGally)
rm(list=ls())
ad.df <- read_xlsx("ad.xlsx")

ad.lm <- ad.df %>%
  select(id=Ref_id, dia = 최종임상진단, sex=성별, edu=교육연수, age=연령,
         VRSnonDM=VRS_DM제외, VitB12, Folate, Creatinine, Homocysteine,
         DM, HbA1c, HBA1c.f = HbA1c_사분위수, SPARE_AD_index, SPARE_BA_index)



ad.lm$DM[ad.lm$DM == 0] <- 2 
ad.lm$DM <- factor(ad.lm$DM)



ad.dm <- ad.lm %>%
  filter(DM == 1)

ad_dm_lm <- lm(SPARE_AD_index ~ dia + sex + age + edu + VRSnonDM + VitB12 + 
     Folate + Creatinine + Homocysteine, data = ad.dm)
tidy(ad_dm_lm)
lm.beta(ad_dm_lm)

ggplot(ad.dm, aes(x = Homocysteine, y = SPARE_AD_index)) + 
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)



ad.nondm <- ad.lm %>%
  filter(DM == 2)

ad_nondm_lm <- lm(SPARE_AD_index ~ dia + sex + age + edu + VRSnonDM + VitB12 + 
              Folate + Creatinine + Homocysteine, data = ad.nondm)

tidy(ad_nondm_lm)
lm.beta(ad_nondm_lm)

model1 <- lm(SPARE_AD_index ~ dia + sex + age + edu + VRSnonDM + VitB12 + 
               Folate + Creatinine + Homocysteine, data = ad.nondm)

avPlots(model1, ~ Homocysteine, ellipse=list(levels=0.5))


avPlots(lm(SPARE_AD_index ~ dia + sex + age + edu + VRSnonDM + VitB12 + 
            Folate + Creatinine + Homocysteine, data = ad.nondm))


ggplot(ad.nondm, aes(x = Homocysteine, y = SPARE_AD_index)) + 
  geom_point() +
  ggcoef(ad_nondm_lm)
