library(tidyverse)
library(readxl)
library(reshape2)
library(ggplot2)
library(lm.beta)
library(devtools)
library(plyr)
library(GGally)
library(car)
library(broom)

rm(list=ls())
ad.df <- read_xlsx("SPARE_AD_SPARE_BA_HVA_202000417.xlsx")
table(ad.df$homocysteine_2_outlier제거, useNA = "always")

ad.lm <- ad.df %>%
  select(id=Ref_id, dia = 최종임상진단, sex=성별, edu=교육연수, age=연령,
         VRSnonDM=VRS_DM제외, VitB12 = VitB12RIA, Folate = FolateRIA, 
         Creatinine = CreatinineS, 
         Homocysteine = homocysteine_2_outlier제거,
         DM = b03_diabetes, HbA1c, HBA1c.f = HbA1c_사분위수, 
         SPARE_AD_index = SPARE_AD_Volume_ICVadjusted, 
         SPARE_BA_index = SPARE_BA_Volume_ICVadjusted)

ad.lm <- ad.lm %>%
  filter(complete.cases(.))

table(ad.lm$Homocysteine, useNA = "always")


ad.lm$DM[ad.lm$DM == 0] <- 2 
ad.lm$DM <- factor(ad.lm$DM)
ad.lm$DM <- relevel(ad.lm$DM, ref = "2")


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


ggplot(ad.nondm, aes(x = Homocysteine, y = SPARE_AD_index)) + 
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)


# avPlots(model1, ~ Homocysteine, ellipse=list(levels=0.5))
# 
# avPlots(lm(SPARE_AD_index ~ dia + sex + age + edu + VRSnonDM + VitB12 + 
#              Folate + Creatinine + Homocysteine, data = ad.nondm))



fit1 <- lm(SPARE_BA_index ~ dia + sex + age + edu + VRSnonDM + VitB12 + 
             Folate + Creatinine + Homocysteine, data = ad.nondm)

fit2 <- lm(SPARE_BA_index ~ dia + sex + age + edu + VRSnonDM + VitB12 + 
             Folate + Creatinine + Homocysteine, data = ad.dm)

(mod_vars = all.vars(formula(fit1))[-1])

preddat_fun = function(data, allvars, var) {
  sums = summarise_at(data, 
                      vars( one_of(allvars), -one_of(var) ), 
                      median) 
  cbind( select_at(data, var), sums)
}

head(preddat_fun(ad.nondm, mod_vars, "sex"))

pred_dats = mod_vars %>%
  set_names() %>%
  map(~preddat_fun(ad.nondm, mod_vars, .x) )


preds_nondm = pred_dats %>%
  map(~augment(fit1, newdata = .x) ) %>%
  map(~mutate(.x, 
              lower = .fitted - 2*.se.fit,
              upper = .fitted + 2*.se.fit,
              pred = .fitted))


pred_dats2 = mod_vars %>%
  set_names() %>%
  map(~preddat_fun(ad.dm, mod_vars, .x) )



preds_dm = pred_dats2 %>%
  map(~augment(fit2, newdata = .x) ) %>%
  map(~mutate(.x, 
              lower = .fitted - 2*.se.fit,
              upper = .fitted + 2*.se.fit,
              pred = .fitted))

#fit1, fit2에서 SPARE_AD_index, SPARE_BA_index로 수정하면서 아래 plot 그리기

ggplot(ad.lm, aes(x = Homocysteine, y = SPARE_AD_index, col = DM)) + 
  geom_point(alpha = 0.6) +
  scale_color_manual(name = "DM",
                     values = c("#2F66F7", "#a50000"),
                     labels=c("Negative", "Positive"))  +
  geom_line(data = preds_dm$Homocysteine, aes(y = pred), size = 1, col = "#a50000") +
  geom_line(data = preds_nondm$Homocysteine, aes(y = pred), size = 1, col = "#2F66F7") +
  xlab("Serum Homocysteine") +
  ylab("SPARE-AD index") +
  scale_x_continuous(expand=c(0.01,0.01)) +
  theme(
    legend.position = c(0.9, 0.9),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 20,  face="bold",
                                margin = margin(t = 20, r = 0, b = 0, l = 0)),
    axis.text.x = element_text(size = 20,  face="bold"),
    axis.title.y = element_text(size = 20, face="bold",
                                margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.text.y = element_text(size = 20, face="bold"),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_blank(),
    panel.background = element_blank())


ggplot(ad.lm, aes(x = Homocysteine, y = SPARE_BA_index, col = DM)) + 
  geom_point(alpha = 0.6) +
  scale_color_manual(name = "DM",
                     values = c("#2F66F7", "#a50000"),
                     labels=c("Negative", "Positive"))  +
  geom_line(data = preds_dm$Homocysteine, aes(y = pred), size = 1, col = "#a50000") +
  geom_line(data = preds_nondm$Homocysteine, aes(y = pred), size = 1, col = "#2F66F7") +
  xlab("Serum Homocysteine") +
  ylab("SPARE-BA index") +
  scale_x_continuous(expand=c(0.01,0.01)) +
  theme(
    legend.position = c(0.9, 0.9),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 20,  face="bold",
                                margin = margin(t = 20, r = 0, b = 0, l = 0)),
    axis.text.x = element_text(size = 20,  face="bold"),
    axis.title.y = element_text(size = 20, face="bold",
                                margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.text.y = element_text(size = 20, face="bold"),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_blank(),
    panel.background = element_blank())
