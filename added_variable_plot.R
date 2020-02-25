# library(tidyverse)
# library(readxl)
# library(reshape2)
# library(ggplot2)
# library(lm.beta)
# library(devtools)
# library(plyr)
# library(GGally)
# library(car)

rm(list=ls())
ad.df <- read_xlsx("ad.xlsx")

ad.lm <- ad.df %>%
  select(id=Ref_id, dia = 최종임상진단, sex=성별, edu=교육연수, age=연령,
         VRSnonDM=VRS_DM제외, VitB12, Folate, Creatinine, Homocysteine,
         DM, HbA1c, HBA1c.f = HbA1c_사분위수, SPARE_AD_index, SPARE_BA_index)

ad.lm <- ad.lm %>%
  filter(complete.cases(.))

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

(mod_vars = all.vars( formula(fit1) )[-1])

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

pred_dats2$Homocysteine[68,]
range(pred_dats$Homocysteine[,1])
pred_dats2$Homocysteine[69,] <- c(64.09, 2, 0.5, 74, 12, 1, 477.5, 8.6, 1.05)
pred_dats2$Homocysteine[70,] <- c(7.29, 2, 0.5, 74, 12, 1, 477.5, 8.6, 1.05)

preds_dm = pred_dats2 %>%
  map(~augment(fit2, newdata = .x) ) %>%
  map(~mutate(.x, 
              lower = .fitted - 2*.se.fit,
              upper = .fitted + 2*.se.fit,
              pred = .fitted))



ggplot(ad.lm, aes(x = Homocysteine, y = SPARE_AD_index, col = DM)) + 
  geom_point(alpha = 0.6) +
  scale_color_manual(name = "DM",
                     values = c("#fac409", "#0b8def"),
                     labels=c("Positive", "Negative"))  +
  geom_line(data = preds_dm$Homocysteine, aes(y = pred), size = 1, col = "#fac409") +
  geom_line(data = preds_nondm$Homocysteine, aes(y = pred), size = 1, col = "#0b8def") +
  xlab("Serum Homocysteine") +
  ylab("SPARE-AD index") +
  scale_x_continuous(expand=c(0.01,0.01)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme_classic()


ggplot(ad.lm, aes(x = Homocysteine, y = SPARE_BA_index, col = DM)) + 
  geom_point(alpha = 0.6) +
  scale_color_manual(name = "DM",
                     values = c("#fac409", "#0b8def"),
                     labels=c("Positive", "Negative"))  +
  geom_line(data = preds_dm$Homocysteine, aes(y = pred), size = 1, col = "#fac409") +
  geom_line(data = preds_nondm$Homocysteine, aes(y = pred), size = 1, col = "#0b8def") +
  xlab("Serum Homocysteine") +
  ylab("SPARE-BA index") +
  scale_x_continuous(expand=c(0.01,0.01)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme_classic()
