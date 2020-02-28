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
ad.df <- read_xlsx("ad.xlsx")


ad.df <- ad.df %>%
  mutate(HbA1c_quartile_positivity = case_when(HbA1c_사분위수 %in% c(0,1,2) ~ 0,
                                               HbA1c_사분위수 == 3 ~ 1))
ad.lm <- ad.df %>%
  select(id=Ref_id, dia = 최종임상진단, sex=성별, edu=교육연수, age=연령,
         VRSnonDM=VRS_DM제외, VitB12, Folate, Creatinine, Homocysteine,
         DM, HbA1c, HbA1c_quartile_positivity, SPARE_AD_index, SPARE_BA_index)

ad.lm <- ad.lm %>%
  filter(complete.cases(.))


ad.dm <- ad.lm %>%
  filter(HbA1c_quartile_positivity == 1)


ad.nondm <- ad.lm %>%
  filter(HbA1c_quartile_positivity == 0)


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

pred_dats2$Homocysteine[69,] <- c(64.09, 2, 0, 73.5, 12, 1, 487, 8.8, 1.03)
pred_dats2$Homocysteine[70,] <- c(7.29, 2, 0, 73.5, 12, 1, 487, 8.8, 1.03)

preds_dm = pred_dats2 %>%
  map(~augment(fit2, newdata = .x) ) %>%
  map(~mutate(.x, 
              lower = .fitted - 2*.se.fit,
              upper = .fitted + 2*.se.fit,
              pred = .fitted))


#fit1, fit2에서 SPARE_AD_index, SPARE_BA_index로 수정하면서 아래 plot 그리기

ggplot(ad.lm, aes(x = Homocysteine, y = SPARE_BA_index, col = factor(HbA1c_quartile_positivity))) + 
  geom_point(alpha = 0.6) +
  scale_color_manual(name = "HbA1c level",
                     values = c("#2F66F7", "#a50000"),
                     labels=c("75 % <", "75 % >="))  +
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


ggplot(ad.lm, aes(x = Homocysteine, y = SPARE_AD_index, col = factor(HbA1c_quartile_positivity))) + 
  geom_point(alpha = 0.6) +
  scale_color_manual(name = "HbA1c level",
                     values = c("#2F66F7", "#a50000"),
                     labels=c("75 % <", "75 % >="))  +
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
    
