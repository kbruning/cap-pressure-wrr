library(R.matlab)

data <- readMat("FP_ND_EditedData.mat")

porosity <- 0.36

sw <- data$sw
epn <- (1-sw)*porosity
epwn <- data$epwn
epns <- data$epns
Jpwn <- data$Jpwn
epwn.epns <- data$epns+data$epwn
Jpwnepwn <- data$Jpwn*data$epwn
Jpnsepns <- data$Jpns.epns
Jpwnepwn.Jpnsepns <- Jpwnepwn + Jpnsepns
Xpn <- data$Xpn

##LM 1 of 16
fit <- lm(epn ~ epwn.epns + Jpwnepwn.Jpnsepns + Xpn + 0)
rsq_1 <- summary(fit)$r.squared
rse_1 <- summary(fit)$sigma
a1_1 <- summary(fit)$coefficients[1]
a2_1 <- summary(fit)$coefficients[2]
a3_1 <- summary(fit)$coefficients[3]

##LM 2 of 16
fit <- lm(Jpwnepwn.Jpnsepns ~ epn + epwn.epns + Xpn + 0)
rsq_2 <- summary(fit)$r.squared
rse_2 <- summary(fit)$sigma
a1_2 <- summary(fit)$coefficients[1]
a2_2 <- summary(fit)$coefficients[2]
a3_2 <- summary(fit)$coefficients[3]

##LM 3 of 16
fit <- lm(epn ~ epwn + Jpwnepwn + Xpn + 0)
rsq_3 <- summary(fit)$r.squared
rse_3 <- summary(fit)$sigma
a1_3 <- summary(fit)$coefficients[1]
a2_3 <- summary(fit)$coefficients[2]
a3_3 <- summary(fit)$coefficients[3]

##LM 4 of 16
fit <- lm(Jpwnepwn ~ epn + epwn + Xpn + 0)
rsq_4 <- summary(fit)$r.squared
rse_4 <- summary(fit)$sigma
a1_4 <- summary(fit)$coefficients[1]
a2_4 <- summary(fit)$coefficients[2]
a3_4 <- summary(fit)$coefficients[3]

##LM 5 of 16
fit <- lm(epn ~ epwn.epns + Jpwnepwn.Jpnsepns + 0)
rsq_5 <- summary(fit)$r.squared
rse_5 <- summary(fit)$sigma
a1_5 <- summary(fit)$coefficients[1]
a2_5 <- summary(fit)$coefficients[2]
a3_5 <- 0

##LM 6 of 16
fit <- lm(Jpwnepwn.Jpnsepns ~ epn + epwn.epns + 0)
rsq_6 <- summary(fit)$r.squared
rse_6 <- summary(fit)$sigma
a1_6 <- summary(fit)$coefficients[1]
a2_6 <- summary(fit)$coefficients[2]
a3_6 <- 0

##LM 7 of 16
fit <- lm(epn ~ epwn + Jpwnepwn + 0)
rsq_7 <- summary(fit)$r.squared
rse_7 <- summary(fit)$sigma
a1_7 <- summary(fit)$coefficients[1]
a2_7 <- summary(fit)$coefficients[2]
a3_7 <- 0

##LM 8 of 16
fit <- lm(Jpwnepwn ~ epn + epwn + 0)
rsq_8 <- summary(fit)$r.squared
rse_8 <- summary(fit)$sigma
a1_8 <- summary(fit)$coefficients[1]
a2_8 <- summary(fit)$coefficients[2]
a3_8 <- 0

##LM 9 of 16
fit <- lm(epn ~ Jpwnepwn.Jpnsepns + Xpn + 0)
rsq_9 <- summary(fit)$r.squared
rse_9 <- summary(fit)$sigma
a1_9 <- summary(fit)$coefficients[1]
a2_9 <- summary(fit)$coefficients[2]
a3_9 <- 0

##LM 10 of 16
fit <- lm(Jpwnepwn.Jpnsepns ~ epn + Xpn + 0)
rsq_10 <- summary(fit)$r.squared
rse_10 <- summary(fit)$sigma
a1_10 <- summary(fit)$coefficients[1]
a2_10 <- summary(fit)$coefficients[2]
a3_10 <- 0

##LM 11 of 16
fit <- lm(epn ~ Jpwnepwn + Xpn + 0)
rsq_11 <- summary(fit)$r.squared
rse_11 <- summary(fit)$sigma
a1_11 <- summary(fit)$coefficients[1]
a2_11 <- summary(fit)$coefficients[2]
a3_11 <- 0

##LM 12 of 16
fit <- lm(Jpwnepwn ~ epn + Xpn + 0)
rsq_12 <- summary(fit)$r.squared
rse_12 <- summary(fit)$sigma
a1_12 <- summary(fit)$coefficients[1]
a2_12 <- summary(fit)$coefficients[2]
a3_12 <- 0

##LM 13 of 16
fit <- lm(epn ~ Jpwnepwn.Jpnsepns + 0)
rsq_13 <- summary(fit)$r.squared
rse_13 <- summary(fit)$sigma
a1_13 <- summary(fit)$coefficients[1]
a2_13 <- 0
a3_13 <- 0

##LM 14 of 16
fit <- lm(Jpwnepwn.Jpnsepns ~ epn + 0)
rsq_14 <- summary(fit)$r.squared
rse_14 <- summary(fit)$sigma
a1_14 <- summary(fit)$coefficients[1]
a2_14 <- 0
a3_14 <- 0

##LM 15 of 16
fit <- lm(epn ~ Jpwnepwn + 0)
rsq_15 <- summary(fit)$r.squared
rse_15 <- summary(fit)$sigma
a1_15 <- summary(fit)$coefficients[1]
a2_15 <- 0
a3_15 <- 0

##LM 16 of 16
fit <- lm(Jpwnepwn ~ epn + 0)
rsq_16 <- summary(fit)$r.squared
rse_16 <- summary(fit)$sigma
a1_16 <- summary(fit)$coefficients[1]
a2_16 <- 0
a3_16 <- 0

##LM Extra 1 of 3
fit <- lm(Jpwn ~ sw + 0)
rsq_ex_1 <- summary(fit)$r.squared
rse_ex_1 <- summary(fit)$sigma
a1_ex_1 <- summary(fit)$coefficients[1]
a2_ex_1 <- 0
a3_ex_1 <- 0

##LM Extra 2 of 3
fit <- lm(Jpwn ~ sw + epwn + 0)
rsq_ex_2 <- summary(fit)$r.squared
rse_ex_2 <- summary(fit)$sigma
a1_ex_2 <- summary(fit)$coefficients[1]
a2_ex_2 <- summary(fit)$coefficients[2]
a3_ex_2 <- 0

##LM Extra 3 of 3
fit <- lm(Jpwn ~ sw + epwn + Xpn + 0)
rsq_ex_3 <- summary(fit)$r.squared
rse_ex_3 <- summary(fit)$sigma
a1_ex_3 <- summary(fit)$coefficients[1]
a2_ex_3 <- summary(fit)$coefficients[2]
a3_ex_3 <- summary(fit)$coefficients[3]


rsq <- c(rsq_1, rsq_2, rsq_3, rsq_4, rsq_5, rsq_6, rsq_7, rsq_8, rsq_9, rsq_10, 
         rsq_11, rsq_12, rsq_13, rsq_14,rsq_15, rsq_16, rsq_ex_1, rsq_ex_2, rsq_ex_3)

rse <- c(rse_1, rse_2, rse_3, rse_4, rse_5, rse_6, rse_7, rse_8, rse_9, rse_10,
         rse_11, rse_12, rse_13, rse_14, rse_15, rse_16, rse_ex_1, rse_ex_2, rse_ex_3)

a1 <- c(a1_1, a1_2, a1_3, a1_4, a1_5, a1_6, a1_7, a1_8, a1_9, a1_10, a1_11, a1_12,
        a1_13, a1_14, a1_15, a1_16, a1_ex_1, a1_ex_2, a1_ex_3)

a2 <- c(a2_1, a2_2, a2_3, a2_4, a2_5, a2_6, a2_7, a2_8, a2_9, a2_10, a2_11, a2_12,
        a2_13, a2_14, a2_15, a2_16, a2_ex_1, a2_ex_2, a2_ex_3)

a3 <- c(a3_1, a3_2, a3_3, a3_4, a3_5, a3_6, a3_7, a3_8, a3_9, a3_10, a3_11, a3_12,
        a3_13, a3_14, a3_15, a3_16, a3_ex_1, a3_ex_2, a3_ex_3)

data2 <- data.frame(rsq,rse,a1,a2,a3)

head(data2,n=19)