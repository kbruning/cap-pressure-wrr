require(R.matlab)

data <- readMat("IB_ND.mat")

sw <- data$sw
epn <- data$epn 
Jpwn <- data$Jpwn
Jpns <- data$Jpns
epwn <- data$epwn
epns <- data$epns
epwn.epns <- data$epns+data$epwn   
Jpwnepwn <- data$Jpwn*data$epwn
Jpnsepns <- data$Jpns*data$epns
Jpwnepwn.Jpnsepns <- Jpwnepwn + Jpnsepns

##LM 1 of 16
rsq_1 <- 0
rse_1 <- 0
a1_1 <- 0
a2_1 <- 0
a3_1 <- 0

##LM 2 of 16
rsq_2 <- 0
rse_2 <- 0
a1_2 <- 0
a2_2 <- 0
a3_2 <- 0

##LM 3 of 16
rsq_3 <- 0
rse_3 <- 0
a1_3 <- 0
a2_3 <- 0
a3_3 <- 0

##LM 4 of 16
rsq_4 <- 0
rse_4 <- 0
a1_4 <- 0
a2_4 <- 0
a3_4 <- 0

##LM 5 of 16
v_predictor <- c("epwn.epns","Jpwnepwn.Jpnsepns")
v_response <- "epn"
f <- paste(v_predictor, collapse = "+")
f <- paste(v_response, f, sep = "~")
f <- as.formula(f)
fit <- lm(f)
rsq_5 <- summary(fit)$r.squared
rse_5 <- summary(fit)$sigma
a1_5 <- summary(fit)$coefficients[2]
a2_5 <- summary(fit)$coefficients[3]
a3_5 <- summary(fit)$coefficients[1]

##LM 6 of 16
v_predictor <- c("epn","epwn.epns")
v_response <- "Jpwnepwn.Jpnsepns"
f <- paste(v_predictor, collapse = "+")
f <- paste(v_response, f, sep = "~")
f <- as.formula(f)
fit <- lm(f)
rsq_6 <- summary(fit)$r.squared
rse_6 <- summary(fit)$sigma
a1_6 <- summary(fit)$coefficients[2]
a2_6 <- summary(fit)$coefficients[3]
a3_6 <- summary(fit)$coefficients[1]

##LM 7 of 16
v_predictor <- c("epwn","Jpwnepwn")
v_response <- "epn"
f <- paste(v_predictor, collapse = "+")
f <- paste(v_response, f, sep = "~")
f <- as.formula(f)
fit <- lm(f)
rsq_7 <- summary(fit)$r.squared
rse_7 <- summary(fit)$sigma
a1_7 <- summary(fit)$coefficients[2]
a2_7 <- summary(fit)$coefficients[3]
a3_7 <- summary(fit)$coefficients[1]

##LM 8 of 16
v_predictor <- c("epn","epwn")
v_response <- "Jpwnepwn"
f <- paste(v_predictor, collapse = "+")
f <- paste(v_response, f, sep = "~")
f <- as.formula(f)
fit <- lm(f)
rsq_8 <- summary(fit)$r.squared
rse_8 <- summary(fit)$sigma
a1_8 <- summary(fit)$coefficients[2]
a2_8 <- summary(fit)$coefficients[3]
a3_8 <- summary(fit)$coefficients[1]

##LM 9 of 16
rsq_9 <- 0
rse_9 <- 0
a1_9 <- 0
a2_9 <- 0
a3_9 <- 0

##LM 10 of 16
rsq_10 <- 0
rse_10 <- 0
a1_10 <- 0
a2_10 <- 0
a3_10 <- 0

##LM 11 of 16
rsq_11 <- 0
rse_11 <- 0
a1_11 <- 0
a2_11 <- 0
a3_11 <- 0

##LM 12 of 16
rsq_12 <- 0
rse_12 <- 0
a1_12 <- 0
a2_12 <- 0
a3_12 <- 0

##LM 13 of 16
v_predictor <- c("Jpwnepwn.Jpnsepns")
v_response <- "epn"
f <- paste(v_predictor, collapse = "+")
f <- paste(v_response, f, sep = "~")
f <- as.formula(f)
fit <- lm(f)
rsq_13 <- summary(fit)$r.squared
rse_13 <- summary(fit)$sigma
a1_13 <- summary(fit)$coefficients[2]
a2_13 <- 0
a3_13 <- summary(fit)$coefficients[1]

##LM 14 of 16
v_predictor <- c("epn")
v_response <- "Jpwnepwn.Jpnsepns"
f <- paste(v_predictor, collapse = "+")
f <- paste(v_response, f, sep = "~")
f <- as.formula(f)
fit <- lm(f)
rsq_14 <- summary(fit)$r.squared
rse_14 <- summary(fit)$sigma
a1_14 <- summary(fit)$coefficients[2]
a2_14 <- 0
a3_14 <- summary(fit)$coefficients[1]

##LM 15 of 16
v_predictor <- c("Jpwnepwn")
v_response <- "epn"
f <- paste(v_predictor, collapse = "+")
f <- paste(v_response, f, sep = "~")
f <- as.formula(f)
fit <- lm(f)
rsq_15 <- summary(fit)$r.squared
rse_15 <- summary(fit)$sigma
a1_15 <- summary(fit)$coefficients[2]
a2_15 <- 0
a3_15 <- summary(fit)$coefficients[1]

##LM 16 of 16
v_predictor <- c("epn")
v_response <- "Jpwnepwn"
f <- paste(v_predictor, collapse = "+")
f <- paste(v_response, f, sep = "~")
f <- as.formula(f)
fit <- lm(f)
rsq_16 <- summary(fit)$r.squared
rse_16 <- summary(fit)$sigma
a1_16 <- summary(fit)$coefficients[2]
a2_16 <- 0
a3_16 <- summary(fit)$coefficients[1]

##LM Extra 1 of 3
v_predictor <- c("sw")
v_response <- "Jpwn"
f <- paste(v_predictor, collapse = "+")
f <- paste(v_response, f, sep = "~")
f <- as.formula(f)
fit <- lm(f)
rsq_ex_1 <- summary(fit)$r.squared
rse_ex_1 <- summary(fit)$sigma
a1_ex_1 <- summary(fit)$coefficients[2]
a2_ex_1 <- 0
a3_ex_1 <- summary(fit)$coefficients[1]

##LM Extra 2 of 3
v_predictor <- c("sw","epwn")
v_response <- "Jpwn"
f <- paste(v_predictor, collapse = "+")
f <- paste(v_response, f, sep = "~")
f <- as.formula(f)
fit <- lm(f)
rsq_ex_2 <- summary(fit)$r.squared
rse_ex_2 <- summary(fit)$sigma
a1_ex_2 <- summary(fit)$coefficients[2]
a2_ex_2 <- summary(fit)$coefficients[3]
a3_ex_2 <- summary(fit)$coefficients[1]

##LM Extra 3 of 3
rsq_ex_3 <- 0
rse_ex_3 <- 0
a1_ex_3 <- 0
a2_ex_3 <- 0
a3_ex_3 <- 0


rsq <- c(rsq_1, rsq_2, rsq_3, rsq_4, rsq_5, rsq_6, rsq_7, rsq_8, rsq_9, rsq_10, 
         rsq_11, rsq_12, rsq_13, rsq_14,rsq_15, rsq_16, rsq_ex_1, rsq_ex_2, rsq_ex_3)

rse <- c(rse_1, rse_2, rse_3, rse_4, rse_5, rse_6, rse_7, rse_8, rse_9, rse_10,
         rse_11, rse_12, rse_13, rse_14, rse_15, rse_16, rse_ex_1, rse_ex_2, rse_ex_3)

a1 <- c(a1_1, a1_2, a1_3, a1_4, a1_5, a1_6, a1_7, a1_8, a1_9, a1_10, a1_11, a1_12,
        a1_13, a1_14, a1_15, a1_16, a1_ex_1, a1_ex_2, a1_ex_3)

a2 <- c(a2_1, a2_2, a2_3, a2_4, a2_5, a2_6, a2_7, a2_8, a2_9, a2_10, a2_11, a2_12,
        a2_13, a2_14, a2_15, a2_16, a2_ex_1, a2_ex_2, a2_ex_3)

intercept <- c(a3_1, a3_2, a3_3, a3_4, a3_5, a3_6, a3_7, a3_8, a3_9, a3_10, a3_11, a3_12,
        a3_13, a3_14, a3_15, a3_16, a3_ex_1, a3_ex_2, a3_ex_3)

data2 <- data.frame(rsq,rse,a1,a2,intercept)

head(data2,n=19)