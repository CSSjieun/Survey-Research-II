
## ----include=FALSE--------------------------------------------------------------------------------------------
library(haven)
library(tidyverse)
library(knitr)
library(gmodels)
library(ggplot2)
library(openxlsx)
library(skimr)
library(ggplot2)
library(janitor)
library(readr)
library(dplyr)
library(labelled)
library(essurvey)


## -------------------------------------------------------------------------------------------------------------
data_full <- read_csv("ESS_es.csv")
dim(data_full)



## -------------------------------------------------------------------------------------------------------------
data<- data_full |> select (essround, idno, agea, eduyrs, eisced, gndr, iscoco, isco08, hinctnt, hinctnta)


## -------------------------------------------------------------------------------------------------------------
data <- data |>
  mutate(
    male = car::recode(gndr, "2=0; 9=NA"),
    male = factor(male, levels = c(0, 1), labels = c("female", "male"))
  )

table(data$essround, data$male)


## -------------------------------------------------------------------------------------------------------------
data <- data |>
  mutate(
    age = replace(agea, agea > 103, NA)
  )

summary(data$age) # 999 and other values above 


## -------------------------------------------------------------------------------------------------------------
data <- data |>
  mutate(
    age_rec = cut(agea, breaks = c(0, 19, 29, 44, 64, 103),
                  labels = c("<20", "20-29", "30-44", "45-64", ">=65"))
  )

with(data, table(essround, age_rec))


## -------------------------------------------------------------------------------------------------------------
data <- data |>
  mutate(
    years_schooling = replace(eduyrs, eduyrs > 60, NA)
  )

summary(data$years_schooling)


## -------------------------------------------------------------------------------------------------------------
data <- data |>
  mutate(
    educ_level = case_when(
      eisced %in% c(1, 2) ~ "1",
      eisced %in% c(3, 4) ~ "2",
      eisced %in% c(5, 6, 7) ~ "3",
      eisced %in% c(0, 55, 77, 88, 99) ~ NA_character_,
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    educ_level = fct_recode(educ_level,
                            "Low education" = "1",
                            "Secondary education" = "2",
                            "High education" = "3")
  )

with(data, table(essround, educ_level))


## -------------------------------------------------------------------------------------------------------------
data <- data |>
  mutate(hincome = case_when(
    essround <= 3 ~ hinctnt,
    essround >= 4 ~ hinctnta,
    TRUE ~ NA_real_
  )) 
 # |> select(-hinctnt, -hinctnta) # if you want to remove previous vars


## -------------------------------------------------------------------------------------------------------------
data <- data |>
  mutate(hincome = ifelse(hincome > 10, NA, hincome))

with(data, table(essround, hincome))


## -------------------------------------------------------------------------------------------------------------
data <- data |>
  mutate(occup_cat = case_when(
    essround <= 5 ~ iscoco,
    essround >= 6 ~ isco08,
    TRUE ~ NA_real_
  )) 
 # |> select(-iscoco, -ISCO08) # if you want to remove previous vars


## -------------------------------------------------------------------------------------------------------------
data <- data |> 
  mutate(
    occup_cat = replace(occup_cat, which(occup_cat > 10000), NA),
    occup_cat = cut(
      occup_cat,
      breaks = c(0, 1999, 2999, 3999, 4999, 5999, 6999, 9999),
      labels = c(
        "managers",
        "professionals",
        "technicians",
        "clerical",
        "service",
        "agricultural",
        "blue-collar")))


## -------------------------------------------------------------------------------------------------------------
with(data, table(essround, occup_cat))


## -------------------------------------------------------------------------------------------------------------
data2<-data_full |> select (idno, essround, emplrel, emplno) 
data <-left_join(data, data2, by=c("essround", "idno"))



## -------------------------------------------------------------------------------------------------------------
data$isco_mainjob <- data$iscoco
data$isco_mainjob[is.na(data$isco_mainjob)] <- -9
var_label(data$isco_mainjob) <- "Current occupation of respondent - isco88 4-digit"


## -------------------------------------------------------------------------------------------------------------
data$emplrel_r1 <- data$emplrel
data$emplrel_r1[is.na(data$emplrel_r1)] <- 9
val_label(data$emplrel_r1, 9) <- "Missing"
table(data$emplrel_r1)



## -------------------------------------------------------------------------------------------------------------
data$emplno_r1 <- data$emplno
data$emplno_r1[is.na(data$emplno_r1)] <- 0
data$emplno_r1[data$emplno_r1 >= 1 & data$emplno_r1 <= 9] <- 1
data$emplno_r1[data$emplno_r1 >= 10 & data$emplno_r1 <= 66665] <- 2
val_labels(data$emplno_r1) <- c("0 employees" = 0,
                           "1-9 employees" = 1,
                           "10+ employees" = 2)
table(data$emplno_r1)



## -------------------------------------------------------------------------------------------------------------
data$selfem_mainjob <- NA
data$selfem_mainjob[data$emplrel_r1 == 1 | data$emplrel_r1 == 9] <- 1
data$selfem_mainjob[data$emplrel_r1 == 2 & data$emplno_r1 == 0] <- 2
data$selfem_mainjob[data$emplrel_r1 == 3] <- 2
data$selfem_mainjob[data$emplrel_r1 == 2 & data$emplno_r1 == 1] <- 3
data$selfem_mainjob[data$emplrel_r1 == 2 & data$emplno_r1 == 2] <- 4
val_labels(data$selfem_mainjob) <- c("Not self-employed" = 1,
                                  "Self-empl without employees" = 2,
                                  "Self-empl with 1-9 employees" = 3,
                                  "Self-empl with 10 or more" = 4)
var_label(data$selfem_mainjob) <- "Employment status for respondants"
table(data$selfem_mainjob)


## -------------------------------------------------------------------------------------------------------------
data$class16_r1 <- -9



## -------------------------------------------------------------------------------------------------------------
data$class16_r1[data$selfem_mainjob == 4] <- 1



## -------------------------------------------------------------------------------------------------------------
data$class16_r1[(data$selfem_mainjob == 2 | data$selfem_mainjob == 3) & data$isco_mainjob >= 2000 & data$isco_mainjob <= 2229] <- 2
data$class16_r1[(data$selfem_mainjob == 2 | data$selfem_mainjob == 3) & data$isco_mainjob >= 2300 & data$isco_mainjob <= 2470] <- 2


## -------------------------------------------------------------------------------------------------------------
data$class16_r1[data$selfem_mainjob == 3 & data$isco_mainjob >= 1000 & data$isco_mainjob <= 1999] <- 3
data$class16_r1[data$selfem_mainjob == 3 & data$isco_mainjob >= 3000 & data$isco_mainjob <= 9333] <- 3
data$class16_r1[data$selfem_mainjob == 3 & data$isco_mainjob == 2230] <- 3



## -------------------------------------------------------------------------------------------------------------
data$class16_r1[data$selfem_mainjob == 2 & data$isco_mainjob >= 1000 & data$isco_mainjob <= 1999] <- 4
data$class16_r1[data$selfem_mainjob == 2 & data$isco_mainjob >= 3000 & data$isco_mainjob <= 9333] <- 4
data$class16_r1[data$selfem_mainjob == 2 & data$isco_mainjob == 2230] <- 4


## -------------------------------------------------------------------------------------------------------------
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 2100 &  data$isco_mainjob <= 2213] <- 5


## -------------------------------------------------------------------------------------------------------------
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 3100 &  data$isco_mainjob <= 3152] <- 6
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 3210 &  data$isco_mainjob <= 3213] <- 6
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 3434] <- 6


## -------------------------------------------------------------------------------------------------------------
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 6000 &  data$isco_mainjob <= 7442] <- 7
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 8310 &  data$isco_mainjob <= 8312] <- 7
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 8324 &  data$isco_mainjob <= 8330] <- 7
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 8332 &  data$isco_mainjob <= 8340] <- 7


## -------------------------------------------------------------------------------------------------------------
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 8000 &  data$isco_mainjob <= 8300] <- 8
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 8320 &  data$isco_mainjob <= 8321] <- 8
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 8331] <- 8
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 9153 &  data$isco_mainjob <= 9333] <- 8


## -------------------------------------------------------------------------------------------------------------
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 1000 &  data$isco_mainjob <= 1239] <- 9
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 2400 &  data$isco_mainjob <= 2429] <- 9
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 2441] <- 9
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 2470] <- 9


## -------------------------------------------------------------------------------------------------------------
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 1300 &  data$isco_mainjob <= 1319] <- 10
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 3400 &  data$isco_mainjob <= 3433] <- 10
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 3440 &  data$isco_mainjob <= 3450] <- 10


## -------------------------------------------------------------------------------------------------------------
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 4000 &  data$isco_mainjob <= 4112] <- 11
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 4114 &  data$isco_mainjob <= 4210] <- 11
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 4212 &  data$isco_mainjob <= 4222] <- 11


## -------------------------------------------------------------------------------------------------------------
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 4113] <- 12
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 4211] <- 12
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 4223] <- 12


## -------------------------------------------------------------------------------------------------------------
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 2220 &  data$isco_mainjob <= 2229] <- 13
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 2300 &  data$isco_mainjob <= 2320] <- 13
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 2340 &  data$isco_mainjob <= 2359] <- 13
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 2430 &  data$isco_mainjob <= 2440] <- 13
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 2442 &  data$isco_mainjob <= 2443] <- 13
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 2445] <- 13
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 2451] <- 13
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 2460] <- 13


## -------------------------------------------------------------------------------------------------------------
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 2230] <- 14
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 2330 &  data$isco_mainjob <= 2332] <- 14
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 2444] <- 14
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 2446 &  data$isco_mainjob <= 2450] <- 14
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 2452 &  data$isco_mainjob <= 2455] <- 14
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 3200] <- 14
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 3220 &  data$isco_mainjob <= 3224] <- 14
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 3226] <- 14
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 3229 &  data$isco_mainjob <= 3340] <- 14
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 3460 &  data$isco_mainjob <= 3472] <- 14
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 3480] <- 14



## -------------------------------------------------------------------------------------------------------------
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 3225] <- 15
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 3227 &  data$isco_mainjob <= 3228] <- 15
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 3473 &  data$isco_mainjob <= 3475] <- 15
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 5000 &  data$isco_mainjob <= 5113] <- 15
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 5122] <- 15
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 5131 &  data$isco_mainjob <= 5132] <- 15
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 5140 &  data$isco_mainjob <= 5141] <- 15
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 5143] <- 15
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 5160 &  data$isco_mainjob <= 5220] <- 15
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 8323] <- 15


## -------------------------------------------------------------------------------------------------------------
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 5120 &  data$isco_mainjob <= 5121] <- 16
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 5123 &  data$isco_mainjob <= 5130] <- 16
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 5133 &  data$isco_mainjob <= 5139] <- 16
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 5142] <- 16
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 5149] <- 16
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 5230] <- 16
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob == 8322] <- 16
data$class16_r1[data$selfem_mainjob == 1 & data$isco_mainjob >= 9100 &  data$isco_mainjob <= 9152] <- 16


## -------------------------------------------------------------------------------------------------------------
data$class16_r1[data$class16_r1 == -9] <- NA
val_labels(data$class16_r1) <- c("Large employers" = 1,
                             "Self-employed professionals" = 2,
                             "Small business owners with employees" = 3,
                             "Small business owners without employees" = 4,
                             "Technical experts" = 5,
                             "Technicians" = 6,
                             "Skilled manual" = 7,
                             "Low-skilled manual" = 8,
                             "Higher-grade managers and administrators" = 9,
                             "Lower-grade managers and administrators" = 10,
                             "Skilled clerks" = 11,
                             "Unskilled clerks" = 12,
                             "Socio-cultural professionals" = 13,
                             "Socio-cultural semi-professionals" = 14,
                             "Skilled service" = 15,
                             "Low-skilled service" = 16)
var_label(data$class16_r1) <- "Respondent's Oesch class position - 16 classes"

table(data$class16_r1)


## -------------------------------------------------------------------------------------------------------------
data$class8_r1 <- NA
data$class8_r1[data$class16_r1 <= 2] <- 1
data$class8_r1[data$class16_r1 == 3 | data$class16_r1 == 4] <- 2
data$class8_r1[data$class16_r1 == 5 | data$class16_r1 == 6] <- 3
data$class8_r1[data$class16_r1 == 7 | data$class16_r1 == 8] <- 4
data$class8_r1[data$class16_r1 == 9 | data$class16_r1 == 10] <- 5
data$class8_r1[data$class16_r1 == 11 | data$class16_r1 == 12] <- 6
data$class8_r1[data$class16_r1 == 13 | data$class16_r1 == 14] <- 7
data$class8_r1[data$class16_r1 == 15 | data$class16_r1 == 16] <- 8
val_labels(data$class8_r1) <- c("Self-employed professionals and large employers" = 1,
                            "Small business owners" = 2,
                            "Technical (semi-)professionals" = 3,
                            "Production workers" = 4,
                            "(Associate) managers" = 5,
                            "Clerks" = 6,
                            "Socio-cultural (semi-)professionals" = 7,
                            "Service workers" = 8)
var_label(data$class8_r1) <- "Respondent's Oesch class position - 8 classes"
table(data$class8_r1)


## -------------------------------------------------------------------------------------------------------------
data$class5_r1 <- NA
data$class5_r1[data$class16_r1 <= 2 | data$class16_r1 == 5 | data$class16_r1 == 9 | data$class16_r1 == 13] <- 1
data$class5_r1[data$class16_r1 == 6 | data$class16_r1 == 10 | data$class16_r1 == 14] <- 2
data$class5_r1[data$class16_r1 == 3 | data$class16_r1 == 4] <- 3
data$class5_r1[data$class16_r1 == 7 | data$class16_r1 == 11 | data$class16_r1 == 15] <- 4
data$class5_r1[data$class16_r1 == 8 | data$class16_r1 == 12 | data$class16_r1 == 16] <- 5
val_labels(data$class5_r1) <- c("Higher-grade service class" = 1,
                            "Lower-grade service class" = 2,
                            "Small business owners" = 3,
                            "Skilled workers" = 4,
                            "Unskilled workers" = 5)
var_label(data$class5_r1) <- "Respondent's Oesch class position - 5 classes"



## -------------------------------------------------------------------------------------------------------------
data$isco_mainjob <- data$isco08
data$isco_mainjob[is.na(data$isco_mainjob)] <- -9
var_label(data$isco_mainjob) <- "Current occupation of respondent - isco88 4-digit"


## -------------------------------------------------------------------------------------------------------------
data$emplrel_r2 <- data$emplrel
data$emplrel_r2[is.na(data$emplrel_r2)] <- 9
val_label(data$emplrel_r2, 9) <- "Missing"
table(data$emplrel_r2)



## -------------------------------------------------------------------------------------------------------------
data$emplno_r2 <- data$emplno
data$emplno_r2[is.na(data$emplno_r2)] <- 0
data$emplno_r2[data$emplno_r2 >= 1 & data$emplno_r2 <= 9] <- 1
data$emplno_r2[data$emplno_r2 >= 10 & data$emplno_r2 <= 66665] <- 2
val_labels(data$emplno_r2) <- c("0 employees" = 0,
                           "1-9 employees" = 1,
                           "10+ employees" = 2)
table(data$emplno_r2)



## -------------------------------------------------------------------------------------------------------------
data$selfem_mainjob <- NA
data$selfem_mainjob[data$emplrel_r2 == 1 | data$emplrel_r2 == 9] <- 1
data$selfem_mainjob[data$emplrel_r2 == 2 & data$emplno_r2 == 0] <- 2
data$selfem_mainjob[data$emplrel_r2 == 3] <- 2
data$selfem_mainjob[data$emplrel_r2 == 2 & data$emplno_r2 == 1] <- 3
data$selfem_mainjob[data$emplrel_r2 == 2 & data$emplno_r2 == 2] <- 4
val_labels(data$selfem_mainjob) <- c("Not self-employed" = 1,
                                  "Self-empl without employees" = 2,
                                  "Self-empl with 1-9 employees" = 3,
                                  "Self-empl with 10 or more" = 4)
var_label(data$selfem_mainjob) <- "Employment status for respondants"
table(data$selfem_mainjob)


## -------------------------------------------------------------------------------------------------------------
data$class16_r2 <- -9



## -------------------------------------------------------------------------------------------------------------
data$class16_r2[data$selfem_mainjob == 4] <- 1



## -------------------------------------------------------------------------------------------------------------
data$class16_r2[(data$selfem_mainjob == 2 | data$selfem_mainjob == 3) & data$isco_mainjob >= 2000 & data$isco_mainjob <= 2229] <- 2
data$class16_r2[(data$selfem_mainjob == 2 | data$selfem_mainjob == 3) & data$isco_mainjob >= 2300 & data$isco_mainjob <= 2470] <- 2


## -------------------------------------------------------------------------------------------------------------
data$class16_r2[data$selfem_mainjob == 3 & data$isco_mainjob >= 1000 & data$isco_mainjob <= 1999] <- 3
data$class16_r2[data$selfem_mainjob == 3 & data$isco_mainjob >= 3000 & data$isco_mainjob <= 9333] <- 3
data$class16_r2[data$selfem_mainjob == 3 & data$isco_mainjob == 2230] <- 3



## -------------------------------------------------------------------------------------------------------------
data$class16_r2[data$selfem_mainjob == 2 & data$isco_mainjob >= 1000 & data$isco_mainjob <= 1999] <- 4
data$class16_r2[data$selfem_mainjob == 2 & data$isco_mainjob >= 3000 & data$isco_mainjob <= 9333] <- 4
data$class16_r2[data$selfem_mainjob == 2 & data$isco_mainjob == 2230] <- 4


## -------------------------------------------------------------------------------------------------------------
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 2100 &  data$isco_mainjob <= 2213] <- 5


## -------------------------------------------------------------------------------------------------------------
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 3100 &  data$isco_mainjob <= 3152] <- 6
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 3210 &  data$isco_mainjob <= 3213] <- 6
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 3434] <- 6


## -------------------------------------------------------------------------------------------------------------
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 6000 &  data$isco_mainjob <= 7442] <- 7
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 8310 &  data$isco_mainjob <= 8312] <- 7
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 8324 &  data$isco_mainjob <= 8330] <- 7
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 8332 &  data$isco_mainjob <= 8340] <- 7


## -------------------------------------------------------------------------------------------------------------
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 8000 &  data$isco_mainjob <= 8300] <- 8
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 8320 &  data$isco_mainjob <= 8321] <- 8
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 8331] <- 8
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 9153 &  data$isco_mainjob <= 9333] <- 8


## -------------------------------------------------------------------------------------------------------------
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 1000 &  data$isco_mainjob <= 1239] <- 9
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 2400 &  data$isco_mainjob <= 2429] <- 9
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 2441] <- 9
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 2470] <- 9


## -------------------------------------------------------------------------------------------------------------
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 1300 &  data$isco_mainjob <= 1319] <- 10
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 3400 &  data$isco_mainjob <= 3433] <- 10
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 3440 &  data$isco_mainjob <= 3450] <- 10


## -------------------------------------------------------------------------------------------------------------
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 4000 &  data$isco_mainjob <= 4112] <- 11
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 4114 &  data$isco_mainjob <= 4210] <- 11
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 4212 &  data$isco_mainjob <= 4222] <- 11


## -------------------------------------------------------------------------------------------------------------
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 4113] <- 12
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 4211] <- 12
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 4223] <- 12


## -------------------------------------------------------------------------------------------------------------
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 2220 &  data$isco_mainjob <= 2229] <- 13
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 2300 &  data$isco_mainjob <= 2320] <- 13
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 2340 &  data$isco_mainjob <= 2359] <- 13
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 2430 &  data$isco_mainjob <= 2440] <- 13
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 2442 &  data$isco_mainjob <= 2443] <- 13
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 2445] <- 13
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 2451] <- 13
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 2460] <- 13


## -------------------------------------------------------------------------------------------------------------
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 2230] <- 14
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 2330 &  data$isco_mainjob <= 2332] <- 14
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 2444] <- 14
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 2446 &  data$isco_mainjob <= 2450] <- 14
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 2452 &  data$isco_mainjob <= 2455] <- 14
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 3200] <- 14
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 3220 &  data$isco_mainjob <= 3224] <- 14
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 3226] <- 14
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 3229 &  data$isco_mainjob <= 3340] <- 14
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 3460 &  data$isco_mainjob <= 3472] <- 14
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 3480] <- 14



## -------------------------------------------------------------------------------------------------------------
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 3225] <- 15
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 3227 &  data$isco_mainjob <= 3228] <- 15
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 3473 &  data$isco_mainjob <= 3475] <- 15
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 5000 &  data$isco_mainjob <= 5113] <- 15
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 5122] <- 15
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 5131 &  data$isco_mainjob <= 5132] <- 15
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 5140 &  data$isco_mainjob <= 5141] <- 15
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 5143] <- 15
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 5160 &  data$isco_mainjob <= 5220] <- 15
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 8323] <- 15


## -------------------------------------------------------------------------------------------------------------
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 5120 &  data$isco_mainjob <= 5121] <- 16
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 5123 &  data$isco_mainjob <= 5130] <- 16
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 5133 &  data$isco_mainjob <= 5139] <- 16
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 5142] <- 16
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 5149] <- 16
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 5230] <- 16
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob == 8322] <- 16
data$class16_r2[data$selfem_mainjob == 1 & data$isco_mainjob >= 9100 &  data$isco_mainjob <= 9152] <- 16


## -------------------------------------------------------------------------------------------------------------
data$class16_r2[data$class16_r2 == -9] <- NA
val_labels(data$class16_r2) <- c("Large employers" = 1,
                             "Self-employed professionals" = 2,
                             "Small business owners with employees" = 3,
                             "Small business owners without employees" = 4,
                             "Technical experts" = 5,
                             "Technicians" = 6,
                             "Skilled manual" = 7,
                             "Low-skilled manual" = 8,
                             "Higher-grade managers and administrators" = 9,
                             "Lower-grade managers and administrators" = 10,
                             "Skilled clerks" = 11,
                             "Unskilled clerks" = 12,
                             "Socio-cultural professionals" = 13,
                             "Socio-cultural semi-professionals" = 14,
                             "Skilled service" = 15,
                             "Low-skilled service" = 16)
var_label(data$class16_r2) <- "Respondent's Oesch class position - 16 classes"

table(data$class16_r2)


## -------------------------------------------------------------------------------------------------------------
data$class8_r2 <- NA
data$class8_r2[data$class16_r2 <= 2] <- 1
data$class8_r2[data$class16_r2 == 3 | data$class16_r2 == 4] <- 2
data$class8_r2[data$class16_r2 == 5 | data$class16_r2 == 6] <- 3
data$class8_r2[data$class16_r2 == 7 | data$class16_r2 == 8] <- 4
data$class8_r2[data$class16_r2 == 9 | data$class16_r2 == 10] <- 5
data$class8_r2[data$class16_r2 == 11 | data$class16_r2 == 12] <- 6
data$class8_r2[data$class16_r2 == 13 | data$class16_r2 == 14] <- 7
data$class8_r2[data$class16_r2 == 15 | data$class16_r2 == 16] <- 8
val_labels(data$class8_r2) <- c("Self-employed professionals and large employers" = 1,
                            "Small business owners" = 2,
                            "Technical (semi-)professionals" = 3,
                            "Production workers" = 4,
                            "(Associate) managers" = 5,
                            "Clerks" = 6,
                            "Socio-cultural (semi-)professionals" = 7,
                            "Service workers" = 8)
var_label(data$class8_r2) <- "Respondent's Oesch class position - 8 classes"
table(data$class8_r2)


## -------------------------------------------------------------------------------------------------------------
data$class5_r2 <- NA
data$class5_r2[data$class16_r2 <= 2 | data$class16_r2 == 5 | data$class16_r2 == 9 | data$class16_r2 == 13] <- 1
data$class5_r2[data$class16_r2 == 6 | data$class16_r2 == 10 | data$class16_r2 == 14] <- 2
data$class5_r2[data$class16_r2 == 3 | data$class16_r2 == 4] <- 3
data$class5_r2[data$class16_r2 == 7 | data$class16_r2 == 11 | data$class16_r2 == 15] <- 4
data$class5_r2[data$class16_r2 == 8 | data$class16_r2 == 12 | data$class16_r2 == 16] <- 5
val_labels(data$class5_r2) <- c("Higher-grade service class" = 1,
                            "Lower-grade service class" = 2,
                            "Small business owners" = 3,
                            "Skilled workers" = 4,
                            "Unskilled workers" = 5)
var_label(data$class5_r2) <- "Respondent's Oesch class position - 5 classes"



## -------------------------------------------------------------------------------------------------------------
data$class5_r[data$essround<=10]<-data$class5_r2
data$class5_r[data$essround<6]<-data$class5_r1

# As factor 
data$class5_r <- factor(data$class5_r, 
                        levels=c(1,2,3,4,5), 
                        labels=c("Higher-grade service class",
                            "Lower-grade service class",
                            "Small business owners",
                            "Skilled workers",
                            "Unskilled workers"))
with(data, table(essround, class5_r))




