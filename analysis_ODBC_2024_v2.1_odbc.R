


# Datamanagement ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(forestmodel)
library(broom)
library(knitr)
library(DBI)
library(odbc)

rf <- function(data, dig = 2) {
  format(round(data, digits = dig), nsmall = dig)
}

CC_calc <- function(df, columnName) {
  code_values <- c('01' = 1, '02' = 1, '03' = 1, '04' = 1, '05' = 1, '06' = 1,
                   '07' = 1, '08' = 1, '09' = 1, '10' = 2, '11' = 2, '12' = 2,
                   '13' = 2, '14' = 0, '15' = 1, '16' = 0, '17' = 3)
  df$com <- sapply(df[[columnName]], function(row) {
    if (is.null(row) || is.na(row)) {
      return(0)
    } else {
      codes <- unlist(strsplit(as.character(row), ","))
      
      if ("09" %in% codes && "10" %in% codes) {
        codes <- codes[codes != "09"]
      }
      if ("15" %in% codes && "17" %in% codes) {
        codes <- codes[codes != "15"]
      }
      
      sum(sapply(codes, function(code) {
        code_trimmed <- trimws(code)
        
        if (code_trimmed %in% names(code_values)) {
          return(code_values[[code_trimmed]])
        } else {
          return(0)
        }
      }))
    }
  })
  return(df)
}

# ODBC --------------------------------------------------------------------

my_oracle <- dbConnect(odbc::odbc(),
                       Driver = "Oracle in instantclient_21_12",
                       DBQ = "localhost:1521/XEPDB1", 
                       UID = "system",
                       PWD = "test1234",
                       trusted_connection = TRUE)

# In the following "DGPDB_INT.SIM_*" should be replaced with the schema name
# and correct table name

query1 <- "SELECT
  st.CLINICAL_TRIAL,
  st.ENCORE_PATIENT_ID,
  st.START_DATE_OF_REGIMEN
FROM
  DGPDB_INT.SIM_SACT_REGIMEN st"

query2 <- "SELECT
  at.PATIENTID,
  at.TUMOURID,
  at.DIAGNOSISDATEBEST,
  at.AGE, 
  at.GENDER,
  at.QUINTILE_2019,
  at.STAGE_BEST,
  at.COMORBIDITIES_27_03,
  at.SITE_ICD10_O2_3CHAR
FROM
  DGPDB_INT.SIM_AV_TUMOUR at"


sact_regimen <- dbGetQuery(my_oracle, query1)
av_tumour <- dbGetQuery(my_oracle, query2)

# Datamanagement ----------------------------------------------------------

sact_1 <- sact_regimen  |> 
  mutate(trial_all = CLINICAL_TRIAL %in% c("y", "Y","01","1")) |> 
  group_by(ENCORE_PATIENT_ID) |> 
  summarize(trial_all = any(trial_all), 
            date = min(START_DATE_OF_REGIMEN)) |> 
  left_join(av_tumour, by = c("ENCORE_PATIENT_ID"="PATIENTID")) |> 
  filter(date > DIAGNOSISDATEBEST) |> # only include diagnoses before first treatment
  mutate(dif = difftime(date, DIAGNOSISDATEBEST)) |> 
  group_by(ENCORE_PATIENT_ID) |> 
  filter(dif == min(dif)) |> # diagnosis closest to first registered treatment 
  filter(row_number() == 1) |>  # remove diagnosis with duplicate diagnosis dates
  ungroup()

sact_2 <- sact_1 |> 
  mutate(PD = SITE_ICD10_O2_3CHAR,
         ip = as.numeric(substr(PD, 2, 3)),
         diag = case_when(ip %in% c(47, 69:72)       ~ "Eye, brain and CNS",
                          ip %in% c(50)              ~ "Breast",
                          ip %in% c(51:58)           ~ "Gynaecological",
                          ip %in% c(0:14, 30:32)     ~ "Head and Neck",
                          ip %in% c(18:21)           ~ "Lower gastrointestinal",
                          ip %in% c(33:34, 37:39,45) ~ "Lung and bronchus",
                          ip %in% c(15:17, 22:25)    ~ "Upper gastrointestinal",
                          ip %in% c(60:68)           ~ "Urology",
                          ip %in% c(43:44)           ~ "Skin",
                          ip %in% c(81:86, 90:96)    ~ "Haematologic",
                          TRUE                       ~ "Ill-defined and unspecified")) |> 
  filter(diag != "Haematologic", 
         substr(PD, 1, 1) == "C", 
         between(AGE, 18, 130),
         GENDER %in% c(1,2))

sact_3 <- CC_calc(sact_2, "COMORBIDITIES_27_03")

sact_4 <- sact_3 |> 
  mutate(sex_group = if_else(GENDER == 2, "Female", "Male"), 
         age_group = case_when(between(AGE, 18, 44)  ~ "18-44",
                               between(AGE, 45, 64)  ~ "45-64",
                               between(AGE, 65, 74)  ~ "65-74",
                               between(AGE, 75, 130) ~ "> 75"),
         seps = if_else(QUINTILE_2019 == "1 - most deprived", "vulnerable", "non-vulnerable"),
         year = year(DIAGNOSISDATEBEST),
         tnm = substr(STAGE_BEST, 0, 1),
         tnm = case_when(tnm == "1" ~ "I",
                         tnm == "2" ~ "II",
                         tnm == "3" ~ "III",
                         tnm == "4" ~ "IV",
                         TRUE ~ "Not recorded"),
         com = if_else(com>3,"4+", as.factor(as.character(com))), 
         Total = "total")

# Table 1 -----------------------------------------------------------------


facts <- c("Total", "trial_all", "sex_group", "age_group", "seps", "diag", "com","tnm")
facts2 <- c("Total", "trial_all", "sex_group", "age_group", "diag", "com","tnm")

tmp <- bind_rows(sact_4 |> mutate(seps = "total"), sact_4) |> 
  mutate(Total = "tots",
         trial_all = as.character(trial_all),
         seps = as.factor(seps),
         seps = fct_relevel(seps, c("total", "non-vulnerable", "vulnerable")))

A <- tmp |> 
  dplyr::select(all_of(facts)) |> 
  pivot_longer(names_to = "group", values_to ="stratification",-seps) |> 
  mutate(group = fct_relevel(group, facts2)) |> 
  group_by(group, seps) |> 
  mutate(nn=n()) |> 
  group_by(group, stratification, seps) |> 
  summarize(N=paste0(n(), " (",format(round(n()/mean(nn)*100, digits = 1),nsmall = 1),"%)")) |> 
  pivot_wider(names_from = seps, values_from = N)

B <- tmp |> 
  dplyr::select(AGE,seps) |> 
  pivot_longer(names_to = "group", values_to ="stratification",-seps) |> 
  group_by(group, seps) |> 
  summarize(aa = paste0(round(median(stratification, na.rm=T),1)," (",
                        round(quantile(stratification, probs=0.25,na.rm=T)[[1]]),":",
                        round(quantile(stratification, probs=0.75,na.rm=T)[[1]]),")")) |> 
  pivot_wider(names_from = seps, values_from = aa)

C <- tmp |> 
  dplyr::select(all_of(facts)) |> 
  pivot_longer(names_to = "group", values_to ="stratification",-seps) |> 
  filter(seps!="total",stratification!="tots") |> 
  group_by(group, seps,stratification) |> 
  count() |> 
  pivot_wider(names_from = seps, values_from = n) |> 
  group_by(group) |> 
  group_modify(~tibble(p_value=format.pval(round(chisq.test(rbind(.x$"non-vulnerable",.x$"vulnerable"))$"p.value",3),
                                           eps=0.001,digits=3,nsmall=3,scientific=FALSE)))

t1 <- A |> ungroup() |> 
  filter(!is.na(stratification)) |> 
  add_row(.before = c(6),B[1,]) |> 
  left_join(C, by="group") |> 
  kable(format = "html")

t1 |> cat(file="t1.html")


# Table 2 -----------------------------------------------------------------

tmp <- bind_rows(sact_4 |> mutate(diag = "Total"), sact_4) |> 
  mutate(seps = as.factor(seps),
         seps = fct_relevel(seps, c("non-vulnerable", "vulnerable")))

t2 <- tmp |> 
  mutate(ttb = trial_all*1) |> 
  group_by(diag) |> 
  group_modify(~ rbind(
    glm(ttb ~ seps + age_group + sex_group + year + com + tnm, data=.x, family="binomial") |> 
      tidy(exponentiate=T, conf.int=T) |> 
      mutate(n=nrow(.x),ntrial=sum(.x$trial_all), type="adj"),
    glm(ttb ~ seps + age_group + sex_group + year, data=.x, family="binomial") |> 
      tidy(exponentiate=T, conf.int=T) |> 
      mutate(n=nrow(.x),ntrial=sum(.x$trial_all), type="crude"))) |> 
  filter(term=="sepsvulnerable") |> 
  arrange(desc(n)) |> 
  mutate(est=paste0(rf(estimate,2)," (",rf(conf.low,2),"-",rf(conf.high,2),")"),
         p.value = format.pval(p.value, nsmall=3, eps = 0.001, digits=3),
         par=round(ntrial/n*100,1)) |> 
  select(diag, est, p.value, n,ntrial,par,type) |> 
  pivot_wider(names_from = type, values_from = c(est, p.value))

t2 |> 
  kable(format = "html") |> 
  cat(file="t2.html")



# Table 3 (not required) --------------------------------------------------

# Estimated to take 5-6 hours on a average laptop

df <- bind_rows(sact_4, sact_4 |> mutate(diag = "Total")) |> 
  mutate(ttb = trial_all*1,
         rs = if_else(seps == "vulnerable",1,0)) |> 
  group_by(diag) |> 
  group_modify(~ {
    tmle_fit <- tmle::tmle(Y = .x$ttb,
                           A = .x$rs,
                           W = .x[,c("age_group","sex_group","year","com","tnm")],
                           Q.SL.library = "SL.randomForest",
                           family="binomial",
                           V.Q = 5)
    tibble(estimate = tmle_fit$estimates$OR$psi,
           lower = tmle_fit$estimates$OR$CI[1],
           upper = tmle_fit$estimates$OR$CI[2],
           pp = tmle_fit$estimates$OR$pvalue,
           n = nrow(.x),
           ntrial = sum(.x$trial_all),
           par = round(ntrial/n*100,1)) |>
      arrange(desc(n)) |> 
      mutate(est = paste0(round(estimate,2)," (",round(lower,2),"-",round(upper,2),")"),
             p.value = format.pval(pp, nsmall=4)) |> 
      select(-upper,-lower,-estimate,-pp)
  })

df |> arrange(desc(n)) |> 
  kable(format = "html") |> 
  cat(file="t3.html")

