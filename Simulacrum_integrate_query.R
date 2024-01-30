# beskrivelse
#forfattere, dato, output, filnavne


# Libraries and functions -------------------------------------------------


library(tidyverse)
library(lubridate)
library(broom)
library(knitr)
library(DBI)
library(odbc)
library(dbarts)
library(tmle)

rf <- function(data, dig = 2) {
  format(round(data, digits = dig), nsmall = dig)
}

# ODBC --------------------------------------------------------------------

my_oracle <- dbConnect(odbc::odbc(),
                       Driver = "Oracle in instantclient_21_12",
                       DBQ = "localhost:1521/XEPDB1", 
                       UID = "system",
                       PWD = "test1234",
                       trusted_connection = TRUE)

# In the following "ANONYMOUS.SIM_*" should be replaced with the schema name
# and correct table name

query <- "SELECT
  sp.MERGED_PATIENT_ID,
  sp.LINK_NUMBER,
  at.PATIENTID,
  at.DIAGNOSISDATEBEST,
  at.SITE_ICD10_O2_3CHAR,
  at.AGE,
  at.SEX,
  at.QUINTILE_2015,
  at.STAGE_BEST,
  at.ACE27,
  st.MERGED_TUMOUR_ID,
  st.PRIMARY_DIAGNOSIS,
  sr.CLINICAL_TRIAL,
  sr.HEIGHT_AT_START_OF_REGIMEN,
  sr.WEIGHT_AT_START_OF_REGIMEN
FROM
  ANONYMOUS.SIM_SACT_PATIENT sp
LEFT JOIN
  ANONYMOUS.SIM_AV_TUMOUR at
ON
  sp.LINK_NUMBER = at.LINKNUMBER
LEFT JOIN
  ANONYMOUS.SIM_SACT_TUMOUR st
ON
  sp.MERGED_PATIENT_ID = st.MERGED_PATIENT_ID
LEFT JOIN
  ANONYMOUS.SIM_SACT_REGIMEN sr
ON
  sp.MERGED_PATIENT_ID = sr.MERGED_PATIENT_ID AND st.MERGED_TUMOUR_ID = sr.MERGED_TUMOUR_ID"


sact_1 <- dbGetQuery(my_oracle, query)


# Datamanagement ----------------------------------------------------------

sact_2 <- sact_1 |> 
  mutate(ip = as.numeric(substr(PRIMARY_DIAGNOSIS, 2, 3)),
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
         !is.na(MERGED_TUMOUR_ID), 
         substr(PRIMARY_DIAGNOSIS, 1, 1) == "C", 
         AGE >= 18)

sact_3 <- sact_2 |> 
  group_by(PATIENTID) |> 
  mutate(trial_all = any(CLINICAL_TRIAL %in% c("y", "Y","01","1"))) |> #We assume the following, y,Y,01,1 are indicators for clinical trial
  summarize(across(everything(), first))

sact_4 <- sact_3 |> 
  filter(SEX %in% c(1,2)) %>% 
  mutate(sex_group = if_else(SEX == 2, "Female", "Male"), 
         age_group = case_when(between(AGE, 18, 44)  ~ "18-44",
                               between(AGE, 45, 64)  ~ "45-64",
                               between(AGE, 65, 74)  ~ "65-74",
                               between(AGE, 75, 130) ~ "> 75"),
         seps = if_else(QUINTILE_2015 == "5 - most deprived", "vulnerable", "non-vulnerable"),
         year = year(DIAGNOSISDATEBEST),
         tnm = substr(STAGE_BEST, 0, 1),
         tnm = case_when(tnm == "1" ~ "I",
                         tnm == "2" ~ "II",
                         tnm == "3" ~ "III",
                         tnm == "4" ~ "IV",
                         TRUE ~ "Not recorded"),
         ACE27 = if_else(ACE27 == 9 | is.na(ACE27), "Not recorded", as.character(substr(ACE27,0,1))),
         com = as.factor(ACE27),
         Total = "total")



# Table 1 -----------------------------------------------------------------

facts <- c("Total", "trial_all", "sex_group", "age_group", "seps", "diag", "com","tnm")

tmp <- bind_rows(sact_4 |> mutate(seps = "total"), sact_4) |> 
  mutate(Total = "tots",
         trial_all = as.character(trial_all),
         sep = fct_relevel(seps, c("total", "non-vulnerable", "vulnerable")))

A <- tmp |> 
  dplyr::select(all_of(facts)) |> 
  pivot_longer(names_to = "group", values_to ="stratification",-seps) |> 
  mutate(group = fct_relevel(group, facts)) |> 
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
  mutate(seps = fct_relevel(seps, c("total", "non-vulnerable", "vulnerable")))

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

