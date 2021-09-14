# A wide-formatted version on the code is necessary for the PROC TRAJ procedure in SAS
library(dplyr)
library(haven)


CrossWalk <- read_sas("~/sdrive/sasusershare/USER/MMASTERS/Medicare/Corinne-Frailty/Data/cps2_beneid.sas7bdat")

MortInfo <- read_sas("~/sdrive/sasusershare/CPS/MORTALITY/smgmrt18d210524.sas7bdat") %>%
  select(ID, DEATH_YR)

VerifiedInfo <- read_sas("~/sdrive/sasusershare/CPS/NUTRITION/VERIFIED/may2021verified.sas7bdat") %>% select(ID, INCCANV, SITEC, HISTOGY, DODYR, DODMO, DODDY) %>%
  mutate(DODYR = as.numeric(DODYR)) %>%
  group_by(ID) %>% slice_min(order_by=DODYR, n=1) %>%
  group_by(ID) %>% mutate(multDiagExclude = ifelse(n()>1,1,0)) %>%
  mutate(CA92_99 = ifelse(DODYR<1999,1,0))
# If there are multiple cancer diagnoses in the first year we have a diagnosis in, exclude them for now
# Also if they are diagnosed before our Medicare data starts in 1999 exclude them

Derived82Men <- read_sas("~/sdrive/sasusershare/CPS/CPS2/DERIVED/cps2menkeyvar3.sas7bdat") %>% select(ID, CA82)
Derived82Women <- read_sas("~/sdrive/sasusershare/CPS/CPS2/DERIVED/cps2femkeyvar3.sas7bdat") %>% select(ID, CA82)
DER82 <- rbind(Derived82Men, Derived82Women) %>% mutate(CA82Exclude = ifelse(CA82 %in% c('N','73'),0,1))

Derived92Men <- read_sas("~/sdrive/sasusershare/CPS/NUTRITION/DERIVED/survey92menkeyvar2.sas7bdat") %>% select(ID, CA92) 
Derived92Women <- read_sas("~/sdrive/sasusershare/CPS/NUTRITION/DERIVED/survey92femkeyvar2.sas7bdat") %>% select(ID, CA92)
DER92 <- rbind(Derived92Men,Derived92Women) %>% mutate(CA92Exclude = ifelse(CA92 > 0,1,0))
# Exclude them if they reported cancer in 82 or 92

rm(Derived82Men, Derived82Women, Derived92Men, Derived92Women)

MasterWomen <- read_sas("~/sdrive/sasusershare/CPS/MASTER/cps2smgmst18f.sas7bdat") %>% 
  select(ID, RACE, NUT_92, NUT_97, NUT_99, NUT_01, NUT_03, NUT_05, NUT_07, NUT_09, NUT_11, NUT_13, NUT_15)
MasterMen <- read_sas("~/sdrive/sasusershare/CPS/MASTER/cps2smgmst18m.sas7bdat")%>% 
  select(ID, RACE, NUT_92, NUT_97, NUT_99, NUT_01, NUT_03, NUT_05, NUT_07, NUT_09, NUT_11, NUT_13, NUT_15)

Master <- rbind(MasterWomen, MasterMen) %>% filter(NUT_92==1) %>%
  mutate(lastSurv = case_when(
    NUT_15==1 ~2015,
    NUT_13==1 ~2013,
    NUT_11==1 ~2011,
    NUT_09==1 ~2009,
    NUT_07==1 ~2007,
    NUT_05==1 ~2005,
    NUT_03==1 ~2003,
    NUT_01==1 ~2001,
    NUT_99==1 ~1999,
    NUT_97==1 ~1997,
    NUT_92==1 ~1992
  )) %>%
  mutate(LostFU = ifelse(lastSurv<1999,1,0))
# Remove anyone that hasn't responded since 1999, and create a last survey variable

rm(MasterWomen, MasterMen)

# Original 82 Data for Education
OrigWomen <- read_sas("~/sdrive/sasusershare/CPS/CPS2/ORIGINAL/cps2femoriginal.sas7bdat") %>% 
  select(ID, EDUCATE)
OrigMen <- read_sas("~/sdrive/sasusershare/CPS/CPS2/ORIGINAL/cps2menoriginal.sas7bdat") %>% 
  select(ID, EDUCATE)
OrigBoth<- rbind(OrigWomen, OrigMen)
rm(OrigMen, OrigWomen)


CPSData <- plyr::join_all(list(CrossWalk, MortInfo, VerifiedInfo, DER82, DER92, Master, OrigBoth), by="ID", type="left") %>%
  filter(CA82Exclude==0, CA92Exclude==0, multDiagExclude == 0 | is.na(multDiagExclude), CA92_99==0 | is.na(CA92_99), LostFU==0) %>%
  mutate(CASurvivor = ifelse(is.na(DODYR) == FALSE, 1, 0))

rm(MortInfo, CrossWalk, VerifiedInfo, DER82, DER92, Master, OrigBoth)

# 119,122 people left after CPS-based exclusions

# The comorbidity files we created using the SAS macro for NCI Comorbidity Index

comorb99 <- read_sas("~/sdrive/sasusershare/USER/MMASTERS/Medicare/Corinne-Frailty/Data/com99.sas7bdat") %>%
  rename_with(~paste0(.,"_99"), 2:37) %>% mutate(has99c=1)
comorb00 <- read_sas("~/sdrive/sasusershare/USER/MMASTERS/Medicare/Corinne-Frailty/Data/com00.sas7bdat") %>%
  rename_with(~paste0(.,"_00"), 2:37) %>% mutate(has00c=1)
comorb01 <- read_sas("~/sdrive/sasusershare/USER/MMASTERS/Medicare/Corinne-Frailty/Data/com01.sas7bdat") %>%
  rename_with(~paste0(.,"_01"), 2:37) %>% mutate(has01c=1)
comorb02 <- read_sas("~/sdrive/sasusershare/USER/MMASTERS/Medicare/Corinne-Frailty/Data/com02.sas7bdat") %>%
  rename_with(~paste0(.,"_02"), 2:37) %>% mutate(has02c=1)
comorb03 <- read_sas("~/sdrive/sasusershare/USER/MMASTERS/Medicare/Corinne-Frailty/Data/com03.sas7bdat") %>%
  rename_with(~paste0(.,"_03"), 2:37) %>% mutate(has03c=1)
comorb04 <- read_sas("~/sdrive/sasusershare/USER/MMASTERS/Medicare/Corinne-Frailty/Data/com04.sas7bdat") %>%
  rename_with(~paste0(.,"_04"), 2:37) %>% mutate(has04c=1)
comorb05 <- read_sas("~/sdrive/sasusershare/USER/MMASTERS/Medicare/Corinne-Frailty/Data/com05.sas7bdat") %>%
  rename_with(~paste0(.,"_05"), 2:37) %>% mutate(has05c=1)
comorb06 <- read_sas("~/sdrive/sasusershare/USER/MMASTERS/Medicare/Corinne-Frailty/Data/com06.sas7bdat") %>%
  rename_with(~paste0(.,"_06"), 2:37) %>% mutate(has06c=1)
comorb07 <- read_sas("~/sdrive/sasusershare/USER/MMASTERS/Medicare/Corinne-Frailty/Data/com07.sas7bdat") %>%
  rename_with(~paste0(.,"_07"), 2:37) %>% mutate(has07c=1)
comorb08 <- read_sas("~/sdrive/sasusershare/USER/MMASTERS/Medicare/Corinne-Frailty/Data/com08.sas7bdat") %>%
  rename_with(~paste0(.,"_08"), 2:37) %>% mutate(has08c=1)
comorb09 <- read_sas("~/sdrive/sasusershare/USER/MMASTERS/Medicare/Corinne-Frailty/Data/com09.sas7bdat") %>%
  rename_with(~paste0(.,"_09"), 2:37) %>% mutate(has09c=1)
comorb10 <- read_sas("~/sdrive/sasusershare/USER/MMASTERS/Medicare/Corinne-Frailty/Data/com10.sas7bdat") %>%
  rename_with(~paste0(.,"_10"), 2:37) %>% mutate(has10c=1)
comorb11 <- read_sas("~/sdrive/sasusershare/USER/MMASTERS/Medicare/Corinne-Frailty/Data/com11.sas7bdat") %>%
  rename_with(~paste0(.,"_11"), 2:37) %>% mutate(has11c=1)
comorb12 <- read_sas("~/sdrive/sasusershare/USER/MMASTERS/Medicare/Corinne-Frailty/Data/com12.sas7bdat") %>%
  rename_with(~paste0(.,"_12"), 2:37) %>% mutate(has12c=1)
comorb13 <- read_sas("~/sdrive/sasusershare/USER/MMASTERS/Medicare/Corinne-Frailty/Data/com13.sas7bdat") %>%
  rename_with(~paste0(.,"_13"), 2:37) %>% mutate(has13c=1)
comorb14 <- read_sas("~/sdrive/sasusershare/USER/MMASTERS/Medicare/Corinne-Frailty/Data/com14.sas7bdat") %>%
  rename_with(~paste0(.,"_14"), 2:37) %>% mutate(has14c=1)


CPS_Comorb <- plyr::join_all(list(CPSData, comorb99, comorb00, comorb01, comorb02, comorb03, comorb04, comorb05, comorb06, comorb07, comorb08, comorb09, comorb10, comorb11, comorb12, comorb13, comorb14), by="BENE_ID", type = "left") %>%
  mutate(has99c=ifelse(is.na(has99c),0,1),
         has00c=ifelse(is.na(has00c),0,1),
         has01c=ifelse(is.na(has01c),0,1),
         has02c=ifelse(is.na(has02c),0,1),
         has03c=ifelse(is.na(has03c),0,1),
         has04c=ifelse(is.na(has04c),0,1),
         has05c=ifelse(is.na(has05c),0,1),
         has06c=ifelse(is.na(has06c),0,1),
         has07c=ifelse(is.na(has07c),0,1),
         has08c=ifelse(is.na(has08c),0,1),
         has09c=ifelse(is.na(has09c),0,1),
         has10c=ifelse(is.na(has10c),0,1),
         has11c=ifelse(is.na(has11c),0,1),
         has12c=ifelse(is.na(has12c),0,1),
         has13c=ifelse(is.na(has13c),0,1),
         has14c=ifelse(is.na(has14c),0,1))

rm(CPSData, comorb99, comorb00, comorb01, comorb02, comorb03, comorb04, comorb05, comorb06, comorb07, comorb08, comorb09, comorb10, comorb11, comorb12, comorb13, comorb14)

# Add a rolling NCI Index score. It may need to be lagged in the future
CPS_Comorb <- CPS_Comorb %>% 
  mutate(
    acute_mi_99N = case_when(acute_mi_99==1 ~1, TRUE~0),
    acute_mi_00N = case_when(acute_mi_00==1 | acute_mi_99N==1 ~1, TRUE~0),
    acute_mi_01N = case_when(acute_mi_01==1 | acute_mi_00N==1 ~1, TRUE~0),
    acute_mi_02N = case_when(acute_mi_02==1 | acute_mi_01N==1 ~1, TRUE~0),
    acute_mi_03N = case_when(acute_mi_03==1 | acute_mi_02N==1 ~1, TRUE~0),
    acute_mi_04N = case_when(acute_mi_04==1 | acute_mi_03N==1 ~1, TRUE~0),
    acute_mi_05N = case_when(acute_mi_05==1 | acute_mi_04N==1 ~1, TRUE~0),
    acute_mi_06N = case_when(acute_mi_06==1 | acute_mi_05N==1 ~1, TRUE~0),
    acute_mi_07N = case_when(acute_mi_07==1 | acute_mi_06N==1 ~1, TRUE~0),
    acute_mi_08N = case_when(acute_mi_08==1 | acute_mi_07N==1 ~1, TRUE~0),
    acute_mi_09N = case_when(acute_mi_09==1 | acute_mi_08N==1 ~1, TRUE~0),
    acute_mi_10N = case_when(acute_mi_10==1 | acute_mi_09N==1 ~1, TRUE~0),
    acute_mi_11N = case_when(acute_mi_11==1 | acute_mi_10N==1 ~1, TRUE~0),
    acute_mi_12N = case_when(acute_mi_12==1 | acute_mi_11N==1 ~1, TRUE~0),
    acute_mi_13N = case_when(acute_mi_13==1 | acute_mi_12N==1 ~1, TRUE~0),
    acute_mi_14N = case_when(acute_mi_14==1 | acute_mi_13N==1 ~1, TRUE~0),
    history_mi_99N = case_when(history_mi_99==1 ~1, TRUE~0),
    history_mi_00N = case_when(history_mi_00==1 | history_mi_99N==1 ~1, TRUE~0),
    history_mi_01N = case_when(history_mi_01==1 | history_mi_00N==1 ~1, TRUE~0),
    history_mi_02N = case_when(history_mi_02==1 | history_mi_01N==1 ~1, TRUE~0),
    history_mi_03N = case_when(history_mi_03==1 | history_mi_02N==1 ~1, TRUE~0),
    history_mi_04N = case_when(history_mi_04==1 | history_mi_03N==1 ~1, TRUE~0),
    history_mi_05N = case_when(history_mi_05==1 | history_mi_04N==1 ~1, TRUE~0),
    history_mi_06N = case_when(history_mi_06==1 | history_mi_05N==1 ~1, TRUE~0),
    history_mi_07N = case_when(history_mi_07==1 | history_mi_06N==1 ~1, TRUE~0),
    history_mi_08N = case_when(history_mi_08==1 | history_mi_07N==1 ~1, TRUE~0),
    history_mi_09N = case_when(history_mi_09==1 | history_mi_08N==1 ~1, TRUE~0),
    history_mi_10N = case_when(history_mi_10==1 | history_mi_09N==1 ~1, TRUE~0),
    history_mi_11N = case_when(history_mi_11==1 | history_mi_10N==1 ~1, TRUE~0),
    history_mi_12N = case_when(history_mi_12==1 | history_mi_11N==1 ~1, TRUE~0),
    history_mi_13N = case_when(history_mi_13==1 | history_mi_12N==1 ~1, TRUE~0),
    history_mi_14N = case_when(history_mi_14==1 | history_mi_13N==1 ~1, TRUE~0),
    chf_99N = case_when(chf_99==1 ~1, TRUE~0),
    chf_00N = case_when(chf_00==1 | chf_99N==1 ~1, TRUE~0),
    chf_01N = case_when(chf_01==1 | chf_00N==1 ~1, TRUE~0),
    chf_02N = case_when(chf_02==1 | chf_01N==1 ~1, TRUE~0),
    chf_03N = case_when(chf_03==1 | chf_02N==1 ~1, TRUE~0),
    chf_04N = case_when(chf_04==1 | chf_03N==1 ~1, TRUE~0),
    chf_05N = case_when(chf_05==1 | chf_04N==1 ~1, TRUE~0),
    chf_06N = case_when(chf_06==1 | chf_05N==1 ~1, TRUE~0),
    chf_07N = case_when(chf_07==1 | chf_06N==1 ~1, TRUE~0),
    chf_08N = case_when(chf_08==1 | chf_07N==1 ~1, TRUE~0),
    chf_09N = case_when(chf_09==1 | chf_08N==1 ~1, TRUE~0),
    chf_10N = case_when(chf_10==1 | chf_09N==1 ~1, TRUE~0),
    chf_11N = case_when(chf_11==1 | chf_10N==1 ~1, TRUE~0),
    chf_12N = case_when(chf_12==1 | chf_11N==1 ~1, TRUE~0),
    chf_13N = case_when(chf_13==1 | chf_12N==1 ~1, TRUE~0),
    chf_14N = case_when(chf_14==1 | chf_13N==1 ~1, TRUE~0),
    pvd_99N = case_when(pvd_99==1 ~1, TRUE~0),
    pvd_00N = case_when(pvd_00==1 | pvd_99N==1 ~1, TRUE~0),
    pvd_01N = case_when(pvd_01==1 | pvd_00N==1 ~1, TRUE~0),
    pvd_02N = case_when(pvd_02==1 | pvd_01N==1 ~1, TRUE~0),
    pvd_03N = case_when(pvd_03==1 | pvd_02N==1 ~1, TRUE~0),
    pvd_04N = case_when(pvd_04==1 | pvd_03N==1 ~1, TRUE~0),
    pvd_05N = case_when(pvd_05==1 | pvd_04N==1 ~1, TRUE~0),
    pvd_06N = case_when(pvd_06==1 | pvd_05N==1 ~1, TRUE~0),
    pvd_07N = case_when(pvd_07==1 | pvd_06N==1 ~1, TRUE~0),
    pvd_08N = case_when(pvd_08==1 | pvd_07N==1 ~1, TRUE~0),
    pvd_09N = case_when(pvd_09==1 | pvd_08N==1 ~1, TRUE~0),
    pvd_10N = case_when(pvd_10==1 | pvd_09N==1 ~1, TRUE~0),
    pvd_11N = case_when(pvd_11==1 | pvd_10N==1 ~1, TRUE~0),
    pvd_12N = case_when(pvd_12==1 | pvd_11N==1 ~1, TRUE~0),
    pvd_13N = case_when(pvd_13==1 | pvd_12N==1 ~1, TRUE~0),
    pvd_14N = case_when(pvd_14==1 | pvd_13N==1 ~1, TRUE~0),
    cvd_99N = case_when(cvd_99==1 ~1, TRUE~0),
    cvd_00N = case_when(cvd_00==1 | cvd_99N==1 ~1, TRUE~0),
    cvd_01N = case_when(cvd_01==1 | cvd_00N==1 ~1, TRUE~0),
    cvd_02N = case_when(cvd_02==1 | cvd_01N==1 ~1, TRUE~0),
    cvd_03N = case_when(cvd_03==1 | cvd_02N==1 ~1, TRUE~0),
    cvd_04N = case_when(cvd_04==1 | cvd_03N==1 ~1, TRUE~0),
    cvd_05N = case_when(cvd_05==1 | cvd_04N==1 ~1, TRUE~0),
    cvd_06N = case_when(cvd_06==1 | cvd_05N==1 ~1, TRUE~0),
    cvd_07N = case_when(cvd_07==1 | cvd_06N==1 ~1, TRUE~0),
    cvd_08N = case_when(cvd_08==1 | cvd_07N==1 ~1, TRUE~0),
    cvd_09N = case_when(cvd_09==1 | cvd_08N==1 ~1, TRUE~0),
    cvd_10N = case_when(cvd_10==1 | cvd_09N==1 ~1, TRUE~0),
    cvd_11N = case_when(cvd_11==1 | cvd_10N==1 ~1, TRUE~0),
    cvd_12N = case_when(cvd_12==1 | cvd_11N==1 ~1, TRUE~0),
    cvd_13N = case_when(cvd_13==1 | cvd_12N==1 ~1, TRUE~0),
    cvd_14N = case_when(cvd_14==1 | cvd_13N==1 ~1, TRUE~0),
    copd_99N = case_when(copd_99==1 ~1, TRUE~0),
    copd_00N = case_when(copd_00==1 | copd_99N==1 ~1, TRUE~0),
    copd_01N = case_when(copd_01==1 | copd_00N==1 ~1, TRUE~0),
    copd_02N = case_when(copd_02==1 | copd_01N==1 ~1, TRUE~0),
    copd_03N = case_when(copd_03==1 | copd_02N==1 ~1, TRUE~0),
    copd_04N = case_when(copd_04==1 | copd_03N==1 ~1, TRUE~0),
    copd_05N = case_when(copd_05==1 | copd_04N==1 ~1, TRUE~0),
    copd_06N = case_when(copd_06==1 | copd_05N==1 ~1, TRUE~0),
    copd_07N = case_when(copd_07==1 | copd_06N==1 ~1, TRUE~0),
    copd_08N = case_when(copd_08==1 | copd_07N==1 ~1, TRUE~0),
    copd_09N = case_when(copd_09==1 | copd_08N==1 ~1, TRUE~0),
    copd_10N = case_when(copd_10==1 | copd_09N==1 ~1, TRUE~0),
    copd_11N = case_when(copd_11==1 | copd_10N==1 ~1, TRUE~0),
    copd_12N = case_when(copd_12==1 | copd_11N==1 ~1, TRUE~0),
    copd_13N = case_when(copd_13==1 | copd_12N==1 ~1, TRUE~0),
    copd_14N = case_when(copd_14==1 | copd_13N==1 ~1, TRUE~0),
    dementia_99N = case_when(dementia_99==1 ~1, TRUE~0),
    dementia_00N = case_when(dementia_00==1 | dementia_99N==1 ~1, TRUE~0),
    dementia_01N = case_when(dementia_01==1 | dementia_00N==1 ~1, TRUE~0),
    dementia_02N = case_when(dementia_02==1 | dementia_01N==1 ~1, TRUE~0),
    dementia_03N = case_when(dementia_03==1 | dementia_02N==1 ~1, TRUE~0),
    dementia_04N = case_when(dementia_04==1 | dementia_03N==1 ~1, TRUE~0),
    dementia_05N = case_when(dementia_05==1 | dementia_04N==1 ~1, TRUE~0),
    dementia_06N = case_when(dementia_06==1 | dementia_05N==1 ~1, TRUE~0),
    dementia_07N = case_when(dementia_07==1 | dementia_06N==1 ~1, TRUE~0),
    dementia_08N = case_when(dementia_08==1 | dementia_07N==1 ~1, TRUE~0),
    dementia_09N = case_when(dementia_09==1 | dementia_08N==1 ~1, TRUE~0),
    dementia_10N = case_when(dementia_10==1 | dementia_09N==1 ~1, TRUE~0),
    dementia_11N = case_when(dementia_11==1 | dementia_10N==1 ~1, TRUE~0),
    dementia_12N = case_when(dementia_12==1 | dementia_11N==1 ~1, TRUE~0),
    dementia_13N = case_when(dementia_13==1 | dementia_12N==1 ~1, TRUE~0),
    dementia_14N = case_when(dementia_14==1 | dementia_13N==1 ~1, TRUE~0),
    paralysis_99N = case_when(paralysis_99==1 ~1, TRUE~0),
    paralysis_00N = case_when(paralysis_00==1 | paralysis_99N==1 ~1, TRUE~0),
    paralysis_01N = case_when(paralysis_01==1 | paralysis_00N==1 ~1, TRUE~0),
    paralysis_02N = case_when(paralysis_02==1 | paralysis_01N==1 ~1, TRUE~0),
    paralysis_03N = case_when(paralysis_03==1 | paralysis_02N==1 ~1, TRUE~0),
    paralysis_04N = case_when(paralysis_04==1 | paralysis_03N==1 ~1, TRUE~0),
    paralysis_05N = case_when(paralysis_05==1 | paralysis_04N==1 ~1, TRUE~0),
    paralysis_06N = case_when(paralysis_06==1 | paralysis_05N==1 ~1, TRUE~0),
    paralysis_07N = case_when(paralysis_07==1 | paralysis_06N==1 ~1, TRUE~0),
    paralysis_08N = case_when(paralysis_08==1 | paralysis_07N==1 ~1, TRUE~0),
    paralysis_09N = case_when(paralysis_09==1 | paralysis_08N==1 ~1, TRUE~0),
    paralysis_10N = case_when(paralysis_10==1 | paralysis_09N==1 ~1, TRUE~0),
    paralysis_11N = case_when(paralysis_11==1 | paralysis_10N==1 ~1, TRUE~0),
    paralysis_12N = case_when(paralysis_12==1 | paralysis_11N==1 ~1, TRUE~0),
    paralysis_13N = case_when(paralysis_13==1 | paralysis_12N==1 ~1, TRUE~0),
    paralysis_14N = case_when(paralysis_14==1 | paralysis_13N==1 ~1, TRUE~0),
    diabetes_99N = case_when(diabetes_99==1 ~1, TRUE~0),
    diabetes_00N = case_when(diabetes_00==1 | diabetes_99N==1 ~1, TRUE~0),
    diabetes_01N = case_when(diabetes_01==1 | diabetes_00N==1 ~1, TRUE~0),
    diabetes_02N = case_when(diabetes_02==1 | diabetes_01N==1 ~1, TRUE~0),
    diabetes_03N = case_when(diabetes_03==1 | diabetes_02N==1 ~1, TRUE~0),
    diabetes_04N = case_when(diabetes_04==1 | diabetes_03N==1 ~1, TRUE~0),
    diabetes_05N = case_when(diabetes_05==1 | diabetes_04N==1 ~1, TRUE~0),
    diabetes_06N = case_when(diabetes_06==1 | diabetes_05N==1 ~1, TRUE~0),
    diabetes_07N = case_when(diabetes_07==1 | diabetes_06N==1 ~1, TRUE~0),
    diabetes_08N = case_when(diabetes_08==1 | diabetes_07N==1 ~1, TRUE~0),
    diabetes_09N = case_when(diabetes_09==1 | diabetes_08N==1 ~1, TRUE~0),
    diabetes_10N = case_when(diabetes_10==1 | diabetes_09N==1 ~1, TRUE~0),
    diabetes_11N = case_when(diabetes_11==1 | diabetes_10N==1 ~1, TRUE~0),
    diabetes_12N = case_when(diabetes_12==1 | diabetes_11N==1 ~1, TRUE~0),
    diabetes_13N = case_when(diabetes_13==1 | diabetes_12N==1 ~1, TRUE~0),
    diabetes_14N = case_when(diabetes_14==1 | diabetes_13N==1 ~1, TRUE~0),
    diabetes_comp_99N = case_when(diabetes_comp_99==1 ~1, TRUE~0),
    diabetes_comp_00N = case_when(diabetes_comp_00==1 | diabetes_comp_99N==1 ~1, TRUE~0),
    diabetes_comp_01N = case_when(diabetes_comp_01==1 | diabetes_comp_00N==1 ~1, TRUE~0),
    diabetes_comp_02N = case_when(diabetes_comp_02==1 | diabetes_comp_01N==1 ~1, TRUE~0),
    diabetes_comp_03N = case_when(diabetes_comp_03==1 | diabetes_comp_02N==1 ~1, TRUE~0),
    diabetes_comp_04N = case_when(diabetes_comp_04==1 | diabetes_comp_03N==1 ~1, TRUE~0),
    diabetes_comp_05N = case_when(diabetes_comp_05==1 | diabetes_comp_04N==1 ~1, TRUE~0),
    diabetes_comp_06N = case_when(diabetes_comp_06==1 | diabetes_comp_05N==1 ~1, TRUE~0),
    diabetes_comp_07N = case_when(diabetes_comp_07==1 | diabetes_comp_06N==1 ~1, TRUE~0),
    diabetes_comp_08N = case_when(diabetes_comp_08==1 | diabetes_comp_07N==1 ~1, TRUE~0),
    diabetes_comp_09N = case_when(diabetes_comp_09==1 | diabetes_comp_08N==1 ~1, TRUE~0),
    diabetes_comp_10N = case_when(diabetes_comp_10==1 | diabetes_comp_09N==1 ~1, TRUE~0),
    diabetes_comp_11N = case_when(diabetes_comp_11==1 | diabetes_comp_10N==1 ~1, TRUE~0),
    diabetes_comp_12N = case_when(diabetes_comp_12==1 | diabetes_comp_11N==1 ~1, TRUE~0),
    diabetes_comp_13N = case_when(diabetes_comp_13==1 | diabetes_comp_12N==1 ~1, TRUE~0),
    diabetes_comp_14N = case_when(diabetes_comp_14==1 | diabetes_comp_13N==1 ~1, TRUE~0),
    renal_disease_99N = case_when(renal_disease_99==1 ~1, TRUE~0),
    renal_disease_00N = case_when(renal_disease_00==1 | renal_disease_99N==1 ~1, TRUE~0),
    renal_disease_01N = case_when(renal_disease_01==1 | renal_disease_00N==1 ~1, TRUE~0),
    renal_disease_02N = case_when(renal_disease_02==1 | renal_disease_01N==1 ~1, TRUE~0),
    renal_disease_03N = case_when(renal_disease_03==1 | renal_disease_02N==1 ~1, TRUE~0),
    renal_disease_04N = case_when(renal_disease_04==1 | renal_disease_03N==1 ~1, TRUE~0),
    renal_disease_05N = case_when(renal_disease_05==1 | renal_disease_04N==1 ~1, TRUE~0),
    renal_disease_06N = case_when(renal_disease_06==1 | renal_disease_05N==1 ~1, TRUE~0),
    renal_disease_07N = case_when(renal_disease_07==1 | renal_disease_06N==1 ~1, TRUE~0),
    renal_disease_08N = case_when(renal_disease_08==1 | renal_disease_07N==1 ~1, TRUE~0),
    renal_disease_09N = case_when(renal_disease_09==1 | renal_disease_08N==1 ~1, TRUE~0),
    renal_disease_10N = case_when(renal_disease_10==1 | renal_disease_09N==1 ~1, TRUE~0),
    renal_disease_11N = case_when(renal_disease_11==1 | renal_disease_10N==1 ~1, TRUE~0),
    renal_disease_12N = case_when(renal_disease_12==1 | renal_disease_11N==1 ~1, TRUE~0),
    renal_disease_13N = case_when(renal_disease_13==1 | renal_disease_12N==1 ~1, TRUE~0),
    renal_disease_14N = case_when(renal_disease_14==1 | renal_disease_13N==1 ~1, TRUE~0),
    mild_liver_disease_99N = case_when(mild_liver_disease_99==1 ~1, TRUE~0),
    mild_liver_disease_00N = case_when(mild_liver_disease_00==1 | mild_liver_disease_99N==1 ~1, TRUE~0),
    mild_liver_disease_01N = case_when(mild_liver_disease_01==1 | mild_liver_disease_00N==1 ~1, TRUE~0),
    mild_liver_disease_02N = case_when(mild_liver_disease_02==1 | mild_liver_disease_01N==1 ~1, TRUE~0),
    mild_liver_disease_03N = case_when(mild_liver_disease_03==1 | mild_liver_disease_02N==1 ~1, TRUE~0),
    mild_liver_disease_04N = case_when(mild_liver_disease_04==1 | mild_liver_disease_03N==1 ~1, TRUE~0),
    mild_liver_disease_05N = case_when(mild_liver_disease_05==1 | mild_liver_disease_04N==1 ~1, TRUE~0),
    mild_liver_disease_06N = case_when(mild_liver_disease_06==1 | mild_liver_disease_05N==1 ~1, TRUE~0),
    mild_liver_disease_07N = case_when(mild_liver_disease_07==1 | mild_liver_disease_06N==1 ~1, TRUE~0),
    mild_liver_disease_08N = case_when(mild_liver_disease_08==1 | mild_liver_disease_07N==1 ~1, TRUE~0),
    mild_liver_disease_09N = case_when(mild_liver_disease_09==1 | mild_liver_disease_08N==1 ~1, TRUE~0),
    mild_liver_disease_10N = case_when(mild_liver_disease_10==1 | mild_liver_disease_09N==1 ~1, TRUE~0),
    mild_liver_disease_11N = case_when(mild_liver_disease_11==1 | mild_liver_disease_10N==1 ~1, TRUE~0),
    mild_liver_disease_12N = case_when(mild_liver_disease_12==1 | mild_liver_disease_11N==1 ~1, TRUE~0),
    mild_liver_disease_13N = case_when(mild_liver_disease_13==1 | mild_liver_disease_12N==1 ~1, TRUE~0),
    mild_liver_disease_14N = case_when(mild_liver_disease_14==1 | mild_liver_disease_13N==1 ~1, TRUE~0),
    liver_disease_99N = case_when(liver_disease_99==1 ~1, TRUE~0),
    liver_disease_00N = case_when(liver_disease_00==1 | liver_disease_99N==1 ~1, TRUE~0),
    liver_disease_01N = case_when(liver_disease_01==1 | liver_disease_00N==1 ~1, TRUE~0),
    liver_disease_02N = case_when(liver_disease_02==1 | liver_disease_01N==1 ~1, TRUE~0),
    liver_disease_03N = case_when(liver_disease_03==1 | liver_disease_02N==1 ~1, TRUE~0),
    liver_disease_04N = case_when(liver_disease_04==1 | liver_disease_03N==1 ~1, TRUE~0),
    liver_disease_05N = case_when(liver_disease_05==1 | liver_disease_04N==1 ~1, TRUE~0),
    liver_disease_06N = case_when(liver_disease_06==1 | liver_disease_05N==1 ~1, TRUE~0),
    liver_disease_07N = case_when(liver_disease_07==1 | liver_disease_06N==1 ~1, TRUE~0),
    liver_disease_08N = case_when(liver_disease_08==1 | liver_disease_07N==1 ~1, TRUE~0),
    liver_disease_09N = case_when(liver_disease_09==1 | liver_disease_08N==1 ~1, TRUE~0),
    liver_disease_10N = case_when(liver_disease_10==1 | liver_disease_09N==1 ~1, TRUE~0),
    liver_disease_11N = case_when(liver_disease_11==1 | liver_disease_10N==1 ~1, TRUE~0),
    liver_disease_12N = case_when(liver_disease_12==1 | liver_disease_11N==1 ~1, TRUE~0),
    liver_disease_13N = case_when(liver_disease_13==1 | liver_disease_12N==1 ~1, TRUE~0),
    liver_disease_14N = case_when(liver_disease_14==1 | liver_disease_13N==1 ~1, TRUE~0),
    ulcers_99N = case_when(ulcers_99==1 ~1, TRUE~0),
    ulcers_00N = case_when(ulcers_00==1 | ulcers_99N==1 ~1, TRUE~0),
    ulcers_01N = case_when(ulcers_01==1 | ulcers_00N==1 ~1, TRUE~0),
    ulcers_02N = case_when(ulcers_02==1 | ulcers_01N==1 ~1, TRUE~0),
    ulcers_03N = case_when(ulcers_03==1 | ulcers_02N==1 ~1, TRUE~0),
    ulcers_04N = case_when(ulcers_04==1 | ulcers_03N==1 ~1, TRUE~0),
    ulcers_05N = case_when(ulcers_05==1 | ulcers_04N==1 ~1, TRUE~0),
    ulcers_06N = case_when(ulcers_06==1 | ulcers_05N==1 ~1, TRUE~0),
    ulcers_07N = case_when(ulcers_07==1 | ulcers_06N==1 ~1, TRUE~0),
    ulcers_08N = case_when(ulcers_08==1 | ulcers_07N==1 ~1, TRUE~0),
    ulcers_09N = case_when(ulcers_09==1 | ulcers_08N==1 ~1, TRUE~0),
    ulcers_10N = case_when(ulcers_10==1 | ulcers_09N==1 ~1, TRUE~0),
    ulcers_11N = case_when(ulcers_11==1 | ulcers_10N==1 ~1, TRUE~0),
    ulcers_12N = case_when(ulcers_12==1 | ulcers_11N==1 ~1, TRUE~0),
    ulcers_13N = case_when(ulcers_13==1 | ulcers_12N==1 ~1, TRUE~0),
    ulcers_14N = case_when(ulcers_14==1 | ulcers_13N==1 ~1, TRUE~0),
    rheum_disease_99N = case_when(rheum_disease_99==1 ~1, TRUE~0),
    rheum_disease_00N = case_when(rheum_disease_00==1 | rheum_disease_99N==1 ~1, TRUE~0),
    rheum_disease_01N = case_when(rheum_disease_01==1 | rheum_disease_00N==1 ~1, TRUE~0),
    rheum_disease_02N = case_when(rheum_disease_02==1 | rheum_disease_01N==1 ~1, TRUE~0),
    rheum_disease_03N = case_when(rheum_disease_03==1 | rheum_disease_02N==1 ~1, TRUE~0),
    rheum_disease_04N = case_when(rheum_disease_04==1 | rheum_disease_03N==1 ~1, TRUE~0),
    rheum_disease_05N = case_when(rheum_disease_05==1 | rheum_disease_04N==1 ~1, TRUE~0),
    rheum_disease_06N = case_when(rheum_disease_06==1 | rheum_disease_05N==1 ~1, TRUE~0),
    rheum_disease_07N = case_when(rheum_disease_07==1 | rheum_disease_06N==1 ~1, TRUE~0),
    rheum_disease_08N = case_when(rheum_disease_08==1 | rheum_disease_07N==1 ~1, TRUE~0),
    rheum_disease_09N = case_when(rheum_disease_09==1 | rheum_disease_08N==1 ~1, TRUE~0),
    rheum_disease_10N = case_when(rheum_disease_10==1 | rheum_disease_09N==1 ~1, TRUE~0),
    rheum_disease_11N = case_when(rheum_disease_11==1 | rheum_disease_10N==1 ~1, TRUE~0),
    rheum_disease_12N = case_when(rheum_disease_12==1 | rheum_disease_11N==1 ~1, TRUE~0),
    rheum_disease_13N = case_when(rheum_disease_13==1 | rheum_disease_12N==1 ~1, TRUE~0),
    rheum_disease_14N = case_when(rheum_disease_14==1 | rheum_disease_13N==1 ~1, TRUE~0),
    aids_99N = case_when(aids_99==1 ~1, TRUE~0),
    aids_00N = case_when(aids_00==1 | aids_99N==1 ~1, TRUE~0),
    aids_01N = case_when(aids_01==1 | aids_00N==1 ~1, TRUE~0),
    aids_02N = case_when(aids_02==1 | aids_01N==1 ~1, TRUE~0),
    aids_03N = case_when(aids_03==1 | aids_02N==1 ~1, TRUE~0),
    aids_04N = case_when(aids_04==1 | aids_03N==1 ~1, TRUE~0),
    aids_05N = case_when(aids_05==1 | aids_04N==1 ~1, TRUE~0),
    aids_06N = case_when(aids_06==1 | aids_05N==1 ~1, TRUE~0),
    aids_07N = case_when(aids_07==1 | aids_06N==1 ~1, TRUE~0),
    aids_08N = case_when(aids_08==1 | aids_07N==1 ~1, TRUE~0),
    aids_09N = case_when(aids_09==1 | aids_08N==1 ~1, TRUE~0),
    aids_10N = case_when(aids_10==1 | aids_09N==1 ~1, TRUE~0),
    aids_11N = case_when(aids_11==1 | aids_10N==1 ~1, TRUE~0),
    aids_12N = case_when(aids_12==1 | aids_11N==1 ~1, TRUE~0),
    aids_13N = case_when(aids_13==1 | aids_12N==1 ~1, TRUE~0),
    aids_14N = case_when(aids_14==1 | aids_13N==1 ~1, TRUE~0)) %>% 
  mutate(
    NCI_New99 = (1.14*acute_mi_99N) + (1.08*history_mi_99N) + (1.91*chf_99N) + (1.30*pvd_99N) + (1.32*cvd_99N) + (1.69*copd_99N) + (2.06*dementia_99N) + (1.49*paralysis_99N) + pmax((1.34*diabetes_99N), (1.34*diabetes_comp_99N)) + (1.60*renal_disease_99N) + pmax((2.09*mild_liver_disease_99N), (2.09*liver_disease_99N)) + (1.08*ulcers_99N) + (1.25*rheum_disease_99N) + (1.79*aids_99N),
    NCI_New00 = (1.14*acute_mi_00N) + (1.08*history_mi_00N) + (1.91*chf_00N) + (1.30*pvd_00N) + (1.32*cvd_00N) + (1.69*copd_00N) + (2.06*dementia_00N) + (1.49*paralysis_00N) + pmax((1.34*diabetes_00N), (1.34*diabetes_comp_00N)) + (1.60*renal_disease_00N) + pmax((2.09*mild_liver_disease_00N), (2.09*liver_disease_00N)) + (1.08*ulcers_00N) + (1.25*rheum_disease_00N) + (1.79*aids_00N),
    NCI_New01 = (1.14*acute_mi_01N) + (1.08*history_mi_01N) + (1.91*chf_01N) + (1.30*pvd_01N) + (1.32*cvd_01N) + (1.69*copd_01N) + (2.06*dementia_01N) + (1.49*paralysis_01N) + pmax((1.34*diabetes_01N), (1.34*diabetes_comp_01N)) + (1.60*renal_disease_01N) + pmax((2.09*mild_liver_disease_01N), (2.09*liver_disease_01N)) + (1.08*ulcers_01N) + (1.25*rheum_disease_01N) + (1.79*aids_01N),
    NCI_New02 = (1.14*acute_mi_02N) + (1.08*history_mi_02N) + (1.91*chf_02N) + (1.30*pvd_02N) + (1.32*cvd_02N) + (1.69*copd_02N) + (2.06*dementia_02N) + (1.49*paralysis_02N) + pmax((1.34*diabetes_02N), (1.34*diabetes_comp_02N)) + (1.60*renal_disease_02N) + pmax((2.09*mild_liver_disease_02N), (2.09*liver_disease_02N)) + (1.08*ulcers_02N) + (1.25*rheum_disease_02N) + (1.79*aids_02N),
    NCI_New03 = (1.14*acute_mi_03N) + (1.08*history_mi_03N) + (1.91*chf_03N) + (1.30*pvd_03N) + (1.32*cvd_03N) + (1.69*copd_03N) + (2.06*dementia_03N) + (1.49*paralysis_03N) + pmax((1.34*diabetes_03N), (1.34*diabetes_comp_03N)) + (1.60*renal_disease_03N) + pmax((2.09*mild_liver_disease_03N), (2.09*liver_disease_03N)) + (1.08*ulcers_03N) + (1.25*rheum_disease_03N) + (1.79*aids_03N),
    NCI_New04 = (1.14*acute_mi_04N) + (1.08*history_mi_04N) + (1.91*chf_04N) + (1.30*pvd_04N) + (1.32*cvd_04N) + (1.69*copd_04N) + (2.06*dementia_04N) + (1.49*paralysis_04N) + pmax((1.34*diabetes_04N), (1.34*diabetes_comp_04N)) + (1.60*renal_disease_04N) + pmax((2.09*mild_liver_disease_04N), (2.09*liver_disease_04N)) + (1.08*ulcers_04N) + (1.25*rheum_disease_04N) + (1.79*aids_04N),
    NCI_New05 = (1.14*acute_mi_05N) + (1.08*history_mi_05N) + (1.91*chf_05N) + (1.30*pvd_05N) + (1.32*cvd_05N) + (1.69*copd_05N) + (2.06*dementia_05N) + (1.49*paralysis_05N) + pmax((1.34*diabetes_05N), (1.34*diabetes_comp_05N)) + (1.60*renal_disease_05N) + pmax((2.09*mild_liver_disease_05N), (2.09*liver_disease_05N)) + (1.08*ulcers_05N) + (1.25*rheum_disease_05N) + (1.79*aids_05N),
    NCI_New06 = (1.14*acute_mi_06N) + (1.08*history_mi_06N) + (1.91*chf_06N) + (1.30*pvd_06N) + (1.32*cvd_06N) + (1.69*copd_06N) + (2.06*dementia_06N) + (1.49*paralysis_06N) + pmax((1.34*diabetes_06N), (1.34*diabetes_comp_06N)) + (1.60*renal_disease_06N) + pmax((2.09*mild_liver_disease_06N), (2.09*liver_disease_06N)) + (1.08*ulcers_06N) + (1.25*rheum_disease_06N) + (1.79*aids_06N),
    NCI_New07 = (1.14*acute_mi_07N) + (1.08*history_mi_07N) + (1.91*chf_07N) + (1.30*pvd_07N) + (1.32*cvd_07N) + (1.69*copd_07N) + (2.06*dementia_07N) + (1.49*paralysis_07N) + pmax((1.34*diabetes_07N), (1.34*diabetes_comp_07N)) + (1.60*renal_disease_07N) + pmax((2.09*mild_liver_disease_07N), (2.09*liver_disease_07N)) + (1.08*ulcers_07N) + (1.25*rheum_disease_07N) + (1.79*aids_07N),
    NCI_New08 = (1.14*acute_mi_08N) + (1.08*history_mi_08N) + (1.91*chf_08N) + (1.30*pvd_08N) + (1.32*cvd_08N) + (1.69*copd_08N) + (2.06*dementia_08N) + (1.49*paralysis_08N) + pmax((1.34*diabetes_08N), (1.34*diabetes_comp_08N)) + (1.60*renal_disease_08N) + pmax((2.09*mild_liver_disease_08N), (2.09*liver_disease_08N)) + (1.08*ulcers_08N) + (1.25*rheum_disease_08N) + (1.79*aids_08N),
    NCI_New09 = (1.14*acute_mi_09N) + (1.08*history_mi_09N) + (1.91*chf_09N) + (1.30*pvd_09N) + (1.32*cvd_09N) + (1.69*copd_09N) + (2.06*dementia_09N) + (1.49*paralysis_09N) + pmax((1.34*diabetes_09N), (1.34*diabetes_comp_09N)) + (1.60*renal_disease_09N) + pmax((2.09*mild_liver_disease_09N), (2.09*liver_disease_09N)) + (1.08*ulcers_09N) + (1.25*rheum_disease_09N) + (1.79*aids_09N),
    NCI_New10 = (1.14*acute_mi_10N) + (1.08*history_mi_10N) + (1.91*chf_10N) + (1.30*pvd_10N) + (1.32*cvd_10N) + (1.69*copd_10N) + (2.06*dementia_10N) + (1.49*paralysis_10N) + pmax((1.34*diabetes_10N), (1.34*diabetes_comp_10N)) + (1.60*renal_disease_10N) + pmax((2.09*mild_liver_disease_10N), (2.09*liver_disease_10N)) + (1.08*ulcers_10N) + (1.25*rheum_disease_10N) + (1.79*aids_10N),
    NCI_New11 = (1.14*acute_mi_11N) + (1.08*history_mi_11N) + (1.91*chf_11N) + (1.30*pvd_11N) + (1.32*cvd_11N) + (1.69*copd_11N) + (2.06*dementia_11N) + (1.49*paralysis_11N) + pmax((1.34*diabetes_11N), (1.34*diabetes_comp_11N)) + (1.60*renal_disease_11N) + pmax((2.09*mild_liver_disease_11N), (2.09*liver_disease_11N)) + (1.08*ulcers_11N) + (1.25*rheum_disease_11N) + (1.79*aids_11N),
    NCI_New12 = (1.14*acute_mi_12N) + (1.08*history_mi_12N) + (1.91*chf_12N) + (1.30*pvd_12N) + (1.32*cvd_12N) + (1.69*copd_12N) + (2.06*dementia_12N) + (1.49*paralysis_12N) + pmax((1.34*diabetes_12N), (1.34*diabetes_comp_12N)) + (1.60*renal_disease_12N) + pmax((2.09*mild_liver_disease_12N), (2.09*liver_disease_12N)) + (1.08*ulcers_12N) + (1.25*rheum_disease_12N) + (1.79*aids_12N),
    NCI_New13 = (1.14*acute_mi_13N) + (1.08*history_mi_13N) + (1.91*chf_13N) + (1.30*pvd_13N) + (1.32*cvd_13N) + (1.69*copd_13N) + (2.06*dementia_13N) + (1.49*paralysis_13N) + pmax((1.34*diabetes_13N), (1.34*diabetes_comp_13N)) + (1.60*renal_disease_13N) + pmax((2.09*mild_liver_disease_13N), (2.09*liver_disease_13N)) + (1.08*ulcers_13N) + (1.25*rheum_disease_13N) + (1.79*aids_13N),
    NCI_New14 = (1.14*acute_mi_14N) + (1.08*history_mi_14N) + (1.91*chf_14N) + (1.30*pvd_14N) + (1.32*cvd_14N) + (1.69*copd_14N) + (2.06*dementia_14N) + (1.49*paralysis_14N) + pmax((1.34*diabetes_14N), (1.34*diabetes_comp_14N)) + (1.60*renal_disease_14N) + pmax((2.09*mild_liver_disease_14N), (2.09*liver_disease_14N)) + (1.08*ulcers_14N) + (1.25*rheum_disease_14N) + (1.79*aids_14N))


# Now add in the MBSF file data to determine eligibility each year

MBSF99 <- read_sas("~/sdrive/sasusershare/CPS/CPS2/Medicare_March2021/SAS datasets/1999/mbsf_ab_summary_1999.sas7bdat") %>%
  select(BENE_ID, BENE_HI_CVRAGE_TOT_MONS, BENE_SMI_CVRAGE_TOT_MONS, BENE_HMO_CVRAGE_TOT_MONS, BENE_AGE_AT_END_REF_YR) %>%
  mutate(FullCov = case_when(
    BENE_HI_CVRAGE_TOT_MONS==12 & BENE_SMI_CVRAGE_TOT_MONS==12 & BENE_HMO_CVRAGE_TOT_MONS == 0 & BENE_AGE_AT_END_REF_YR >= 65 ~1,
    TRUE ~ 0
  )) %>%
  rename_with(~paste0(.,"_99"), 2:6)
MBSF00 <- read_sas("~/sdrive/sasusershare/CPS/CPS2/Medicare_March2021/SAS datasets/2000/mbsf_ab_summary_2000.sas7bdat") %>%
  select(BENE_ID, BENE_HI_CVRAGE_TOT_MONS, BENE_SMI_CVRAGE_TOT_MONS, BENE_HMO_CVRAGE_TOT_MONS, BENE_AGE_AT_END_REF_YR) %>%
  mutate(FullCov = case_when(
    BENE_HI_CVRAGE_TOT_MONS==12 & BENE_SMI_CVRAGE_TOT_MONS==12 & BENE_HMO_CVRAGE_TOT_MONS == 0 & BENE_AGE_AT_END_REF_YR >= 65 ~1,
    TRUE ~ 0
  )) %>%
  rename_with(~paste0(.,"_00"), 2:6)
MBSF01 <- read_sas("~/sdrive/sasusershare/CPS/CPS2/Medicare_March2021/SAS datasets/2001/mbsf_ab_summary_2001.sas7bdat") %>%
  select(BENE_ID, BENE_HI_CVRAGE_TOT_MONS, BENE_SMI_CVRAGE_TOT_MONS, BENE_HMO_CVRAGE_TOT_MONS, BENE_AGE_AT_END_REF_YR) %>%
  mutate(FullCov = case_when(
    BENE_HI_CVRAGE_TOT_MONS==12 & BENE_SMI_CVRAGE_TOT_MONS==12 & BENE_HMO_CVRAGE_TOT_MONS == 0 & BENE_AGE_AT_END_REF_YR >= 65 ~1,
    TRUE ~ 0
  )) %>%
  rename_with(~paste0(.,"_01"), 2:6)
MBSF02 <- read_sas("~/sdrive/sasusershare/CPS/CPS2/Medicare_March2021/SAS datasets/2002/mbsf_ab_summary_2002.sas7bdat") %>%
  select(BENE_ID, BENE_HI_CVRAGE_TOT_MONS, BENE_SMI_CVRAGE_TOT_MONS, BENE_HMO_CVRAGE_TOT_MONS, BENE_AGE_AT_END_REF_YR) %>%
  mutate(FullCov = case_when(
    BENE_HI_CVRAGE_TOT_MONS==12 & BENE_SMI_CVRAGE_TOT_MONS==12 & BENE_HMO_CVRAGE_TOT_MONS == 0 & BENE_AGE_AT_END_REF_YR >= 65 ~1,
    TRUE ~ 0
  )) %>%
  rename_with(~paste0(.,"_02"), 2:6)
MBSF03 <- read_sas("~/sdrive/sasusershare/CPS/CPS2/Medicare_March2021/SAS datasets/2003/mbsf_ab_summary_2003.sas7bdat") %>%
  select(BENE_ID, BENE_HI_CVRAGE_TOT_MONS, BENE_SMI_CVRAGE_TOT_MONS, BENE_HMO_CVRAGE_TOT_MONS, BENE_AGE_AT_END_REF_YR) %>%
  mutate(FullCov = case_when(
    BENE_HI_CVRAGE_TOT_MONS==12 & BENE_SMI_CVRAGE_TOT_MONS==12 & BENE_HMO_CVRAGE_TOT_MONS == 0 & BENE_AGE_AT_END_REF_YR >= 65 ~1,
    TRUE ~ 0
  )) %>%
  rename_with(~paste0(.,"_03"), 2:6)
MBSF04 <- read_sas("~/sdrive/sasusershare/CPS/CPS2/Medicare_March2021/SAS datasets/2004/mbsf_ab_summary_2004.sas7bdat") %>%
  select(BENE_ID, BENE_HI_CVRAGE_TOT_MONS, BENE_SMI_CVRAGE_TOT_MONS, BENE_HMO_CVRAGE_TOT_MONS, BENE_AGE_AT_END_REF_YR) %>%
  mutate(FullCov = case_when(
    BENE_HI_CVRAGE_TOT_MONS==12 & BENE_SMI_CVRAGE_TOT_MONS==12 & BENE_HMO_CVRAGE_TOT_MONS == 0 & BENE_AGE_AT_END_REF_YR >= 65 ~1,
    TRUE ~ 0
  )) %>%
  rename_with(~paste0(.,"_04"), 2:6)
MBSF05 <- read_sas("~/sdrive/sasusershare/CPS/CPS2/Medicare_March2021/SAS datasets/2005/mbsf_ab_summary_2005.sas7bdat") %>%
  select(BENE_ID, BENE_HI_CVRAGE_TOT_MONS, BENE_SMI_CVRAGE_TOT_MONS, BENE_HMO_CVRAGE_TOT_MONS, BENE_AGE_AT_END_REF_YR) %>%
  mutate(FullCov = case_when(
    BENE_HI_CVRAGE_TOT_MONS==12 & BENE_SMI_CVRAGE_TOT_MONS==12 & BENE_HMO_CVRAGE_TOT_MONS == 0 & BENE_AGE_AT_END_REF_YR >= 65 ~1,
    TRUE ~ 0
  )) %>%
  rename_with(~paste0(.,"_05"), 2:6)
MBSF06 <- read_sas("~/sdrive/sasusershare/CPS/CPS2/Medicare_March2021/SAS datasets/2006/mbsf_abcd_summary_2006.sas7bdat") %>%
  select(BENE_ID, BENE_HI_CVRAGE_TOT_MONS, BENE_SMI_CVRAGE_TOT_MONS, BENE_HMO_CVRAGE_TOT_MONS, AGE_AT_END_REF_YR) %>% rename(BENE_AGE_AT_END_REF_YR = AGE_AT_END_REF_YR) %>%
  mutate(FullCov = case_when(
    BENE_HI_CVRAGE_TOT_MONS==12 & BENE_SMI_CVRAGE_TOT_MONS==12 & BENE_HMO_CVRAGE_TOT_MONS == 0 & BENE_AGE_AT_END_REF_YR >= 65 ~1,
    TRUE ~ 0
  )) %>%
  rename_with(~paste0(.,"_06"), 2:6)
MBSF07 <- read_sas("~/sdrive/sasusershare/CPS/CPS2/Medicare_March2021/SAS datasets/2007/mbsf_abcd_summary_2007.sas7bdat") %>%
  select(BENE_ID, BENE_HI_CVRAGE_TOT_MONS, BENE_SMI_CVRAGE_TOT_MONS, BENE_HMO_CVRAGE_TOT_MONS, AGE_AT_END_REF_YR) %>% rename(BENE_AGE_AT_END_REF_YR = AGE_AT_END_REF_YR) %>%
  mutate(FullCov = case_when(
    BENE_HI_CVRAGE_TOT_MONS==12 & BENE_SMI_CVRAGE_TOT_MONS==12 & BENE_HMO_CVRAGE_TOT_MONS == 0 & BENE_AGE_AT_END_REF_YR >= 65 ~1,
    TRUE ~ 0
  )) %>%
  rename_with(~paste0(.,"_07"), 2:6)
MBSF08 <- read_sas("~/sdrive/sasusershare/CPS/CPS2/Medicare_March2021/SAS datasets/2008/mbsf_abcd_summary_2008.sas7bdat") %>%
  select(BENE_ID, BENE_HI_CVRAGE_TOT_MONS, BENE_SMI_CVRAGE_TOT_MONS, BENE_HMO_CVRAGE_TOT_MONS, AGE_AT_END_REF_YR) %>% rename(BENE_AGE_AT_END_REF_YR = AGE_AT_END_REF_YR) %>%
  mutate(FullCov = case_when(
    BENE_HI_CVRAGE_TOT_MONS==12 & BENE_SMI_CVRAGE_TOT_MONS==12 & BENE_HMO_CVRAGE_TOT_MONS == 0 & BENE_AGE_AT_END_REF_YR >= 65 ~1,
    TRUE ~ 0
  )) %>%
  rename_with(~paste0(.,"_08"), 2:6)
MBSF09 <- read_sas("~/sdrive/sasusershare/CPS/CPS2/Medicare_March2021/SAS datasets/2009/mbsf_abcd_summary_2009.sas7bdat") %>%
  select(BENE_ID, BENE_HI_CVRAGE_TOT_MONS, BENE_SMI_CVRAGE_TOT_MONS, BENE_HMO_CVRAGE_TOT_MONS, AGE_AT_END_REF_YR) %>% rename(BENE_AGE_AT_END_REF_YR = AGE_AT_END_REF_YR) %>%
  mutate(FullCov = case_when(
    BENE_HI_CVRAGE_TOT_MONS==12 & BENE_SMI_CVRAGE_TOT_MONS==12 & BENE_HMO_CVRAGE_TOT_MONS == 0 & BENE_AGE_AT_END_REF_YR >= 65 ~1,
    TRUE ~ 0
  )) %>%
  rename_with(~paste0(.,"_09"), 2:6)
MBSF10 <- read_sas("~/sdrive/sasusershare/CPS/CPS2/Medicare_March2021/SAS datasets/2010/mbsf_abcd_summary_2010.sas7bdat") %>%
  select(BENE_ID, BENE_HI_CVRAGE_TOT_MONS, BENE_SMI_CVRAGE_TOT_MONS, BENE_HMO_CVRAGE_TOT_MONS, AGE_AT_END_REF_YR) %>% rename(BENE_AGE_AT_END_REF_YR = AGE_AT_END_REF_YR) %>%
  mutate(FullCov = case_when(
    BENE_HI_CVRAGE_TOT_MONS==12 & BENE_SMI_CVRAGE_TOT_MONS==12 & BENE_HMO_CVRAGE_TOT_MONS == 0 & BENE_AGE_AT_END_REF_YR >= 65 ~1,
    TRUE ~ 0
  )) %>%
  rename_with(~paste0(.,"_10"), 2:6)
MBSF11 <- read_sas("~/sdrive/sasusershare/CPS/CPS2/Medicare_March2021/SAS datasets/2011/mbsf_abcd_summary_2011.sas7bdat") %>%
  select(BENE_ID, BENE_HI_CVRAGE_TOT_MONS, BENE_SMI_CVRAGE_TOT_MONS, BENE_HMO_CVRAGE_TOT_MONS, AGE_AT_END_REF_YR) %>% rename(BENE_AGE_AT_END_REF_YR = AGE_AT_END_REF_YR) %>%
  mutate(FullCov = case_when(
    BENE_HI_CVRAGE_TOT_MONS==12 & BENE_SMI_CVRAGE_TOT_MONS==12 & BENE_HMO_CVRAGE_TOT_MONS == 0 & BENE_AGE_AT_END_REF_YR >= 65 ~1,
    TRUE ~ 0
  )) %>%
  rename_with(~paste0(.,"_11"), 2:6)
MBSF12 <- read_sas("~/sdrive/sasusershare/CPS/CPS2/Medicare_March2021/SAS datasets/2012/mbsf_abcd_summary_2012.sas7bdat") %>%
  select(BENE_ID, BENE_HI_CVRAGE_TOT_MONS, BENE_SMI_CVRAGE_TOT_MONS, BENE_HMO_CVRAGE_TOT_MONS, AGE_AT_END_REF_YR) %>% rename(BENE_AGE_AT_END_REF_YR = AGE_AT_END_REF_YR) %>%
  mutate(FullCov = case_when(
    BENE_HI_CVRAGE_TOT_MONS==12 & BENE_SMI_CVRAGE_TOT_MONS==12 & BENE_HMO_CVRAGE_TOT_MONS == 0 & BENE_AGE_AT_END_REF_YR >= 65 ~1,
    TRUE ~ 0
  )) %>%
  rename_with(~paste0(.,"_12"), 2:6)
MBSF13 <- read_sas("~/sdrive/sasusershare/CPS/CPS2/Medicare_March2021/SAS datasets/2013/mbsf_abcd_summary_2013.sas7bdat") %>%
  select(BENE_ID, BENE_HI_CVRAGE_TOT_MONS, BENE_SMI_CVRAGE_TOT_MONS, BENE_HMO_CVRAGE_TOT_MONS, AGE_AT_END_REF_YR) %>% rename(BENE_AGE_AT_END_REF_YR = AGE_AT_END_REF_YR) %>%
  mutate(FullCov = case_when(
    BENE_HI_CVRAGE_TOT_MONS==12 & BENE_SMI_CVRAGE_TOT_MONS==12 & BENE_HMO_CVRAGE_TOT_MONS == 0 & BENE_AGE_AT_END_REF_YR >= 65 ~1,
    TRUE ~ 0
  )) %>%
  rename_with(~paste0(.,"_13"), 2:6)
MBSF14 <- read_sas("~/sdrive/sasusershare/CPS/CPS2/Medicare_March2021/SAS datasets/2014/mbsf_abcd_summary_2014.sas7bdat") %>%
  select(BENE_ID, BENE_HI_CVRAGE_TOT_MONS, BENE_SMI_CVRAGE_TOT_MONS, BENE_HMO_CVRAGE_TOT_MONS, AGE_AT_END_REF_YR) %>% rename(BENE_AGE_AT_END_REF_YR = AGE_AT_END_REF_YR) %>%
  mutate(FullCov = case_when(
    BENE_HI_CVRAGE_TOT_MONS==12 & BENE_SMI_CVRAGE_TOT_MONS==12 & BENE_HMO_CVRAGE_TOT_MONS == 0 & BENE_AGE_AT_END_REF_YR >= 65 ~1,
    TRUE ~ 0
  )) %>%
  rename_with(~paste0(.,"_14"), 2:6)

# Merge the MBSF files on to the CPS and Comorbidity data

All_Wide <- plyr::join_all(list(CPS_Comorb, MBSF99, MBSF00, MBSF01, MBSF02, MBSF03, MBSF04, MBSF05, MBSF06, MBSF07, MBSF08, MBSF09, MBSF10, MBSF11, MBSF12, MBSF13, MBSF14), by="BENE_ID", type="left")

rm(CPS_Comorb, MBSF99, MBSF00, MBSF01, MBSF02, MBSF03, MBSF04, MBSF05, MBSF06, MBSF07, MBSF08, MBSF09, MBSF10, MBSF11, MBSF12, MBSF13, MBSF14)

# Garbage collection
gc(verbose=TRUE) #manual garbage collection just in case

# Now the tough part for the wide-format data set-- making the censorship and start and end points work for PROC TRAJ

# Start Date (create one variable that contains a single year): 
#   Latest of:
#   Cancer Diagnosis
#   Earliest Eligible Medicare Year (65+ age, full coverage)
# 
# End Date (create one variable that contains a single year):
#   Earliest of:
#   Last Survey Returned
#   Year of Death
#   First year after Medicare entrance that they lose complete eligibility?

# Create first year they meet eligibility criteria and a start date
All_Wide <- All_Wide %>%
  mutate(FirstCov = case_when(
    FullCov_99==1 ~ 1999,
    FullCov_00==1 ~ 2000,
    FullCov_01==1 ~ 2001,
    FullCov_02==1 ~ 2002,
    FullCov_03==1 ~ 2003,
    FullCov_04==1 ~ 2004,
    FullCov_05==1 ~ 2005,
    FullCov_06==1 ~ 2006,
    FullCov_07==1 ~ 2007,
    FullCov_08==1 ~ 2008,
    FullCov_09==1 ~ 2009,
    FullCov_10==1 ~ 2010,
    FullCov_11==1 ~ 2011,
    FullCov_12==1 ~ 2012,
    FullCov_13==1 ~ 2013,
    FullCov_14==1 ~ 2014,
    TRUE~ as.numeric(NA)
  )) %>%
  filter(is.na(FirstCov) == FALSE) %>%
  mutate(DiagAfterCovered = ifelse(DODYR>=FirstCov | is.na(DODYR) ,1,0)) %>%
  filter(DiagAfterCovered==1) %>%
  mutate(BENE_Start = pmax(DODYR, FirstCov, na.rm = TRUE))
# There were 15,909 that never had a full year of Medicare coverage and were excluded
# and 1,945 that had their cancer diagnoses before their first full year of MC coverage

# Create the first year where they drop out of eligibility
All_Wide <- All_Wide %>%
  mutate(FirstDrop = case_when(
    FirstCov == 1999 & FullCov_00 == 0 ~ 2000,
    FirstCov == 1999 & FullCov_01 == 0 ~ 2001,
    FirstCov == 1999 & FullCov_02 == 0 ~ 2002,
    FirstCov == 1999 & FullCov_03 == 0 ~ 2003,
    FirstCov == 1999 & FullCov_04 == 0 ~ 2004,
    FirstCov == 1999 & FullCov_05 == 0 ~ 2005,
    FirstCov == 1999 & FullCov_06 == 0 ~ 2006,
    FirstCov == 1999 & FullCov_07 == 0 ~ 2007,
    FirstCov == 1999 & FullCov_08 == 0 ~ 2008,
    FirstCov == 1999 & FullCov_09 == 0 ~ 2009,
    FirstCov == 1999 & FullCov_10 == 0 ~ 2010,
    FirstCov == 1999 & FullCov_11 == 0 ~ 2011,
    FirstCov == 1999 & FullCov_12 == 0 ~ 2012,
    FirstCov == 1999 & FullCov_13 == 0 ~ 2013,
    FirstCov == 1999 & FullCov_14 == 0 ~ 2014,
    
    FirstCov == 2000 & FullCov_01 == 0 ~ 2001,
    FirstCov == 2000 & FullCov_02 == 0 ~ 2002,
    FirstCov == 2000 & FullCov_03 == 0 ~ 2003,
    FirstCov == 2000 & FullCov_04 == 0 ~ 2004,
    FirstCov == 2000 & FullCov_05 == 0 ~ 2005,
    FirstCov == 2000 & FullCov_06 == 0 ~ 2006,
    FirstCov == 2000 & FullCov_07 == 0 ~ 2007,
    FirstCov == 2000 & FullCov_08 == 0 ~ 2008,
    FirstCov == 2000 & FullCov_09 == 0 ~ 2009,
    FirstCov == 2000 & FullCov_10 == 0 ~ 2010,
    FirstCov == 2000 & FullCov_11 == 0 ~ 2011,
    FirstCov == 2000 & FullCov_12 == 0 ~ 2012,
    FirstCov == 2000 & FullCov_13 == 0 ~ 2013,
    FirstCov == 2000 & FullCov_14 == 0 ~ 2014,
    
    FirstCov == 2001 & FullCov_02 == 0 ~ 2002,
    FirstCov == 2001 & FullCov_03 == 0 ~ 2003,
    FirstCov == 2001 & FullCov_04 == 0 ~ 2004,
    FirstCov == 2001 & FullCov_05 == 0 ~ 2005,
    FirstCov == 2001 & FullCov_06 == 0 ~ 2006,
    FirstCov == 2001 & FullCov_07 == 0 ~ 2007,
    FirstCov == 2001 & FullCov_08 == 0 ~ 2008,
    FirstCov == 2001 & FullCov_09 == 0 ~ 2009,
    FirstCov == 2001 & FullCov_10 == 0 ~ 2010,
    FirstCov == 2001 & FullCov_11 == 0 ~ 2011,
    FirstCov == 2001 & FullCov_12 == 0 ~ 2012,
    FirstCov == 2001 & FullCov_13 == 0 ~ 2013,
    FirstCov == 2001 & FullCov_14 == 0 ~ 2014,
    
    FirstCov == 2002 & FullCov_03 == 0 ~ 2003,
    FirstCov == 2002 & FullCov_04 == 0 ~ 2004,
    FirstCov == 2002 & FullCov_05 == 0 ~ 2005,
    FirstCov == 2002 & FullCov_06 == 0 ~ 2006,
    FirstCov == 2002 & FullCov_07 == 0 ~ 2007,
    FirstCov == 2002 & FullCov_08 == 0 ~ 2008,
    FirstCov == 2002 & FullCov_09 == 0 ~ 2009,
    FirstCov == 2002 & FullCov_10 == 0 ~ 2010,
    FirstCov == 2002 & FullCov_11 == 0 ~ 2011,
    FirstCov == 2002 & FullCov_12 == 0 ~ 2012,
    FirstCov == 2002 & FullCov_13 == 0 ~ 2013,
    FirstCov == 2002 & FullCov_14 == 0 ~ 2014,
    
    FirstCov == 2003 & FullCov_04 == 0 ~ 2004,
    FirstCov == 2003 & FullCov_05 == 0 ~ 2005,
    FirstCov == 2003 & FullCov_06 == 0 ~ 2006,
    FirstCov == 2003 & FullCov_07 == 0 ~ 2007,
    FirstCov == 2003 & FullCov_08 == 0 ~ 2008,
    FirstCov == 2003 & FullCov_09 == 0 ~ 2009,
    FirstCov == 2003 & FullCov_10 == 0 ~ 2010,
    FirstCov == 2003 & FullCov_11 == 0 ~ 2011,
    FirstCov == 2003 & FullCov_12 == 0 ~ 2012,
    FirstCov == 2003 & FullCov_13 == 0 ~ 2013,
    FirstCov == 2003 & FullCov_14 == 0 ~ 2014,
    
    FirstCov == 2004 & FullCov_05 == 0 ~ 2005,
    FirstCov == 2004 & FullCov_06 == 0 ~ 2006,
    FirstCov == 2004 & FullCov_07 == 0 ~ 2007,
    FirstCov == 2004 & FullCov_08 == 0 ~ 2008,
    FirstCov == 2004 & FullCov_09 == 0 ~ 2009,
    FirstCov == 2004 & FullCov_10 == 0 ~ 2010,
    FirstCov == 2004 & FullCov_11 == 0 ~ 2011,
    FirstCov == 2004 & FullCov_12 == 0 ~ 2012,
    FirstCov == 2004 & FullCov_13 == 0 ~ 2013,
    FirstCov == 2004 & FullCov_14 == 0 ~ 2014,
    
    FirstCov == 2005 & FullCov_06 == 0 ~ 2006,
    FirstCov == 2005 & FullCov_07 == 0 ~ 2007,
    FirstCov == 2005 & FullCov_08 == 0 ~ 2008,
    FirstCov == 2005 & FullCov_09 == 0 ~ 2009,
    FirstCov == 2005 & FullCov_10 == 0 ~ 2010,
    FirstCov == 2005 & FullCov_11 == 0 ~ 2011,
    FirstCov == 2005 & FullCov_12 == 0 ~ 2012,
    FirstCov == 2005 & FullCov_13 == 0 ~ 2013,
    FirstCov == 2005 & FullCov_14 == 0 ~ 2014,
    
    FirstCov == 2006 & FullCov_07 == 0 ~ 2007,
    FirstCov == 2006 & FullCov_08 == 0 ~ 2008,
    FirstCov == 2006 & FullCov_09 == 0 ~ 2009,
    FirstCov == 2006 & FullCov_10 == 0 ~ 2010,
    FirstCov == 2006 & FullCov_11 == 0 ~ 2011,
    FirstCov == 2006 & FullCov_12 == 0 ~ 2012,
    FirstCov == 2006 & FullCov_13 == 0 ~ 2013,
    FirstCov == 2006 & FullCov_14 == 0 ~ 2014,
    
    FirstCov == 2007 & FullCov_08 == 0 ~ 2008,
    FirstCov == 2007 & FullCov_09 == 0 ~ 2009,
    FirstCov == 2007 & FullCov_10 == 0 ~ 2010,
    FirstCov == 2007 & FullCov_11 == 0 ~ 2011,
    FirstCov == 2007 & FullCov_12 == 0 ~ 2012,
    FirstCov == 2007 & FullCov_13 == 0 ~ 2013,
    FirstCov == 2007 & FullCov_14 == 0 ~ 2014,
    
    FirstCov == 2008 & FullCov_09 == 0 ~ 2009,
    FirstCov == 2008 & FullCov_10 == 0 ~ 2010,
    FirstCov == 2008 & FullCov_11 == 0 ~ 2011,
    FirstCov == 2008 & FullCov_12 == 0 ~ 2012,
    FirstCov == 2008 & FullCov_13 == 0 ~ 2013,
    FirstCov == 2008 & FullCov_14 == 0 ~ 2014,
    
    FirstCov == 2009 & FullCov_10 == 0 ~ 2010,
    FirstCov == 2009 & FullCov_11 == 0 ~ 2011,
    FirstCov == 2009 & FullCov_12 == 0 ~ 2012,
    FirstCov == 2009 & FullCov_13 == 0 ~ 2013,
    FirstCov == 2009 & FullCov_14 == 0 ~ 2014,
    
    FirstCov == 2010 & FullCov_11 == 0 ~ 2011,
    FirstCov == 2010 & FullCov_12 == 0 ~ 2012,
    FirstCov == 2010 & FullCov_13 == 0 ~ 2013,
    FirstCov == 2010 & FullCov_14 == 0 ~ 2014,
    
    FirstCov == 2011 & FullCov_12 == 0 ~ 2012,
    FirstCov == 2011 & FullCov_13 == 0 ~ 2013,
    FirstCov == 2011 & FullCov_14 == 0 ~ 2014,
    
    FirstCov == 2012 & FullCov_13 == 0 ~ 2013,
    FirstCov == 2012 & FullCov_14 == 0 ~ 2014,
    
    FirstCov == 2013 & FullCov_14 == 0 ~ 2014,
    
    TRUE ~ 2015
  )) %>%
  mutate(BENE_End = pmin(FirstDrop, DEATH_YR, lastSurv, 2014, na.rm=TRUE))

######################################################################################
# Now make the variables we would need for 

table(All_Wide$BENE_Start, All_Wide$BENE_End, useNA="ifany")

All_Wide_NonExcluded <- All_Wide %>%
  filter((BENE_End-BENE_Start)>=3) # Make sure each of the possibilities lines up, or do we need to lag any of them +1 or -1?

rm(All_Wide)

# Create fixed time variables. Since it's not survey-based we just set a simple integer.
# We are essentially setting time as "years on study"
All_Wide_NonExcluded <- All_Wide_NonExcluded %>%
  mutate(
    time0=0, 
    time1=1,
    time2=2, 
    time3=3,
    time4=4, 
    time5=5,
    time6=6, 
    time7=7,
    time8=8, 
    time9=9,
    time10=10, 
    time11=11,
    time12=12, 
    time13=13,
    time14=14,
    time15=15
  )

#Now make our outcome missing if its before startdate
All_Wide_NonExcluded <- All_Wide_NonExcluded %>%
  mutate(
    NCI_New99 = case_when(
    BENE_Start>1999 ~ as.numeric(NA),
    TRUE ~ NCI_New99),
    NCI_New00 = case_when(
      BENE_Start>2000 ~ as.numeric(NA),
      TRUE ~ NCI_New00),
    NCI_New01 = case_when(
      BENE_Start>2001 ~ as.numeric(NA),
      TRUE ~ NCI_New01),
    NCI_New02 = case_when(
      BENE_Start>2002 ~ as.numeric(NA),
      TRUE ~ NCI_New02),
    NCI_New03 = case_when(
      BENE_Start>2003 ~ as.numeric(NA),
      TRUE ~ NCI_New03),
    NCI_New04 = case_when(
      BENE_Start>2004 ~ as.numeric(NA),
      TRUE ~ NCI_New04),
    NCI_New05 = case_when(
      BENE_Start>2005 ~ as.numeric(NA),
      TRUE ~ NCI_New05),
    NCI_New06 = case_when(
      BENE_Start>2006 ~ as.numeric(NA),
      TRUE ~ NCI_New06),
    NCI_New07 = case_when(
      BENE_Start>2007 ~ as.numeric(NA),
      TRUE ~ NCI_New07),
    NCI_New08 = case_when(
      BENE_Start>2008 ~ as.numeric(NA),
      TRUE ~ NCI_New08),
    NCI_New09 = case_when(
      BENE_Start>2009 ~ as.numeric(NA),
      TRUE ~ NCI_New09),
    NCI_New10 = case_when(
      BENE_Start>2010 ~ as.numeric(NA),
      TRUE ~ NCI_New10),
    NCI_New11 = case_when(
      BENE_Start>2011 ~ as.numeric(NA),
      TRUE ~ NCI_New11),
    NCI_New12 = case_when(
      BENE_Start>2012 ~ as.numeric(NA),
      TRUE ~ NCI_New12),
    NCI_New13 = case_when(
      BENE_Start>2013 ~ as.numeric(NA),
      TRUE ~ NCI_New13),
    NCI_New14 = case_when(
      BENE_Start>2014 ~ as.numeric(NA),
      TRUE ~ NCI_New14)
    )

#Now make our outcome missing if its after enddate
All_Wide_NonExcluded <- All_Wide_NonExcluded %>%
  mutate(
    NCI_New99 = case_when(
      BENE_End < 1999 ~ as.numeric(NA),
      TRUE ~ NCI_New99),
    NCI_New00 = case_when(
      BENE_End < 2000 ~ as.numeric(NA),
      TRUE ~ NCI_New00),
    NCI_New01 = case_when(
      BENE_End < 2001 ~ as.numeric(NA),
      TRUE ~ NCI_New01),
    NCI_New02 = case_when(
      BENE_End < 2002 ~ as.numeric(NA),
      TRUE ~ NCI_New02),
    NCI_New03 = case_when(
      BENE_End < 2003 ~ as.numeric(NA),
      TRUE ~ NCI_New03),
    NCI_New04 = case_when(
      BENE_End < 2004 ~ as.numeric(NA),
      TRUE ~ NCI_New04),
    NCI_New05 = case_when(
      BENE_End < 2005 ~ as.numeric(NA),
      TRUE ~ NCI_New05),
    NCI_New06 = case_when(
      BENE_End < 2006 ~ as.numeric(NA),
      TRUE ~ NCI_New06),
    NCI_New07 = case_when(
      BENE_End < 2007 ~ as.numeric(NA),
      TRUE ~ NCI_New07),
    NCI_New08 = case_when(
      BENE_End < 2008 ~ as.numeric(NA),
      TRUE ~ NCI_New08),
    NCI_New09 = case_when(
      BENE_End < 2009 ~ as.numeric(NA),
      TRUE ~ NCI_New09),
    NCI_New10 = case_when(
      BENE_End < 2010 ~ as.numeric(NA),
      TRUE ~ NCI_New10),
    NCI_New11 = case_when(
      BENE_End < 2011 ~ as.numeric(NA),
      TRUE ~ NCI_New11),
    NCI_New12 = case_when(
      BENE_End < 2012 ~ as.numeric(NA),
      TRUE ~ NCI_New12),
    NCI_New13 = case_when(
      BENE_End < 2013 ~ as.numeric(NA),
      TRUE ~ NCI_New13),
    NCI_New14 = case_when(
      BENE_End < 2014 ~ as.numeric(NA),
      TRUE ~ NCI_New14)
  )

# Now we need to shift scores to a "time zero" based on their start date. 
# Their end date should be 2014 or missing already, so that shouldnt need to be considered

All_Wide_NonExcluded <- All_Wide_NonExcluded %>%
  mutate(
    val0 = case_when(
    BENE_Start == 1999 ~ NCI_New99,
    BENE_Start == 2000 ~ NCI_New00,
    BENE_Start == 2001 ~ NCI_New01,
    BENE_Start == 2002 ~ NCI_New02,
    BENE_Start == 2003 ~ NCI_New03,
    BENE_Start == 2004 ~ NCI_New04,
    BENE_Start == 2005 ~ NCI_New05,
    BENE_Start == 2006 ~ NCI_New06,
    BENE_Start == 2007 ~ NCI_New07,
    BENE_Start == 2008 ~ NCI_New08,
    BENE_Start == 2009 ~ NCI_New09,
    BENE_Start == 2010 ~ NCI_New10,
    BENE_Start == 2011 ~ NCI_New11,
    BENE_Start == 2012 ~ as.numeric(NA),
    BENE_Start == 2013 ~ as.numeric(NA),
    BENE_Start == 2014 ~ as.numeric(NA)
),
    val1 = case_when(
    BENE_Start == 1999 ~ NCI_New00,
    BENE_Start == 2000 ~ NCI_New01,
    BENE_Start == 2001 ~ NCI_New02,
    BENE_Start == 2002 ~ NCI_New03,
    BENE_Start == 2003 ~ NCI_New04,
    BENE_Start == 2004 ~ NCI_New05,
    BENE_Start == 2005 ~ NCI_New06,
    BENE_Start == 2006 ~ NCI_New07,
    BENE_Start == 2007 ~ NCI_New08,
    BENE_Start == 2008 ~ NCI_New09,
    BENE_Start == 2009 ~ NCI_New10,
    BENE_Start == 2010 ~ NCI_New11,
    BENE_Start == 2011 ~ NCI_New12,
    BENE_Start == 2012 ~ as.numeric(NA),
    BENE_Start == 2013 ~ as.numeric(NA),
    BENE_Start == 2014 ~ as.numeric(NA)
),
val2 = case_when(
  BENE_Start == 1999 ~ NCI_New01,
  BENE_Start == 2000 ~ NCI_New02,
  BENE_Start == 2001 ~ NCI_New03,
  BENE_Start == 2002 ~ NCI_New04,
  BENE_Start == 2003 ~ NCI_New05,
  BENE_Start == 2004 ~ NCI_New06,
  BENE_Start == 2005 ~ NCI_New07,
  BENE_Start == 2006 ~ NCI_New08,
  BENE_Start == 2007 ~ NCI_New09,
  BENE_Start == 2008 ~ NCI_New10,
  BENE_Start == 2009 ~ NCI_New11,
  BENE_Start == 2010 ~ NCI_New12,
  BENE_Start == 2011 ~ NCI_New13,
  BENE_Start == 2012 ~ as.numeric(NA),
  BENE_Start == 2013 ~ as.numeric(NA),
  BENE_Start == 2014 ~ as.numeric(NA)
),
val3 = case_when(
  BENE_Start == 1999 ~ NCI_New02,
  BENE_Start == 2000 ~ NCI_New03,
  BENE_Start == 2001 ~ NCI_New04,
  BENE_Start == 2002 ~ NCI_New05,
  BENE_Start == 2003 ~ NCI_New06,
  BENE_Start == 2004 ~ NCI_New07,
  BENE_Start == 2005 ~ NCI_New08,
  BENE_Start == 2006 ~ NCI_New09,
  BENE_Start == 2007 ~ NCI_New10,
  BENE_Start == 2008 ~ NCI_New11,
  BENE_Start == 2009 ~ NCI_New12,
  BENE_Start == 2010 ~ NCI_New13,
  BENE_Start == 2011 ~ NCI_New14,
  BENE_Start == 2012 ~ as.numeric(NA),
  BENE_Start == 2013 ~ as.numeric(NA),
  BENE_Start == 2014 ~ as.numeric(NA)
),
val4 = case_when(
  BENE_Start == 1999 ~ NCI_New03,
  BENE_Start == 2000 ~ NCI_New04,
  BENE_Start == 2001 ~ NCI_New05,
  BENE_Start == 2002 ~ NCI_New06,
  BENE_Start == 2003 ~ NCI_New07,
  BENE_Start == 2004 ~ NCI_New08,
  BENE_Start == 2005 ~ NCI_New09,
  BENE_Start == 2006 ~ NCI_New10,
  BENE_Start == 2007 ~ NCI_New11,
  BENE_Start == 2008 ~ NCI_New12,
  BENE_Start == 2009 ~ NCI_New13,
  BENE_Start == 2010 ~ NCI_New14,
  BENE_Start == 2011 ~ as.numeric(NA),
  BENE_Start == 2012 ~ as.numeric(NA),
  BENE_Start == 2013 ~ as.numeric(NA),
  BENE_Start == 2014 ~ as.numeric(NA)
),
val5 = case_when(
  BENE_Start == 1999 ~ NCI_New04,
  BENE_Start == 2000 ~ NCI_New05,
  BENE_Start == 2001 ~ NCI_New06,
  BENE_Start == 2002 ~ NCI_New07,
  BENE_Start == 2003 ~ NCI_New08,
  BENE_Start == 2004 ~ NCI_New09,
  BENE_Start == 2005 ~ NCI_New10,
  BENE_Start == 2006 ~ NCI_New11,
  BENE_Start == 2007 ~ NCI_New12,
  BENE_Start == 2008 ~ NCI_New13,
  BENE_Start == 2009 ~ NCI_New14,
  BENE_Start == 2010 ~ as.numeric(NA),
  BENE_Start == 2011 ~ as.numeric(NA),
  BENE_Start == 2012 ~ as.numeric(NA),
  BENE_Start == 2013 ~ as.numeric(NA),
  BENE_Start == 2014 ~ as.numeric(NA)
),
val6 = case_when(
  BENE_Start == 1999 ~ NCI_New05,
  BENE_Start == 2000 ~ NCI_New06,
  BENE_Start == 2001 ~ NCI_New07,
  BENE_Start == 2002 ~ NCI_New08,
  BENE_Start == 2003 ~ NCI_New09,
  BENE_Start == 2004 ~ NCI_New10,
  BENE_Start == 2005 ~ NCI_New11,
  BENE_Start == 2006 ~ NCI_New12,
  BENE_Start == 2007 ~ NCI_New13,
  BENE_Start == 2008 ~ NCI_New14,
  BENE_Start == 2009 ~ as.numeric(NA),
  BENE_Start == 2010 ~ as.numeric(NA),
  BENE_Start == 2011 ~ as.numeric(NA),
  BENE_Start == 2012 ~ as.numeric(NA),
  BENE_Start == 2013 ~ as.numeric(NA),
  BENE_Start == 2014 ~ as.numeric(NA)
),
val7 = case_when(
  BENE_Start == 1999 ~ NCI_New06,
  BENE_Start == 2000 ~ NCI_New07,
  BENE_Start == 2001 ~ NCI_New08,
  BENE_Start == 2002 ~ NCI_New09,
  BENE_Start == 2003 ~ NCI_New10,
  BENE_Start == 2004 ~ NCI_New11,
  BENE_Start == 2005 ~ NCI_New12,
  BENE_Start == 2006 ~ NCI_New13,
  BENE_Start == 2007 ~ NCI_New14,
  BENE_Start == 2008 ~ as.numeric(NA),
  BENE_Start == 2009 ~ as.numeric(NA),
  BENE_Start == 2010 ~ as.numeric(NA),
  BENE_Start == 2011 ~ as.numeric(NA),
  BENE_Start == 2012 ~ as.numeric(NA),
  BENE_Start == 2013 ~ as.numeric(NA),
  BENE_Start == 2014 ~ as.numeric(NA)
),
val8 = case_when(
  BENE_Start == 1999 ~ NCI_New07,
  BENE_Start == 2000 ~ NCI_New08,
  BENE_Start == 2001 ~ NCI_New09,
  BENE_Start == 2002 ~ NCI_New10,
  BENE_Start == 2003 ~ NCI_New11,
  BENE_Start == 2004 ~ NCI_New12,
  BENE_Start == 2005 ~ NCI_New13,
  BENE_Start == 2006 ~ NCI_New14,
  BENE_Start == 2007 ~ as.numeric(NA),
  BENE_Start == 2008 ~ as.numeric(NA),
  BENE_Start == 2009 ~ as.numeric(NA),
  BENE_Start == 2010 ~ as.numeric(NA),
  BENE_Start == 2011 ~ as.numeric(NA),
  BENE_Start == 2012 ~ as.numeric(NA),
  BENE_Start == 2013 ~ as.numeric(NA),
  BENE_Start == 2014 ~ as.numeric(NA)
),
val9 = case_when(
  BENE_Start == 1999 ~ NCI_New08,
  BENE_Start == 2000 ~ NCI_New09,
  BENE_Start == 2001 ~ NCI_New10,
  BENE_Start == 2002 ~ NCI_New11,
  BENE_Start == 2003 ~ NCI_New12,
  BENE_Start == 2004 ~ NCI_New13,
  BENE_Start == 2005 ~ NCI_New14,
  BENE_Start == 2006 ~ as.numeric(NA),
  BENE_Start == 2007 ~ as.numeric(NA),
  BENE_Start == 2008 ~ as.numeric(NA),
  BENE_Start == 2009 ~ as.numeric(NA),
  BENE_Start == 2010 ~ as.numeric(NA),
  BENE_Start == 2011 ~ as.numeric(NA),
  BENE_Start == 2012 ~ as.numeric(NA),
  BENE_Start == 2013 ~ as.numeric(NA),
  BENE_Start == 2014 ~ as.numeric(NA)
),
val10 = case_when(
  BENE_Start == 1999 ~ NCI_New09,
  BENE_Start == 2000 ~ NCI_New10,
  BENE_Start == 2001 ~ NCI_New11,
  BENE_Start == 2002 ~ NCI_New12,
  BENE_Start == 2003 ~ NCI_New13,
  BENE_Start == 2004 ~ NCI_New14,
  BENE_Start == 2005 ~ as.numeric(NA),
  BENE_Start == 2006 ~ as.numeric(NA),
  BENE_Start == 2007 ~ as.numeric(NA),
  BENE_Start == 2008 ~ as.numeric(NA),
  BENE_Start == 2009 ~ as.numeric(NA),
  BENE_Start == 2010 ~ as.numeric(NA),
  BENE_Start == 2011 ~ as.numeric(NA),
  BENE_Start == 2012 ~ as.numeric(NA),
  BENE_Start == 2013 ~ as.numeric(NA),
  BENE_Start == 2014 ~ as.numeric(NA)
),
val11 = case_when(
  BENE_Start == 1999 ~ NCI_New10,
  BENE_Start == 2000 ~ NCI_New11,
  BENE_Start == 2001 ~ NCI_New12,
  BENE_Start == 2002 ~ NCI_New13,
  BENE_Start == 2003 ~ NCI_New14,
  BENE_Start == 2004 ~ as.numeric(NA),
  BENE_Start == 2005 ~ as.numeric(NA),
  BENE_Start == 2006 ~ as.numeric(NA),
  BENE_Start == 2007 ~ as.numeric(NA),
  BENE_Start == 2008 ~ as.numeric(NA),
  BENE_Start == 2009 ~ as.numeric(NA),
  BENE_Start == 2010 ~ as.numeric(NA),
  BENE_Start == 2011 ~ as.numeric(NA),
  BENE_Start == 2012 ~ as.numeric(NA),
  BENE_Start == 2013 ~ as.numeric(NA),
  BENE_Start == 2014 ~ as.numeric(NA)
),
val12 = case_when(
  BENE_Start == 1999 ~ NCI_New11,
  BENE_Start == 2000 ~ NCI_New12,
  BENE_Start == 2001 ~ NCI_New13,
  BENE_Start == 2002 ~ NCI_New14,
  BENE_Start == 2003 ~ as.numeric(NA),
  BENE_Start == 2004 ~ as.numeric(NA),
  BENE_Start == 2005 ~ as.numeric(NA),
  BENE_Start == 2006 ~ as.numeric(NA),
  BENE_Start == 2007 ~ as.numeric(NA),
  BENE_Start == 2008 ~ as.numeric(NA),
  BENE_Start == 2009 ~ as.numeric(NA),
  BENE_Start == 2010 ~ as.numeric(NA),
  BENE_Start == 2011 ~ as.numeric(NA),
  BENE_Start == 2012 ~ as.numeric(NA),
  BENE_Start == 2013 ~ as.numeric(NA),
  BENE_Start == 2014 ~ as.numeric(NA)
),
val13 = case_when(
  BENE_Start == 1999 ~ NCI_New12,
  BENE_Start == 2000 ~ NCI_New13,
  BENE_Start == 2001 ~ NCI_New14,
  BENE_Start == 2002 ~ as.numeric(NA),
  BENE_Start == 2003 ~ as.numeric(NA),
  BENE_Start == 2004 ~ as.numeric(NA),
  BENE_Start == 2005 ~ as.numeric(NA),
  BENE_Start == 2006 ~ as.numeric(NA),
  BENE_Start == 2007 ~ as.numeric(NA),
  BENE_Start == 2008 ~ as.numeric(NA),
  BENE_Start == 2009 ~ as.numeric(NA),
  BENE_Start == 2010 ~ as.numeric(NA),
  BENE_Start == 2011 ~ as.numeric(NA),
  BENE_Start == 2012 ~ as.numeric(NA),
  BENE_Start == 2013 ~ as.numeric(NA),
  BENE_Start == 2014 ~ as.numeric(NA)
),
val14 = case_when(
  BENE_Start == 1999 ~ NCI_New13,
  BENE_Start == 2000 ~ NCI_New14,
  BENE_Start == 2001 ~ as.numeric(NA),
  BENE_Start == 2002 ~ as.numeric(NA),
  BENE_Start == 2003 ~ as.numeric(NA),
  BENE_Start == 2004 ~ as.numeric(NA),
  BENE_Start == 2005 ~ as.numeric(NA),
  BENE_Start == 2006 ~ as.numeric(NA),
  BENE_Start == 2007 ~ as.numeric(NA),
  BENE_Start == 2008 ~ as.numeric(NA),
  BENE_Start == 2009 ~ as.numeric(NA),
  BENE_Start == 2010 ~ as.numeric(NA),
  BENE_Start == 2011 ~ as.numeric(NA),
  BENE_Start == 2012 ~ as.numeric(NA),
  BENE_Start == 2013 ~ as.numeric(NA),
  BENE_Start == 2014 ~ as.numeric(NA)
),
val15 = case_when(
  BENE_Start == 1999 ~ NCI_New14,
  BENE_Start == 2000 ~ as.numeric(NA),
  BENE_Start == 2001 ~ as.numeric(NA),
  BENE_Start == 2002 ~ as.numeric(NA),
  BENE_Start == 2003 ~ as.numeric(NA),
  BENE_Start == 2004 ~ as.numeric(NA),
  BENE_Start == 2005 ~ as.numeric(NA),
  BENE_Start == 2006 ~ as.numeric(NA),
  BENE_Start == 2007 ~ as.numeric(NA),
  BENE_Start == 2008 ~ as.numeric(NA),
  BENE_Start == 2009 ~ as.numeric(NA),
  BENE_Start == 2010 ~ as.numeric(NA),
  BENE_Start == 2011 ~ as.numeric(NA),
  BENE_Start == 2012 ~ as.numeric(NA),
  BENE_Start == 2013 ~ as.numeric(NA),
  BENE_Start == 2014 ~ as.numeric(NA)
)
)

# Create an age at start of study variable

All_Wide_NonExcluded <- All_Wide_NonExcluded %>%
  mutate(AgeSurvStart = case_when(
    BENE_Start == 1999 ~ BENE_AGE_AT_END_REF_YR_99,
    BENE_Start == 2000 ~ BENE_AGE_AT_END_REF_YR_00,
    BENE_Start == 2001 ~ BENE_AGE_AT_END_REF_YR_01,
    BENE_Start == 2002 ~ BENE_AGE_AT_END_REF_YR_02,
    BENE_Start == 2003 ~ BENE_AGE_AT_END_REF_YR_03,
    BENE_Start == 2004 ~ BENE_AGE_AT_END_REF_YR_04,
    BENE_Start == 2005 ~ BENE_AGE_AT_END_REF_YR_05,
    BENE_Start == 2006 ~ BENE_AGE_AT_END_REF_YR_06,
    BENE_Start == 2007 ~ BENE_AGE_AT_END_REF_YR_07,
    BENE_Start == 2008 ~ BENE_AGE_AT_END_REF_YR_08,
    BENE_Start == 2009 ~ BENE_AGE_AT_END_REF_YR_09,
    BENE_Start == 2010 ~ BENE_AGE_AT_END_REF_YR_10,
    BENE_Start == 2011 ~ BENE_AGE_AT_END_REF_YR_11,
    BENE_Start == 2012 ~ BENE_AGE_AT_END_REF_YR_12,
    BENE_Start == 2013 ~ BENE_AGE_AT_END_REF_YR_13,
    BENE_Start == 2014 ~ BENE_AGE_AT_END_REF_YR_14
  ))

# Make CA survivorship the Case/Control variable
All_Wide_NonExcluded <- All_Wide_NonExcluded %>%
  mutate(Case_Control = ifelse(CASurvivor==1,"Case", "Control"))

# Make Race White/Other
All_Wide_NonExcluded<- All_Wide_NonExcluded %>%
  mutate(RACENEW = case_when(
    RACE==1 ~ 0,
    TRUE ~ 1
  )) %>%
  mutate(RACENEW= factor(RACENEW, levels=c(0,1), labels=c("White", "Other/Missing")))

# Make Education College Degree-based
All_Wide_NonExcluded<- All_Wide_NonExcluded %>%
  mutate(EDUCATENEW = case_when(
    EDUCATE %in% c(6,7) ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(EDUCATENEW= factor(EDUCATENEW, levels=c(0,1), labels=c("<College Grad", "College Grad")))

# Make Gender a factor
All_Wide_NonExcluded<- All_Wide_NonExcluded %>%
  mutate(GENDER = factor(GENDER, levels=c(1,2), labels=c("Male", "Female")))


# Matching
library(MatchIt)

# gender race education and BENE_Start are exact, AgeSurvStart is within 5
matchmodel <- matchit(CASurvivor ~ GENDER + RACENEW + EDUCATENEW + BENE_Start + AgeSurvStart, data=All_Wide_NonExcluded, exact= ~ GENDER + RACENEW + EDUCATENEW + BENE_Start, caliper = c(AgeSurvStart=5), std.caliper = FALSE)
summary(matchmodel, un=FALSE)
MatchedData <- match.data(matchmodel) 
MatchedData %>% select(GENDER, RACENEW, EDUCATENEW, BENE_Start, AgeSurvStart, subclass) %>% arrange(subclass) %>% head(., n=20)
MatchedData %>% select(BENE_ID, subclass, GENDER, RACENEW, EDUCATENEW, BENE_Start, AgeSurvStart, INCCANV, starts_with("time"), starts_with("val")) %>%
  write.csv(., file="~/sdrive/sasusershare/USER/MMASTERS/Medicare/Corinne-Frailty/Data/TRAJTest.csv", na=".")

# Test<- All_Wide_NonExcluded %>% select(BENE_ID, Case_Control, BENE_Start, BENE_End, starts_with("val")) %>% tidyr::pivot_longer(cols=starts_with("val"), names_to="Observation", values_to="Score") %>%
#   mutate(Observation = case_when(
#     Observation == "val0" ~ 0,
#     Observation == "val1" ~ 1,
#     Observation == "val2" ~ 2,
#     Observation == "val3" ~ 3,
#     Observation == "val4" ~ 4,
#     Observation == "val5" ~ 5,
#     Observation == "val6" ~ 6,
#     Observation == "val7" ~ 7,
#     Observation == "val8" ~ 8,
#     Observation == "val9" ~ 9,
#     Observation == "val10" ~ 10,
#     Observation == "val11" ~ 11,
#     Observation == "val12" ~ 12,
#     Observation == "val13" ~ 13,
#     Observation == "val14" ~ 14,
#     Observation == "val15" ~ 15,
#     Observation == "val16" ~ 16
#   )) %>% filter(BENE_ID %in% sample(BENE_ID,16))
# 
# ggplot(Test,aes(x = Observation, y = Score)) +
#   geom_line(aes(color=Case_Control)+
#   facet_wrap(facets =vars(BENE_ID), nrow=4, ncol=4)
