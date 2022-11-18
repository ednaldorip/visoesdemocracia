###Conversion of databases###
library(haven)
WVSBR2006 <- read_sav("WVSBR2006.sav")
WVSBR2014 <- read_sav("WVSBR2014.sav")
WVSBR2018 <- read_sav("WVSBR2018.sav")

###Removal of labels###
library(sjlabelled)
WVSBR2006 <- remove_all_labels(WVSBR2006)
WVSBR2014 <- remove_all_labels(WVSBR2014)
WVSBR2018 <- remove_all_labels(WVSBR2018)

### Deoendent Variables###
##2006##
# Social View
WVSBR2006$Soc_Dem1 <- WVSBR2006$V152
WVSBR2006$Soc_Dem2 <- WVSBR2006$V155
WVSBR2006$Soc_Dem <- WVSBR2006$Soc_Dem1+WVSBR2006$Soc_Dem2
WVSBR2006$Soc_Dem01 <- 
  WVSBR2006$Soc_Dem/max(WVSBR2006$Soc_Dem, na.rm = TRUE) 
WVSBR2006$Soc_Dem10 <- WVSBR2006$Soc_Dem01*10
# Liberal View
WVSBR2006$Lib_Dem1 <- WVSBR2006$V154
WVSBR2006$Lib_Dem2 <- WVSBR2006$V157
WVSBR2006$Lib_Dem3 <- WVSBR2006$V161
WVSBR2006$Lib_Dem <- 
  WVSBR2006$Lib_Dem1+WVSBR2006$Lib_Dem2+WVSBR2006$Lib_Dem3
WVSBR2006$Lib_Dem01 <- 
  WVSBR2006$Lib_Dem/max(WVSBR2006$Lib_Dem, na.rm = TRUE) 
WVSBR2006$Lib_Dem10 <- WVSBR2006$Lib_Dem01*10
# Participacionist View
library(memisc)
WVSBR2006$Part_Dem1 <- recode(WVSBR2006$V69, 2 <- 3, 0 <- c(1,2,4))
WVSBR2006$Part_Dem2 <- recode(WVSBR2006$V70, 1 <- 3, 0 <- c(1,2,4))
WVSBR2006$Part_Dem3 <- recode(WVSBR2006$V71, 2 <- 2, 0 <- c(1,3,4))
WVSBR2006$Part_Dem4 <- recode(WVSBR2006$V72, 1 <- 2, 0 <- c(1,3,4))
WVSBR2006$Part_Dem <- 
  WVSBR2006$Part_Dem1+WVSBR2006$Part_Dem2+WVSBR2006$Part_Dem3+WVSBR2006$Part_Dem4
WVSBR2006$Part_Dem01 <- 
  WVSBR2006$Part_Dem/max(WVSBR2006$Part_Dem, na.rm = TRUE) 
WVSBR2006$Part_Dem10 <- WVSBR2006$Part_Dem01*10
# Iliberal View
WVSBR2006$Ilib_Dem10 <- WVSBR2006$V156
# Firm Liberal View 
WVSBR2006<- WVSBR2006%>% 
  mutate(LibConvB = case_when((Lib_Dem10 >= 8 & Ilib_Dem10 <= 2) ~ 1,
                              (Lib_Dem10 <= 8 & Ilib_Dem10 >= 2) ~ 0,
                              TRUE ~ 0))

##2014
# Social View
WVSBR2014$Soc_Dem1 <- WVSBR2014$V131
WVSBR2014$Soc_Dem2 <- WVSBR2014$V134
WVSBR2014$Soc_Dem <- WVSBR2014$Soc_Dem1+WVSBR2014$Soc_Dem2
WVSBR2014$Soc_Dem01 <- 
  WVSBR2014$Soc_Dem/max(WVSBR2014$Soc_Dem, na.rm = TRUE) 
WVSBR2014$Soc_Dem10 <- WVSBR2014$Soc_Dem01*10
#Liberal View
WVSBR2014$Lib_Dem1 <- WVSBR2014$V133
WVSBR2014$Lib_Dem2 <- WVSBR2014$V136
WVSBR2014$Lib_Dem3 <- WVSBR2014$V139
WVSBR2014$Lib_Dem <- 
  WVSBR2014$Lib_Dem1+WVSBR2014$Lib_Dem2+WVSBR2014$Lib_Dem3
WVSBR2014$Lib_Dem01 <- 
  WVSBR2014$Lib_Dem/max(WVSBR2014$Lib_Dem, na.rm = TRUE) 
WVSBR2014$Lib_Dem10 <- WVSBR2014$Lib_Dem01*10
#Participacionist View
WVSBR2014$Part_Dem1 <- recode(WVSBR2014$V60, 2 <- 3, 0 <- c(1,2,4))
WVSBR2014$Part_Dem2 <- recode(WVSBR2014$V61, 1 <- 3, 0 <- c(1,2,4))
WVSBR2014$Part_Dem3 <- recode(WVSBR2014$V62, 2 <- 2, 0 <- c(1,3,4))
WVSBR2014$Part_Dem4 <- recode(WVSBR2014$V63, 1 <- 2, 0 <- c(1,3,4))
WVSBR2014$Part_Dem <- 
  WVSBR2014$Part_Dem1+WVSBR2014$Part_Dem2+WVSBR2014$Part_Dem3+WVSBR2014$Part_Dem4
WVSBR2014$Part_Dem01 <- 
  WVSBR2014$Part_Dem/max(WVSBR2014$Part_Dem, na.rm = TRUE) 
WVSBR2014$Part_Dem10 <- WVSBR2014$Part_Dem01*10
#Iliberal View
WVSBR2014$Ilib_Dem10 <- WVSBR2014$V135

#Firm Liberal View
WVSBR2014<- WVSBR2014%>% 
  mutate(LibConvB = case_when((Lib_Dem10 >= 8 & Ilib_Dem10 <= 2) ~ 1,
                              (Lib_Dem10 <= 8 & Ilib_Dem10 >= 2) ~ 0,
                              TRUE ~ 0))

##2018
# Social View
WVSBR2018$Soc_Dem1 <- WVSBR2018$Q241
WVSBR2018$Soc_Dem2 <- WVSBR2018$Q244
WVSBR2018$Soc_Dem <- WVSBR2018$Soc_Dem1+WVSBR2018$Soc_Dem2
WVSBR2018$Soc_Dem01 <- 
  WVSBR2018$Soc_Dem/max(WVSBR2018$Soc_Dem, na.rm = TRUE) 
WVSBR2018$Soc_Dem10 <- WVSBR2018$Soc_Dem01*10
#Liberal View
WVSBR2018$Lib_Dem1 <- WVSBR2018$Q243
WVSBR2018$Lib_Dem2 <- WVSBR2018$Q246
WVSBR2018$Lib_Dem3 <- WVSBR2018$Q249
WVSBR2018$Lib_Dem <- 
  WVSBR2018$Lib_Dem1+WVSBR2018$Lib_Dem2+WVSBR2018$Lib_Dem3
WVSBR2018$Lib_Dem01 <-
  WVSBR2018$Lib_Dem/max(WVSBR2018$Lib_Dem, na.rm = TRUE) 
WVSBR2018$Lib_Dem10 <- WVSBR2018$Lib_Dem01*10
#Participacionist View
WVSBR2018$Part_Dem1 <- recode(WVSBR2018$Q152, 2 <- 3, 0 <- c(1,2,4))
WVSBR2018$Part_Dem2 <- recode(WVSBR2018$Q153, 1 <- 3, 0 <- c(1,2,4))
WVSBR2018$Part_Dem3 <- recode(WVSBR2018$Q154, 2 <- 2, 0 <- c(1,3,4))
WVSBR2018$Part_Dem4 <- recode(WVSBR2018$Q155, 1 <- 2, 0 <- c(1,3,4))
WVSBR2018$Part_Dem <-
  WVSBR2018$Part_Dem1+WVSBR2018$Part_Dem2+WVSBR2018$Part_Dem3+WVSBR2018$Part_Dem4
WVSBR2018$Part_Dem01 <- 
  WVSBR2018$Part_Dem/max(WVSBR2018$Part_Dem, na.rm = TRUE) 
WVSBR2018$Part_Dem10 <- WVSBR2018$Part_Dem01*10
#Iliberal View
WVSBR2018$Ilib_Dem10 <- WVSBR2018$Q245
#Firm Liberal View
WVSBR2018<- WVSBR2018%>% 
  mutate(LibConvB = case_when((Lib_Dem10 >= 8 & Ilib_Dem10 <= 2) ~ 1,
                              (Lib_Dem10 <= 8 & Ilib_Dem10 >= 2) ~ 0,
                              TRUE ~ 0))

###Independent Variables
#Age
WVSBR2006$FxId <- cut(WVSBR2006$V237, c(0, 20, 40, 60, 100))
WVSBR2014$FxId <- cut(WVSBR2014$V242, c(0, 20, 40, 60, 100))
WVSBR2018$FxId <- cut(WVSBR2018$Q262, c(0, 20, 40, 60, 100))
#Education
WVSBR2006$EdSup <- memisc::recode(WVSBR2006$V238, 1 <- 9, 0 <- c(1:8))
WVSBR2014$EdSup <- memisc::recode(WVSBR2014$V248, 1 <- 9, 0 <- c(1:8))
WVSBR2018$EdSup <- memisc::recode(WVSBR2018$Q275, 1 <- c(6:8), 0 <- c(0:5))
#Gender
WVSBR2006$Sexo <- memisc::recode(WVSBR2006$V235, 1 <- 1, 0 <- 2)
WVSBR2014$Sexo <- memisc::recode(WVSBR2014$V240, 1 <- 2, 0 <- 1)
WVSBR2018$Sexo <- memisc::recode(WVSBR2018$Q260, 1 <- 1, 0 <- 2)
#Unemployed
WVSBR2006$Desemp <- memisc::recode(WVSBR2006$V241, 1 <- 7, 0 <- c(1:6, 8))
WVSBR2014$Desemp <- memisc::recode(WVSBR2014$V229, 1 <- 2, 0 <- c(1, 3:8))
WVSBR2018$Desemp <- memisc::recode(WVSBR2018$Q279, 1 <- 7, 0 <- c(1:6, 8))
#Subjective Social Status
WVSBR2006$ClasSub <- memisc::recode(WVSBR2006$V252, 0 <- 5, 1 <- 4, 2 <- 3, 
                                    3 <- 2, 4 <- 1)
WVSBR2014$ClasSub <- memisc::recode(WVSBR2014$V238, 0 <- 5, 1 <- 4, 2 <- 3, 
                                    3 <- 2, 4 <- 1)
WVSBR2018$ClasSub <- memisc::recode(WVSBR2018$Q287, 0 <- 5, 1 <- 4, 2 <- 3, 
                                    3 <- 2, 4 <- 1)
#Interpersonal Confidence
WVSBR2006$ConfInt <- memisc::recode(WVSBR2006$V126, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
WVSBR2014$ConfInt <- memisc::recode(WVSBR2014$V103, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
WVSBR2018$ConfInt <- memisc::recode(WVSBR2018$Q59, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
#Political Confidence (Federal Gov., Parties e Congr.)
WVSBR2006$ConfPol_Gov <- memisc::recode(WVSBR2006$V138,0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)   
WVSBR2006$ConfPol_Part <- memisc::recode(WVSBR2006$V139,0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)   
WVSBR2006$ConfPol_Cong <- memisc::recode(WVSBR2006$V140,0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)   
WVSBR2006$ConfPol <- 
  WVSBR2006$ConfPol_Gov+WVSBR2006$ConfPol_Part+WVSBR2006$ConfPol_Cong
WVSBR2014$ConfPol_Gov <- memisc::recode(WVSBR2014$V115,0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)   
WVSBR2014$ConfPol_Part <- memisc::recode(WVSBR2014$V116,0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)   
WVSBR2014$ConfPol_Cong <- memisc::recode(WVSBR2014$V117,0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)   
WVSBR2014$ConfPol <- 
  WVSBR2014$ConfPol_Gov+WVSBR2014$ConfPol_Part+WVSBR2014$ConfPol_Cong
WVSBR2018$ConfPol_Gov <- memisc::recode(WVSBR2018$Q71,0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)   
WVSBR2018$ConfPol_Part <- memisc::recode(WVSBR2018$Q72,0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)   
WVSBR2018$ConfPol_Cong <- memisc::recode(WVSBR2018$Q73, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)   
WVSBR2018$ConfPol <- 
  WVSBR2018$ConfPol_Gov+WVSBR2018$ConfPol_Part+WVSBR2018$ConfPol_Cong
#Ideology
WVSBR2006$Ideol <- WVSBR2006$V114
WVSBR2014$Ideol <- WVSBR2014$V95
WVSBR2018$Ideol <- WVSBR2018$Q240

#Ideology Extremism (0=moderate, 1=left-wing extremism and 2=right-wing extremism)
WVSBR2006$ExtrIdeol2 <- recode(WVSBR2006$V114, 0 <- c(3:8), 1 <- c(1,2), 2 <- c(9,10))
WVSBR2006$ExtrIdeol2 <- factor(WVSBR2006$ExtrIdeol2, 
                               labels = c("moderado", "extremista de esquerda", 
                                          "extremista de direita"))
WVSBR2014$ExtrIdeol2 <- recode(WVSBR2014$V95, 0 <- c(3:8), 1 <- c(1,2), 2 <- c(9,10))
WVSBR2014$ExtrIdeol2 <- factor(WVSBR2014$ExtrIdeol2, 
                               labels = c("moderado", "extremista de esquerda", 
                                          "extremista de direita"))
WVSBR2018$ExtrIdeol2 <- recode(WVSBR2018$Q240, 0 <- c(3:8), 1 <- c(1,2), 2 <- c(9,10))
WVSBR2018$ExtrIdeol2 <- factor(WVSBR2018$ExtrIdeol2, 
                               labels = c("moderado", "extremista de esquerda", 
                                          "extremista de direita"))

#Interest in politics
WVSBR2006$Int <- memisc::recode(WVSBR2006$V95, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
WVSBR2014$Int <- memisc::recode(WVSBR2014$V84, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)
WVSBR2018$Int <- memisc::recode(WVSBR2018$Q199, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1)

#Economic Attitudes
WVSBR2006$Econ <- WVSBR2006$V118
WVSBR2006$EconDic <- recode(WVSBR2006$V118, 0 <- c(1:5), 1 <- c(6:10))
WVSBR2006$EconDic <- factor(WVSBR2006$EconDic, 
                            labels = c("paternalista", "individualista"))
WVSBR2014$Econ <- (WVSBR2014$V98)
WVSBR2014$EconDic <- recode(WVSBR2014$V98, 0 <- c(1:5), 1 <- c(6:10))
WVSBR2014$EconDic <- factor(WVSBR2014$EconDic, 
                            labels = c("paternalista", "individualista"))
WVSBR2018$Econ <- (WVSBR2014$Q108)
WVSBR2018$EconDic <- recode(WVSBR2018$Q108, 0 <- c(0:5), 1 <- c(6:10))
WVSBR2018$EconDic <- factor(WVSBR2018$EconDic, 
                            labels = c("paternalista", "individualista"))

#Cultural Attitudes
WVSBR2006$Cult <- WVSBR2006$V202+WVSBR2006$V203+WVSBR2006$V204+
  WVSBR2006$V205
WVSBR2006$Cult01 <- WVSBR2006$Cult/max(WVSBR2006$Cult, na.rm=TRUE) 
WVSBR2006$Cult10 <- WVSBR2006$Cult01*10
WVSBR2006$CultDic <- cut(WVSBR2006$Cult10, c(0, 5, 10))
WVSBR2006$CultDic <- factor(WVSBR2006$CultDic,
                            labels = c("conservador", "progressista"))
WVSBR2014$Cult <- WVSBR2014$V203+WVSBR2014$V203A+WVSBR2014$V204+
  WVSBR2014$V205
WVSBR2014$Cult01 <- WVSBR2014$Cult/max(WVSBR2014$Cult, na.rm=TRUE) 
WVSBR2014$Cult10 <- WVSBR2014$Cult01*10
WVSBR2014$CultDic <- cut(WVSBR2014$Cult10, c(0, 5, 10))
WVSBR2014$CultDic <- factor(WVSBR2014$CultDic,
                            labels = c("conservador", "progressista"))
WVSBR2018$Cult <- WVSBR2018$Q182+WVSBR2018$Q183+WVSBR2018$Q184+
  WVSBR2018$Q185
WVSBR2018$Cult01 <- WVSBR2018$Cult/max(WVSBR2018$Cult, na.rm=TRUE) 
WVSBR2018$Cult10 <- WVSBR2018$Cult01*10
WVSBR2018$CultDic <- cut(WVSBR2018$Cult10, c(0, 5, 10))
WVSBR2018$CultDic <- factor(WVSBR2018$CultDic,
                            labels = c("conservador", "progressista"))

#####Models
## Social View
#2006
Mod.SocDem2006 <- lm(Soc_Dem10 ~ FxId + EdSup + Sexo + Desemp + ClasSub +
                       ConfInt + ConfPol + ExtrIdeol2 + Int + EconDic +
                       CultDic, data = WVSBR2006)
#2014
Mod.SocDem2014 <- lm(Soc_Dem10 ~ FxId + EdSup + Sexo + Desemp + ClasSub +
                       ConfInt + ConfPol + ExtrIdeol2 + Int + EconDic +
                       CultDic, data = WVSBR2014)
#2018
Mod.SocDem2018 <- lm(Soc_Dem10 ~ FxId + EdSup + Sexo + Desemp + ClasSub +
                       ConfInt + ConfPol + ExtrIdeol2 + Int + EconDic +
                       CultDic, data = WVSBR2018)


tab_model(Mod.SocDem2006, Mod.SocDem2014, Mod.SocDem2018, 
          dv.labels = c("2006", "2014", "2018"), show.ci = F, auto.label =T,
          show.se = T, collapse.se = T, wrap.labels = 60, p.style ="stars",
          pred.labels = c("Intercepto", "Faixa Etária(20-40)", "Faixa Etária(40-60)",
                          "Faixa Etária(60+)", "Educação(Superior)", "Sexo",
                          "Desempregado", "Classe Social Subjetiva", 
                          "Confiança Interpessoal", "Confiança Política",
                          "Extremista de Esquerda", "Extremista de Direita",
                          "Interesse por Política", "Individualista", 
                          "Progressista"), string.pred = "Preditores")

#Liberal
Mod.LibDem2006 <- lm(Lib_Dem10 ~ FxId + EdSup + Sexo + Desemp + ClasSub +
                       ConfInt + ConfPol + ExtrIdeol2 + Int + EconDic +
                       CultDic, data = WVSBR2006)
Mod.LibDem2014 <- lm(Lib_Dem10 ~ FxId + EdSup + Sexo + Desemp + ClasSub +
                       ConfInt + ConfPol + ExtrIdeol2 + Int + EconDic +
                       CultDic, data = WVSBR2014)
Mod.LibDem2018 <- lm(Lib_Dem10 ~ FxId + EdSup + Sexo + Desemp + ClasSub +
                       ConfInt + ConfPol + ExtrIdeol2 + Int + EconDic +
                       CultDic, data = WVSBR2018)

tab_model(Mod.LibDem2006, Mod.LibDem2014, Mod.LibDem2018,  
          dv.labels = c("2006", "2014", "2018"), show.ci = F, auto.label =T,
          show.se = T, collapse.se = T, wrap.labels = 60, p.style ="stars",
          pred.labels = c("Intercepto", "Faixa Etária(20-40)", "Faixa Etária(40-60)",
                          "Faixa Etária(60+)", "Educação(Superior)", "Sexo",
                          "Desempregado", "Classe Social Subjetiva", 
                          "Confiança Interpessoal", "Confiança Política",
                          "Extremista de Esquerda", "Extremista de Direita",
                          "Interesse por Política", "Individualista", 
                          "Progressista"), string.pred = "Preditores")

#Participacionist
Mod.PartDem2006 <- lm(Part_Dem10 ~ FxId + EdSup + Sexo + Desemp + ClasSub +
                        ConfInt + ConfPol + ExtrIdeol2 + Int + EconDic +
                        CultDic, data = WVSBR2006)
Mod.PartDem2014 <- lm(Part_Dem10 ~ FxId + EdSup + Sexo + Desemp + ClasSub +
                        ConfInt + ConfPol + ExtrIdeol2 + Int + EconDic +
                        CultDic, data = WVSBR2014)
Mod.PartDem2018 <- lm(Part_Dem10 ~ FxId + EdSup + Sexo + Desemp + ClasSub +
                        ConfInt + ConfPol + ExtrIdeol2 + Int + EconDic +
                        CultDic, data = WVSBR2018)

tab_model(Mod.PartDem2006, Mod.PartDem2014, Mod.PartDem2018,  
          dv.labels = c("2006", "2014", "2018"), show.ci = F, auto.label =T,
          show.se = T, collapse.se = T, wrap.labels = 60, p.style ="stars",
          pred.labels = c("Intercepto", "Faixa Etária(20-40)", "Faixa Etária(40-60)",
                          "Faixa Etária(60+)", "Educação(Superior)", "Sexo",
                          "Desempregado", "Classe Social Subjetiva", 
                          "Confiança Interpessoal", "Confiança Política",
                          "Extremista de Esquerda", "Extremista de Direita",
                          "Interesse por Política", "Individualista", 
                          "Progressista"), string.pred = "Preditores")

#Iliberal
Mod.IlibDem2006 <- lm(Ilib_Dem10 ~ FxId + EdSup + Sexo + Desemp + ClasSub +
                        ConfInt + ConfPol + ExtrIdeol2 + Int + EconDic +
                        CultDic, data = WVSBR2006)
Mod.IlibDem2014 <- lm(Ilib_Dem10 ~ FxId + EdSup + Sexo + Desemp + ClasSub +
                        ConfInt + ConfPol + ExtrIdeol2 + Int + EconDic +
                        CultDic, data = WVSBR2014)
Mod.IlibDem2018 <- lm(Ilib_Dem10 ~ FxId + EdSup + Sexo + Desemp + ClasSub +
                        ConfInt + ConfPol + ExtrIdeol2 + Int + EconDic +
                        CultDic, data = WVSBR2018)

tab_model(Mod.IlibDem2006, Mod.IlibDem2014, Mod.IlibDem2018,  
          dv.labels = c("2006", "2014", "2018"), show.ci = F, auto.label =T,
          show.se = T, collapse.se = T, wrap.labels = 60, p.style ="stars",
          pred.labels = c("Intercepto", "Faixa Etária(20-40)", "Faixa Etária(40-60)",
                          "Faixa Etária(60+)", "Educação(Superior)", "Sexo",
                          "Desempregado", "Classe Social Subjetiva", 
                          "Confiança Interpessoal", "Confiança Política",
                          "Extremista de Esquerda", "Extremista de Direita",
                          "Interesse por Política", "Individualista", 
                          "Progressista"), string.pred = "Preditores")

#Firm Liberal
Mod.LibConvBDem2006 <- glm(LibConvB ~ FxId + EdSup + Sexo + Desemp + ClasSub +
                             ConfInt + ConfPol + ExtrIdeol2 + Int + EconDic +
                             CultDic, data = WVSBR2006, family = binomial(link = logit))
Mod.LibConvBDem2014 <- glm(LibConvB ~ FxId + EdSup + Sexo + Desemp + ClasSub +
                             ConfInt + ConfPol + ExtrIdeol2 + Int + EconDic +
                             CultDic, data = WVSBR2014, family = binomial(link = logit))
Mod.LibConvBDem2018 <- glm(LibConvB ~ FxId + EdSup + Sexo + Desemp + ClasSub +
                             ConfInt + ConfPol + ExtrIdeol2 + Int + EconDic +
                             CultDic, data = WVSBR2018, family = binomial(link = logit))

tab_model(Mod.LibConvBDem2006, Mod.LibConvBDem2014, Mod.LibConvBDem2018,  
          dv.labels = c("2006", "2014", "2018"), show.ci = F, auto.label =T,
          show.se = T, collapse.se = T, wrap.labels = 60, p.style ="stars",
          pred.labels = c("Intercepto", "Faixa Etária(20-40)", "Faixa Etária(40-60)",
                          "Faixa Etária(60+)", "Educação(Superior)", "Sexo",
                          "Desempregado", "Classe Social Subjetiva", 
                          "Confiança Interpessoal", "Confiança Política",
                          "Extremista de Esquerda", "Extremista de Direita",
                          "Interesse por Política", "Individualista", 
                          "Progressista"), string.pred = "Preditores")

#Regression plots
library(ggplot2)
library(ggsci)
library(ggprism)
library(jtools)

plotSocDem <- plot_summs(Mod.SocDem2006, Mod.SocDem2014, Mod.SocDem2018, 
                         model.names = c("2006", "2014", "2018"), legend.title = "Anos",
                         inner_ci_level = .9,
                         point.shape = FALSE)

plotSocDem + theme_prism() +
  xlab("Estimated Coefficients") + ylab("Predictors") +
  theme(plot.title = element_text(face = "plain"),
        legend.position = "bottom") + 
  scale_y_discrete(labels= c("Progressive",
                             "Individualist",
                             "Interest in politics",
                             "Right-wing extremism",
                             "Left-wing extremism",
                             "Political Confidence",
                             "Interpersonal Confidence",
                             "Subjective Social Status", 
                             "Unemployed", 
                             "Gender", 
                             "Education (Higher)",
                             "Age Group 60+", 
                             "Age Group 40-60",
                             "Age Group 20-40"))

plotLibDem <- plot_summs(Mod.LibDem2006, Mod.LibDem2014, Mod.LibDem2018, 
                         model.names = c("2006", "2014", "2018"),
                         legend.title = "Anos",
                         inner_ci_level = .9,
                         point.shape = FALSE)

plotLibDem + theme_prism() +
  xlab("Estimated Coefficients") + ylab("Predictors") +
  theme(plot.title = element_text(face = "plain"),
        legend.position = "bottom") + 
  scale_y_discrete(labels= c("Progressive",
                             "Individualist",
                             "Interest in politics",
                             "Right-wing extremism",
                             "Left-wing extremism",
                             "Political Confidence",
                             "Interpersonal Confidence",
                             "Subjective Social Status", 
                             "Unemployed", 
                             "Gender", 
                             "Education (Higher)",
                             "Age Group 60+", 
                             "Age Group 40-60",
                             "Age Group 20-40"))

plotPartDem <- plot_summs(Mod.PartDem2006, Mod.PartDem2014, Mod.PartDem2018, 
                          model.names = c("2006", "2014", "2018"),
                          legend.title = "Anos",
                          inner_ci_level = .9,
                          point.shape = FALSE)

plotPartDem + theme_prism() +
  xlab("Estimated Coefficients") + ylab("Predictors") +
  theme(plot.title = element_text(face = "plain"),
        legend.position = "bottom") + 
  scale_y_discrete(labels= c("Progressive",
                             "Individualist",
                             "Interest in politics",
                             "Right-wing extremism",
                             "Left-wing extremism",
                             "Political Confidence",
                             "Interpersonal Confidence",
                             "Subjective Social Status", 
                             "Unemployed", 
                             "Gender", 
                             "Education (Higher)",
                             "Age Group 60+", 
                             "Age Group 40-60",
                             "Age Group 20-40"))


plotIlibDem <- plot_summs(Mod.IlibDem2006, Mod.IlibDem2014, Mod.IlibDem2018, 
                          model.names = c("2006", "2014", "2018"),
                          legend.title = "Anos",
                          inner_ci_level = .9,
                          point.shape = FALSE)

plotIlibDem + theme_prism() +
  xlab("Estimated Coefficients") + ylab("Predictors") +
  theme(plot.title = element_text(face = "plain"),
        legend.position = "bottom") + 
  scale_y_discrete(labels= c("Progressive",
                             "Individualist",
                             "Interest in politics",
                             "Right-wing extremism",
                             "Left-wing extremism",
                             "Political Confidence",
                             "Interpersonal Confidence",
                             "Subjective Social Status", 
                             "Unemployed", 
                             "Gender", 
                             "Education (Higher)",
                             "Age Group 60+", 
                             "Age Group 40-60",
                             "Age Group 20-40"))


plotLibConvDem <- plot_summs(Mod.LibConvDem2006, Mod.LibConvDem2014, Mod.LibConvDem2018, 
                             model.names = c("2006", "2014", "2018"),
                             legend.title = "Anos",
                             inner_ci_level = .9,
                             point.shape = FALSE)

plotLibConvDem + theme_prism() +
  xlab("Estimated Coefficients") + ylab("Predictors") +
  theme(plot.title = element_text(face = "plain"),
        legend.position = "bottom") + 
  scale_y_discrete(labels= c("Progressive",
                             "Individualist",
                             "Interest in politics",
                             "Right-wing extremism",
                             "Left-wing extremism",
                             "Political Confidence",
                             "Interpersonal Confidence",
                             "Subjective Social Status", 
                             "Unemployed", 
                             "Gender", 
                             "Education (Higher)",
                             "Age Group 60+", 
                             "Age Group 40-60",
                             "Age Group 20-40"))

plotLibConvBDem <- plot_summs(Mod.LibConvBDem2006, Mod.LibConvBDem2014, Mod.LibConvBDem2018, 
                              model.names = c("2006", "2014", "2018"),
                              legend.title = "Anos",
                              inner_ci_level = .9,
                              point.shape = FALSE)

plotLibConvBDem + theme_prism() +
  xlab("Estimated Coefficients") + ylab("Predictors") +
  theme(plot.title = element_text(face = "plain"),
        legend.position = "bottom") + 
  scale_y_discrete(labels= c("Progressive",
                             "Individualist",
                             "Interest in politics",
                             "Right-wing extremism",
                             "Left-wing extremism",
                             "Political Confidence",
                             "Interpersonal Confidence",
                             "Subjective Social Status", 
                             "Unemployed", 
                             "Gender", 
                             "Education (Higher)",
                             "Age Group 60+", 
                             "Age Group 40-60",
                             "Age Group 20-40"))
