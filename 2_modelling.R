
# Przygotowanie środowiska pracy ------------------------------------------

# Wczytanie wykorzystywanych bibliotek
library("MASS")
library("sandwich")
library("zoo")
library("car")
library("lmtest")
library("Formula")
library("plm")
library("stargazer")
library("readxl")
library('stringr')
library('ggplot2')
install.packages('gplots')
library('gplots')
library('tidyverse')

# Ustawienia liczb
options(scipen=999)


# Baza danych -------------------------------------------------------------
df <- read.csv("./data/panel_data.csv") 


# Dane --------------------------------------------------------------------

df_final <- df[, c('Nazwa',
				   'Rok',
				   'TFR', 
				   'bezrobotni_mężczyźni',
				   'bezrobotni_kobiety', 
				   'wynagrodzenia', 
				   'cena_mieszkan_metr_srednia', 
				   'rozwody',
				   'swiadczenia_500_plus',
				   'swiadczenia_spoleczne',
				   'wspolczynnik_feminizacji',
				   'malzenstwa_zawarte',
				   'ludnosc_na_1km2',
				   'praca_ogolem_kobiety',
				   'dochody_ogolem')]

df_final_log <- df_final

df_final_log[, -c(1,2)] <- log(df_final_log[, -c(1,2)])


# Świadczenia 500+ zostały wprowadzone w kwietniu 2016 r.
df_final_log$swiadczenia_500_plus[is.na(df_final_log$swiadczenia_500_plus)] <- 0

# dodanie zmiennej 0-1 dla lat z covidem
df_final_log$covid <- ifelse((df_final_log$Rok == 2020)|(df_final_log$Rok == 2021) , 1, 0)


# Efekty stałe ------------------------------------------------------------

# Heterogeniczność (zróżnicowanie) między regionami 
plotmeans(TFR ~ Nazwa, main="Zróżnicowanie między regionami", data=df)

# Heterogeniczność (zróżnicowanie) między latami
plotmeans(TFR ~ Rok, main="Zróżnicowanie między latami", data=df)


# Formuła modelu  --------------------------------------------------------------

formula = TFR ~
	bezrobotni_mężczyźni +
	bezrobotni_kobiety +
	wynagrodzenia +
	cena_mieszkan_metr_srednia +
	rozwody +
	swiadczenia_500_plus +
	swiadczenia_spoleczne +
	wspolczynnik_feminizacji +
	malzenstwa_zawarte +
	ludnosc_na_1km2 +
	praca_ogolem_kobiety +
	dochody_ogolem +
	covid


# Model POLS --------------------------------------------------------------
# Pooled Ordinary Least Squares

pols <- plm(formula, data = df_final_log, index = c('Nazwa', 'Rok'), model = 'pooling')
summary(pols)

# Model ten nie uwzględnia heterogeniczności między podmiotami i czasem


# Fixed effects model -----------------------------------------------------
# Model z estymatorem efektów stałych

fe <- plm(formula, data = df_final_log, index = c('Nazwa', 'Rok'), model = 'within')
summary(fe)


# Random effects model ----------------------------------------------------
# Model z estymator efektów losowych


re <- plm(formula, data = df_final_log, index = c('Nazwa', 'Rok'), model = 'random')
summary(re)



stargazer(pols, fe, re, 
		  align=TRUE, 
		  type = "html",
		  column.labels = c("POLS", "FE", "RE"),
		  out = "./image/first_results.html")


# TESTY NA  ISTOTNOŚĆ EFEKTÓW INDYWIDUALNYCH I CZASOWYCH --------------------

# TEST BREUSCHA-PAGANA DLA EFEKTÓW CZASOWYCH
plmtest(pols, effect =  c("time"), type=("bp"))

# Dla testu Breuscha-Pagana p-value < 5% => odrzucamy H0 o tym, że efekty czasowe są nieistotne


# TEST F DLA EFEKTÓW INDYWIDUALNYCH
pFtest(fe, pols) 

# p-value < 5% => odrzucamy H0 o tym, że efekty czasowe są nieistotne
# Na tej podstawie można stwierdzić, iż model panelowy jest lepszy niż estymowanie
# za pomocą Metody Najmniejszych Kwadratów.


# TEST HAUSMANA -----------------------------------------------------------
phtest(fe, re)

# p-value < 5% => odrzucamy H0 o tym, że lepszy będzie estymator efektów losowych


# POPRAWA MODELU FE -----------------------------------------
fe <- plm(formula, data = df_final_log, index = c('Nazwa', 'Rok'), model = 'within')
summary(fe)

fixef(fe) # efekty indywidualne

# pierwsza iteracja - usunięcie zmiennej bezrobotni_mezczyzni
formula1 = TFR ~
	bezrobotni_kobiety +
	wynagrodzenia +
	cena_mieszkan_metr_srednia +
	rozwody +
	swiadczenia_500_plus +
	swiadczenia_spoleczne +
	wspolczynnik_feminizacji +
	malzenstwa_zawarte +
	ludnosc_na_1km2 +
	praca_ogolem_kobiety +
	dochody_ogolem +
	covid
	
fe1 <- plm(formula1, data = df_final_log, index = c('Nazwa', 'Rok'), model = 'within')
summary(fe1)

# druga iteracja - usunięcie zmiennej wspolczynnik_feminizacji
formula2 = TFR ~
	bezrobotni_kobiety +
	wynagrodzenia +
	cena_mieszkan_metr_srednia +
	rozwody +
	swiadczenia_500_plus +
	swiadczenia_spoleczne +
	malzenstwa_zawarte +
	ludnosc_na_1km2 +
	praca_ogolem_kobiety +
	dochody_ogolem +
	covid

fe2 <- plm(formula2, data = df_final_log, index = c('Nazwa', 'Rok'), model = 'within')
summary(fe2)

# trzecia iteracja - usunięcie zmiennej dochody_ogolem
formula3 = TFR ~
	bezrobotni_kobiety +
	wynagrodzenia +
	cena_mieszkan_metr_srednia +
	rozwody +
	swiadczenia_500_plus +
	swiadczenia_spoleczne +
	malzenstwa_zawarte +
	ludnosc_na_1km2 +
	praca_ogolem_kobiety +
	covid

fe3 <- plm(formula3, data = df_final_log, index = c('Nazwa', 'Rok'), model = 'within')
summary(fe3)
	
# czwarta iteracja - usunięcie zmiennej ludnosc_na_1km2
formula4 = TFR ~
	bezrobotni_kobiety +
	wynagrodzenia +
	cena_mieszkan_metr_srednia +
	rozwody +
	swiadczenia_500_plus +
	swiadczenia_spoleczne +
	malzenstwa_zawarte +
	praca_ogolem_kobiety +
	covid

fe4 <- plm(formula4, data = df_final_log, index = c('Nazwa', 'Rok'), model = 'within')
summary(fe4)

library("car")
linearHypothesis(model=fe, c("bezrobotni_mężczyźni=0", 
							 "wspolczynnik_feminizacji=0",
							 "dochody_ogolem=0",
							 "ludnosc_na_1km2=0"))
#p-value = 0.8088 => brak podstaw do odrzucenia hipotezy zerowej o łącznej nieistotności zmiennych







# AUTOKORELACJA RESZT
pbgtest(fe) # test Breuscha-Godfreya

# p-value < 5% => odrzucamy H0 o braku autokorelacji reszt


# HETEROSKEDASTYCZNOŚĆ RESZT 
bptest(formula, data = df_final_log, studentize = TRUE) # test Breuscha-Pagana

# p-value < 5% => odrzucamy H0 o homoskedastyczności reszt. 
# Występuje heteroskedastyczność.


# Controlling for heteroskedasticity:
coeftest(fe, vcovHC) # Heteroskedasticity consistent coefficients




# W modelu występują następujące problemy:
# - autokorelacja reszt
# - heteroskedastycznosć reszt
# - zależność przekrojowa






# Interpretacja R2 --------------------------------------------------------

source('./source/static_wide_panels_R2.R')

fe_R2 = static_wide_panels_R2(fe)



# Macierz odporna ---------------------------------------------------------
fe_HC_HC0_group = coeftest(fe, vcov.=vcovHC(fe, type="HC0", cluster="group"))
fe_HC_HC0_group

fe_HC_HC0 = coeftest(fe, vcov.=vcovHC(fe, type="HC0"))
fe_HC_HC0

fe_NW_HC0_group = coeftest(fe, vcov.=vcovNW(fe, type="HC0", cluster="group"))
fe_NW_HC0_group

fe_SCC_HC0_time = coeftest(fe, vcov.=vcovSCC(fe, type="HC0", cluster="time"))
fe_SCC_HC0_time

fe_HC_arellano = coeftest(fe, vcovHC(fe, method = 'arellano'))
fe_HC_arellano


stargazer(fe, fe_HC_HC0_group, fe_HC_HC0, fe_NW_HC0_group, fe_SCC_HC0_time, fe_HC_arellano, type="text")


# Reszty
resid = fe$residuals
resztyframe= cbind(resid,df$TRF)
resztyframe = as.data.frame(resztyframe)

resztyframe %>% ggplot(aes(y = resid, x = V2)) + 
	geom_point(color = 'red') + 
	geom_smooth(method = 'lm')
