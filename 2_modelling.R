
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
library('gplots')
library('tidyverse')
library(foreign)
library(car)

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

# Dodanie zmiennej 0-1 dla lat, gdzie występował COVID-19
df_final_log$covid <- ifelse((df_final_log$Rok == 2020)|(df_final_log$Rok == 2021) , 1, 0)

# Dodanie przedrostka 'ln' dla zlogarytmowanych zmiennych
colnames(df_final_log)[-c(1, 2, 16)] <- paste('ln', colnames(df_final_log)[-c(1, 2, 16)], sep = '_') 


# Podstawowa analiza statystyczna -----------------------------------------

# Dataframe bez zmiennej 'Rok'
df_final_log_without_year <- df_final_log[,!names(df_final_log) %in% c("Rok")]
df_final_log_without_year

# Tabela z podstawowymi statystykami
stargazer(df_final_log_without_year, 
		  align=TRUE, 
		  digits = 2,
		  type = "html",
		  out = "./image/stats_summary.html")

# Eksploracja panelu
coplot(ln_TFR ~ Rok|Nazwa, type="l", data=df_final_log) 
coplot(ln_TFR ~ Rok|Nazwa, type="b", data=df_final_log)


windows()
scatterplot(ln_TFR ~ Rok|Nazwa, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=df_final_log)

# Analiza korelacji
library(corrplot)
windows()
M = cor(df_final_log_without_year[, -c(1)])
corrplot(M)


# Efekty stałe ------------------------------------------------------------

# Heterogeniczność (zróżnicowanie) między regionami 
plotmeans(ln_TFR ~ Nazwa, main="Zróżnicowanie między regionami", data=df_final_log)

# Heterogeniczność (zróżnicowanie) między latami
plotmeans(ln_TFR ~ Rok, main="Zróżnicowanie między latami", data=df_final_log)


# Formuła modelu  --------------------------------------------------------------

formula = ln_TFR ~
	ln_bezrobotni_mężczyźni +
	ln_bezrobotni_kobiety +
	ln_wynagrodzenia +
	ln_cena_mieszkan_metr_srednia +
	ln_rozwody +
	ln_swiadczenia_500_plus +
	ln_swiadczenia_spoleczne +
	ln_wspolczynnik_feminizacji +
	ln_malzenstwa_zawarte +
	ln_ludnosc_na_1km2 +
	ln_praca_ogolem_kobiety +
	ln_dochody_ogolem +
	covid


# Model POLS --------------------------------------------------------------
# Pooled Ordinary Least Squares

pols <- plm(formula, data = df_final_log, index = c('Nazwa', 'Rok'), model = 'pooling')
summary(pols)

# Model ten nie uwzględnia heterogeniczności między podmiotami i czasem

# OLS za pomocą linear regression model
ols <- lm (formula, data = df_final_log)
summary(ols)

# Homoskedastyczność POLS 
res <- residuals(ols)
yhat <- fitted(ols)
plot(yhat, res, xlab = "Wartości dopasowane", ylab = "Reszty")
plot(ols)

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


# TESTY NA ISTOTNOŚĆ EFEKTÓW INDYWIDUALNYCH I CZASOWYCH --------------------

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
formula1 = ln_TFR ~
	ln_bezrobotni_kobiety +
	ln_wynagrodzenia +
	ln_cena_mieszkan_metr_srednia +
	ln_rozwody +
	ln_swiadczenia_500_plus +
	ln_swiadczenia_spoleczne +
	ln_wspolczynnik_feminizacji +
	ln_malzenstwa_zawarte +
	ln_ludnosc_na_1km2 +
	ln_praca_ogolem_kobiety +
	ln_dochody_ogolem +
	covid
	
fe1 <- plm(formula1, data = df_final_log, index = c('Nazwa', 'Rok'), model = 'within')
summary(fe1)

# druga iteracja - usunięcie zmiennej wspolczynnik_feminizacji
formula2 = ln_TFR ~
	ln_bezrobotni_kobiety +
	ln_wynagrodzenia +
	ln_cena_mieszkan_metr_srednia +
	ln_rozwody +
	ln_swiadczenia_500_plus +
	ln_swiadczenia_spoleczne +
	ln_malzenstwa_zawarte +
	ln_ludnosc_na_1km2 +
	ln_praca_ogolem_kobiety +
	ln_dochody_ogolem +
	covid

fe2 <- plm(formula2, data = df_final_log, index = c('Nazwa', 'Rok'), model = 'within')
summary(fe2)

# trzecia iteracja - usunięcie zmiennej dochody_ogolem
formula3 = ln_TFR ~
	ln_bezrobotni_kobiety +
	ln_wynagrodzenia +
	ln_cena_mieszkan_metr_srednia +
	ln_rozwody +
	ln_swiadczenia_500_plus +
	ln_swiadczenia_spoleczne +
	ln_malzenstwa_zawarte +
	ln_ludnosc_na_1km2 +
	ln_praca_ogolem_kobiety +
	covid

fe3 <- plm(formula3, data = df_final_log, index = c('Nazwa', 'Rok'), model = 'within')
summary(fe3)
	
# czwarta iteracja - usunięcie zmiennej ludnosc_na_1km2
formula4 = ln_TFR ~
	ln_bezrobotni_kobiety +
	ln_wynagrodzenia +
	ln_cena_mieszkan_metr_srednia +
	ln_rozwody +
	ln_swiadczenia_500_plus +
	ln_swiadczenia_spoleczne +
	ln_malzenstwa_zawarte +
	ln_praca_ogolem_kobiety +
	covid

fe4 <- plm(formula4, data = df_final_log, index = c('Nazwa', 'Rok'), model = 'within')
summary(fe4)

library("car")
linearHypothesis(model=fe, c("ln_bezrobotni_mężczyźni=0", 
							 "ln_wspolczynnik_feminizacji=0",
							 "ln_dochody_ogolem=0",
							 "ln_ludnosc_na_1km2=0"))
#p-value = 0.8088 => brak podstaw do odrzucenia hipotezy zerowej o łącznej nieistotności zmiennych


# DIAGNOSTYKA MODELU ------------------------------------------------------

# AUTOKORELACJA RESZT
pbgtest(fe) # test Breuscha-Godfreya

# p-value < 5% => odrzucamy H0 o braku autokorelacji reszt


# HETEROSKEDASTYCZNOŚĆ RESZT 
bptest(fe4, studentize = TRUE) # test Breuscha-Pagana

# p-value < 5% => odrzucamy H0 o homoskedastyczności reszt. 
# Występuje heteroskedastyczność.


# ZALEŻNOŚĆ PRZEKROJOWA
pcdtest(fe4, test = c("lm"))
pcdtest(fe4, test = c("cd"))


# W modelu występują następujące problemy:
# - autokorelacja reszt
# - heteroskedastycznosć reszt
# - zależność przekrojowa


# Interpretacja R2 --------------------------------------------------------

source('./source/static_wide_panels_R2.R')

fe_R2 = static_wide_panels_R2(fe4)



# Macierz odporna ---------------------------------------------------------

# Macierze odporne na heteroskedastyczność

fe4_HC_HC0 = coeftest(fe4, vcov.=vcovHC(fe4, type="HC0")) # odporna na heteroskedastyczność
fe4_HC_HC0

fe4_HC_arellano = coeftest(fe4, vcovHC(fe4, method = 'arellano')) # macierz odporna na heteroskedastyczność i autokorelację (rekomendowana na FE)
fe4_HC_arellano

fe4_NW_HC0_group = coeftest(fe4, vcov.=vcovNW(fe4, type="HC0", cluster="group")) # newey west
fe4_NW_HC0_group

fe4_HC_HC0_group = coeftest(fe4, vcov.=vcovHC(fe4, type="HC0", cluster="group"))
fe4_HC_HC0_group

fe4_SCC_HC0_time = coeftest(fe4, vcov.=vcovSCC(fe4, type="HC0", cluster="time")) 
fe4_SCC_HC0_time


# Kryteria informacyjne AIC i BIC -----------------------------------------
source('./source/AIC.R')

AIC_fe4 = aicbic_plm(fe4, "AIC")
BIC_fe4 = aicbic_plm(fe4, "BIC")




stargazer(fe4, fe4_HC_HC0_group, fe4_HC_HC0, fe4_NW_HC0_group, fe4_SCC_HC0_time, fe4_HC_arellano, 
		  type = "html",
		  out = "./image/robust.html"
		  )
