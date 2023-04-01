
# 1. Preparation of the environment ---------------------------------------------

install.packages('tidyverse')

library(tidyverse)
remove(list = ls())
options(scipen=999)


# 2. Data - cleaning and processing ---------------------------------------

# File to connect GUS ids with NUTS ids
GUS_NUTS <- read.csv("./data/GUS_NUTS.csv", sep = ";", encoding = 'UTF-8',
					 colClasses = c("character","character","character")) %>% 
	dplyr::select(Kod_NUTS, Kod_GUS) %>% as_tibble

# TFR - Total Fertility Rate // współczynnik dzietności
TFR <- read.csv("./data/wspolczynnik_dzietnosci/LUDN_2346_CREL_20230311183211.csv",
				sep = ";", dec = ",", header = T, fill = TRUE, encoding = 'UTF-8',
				colClasses = c("character","character","character",
							   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	dplyr::select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "TFR") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc) %>% 
	filter((Rok >= 2012) & (Rok < 2022))


# bezrobotni_wg_wieku_i_plci
bezrobotni_wg_wieku_i_plci <- read.csv("./data/bezrobotni_wg_wieku_i_plci/RYNE_1946_CREL_20230312143348.csv",
									   sep = ";", dec = ",", header = T, fill = TRUE, encoding = 'UTF-8', 
									   colClasses = c("character","character","character", "character",
									   			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	dplyr::select(Kod, Nazwa, Płeć, Rok, Wartosc) %>% 
	mutate(zmienna = "bezrobotni") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna, Płeć),
				values_from = Wartosc) %>%
	filter((Rok >= 2012) & (Rok < 2022))


# cena_mieszkan_metr_mediana
cena_mieszkan_metr_mediana <- read.csv("./data/cena_mieszkan_metr_mediana/RYNE_3787_CREL_20230312175819.csv",
									   sep = ";", dec = ",", header = T, fill = TRUE, encoding = 'UTF-8',
									   colClasses = c("character","character","character", "character",
									   			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	dplyr::select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "cena_mieszkan_metr_mediana") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc) %>%
	filter((Rok >= 2012) & (Rok < 2022))


# cena_mieszkan_metr_srednia
cena_mieszkan_metr_srednia <- read.csv("./data/cena_mieszkan_metr_srednia/RYNE_3788_CREL_20230312175926.csv",
									   sep = ";", dec = ",", header = T, fill = TRUE, encoding = 'UTF-8',
									   colClasses = c("character","character","character", "character",
									   			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	dplyr::select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "cena_mieszkan_metr_srednia") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc) %>%
	filter((Rok >= 2012) & (Rok < 2022))


# ludnosc_ogolem
ludnosc_ogolem <- read.csv("./data/ludnosc_ogolem/LUDN_2137_CREL_20230321175420.csv",
						   sep = ";", dec = ",", header = T, fill = TRUE, encoding = 'UTF-8',
						   colClasses = c("character","character","character", "character",
						   			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	dplyr::select(Kod, Nazwa, Wiek, Rok, Wartosc) %>% 
	mutate(zmienna = "ludnosc_ogolem") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna, Wiek),
				values_from = Wartosc) %>% 
	mutate(ludnosc_ogolem_wiek_rozrodczy = 
		   	`ludnosc_ogolem_15-19` +
		   	`ludnosc_ogolem_20-24` +
		   	`ludnosc_ogolem_25-29` +
		   	`ludnosc_ogolem_30-34` +
		   	`ludnosc_ogolem_35-39` +
		   	`ludnosc_ogolem_40-44` +
		   	`ludnosc_ogolem_45-49` ) %>% 
	mutate(ludnosc_ogolem_wiek_produkcyjny = `ludnosc_ogolem_15-19` +
		   	`ludnosc_ogolem_20-24` +
		   	`ludnosc_ogolem_25-29` +
		   	`ludnosc_ogolem_30-34` +
		   	`ludnosc_ogolem_35-39` +
		   	`ludnosc_ogolem_40-44` +
		   	`ludnosc_ogolem_45-49` +
		   	`ludnosc_ogolem_50-54` +
		   	`ludnosc_ogolem_55-59`) %>%
	filter((Rok >= 2012) & (Rok < 2022))

ludnosc_ogolem <- ludnosc_ogolem %>% dplyr::select(NUTS_ID, Kod_GUS, Nazwa, Rok,
												   ludnosc_ogolem_ogółem,
												   ludnosc_ogolem_wiek_rozrodczy,
												   ludnosc_ogolem_wiek_produkcyjny) 

# ludnosc_kobiety
ludnosc_kobiety <- read.csv("./data/ludnosc_kobiety/LUDN_2137_CREL_20230321175737.csv",
							sep = ";", dec = ",", header = T, fill = TRUE, encoding='UTF-8',
							colClasses = c("character","character","character", "character",
										   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	dplyr::select(Kod, Nazwa, Wiek, Rok, Wartosc) %>% 
	mutate(zmienna = "ludnosc_kobiety") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna, Wiek),
				values_from = Wartosc) %>% 
	mutate(ludnosc_kobiety_wiek_rozrodczy = `ludnosc_kobiety_15-19` +
		   	`ludnosc_kobiety_20-24` +
		   	`ludnosc_kobiety_25-29` +
		   	`ludnosc_kobiety_30-34` +
		   	`ludnosc_kobiety_35-39` +
		   	`ludnosc_kobiety_40-44` +
		   	`ludnosc_kobiety_45-49` ) %>% 
	mutate(ludnosc_kobiety_wiek_produkcyjny = `ludnosc_kobiety_15-19` +
		   	`ludnosc_kobiety_20-24` +
		   	`ludnosc_kobiety_25-29` +
		   	`ludnosc_kobiety_30-34` +
		   	`ludnosc_kobiety_35-39` +
		   	`ludnosc_kobiety_40-44` +
		   	`ludnosc_kobiety_45-49` +
		   	`ludnosc_kobiety_50-54` +
		   	`ludnosc_kobiety_55-59`) %>%
	filter((Rok >= 2012) & (Rok < 2022))

ludnosc_kobiety <- ludnosc_kobiety %>% dplyr::select(NUTS_ID, Kod_GUS, Nazwa, Rok,
													 ludnosc_kobiety_ogółem,
													 ludnosc_kobiety_wiek_rozrodczy,
													 ludnosc_kobiety_wiek_produkcyjny) 

# ludnosc_mezczyzni
ludnosc_mezczyzni <- read.csv("./data/ludnosc_mezczyzni/LUDN_2137_CREL_20230321175653.csv",
							  sep = ";", dec = ",", header = T, fill = TRUE, encoding = 'UTF-8',
							  colClasses = c("character","character","character", "character",
							  			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	dplyr::select(Kod, Nazwa, Wiek, Rok, Wartosc) %>% 
	mutate(zmienna = "ludnosc_mezczyzni") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna, Wiek),
				values_from = Wartosc) %>% 
	mutate(ludnosc_mezczyzni_wiek_rozrodczy = 
		   	`ludnosc_mezczyzni_15-19` +
		   	`ludnosc_mezczyzni_20-24` +
		   	`ludnosc_mezczyzni_25-29` +
		   	`ludnosc_mezczyzni_30-34` +
		   	`ludnosc_mezczyzni_35-39` +
		   	`ludnosc_mezczyzni_40-44` +
		   	`ludnosc_mezczyzni_45-49` ) %>% 
	mutate(ludnosc_mezczyzni_wiek_produkcyjny = `ludnosc_mezczyzni_15-19` +
		   	`ludnosc_mezczyzni_20-24` +
		   	`ludnosc_mezczyzni_25-29` +
		   	`ludnosc_mezczyzni_30-34` +
		   	`ludnosc_mezczyzni_35-39` +
		   	`ludnosc_mezczyzni_40-44` +
		   	`ludnosc_mezczyzni_45-49` +
		   	`ludnosc_mezczyzni_50-54` +
		   	`ludnosc_mezczyzni_55-59`) %>%
	filter((Rok >= 2012) & (Rok < 2022))

ludnosc_mezczyzni <- ludnosc_mezczyzni %>% dplyr::select(NUTS_ID, Kod_GUS, Nazwa, Rok,
														 ludnosc_mezczyzni_ogółem,
														 ludnosc_mezczyzni_wiek_rozrodczy,
														 ludnosc_mezczyzni_wiek_produkcyjny) 


# malzenstwa_zawarte
malzenstwa_zawarte <- read.csv("./data/malzenstwa_zawarte/LUDN_3430_CREL_20230312142004.csv",
							   sep = ";", dec = ",", header = T, fill = TRUE, encoding = 'UTF-8',
							   colClasses = c("character","character","character",
							   			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	dplyr::select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "malzenstwa_zawarte") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>%
	filter((Rok >= 2012) & (Rok < 2022)) %>%
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc)


# mieszkania_uzytkowe
mieszkania_uzytkowe <- read.csv("./data/mieszkania_uzytkowe/PRZE_3820_CREL_20230312174157.csv",
								sep = ";", dec = ",", header = T, fill = TRUE, encoding = 'UTF-8',
								colClasses = c("character","character","character",
											   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	mutate(zmienna = case_when(Wskaźniki == "mieszkania oddane do użytkowania na 1000 ludności" ~ "mieszkania_oddane",
							   Wskaźniki == "przeciętna powierzchnia użytkowa 1 mieszkania oddanego do użytkowania" ~ "mieszkania_przecietna_powierzchnia",
							   Wskaźniki == "nowe budynki mieszkalne na 1000 ludności" ~ "mieszkania_nowe",
							   Wskaźniki == "przeciętna liczba izb w 1 mieszkaniu oddanym do użytkowania" ~ "mieszkania_izby")
	) %>% 
	dplyr::select(Kod, Nazwa, Rok, Wartosc, zmienna) %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>%
	filter((Rok >= 2012) & (Rok < 2022)) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc)


# rozwody_i_separacje
rozwody_i_separacje <- read.csv("./data/rozwody_i_separacje/LUDN_1971_CREL_20230312142529.csv",
								sep = ";", dec = ",", header = T, fill = TRUE, encoding = 'UTF-8',
								colClasses = c("character","character","character",
											   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	mutate(zmienna = case_when(`Rozwody.i.separacje` == "rozwody na 10 tys. ludności" ~ "rozwody_na_10_tys",
							   `Rozwody.i.separacje` == "separacje" ~ "separacje",
							   `Rozwody.i.separacje` == "rozwody" ~ "rozwody",
							   `Rozwody.i.separacje` == "separacje na 10 tys. ludności" ~ "separacje_na_10_tys")
	) %>% 
	dplyr::select(Kod, Nazwa, Rok, Wartosc, zmienna) %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc) %>%
	filter((Rok >= 2012) & (Rok < 2022))


# swiadczenia_500_plus
swiadczenia_500_plus <- read.csv("./data/swiadczenia_500_plus/OCHR_3803_CREL_20230312173651.csv",
								 sep = ";", dec = ",", header = T, fill = TRUE, encoding = 'UTF-8',
								 colClasses = c("character","character","character",
								 			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	dplyr::select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "swiadczenia_500_plus") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc) %>%
	filter((Rok >= 2012) & (Rok < 2022))


# swiadczenia_spoleczne
swiadczenia_spoleczne <- read.csv("./data/swiadczenia_spoleczne/OCHR_2993_CREL_20230312173337.csv",
								  sep = ";", dec = ",", header = T, fill = TRUE, encoding = 'UTF-8',
								  colClasses = c("character","character","character",
								  			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	dplyr::select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "swiadczenia_spoleczne") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc) %>%
	filter((Rok >= 2012) & (Rok < 2022))


# wspolczynnik_feminizacji
wspolczynnik_feminizacji <- read.csv("./data/wspolczynnik_feminizacji/LUDN_3429_CREL_20230312094222.csv",
									 sep = ";", dec = ",", header = T, fill = TRUE, encoding = 'UTF-8',
									 colClasses = c("character","character","character",
									 			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	dplyr::select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "wspolczynnik_feminizacji") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc) %>%
	filter((Rok >= 2012) & (Rok < 2022))


# wynagrodzenia
wynagrodzenia <- read.csv("./data/wynagrodzenia/WYNA_2497_CREL_20230312151155.csv",
						  sep = ";", dec = ",", header = T, fill = TRUE, encoding = 'UTF-8',
						  colClasses = c("character","character","character",
						  			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	dplyr::select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "wynagrodzenia") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc) %>%
	filter((Rok >= 2012) & (Rok < 2022))


# zlobki
zlobki <- read.csv("./data/zlobki/OCHR_3227_CREL_20230312181227.csv",
				   sep = ";", dec = ",", header = T, fill = TRUE, encoding = 'UTF-8',
				   colClasses = c("character","character","character",
				   			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	dplyr::select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "zlobki") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc) %>%
	filter((Rok >= 2012) & (Rok < 2022))


# gestosc_zaludnienia
gestosc_zaludnienia <- read.csv("./data/gestosc_zaludnienia/LUDN_2425_CREL_20230325223401.csv",
								sep = ";", dec = ",", header = T, fill = TRUE, encoding = 'UTF-8',
								colClasses = c("character","character","character",
											   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	mutate(zmienna = case_when(Wskaźniki == "ludność na 1 km2" ~ "ludnosc_na_1km2",
							   Wskaźniki == "gęstość zaludnienia powierzchni zabudowanej i zurbanizowanej (osoby/km2)" ~ "gestosc_zaludnienia_pow_zabudowana",
							   Wskaźniki == "zmiana liczby ludności na 1000 mieszkańców" ~ "zmiana_liczby_ludnosci",
							   Wskaźniki == "ludność w tysiącach" ~ "ludnosc_tys",
							   Wskaźniki == "ludność w tysiącach mężczyźni" ~ "ludnosc_tys_mezczyzni",
							   Wskaźniki == "ludność w tysiącach kobiety" ~ "ludnosc_tys_kobiety")
	) %>% 
	
	dplyr::select(Kod, Nazwa, Rok, Wartosc, zmienna) %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>%
	filter((Rok >= 2012) & (Rok < 2022)) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc)


# praca_sekcje
praca_sekcje <- read.csv("./data/praca_sekcje/RYNE_2813_CREL_20230325223718.csv",
						 sep = ";", dec = ",", header = T, fill = TRUE, encoding = 'UTF-8',
						 colClasses = c("character","character","character",
						 			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	mutate(zmienna = case_when(Sektory.ekonomiczne == "ogółem" ~ "praca_ogolem_na_10_tys",
							   Sektory.ekonomiczne == "rolnictwo, leśnictwo, łowiectwo i rybactwo" ~ "praca_rolnictwo_na_10_tys",
							   Sektory.ekonomiczne == "przemysł i budownictwo" ~ "praca_przemysl_na_10_tys",
							   Sektory.ekonomiczne == "handel; naprawa pojazdów samochodowych; transport i gospodarka magazynowa; zakwaterowanie i gastronomia; informacja i komunikacja" ~ "praca_handel_na_10_tys",
							   Sektory.ekonomiczne == "działalność finansowa i ubezpieczeniowa; obsługa rynku nieruchomości" ~ "praca_finanse_na_10_tys",
							   Sektory.ekonomiczne == "pozostałe usługi" ~ "praca_pozostale_na_10_tys")
	) %>% 
	dplyr::select(Kod, Nazwa, Płeć, Rok, Wartosc, zmienna) %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna, Płeć),
				values_from = Wartosc) %>%
	filter((Rok >= 2012) & (Rok < 2022))


# zgony
covid <- read.csv("./data/zgony/demo_r_mwk3_ts_linear.csv", sep = ",", encoding = 'UTF-8')
covid <- covid[grep("^PL", covid$geo), ]

covid$year <- substr(covid$TIME_PERIOD, 1, 4)
DEATHS_YEAR <- aggregate(OBS_VALUE ~ year + geo + sex, data = covid, FUN = sum)
DEATHS_YEAR$year <- as.integer(DEATHS_YEAR$year)

zgony <- DEATHS_YEAR %>% 
	as_tibble %>% 
	dplyr::select(year, geo, sex, OBS_VALUE) %>% 
	rename(Rok = year) %>%
	rename(Płeć = sex) %>%
	rename(Kod_NUTS = geo) %>%
	rename(Wartosc = OBS_VALUE) %>%
	mutate(zmienna = "zgony") %>% 
	left_join(GUS_NUTS, by = "Kod_NUTS") %>%
	drop_na(Kod_GUS) %>%
	rename(NUTS_ID = Kod_NUTS) %>%
	pivot_wider(id_cols = c(Rok, NUTS_ID, Kod_GUS), names_from = c(zmienna, Płeć),
				values_from = Wartosc) %>%
	rename(zgony_kobiety = zgony_F) %>%
	rename(zgony_mezczyzni = zgony_M) %>%
	rename(zgony_ogolem = zgony_T) %>%
	relocate(NUTS_ID) %>%
	filter((Rok >= 2012) & (Rok < 2022))

rm(covid)
rm(DEATHS_YEAR)

# dochod
dochod <- read.csv("./data/dochod/FINA_2627_CREL_20230331174538.csv",
				   sep = ";", dec = ",", header = T, fill = TRUE, encoding = 'UTF-8',
				   colClasses = c("character","character","character",'character',
				   			   NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	mutate(zmienna = case_when(Wskaźniki == "ogółem" ~ "dochody_ogolem",
							   Wskaźniki == "dochody własne" ~ "dochody_wlasne",
							   Wskaźniki == "udziały w podatkach stanowiących dochody budżetu państwa razem" ~ "dochody_podatki",
							   Wskaźniki == "dochody własne - udziały w podatkach stanowiących dochody budżetu państwa podatek dochodowy od osób fizycznych" ~ "dochody_podatki_osoby_fizyczne",
							   Wskaźniki == "dochody własne - udziały w podatkach stanowiących dochody budżetu państwa podatek dochodowy od osób prawnych" ~ "dochody_podatki_osoby_prawne",
							   Wskaźniki == "dochody własne - dochody podatkowe ustalone i pobierane na podstawie odrębnych ustaw" ~ "dochody_podatki_pozostale")
	) %>% 
	dplyr::select(Kod, Nazwa, Rok, Wartosc, zmienna) %>% 
	
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>%
	filter((Rok >= 2012) & (Rok < 2022)) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc)


# 3. Preapration of final data frame --------------------------------------

GUS_NUTS <- GUS_NUTS %>% rename(NUTS_ID = Kod_NUTS)

ludnosc_ogolem <- ludnosc_ogolem %>% dplyr::select(NUTS_ID, Kod_GUS, Nazwa, Rok, ludnosc_ogolem_ogółem, ludnosc_ogolem_wiek_rozrodczy, ludnosc_ogolem_wiek_produkcyjny)
ludnosc_kobiety <- ludnosc_kobiety %>% dplyr::select(NUTS_ID, Kod_GUS, Nazwa, Rok, ludnosc_kobiety_ogółem, ludnosc_kobiety_wiek_rozrodczy, ludnosc_kobiety_wiek_produkcyjny)
ludnosc_mezczyzni <- ludnosc_mezczyzni %>% dplyr::select(NUTS_ID, Kod_GUS, Nazwa, Rok, ludnosc_mezczyzni_ogółem, ludnosc_mezczyzni_wiek_rozrodczy, ludnosc_mezczyzni_wiek_produkcyjny)

df <- ludnosc_ogolem %>% dplyr::select(NUTS_ID, Kod_GUS, Nazwa, Rok) %>%
	left_join(TFR[,-c(2,3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok")) %>% 
	left_join(ludnosc_ogolem[,-c(2,3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok")) %>%
	left_join(ludnosc_kobiety[,-c(2,3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok")) %>%
	left_join(ludnosc_mezczyzni[,-c(2,3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok")) %>%
	left_join(bezrobotni_wg_wieku_i_plci[,-c(2,3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok")) %>%
	left_join(wynagrodzenia[,-c(2,3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok")) %>%
	left_join(cena_mieszkan_metr_mediana[,-c(2,3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok")) %>%
	left_join(cena_mieszkan_metr_srednia[,-c(2,3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok")) %>%
	left_join(mieszkania_uzytkowe[,-c(2,3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok")) %>%
	left_join(rozwody_i_separacje[,-c(2,3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok")) %>%
	left_join(swiadczenia_500_plus[,-c(2,3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok")) %>%
	left_join(swiadczenia_spoleczne[,-c(2,3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok")) %>%
	left_join(wspolczynnik_feminizacji[,-c(2,3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok")) %>%
	left_join(zlobki[,-c(2,3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok")) %>%
	left_join(malzenstwa_zawarte[,-c(2,3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok")) %>%
	left_join(gestosc_zaludnienia[,-c(2,3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok")) %>%
	left_join(praca_sekcje[,-c(2,3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok")) %>%
	left_join(zgony[,-c(3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok")) %>%
	left_join(dochod[,-c(2,3)], by = c("NUTS_ID" = "NUTS_ID", "Rok" = "Rok"))


# transformacja zmiennych w df
df <- df %>% 
	mutate(swiadczenia_ogolem = swiadczenia_500_plus + swiadczenia_spoleczne)


# Świadczenia 500+ zostały wprowadzone w kwietniu 2016 r.
df$swiadczenia_500_plus[is.na(df$swiadczenia_500_plus)] <- 0
df$swiadczenia_ogolem <- df$swiadczenia_500_plus + df$swiadczenia_spoleczne

# dodanie zmiennej 0-1 dla lat z covidem
df$covid <- ifelse((df$Rok == 2020)|(df$Rok == 2021) , 1, 0)

# Zapisujemy dataframe do pliku csv
write.csv(df_final, file = './data/panel_data_2002-2021.csv')

