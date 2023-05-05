library("readxl")
library("sandwich")
library("knitr")
library("tidyverse")

options(scipen = 999)

#LOAD DATA
df <- read_excel("data/fertility_rate_poland.xlsx") 
View(df)

# FERTILITY RATE OVER TIME
f_rate <- ggplot(df,aes(x=Date,y=Ferility_rate))+
	geom_line()+
	scale_color_manual(values = c("blue")) +
	theme_minimal()
f_rate
