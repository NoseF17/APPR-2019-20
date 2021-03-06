library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(shiny)
library(readr)
library(dplyr)
library(mosaic)
library(farver)
library(tmaptools)

options(gsubfn.engine="R")
library("readxl")
library("openxlsx")
library(tmap)
library(ggplot2)
library(scales)


# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")
