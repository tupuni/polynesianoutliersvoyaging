library(tidyverse)
library(here)

## paths to db : insert path to the sqlite files
path_to_georoc <- "~/Documents/Github/georoc-data/georoc.sqlite"
path_to_pofatu <- "~/Documents/Github/pofatu-data/dist/pofatu.sqlite"

## load data
inventory <- read.csv(here("analysis", "data", "raw_data", "inventory.csv"),
                      header=TRUE, sep=",", stringsAsFactors=FALSE)
icpaes <- read.csv(here("analysis", "data", "raw_data", "icpaes.csv"),
                   header=TRUE, sep=",", stringsAsFactors=FALSE)
icpms <- read.csv(here("analysis", "data", "raw_data", "icpms.csv"),
                  header=TRUE, sep=",", stringsAsFactors=FALSE)
mcicpms <- read.csv(here("analysis", "data", "raw_data", "mcicpms.csv"),
                    header=TRUE, sep=",", stringsAsFactors=FALSE)

## assemble and join data
samples <- inventory %>% dplyr::select(`Sample`,`Latitude`,`Longitude`) %>%
  rename("lat" = Latitude, "long" = Longitude)
oxides <- icpaes %>% slice(1:23) %>% dplyr::select(c(1:13))
traces <- icpms %>% slice(1:23)
isotopes <- mcicpms %>% slice(1:23)
joined_data <- right_join(samples, oxides, by = c("Sample")) %>%
  right_join(., traces, by = c("Sample", "Type", "Location")) %>%
  right_join(., isotopes, by = c("Sample", "Type", "Location"))
write.csv(joined_data,"analysis/data/derived_data/joined_data.csv")

## makes Ti(ppm) from TiO2(%)
Ti_from_TiO2 <- function(input) {
  output <- input %>%
    mutate(Ti=(((TiO2*10000)/79.8650)*47.867))
  return(output)
}
joined_data <- Ti_from_TiO2(joined_data)

## makes K(ppm) from K2O(%)
K_from_K2O <- function(input) {
  output <- input %>%
    mutate(K=((K2O*10000)/94)*78)
  return(output)
}
joined_data <- K_from_K2O(joined_data)

## converts FeO% to Fe2O3%
Fe2O3_from_FeO <- function(input) {
  output <- input %>%
    mutate(Fe2O3 = ifelse(Fe2O3 %in% NA, FeO*1.1113, Fe2O3))
  return(output)
}

## ranges()
summary(joined_data[,c(
  "Sample","SiO2","K2O","Na2O","MgO","Nb","La","Ba","Ta","Sr","Nd","Pb",
  "Sm","Zr","Yb","Ti")])
ranges_IAB <- function(df) {
  df_ranges <- df %>%
    dplyr::transmute(Sample = Sample,
      `SiO2 min` = SiO2 - 1.5, `SiO2 max` = SiO2 + 1.5,
      `MgO min` = MgO - 1.5, `MgO max` = MgO + 1.5,
      `Na2O min` = Na2O - 1.5, `Na2O max` = Na2O + 1.5,
      `K2O min` = K2O - 1.5, `K2O max` = K2O + 1.5,
      `Cs min` = (Cs-((Cs*50)/100)), `Cs max` = (Cs+((Cs*50)/100)), # +/- 50%
      `Rb min` = (Rb-((Rb*50)/100)), `Rb max` = (Rb+((Rb*50)/100)), # +/- 50%
      `Ba min` = (Ba-((Ba*50)/100)), `Ba max` = (Ba+((Ba*50)/100)), # +/- 50%
      `Th min` = (Th-((Th*50)/100)), `Th max` = (Th+((Th*50)/100)), # +/- 50%
      `Ce min` = (Ce-((Ce*50)/100)), `Ce max` = (Ce+((Ce*50)/100)), # +/- 50%
      `Pb min` = (Pb-((Pb*50)/100)), `Pb max` = (Pb+((Pb*50)/100)), # +/- 50%
      `Nd min` = (Nd-((Nd*50)/100)), `Nd max` = (Nd+((Nd*50)/100)), # +/- 50%
      `Sr min` = (Sr-((Sr*50)/100)), `Sr max` = (Sr+((Sr*50)/100)), # +/- 50%
      `Nb min` = (Nb-((Nb*50)/100)), `Nb max` = (Nb+((Nb*50)/100)), # +/- 50%
      `Sm min` = (Sm-((Sm*50)/100)), `Sm max` = (Sm+((Sm*50)/100)), # +/- 50%
      `Zr min` = (Zr-((Zr*50)/100)), `Zr max` = (Zr+((Zr*50)/100)), # +/- 50%
      `Yb min` = (Yb-((Yb*50)/100)), `Yb max` = (Yb+((Yb*50)/100)), # +/- 50%
      `Ti min` = (Ti-((Ti*50)/100)), `Ti max` = (Ti+((Ti*50)/100)), # +/- 50%
      `Nd143_Nd144 min` = (Nd143_Nd144-((Nd143_Nd144*1)/1000)), # 1‰
      `Nd143_Nd144 max` = (Nd143_Nd144+((Nd143_Nd144*1)/1000)), # 1‰
      `Sr87_Sr86 min` = (Sr87_Sr86-((Sr87_Sr86*1)/1000)), # 1‰
      `Sr87_Sr86 max` = (Sr87_Sr86+((Sr87_Sr86*1)/1000)), # 1‰
      `Pb206_Pb204 min` = (Pb206_Pb204-((Pb206_Pb204*.5)/100)), # .5%
      `Pb206_Pb204 max` = (Pb206_Pb204+((Pb206_Pb204*.5)/100)), # .5%
      `Pb207_Pb204 min` = (Pb207_Pb204-((Pb207_Pb204*.5)/100)), # .5%
      `Pb207_Pb204 max` = (Pb207_Pb204+((Pb207_Pb204*.5)/100)), # .5%
      `Pb208_Pb204 min` = (Pb208_Pb204-((Pb208_Pb204*.5)/100)), # .5%
      `Pb208_Pb204 max` = (Pb208_Pb204+((Pb208_Pb204*.5)/100))  # .5%
    )
  return(df_ranges)
}

ranges_OIB <- function(df) {
  df_ranges <- df %>%
    dplyr::transmute(
      Sample = Sample,
      `SiO2 min` = SiO2 - 1.5, `SiO2 max` = SiO2 + 1.5,
      `MgO min` = MgO - 1.5, `MgO max` = MgO + 1.5,
      `Na2O min` = Na2O - 1.5, `Na2O max` = Na2O + 1.5,
      `K2O min` = K2O - 1.5, `K2O max` = K2O + 1.5,
      `Cs min` = (Cs-((Cs*50)/100)), `Cs max` = (Cs+((Cs*50)/100)), # +/- 50%
      `Rb min` = (Rb-((Rb*50)/100)), `Rb max` = (Rb+((Rb*50)/100)), # +/- 50%
      `Ba min` = (Ba-((Ba*50)/100)), `Ba max` = (Ba+((Ba*50)/100)), # +/- 50%
      `Th min` = (Th-((Th*50)/100)), `Th max` = (Th+((Th*50)/100)), # +/- 50%
      `Ce min` = (Ce-((Ce*50)/100)), `Ce max` = (Ce+((Ce*50)/100)), # +/- 50%
      `Pb min` = (Pb-((Pb*50)/100)), `Pb max` = (Pb+((Pb*50)/100)), # +/- 50%
      `Nd min` = (Nd-((Nd*50)/100)), `Nd max` = (Nd+((Nd*50)/100)), # +/- 50%
      `Sr min` = (Sr-((Sr*50)/100)), `Sr max` = (Sr+((Sr*50)/100)), # +/- 50%
      `Nb min` = (Nb-((Nb*50)/100)), `Nb max` = (Nb+((Nb*50)/100)), # +/- 50%
      `Sm min` = (Sm-((Sm*50)/100)), `Sm max` = (Sm+((Sm*50)/100)), # +/- 50%
      `Zr min` = (Zr-((Zr*50)/100)), `Zr max` = (Zr+((Zr*50)/100)), # +/- 50%
      `Yb min` = (Yb-((Yb*50)/100)), `Yb max` = (Yb+((Yb*50)/100)), # +/- 50%
      `Ti min` = (Ti-((Ti*50)/100)), `Ti max` = (Ti+((Ti*50)/100)), # +/- 50%
      `Nd143_Nd144 min` = (Nd143_Nd144-((Nd143_Nd144*1)/1000)), # 1‰
      `Nd143_Nd144 max` = (Nd143_Nd144+((Nd143_Nd144*1)/1000)), # 1‰
      `Sr87_Sr86 min` = (Sr87_Sr86-((Sr87_Sr86*1)/1000)), # 1‰
      `Sr87_Sr86 max` = (Sr87_Sr86+((Sr87_Sr86*1)/1000)), # 1‰
      `Pb206_Pb204 min` = (Pb206_Pb204-((Pb206_Pb204*1)/100)), # 1%
      `Pb206_Pb204 max` = (Pb206_Pb204+((Pb206_Pb204*1)/100)), # 1%
      `Pb207_Pb204 min` = (Pb207_Pb204-((Pb207_Pb204*1)/100)), # 1%
      `Pb207_Pb204 max` = (Pb207_Pb204+((Pb207_Pb204*1)/100)), # 1%
      `Pb208_Pb204 min` = (Pb208_Pb204-((Pb208_Pb204*1)/100)), # 1%
      `Pb208_Pb204 max` = (Pb208_Pb204+((Pb208_Pb204*1)/100))  # 1%
    )
  return(df_ranges)
  }

ranges_s_IAB <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-03","E-11-06","E-11-07","K-12-28","K-12-29")) %>% ranges_IAB()
write_csv(ranges_s_IAB, (here(
  "analysis", "data", "derived_data","IAB_ranges.csv")))

ranges_s_OIB <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-08","T-12-06","T-12-07","T-12-08","T-12-09","T-12-10",
  "K-12-24","K-12-25","K-12-26")) %>% ranges_OIB()
write_csv(ranges_s_OIB, (here(
  "analysis", "data", "derived_data","OIB_ranges.csv")))

## normalizes trace element values to primitive mantle,
## based on PM values from McDonough & Sun (1989,1995)
sun <- read.csv(here(
  "analysis", "data", "raw_data", "sun_mcdonough.csv"),
  header=TRUE, sep=",", stringsAsFactors=FALSE)

normalize_to_pm <- function(df) {
  df_normalized <- df %>%
    dplyr::transmute(
      Sample=Sample, Location=Location, Cs=Cs/sun[2,"Cs"],Rb=Rb/sun[2,"Rb"],Ba=Ba/sun[2,"Ba"],
      Th=Th/sun[2,"Th"],U=U/sun[2,"U"],Nb=Nb/sun[2,"Nb"],Ta=Ta/sun[2,"Ta"],La=La/sun[2,"La"],
      Ce=Ce/sun[2,"Ce"],Pr=Pr/sun[2,"Pr"],Pb=Pb/sun[2,"Pb"],Nd=Nd/sun[2,"Nd"],Sr=Sr/sun[2,"Sr"],
      Sm=Sm/sun[2,"Sm"],Zr=Zr/sun[2,"Zr"],Hf=Hf/sun[2,"Hf"],Ti=Ti/sun[2,"Ti"],Eu=Eu/sun[2,"Eu"],
      Gd=Gd/sun[2,"Gd"],Tb=Tb/sun[2,"Tb"],Dy=Dy/sun[2,"Dy"],Ho=Ho/sun[2,"Ho"],Y=Y/sun[2,"Y"],
      Er=Er/sun[2,"Er"],Li=Li/sun[2,"Li"],Yb=Yb/sun[2,"Yb"],Lu=Lu/sun[2,"Lu"]) %>%
    gather("var","conc",Cs:Lu)
  return(df_normalized)
}
joined_data_norm <- normalize_to_pm(joined_data)

## georoc_location_from_file_id()
get_georoc_location <- function(georoc_output) {
  georoc_output_with_location <- georoc_output %>%
    mutate(
      Location = case_when(
        grepl("BANDA_ARC", file_id) ~ "Banda Arc",
        grepl("BISMARCK_ARC", file_id) ~ "Bismarck Arc",
        grepl("LUZON_ARC", file_id) ~ "Luzon Arc",
        grepl("MARIANA_ARC", file_id) ~ "Mariana Arc",
        grepl("NEW_CALEDONIA", file_id) ~ "New Caledonia",
        grepl("NEW_HEBRIDES_ARC", file_id) ~ "Vanuatu Arc",
        grepl("NEW_ZEALAND", file_id) ~ "New Zealand",
        grepl("SOLOMON_ISLAND_ARC", file_id) ~ "Solomon Arc",
        grepl("SULAWESI_ARC", file_id) ~ "Sulawesi Arc",
        grepl("SUNDA_ARC", file_id) ~ "Sunda Arc",
        grepl("TONGA_ARC", file_id) ~ "Tonga-Fiji",
        LOCATION == "TONGA ARC / WALLIS-FUTUNA ISLANDS / WALLIS (UVEA) ISLANDS" ~ "Uvea (Wallis)",
        grepl("YAP_ARC", file_id) ~ "Yap Arc",
        grepl("AUSTRAL-COOK_ISLANDS", file_id) & long > -155 ~ "Austral-Cook chain",
        grepl("AUSTRAL-COOK_ISLANDS", file_id) & long < -155 ~ "Austral-Cook chain",
        grepl("CAROLINE_ISLANDS", file_id) ~ "Caroline islands",
        grepl("EASTER_SEAMOUNT_CHAIN", file_id) ~ "Rapa Nui",
        grepl("HAWAIIAN_ISLANDS", file_id) ~ "Hawai'i islands",
        grepl("MARQUESAS", file_id) ~ "Marquesas islands",
        grepl("PITCAIRN-GAMBIER", file_id) & long > -132 ~ "Pitcairn-Gambier chain",
        grepl("PITCAIRN-GAMBIER", file_id) & long < -132 ~ "Pitcairn-Gambier chain",
        grepl("SAMOAN_ISLANDS", file_id) ~ "Samoan islands",
        grepl("SOCIETY_ISLANDS", file_id) ~ "Society islands",
        grepl("TUAMOTU_ISLANDS", file_id) ~ "Tuamotu islands",
        TRUE ~ "na"))
  return(georoc_output_with_location)
}

## pofatu_location()
pofatu_location <- function(pofatu_output) {
  pofatu_output_with_location <- pofatu_output %>%
    mutate(
      Location = case_when(
        location_region == "AUSTRAL" ~ "Austral-Cook chain",
        location_region == "CAROLINE" ~ "Caroline islands",
        location_region == "COOK ISLANDS" ~ "Austral-Cook chain",
        location_region == "FIJI" ~ "Tonga Arc",
        location_region == "FIJI/LAU_GROUP" ~ "Tonga Arc",
        location_region == "GAMBIER" ~ "Pitcairn-Gambier chain",
        location_region == "HAWAI'I" ~ "Hawai'i islands",
        location_region == "HENDERSON" ~ "Pitcairn-Gambier chain",
        location_region == "MARQUESAS" ~ "Marquesas islands",
        location_region == "NEW GUINEA" ~ "New Guinea",
        location_region == "NEW ZEALAND" ~ "New Zealand",
        location_region == "PITCAIRN" ~ "Pitcairn-Gambier chain",
        location_region == "RAPA NUI" ~ "Rapa Nui",
        location_region == "ROTUMA" ~ "Tonga Arc",
        location_region == "SAMOA" ~ "Samoan islands",
        location_region == "SOCIETY" ~ "Society islands",
        location_region == "SOLOMON" ~ "Solomon Arc",
        location_region == "SOLOMON/STA_CRUZ_GROUP" ~ "Solomon Arc",
        location_region == "TIMOR" ~ "Timor",
        location_region == "TOKELAU" ~ "Tokelau islands",
        location_region == "TONGA" ~ "Tonga Arc",
        location_region == "TUAMOTU" ~ "Tuamotu islands",
        location_region == "TUVALU" ~ "Tuvalu islands",
        location_region == "UVEA" ~ "Uvea island",
        location_region == "VANUATU" ~ "Vanuatu Arc",
        TRUE ~ "na"))
  return(pofatu_output_with_location)
}

