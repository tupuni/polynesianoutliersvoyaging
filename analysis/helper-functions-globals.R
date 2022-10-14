library(tidyverse)
library(here)

## load data
inventory <- read.csv(here("analysis", "data", "raw_data", "inventory.csv"),
                      header=TRUE, sep=",", stringsAsFactors=FALSE)
icpaes <- read.csv(here("analysis", "data", "raw_data", "icpaes.csv"),
                   header=TRUE, sep=",", stringsAsFactors=FALSE)
icpms <- read.csv(here("analysis", "data", "raw_data", "icpms.csv"),
                  header=TRUE, sep=",", stringsAsFactors=FALSE)
mcicpms <- read.csv(here("analysis", "data", "raw_data", "mcicpms.csv"),
                    header=TRUE, sep=",", stringsAsFactors=FALSE)
names(mcicpms) <- gsub(x = names(mcicpms), pattern = "\\X", replacement = "") %>%
  gsub(pattern = "143Nd.144Nd", replacement = "Nd143_Nd144") %>%
  gsub(pattern = "87Sr.86Sr", replacement = "Sr87_Sr86") %>%
  gsub(pattern = "206Pb.204Pb", replacement = "Pb206_Pb204") %>%
  gsub(pattern = "207Pb.204Pb", replacement = "Pb207_Pb204") %>%
  gsub(pattern = "208Pb.204Pb", replacement = "Pb208_Pb204") %>%
  gsub(pattern = "\\.SD", replacement = "\\_SD")

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

## get additional literature data
price2014 <- read.csv(here("analysis", "data", "raw_data", "price2014G3.csv"),
                      header=TRUE, sep=",", stringsAsFactors=FALSE) %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::mutate(Location = case_when(
    grepl("Wallis Island", Site) ~ "Uvea",
    grepl("Fiji", Site) ~ "North Fiji Basin"))

price2017 <- read.csv(here("analysis", "data", "raw_data", "price2017G3.csv"),
                      header=TRUE, sep=",", stringsAsFactors=FALSE) %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::filter(grepl('Rotuma|Fiji|Yasawa|Cikobia|Futuna', Site)) %>%
  dplyr::mutate(Location = case_when(
    grepl("Rotuma", Site) ~ "Rotuma",
    grepl("Futuna", Site) ~ "Futuna",
    grepl("Cikobia", Site) ~ "Cikobia",
    grepl("Yasawa", Site) ~ "North Fiji Basin",
    grepl("Fiji", Site) ~ "North Fiji Basin"))

jeanvoine2021 <- read.csv(here("analysis", "data", "raw_data", "jeanvoine2021JP.csv"),
                          header=TRUE, sep=",", stringsAsFactors=FALSE) %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::mutate(Location = case_when(grepl("Fatu Kapa", Site) ~ "Fatu Kapa"))

zhang2020 <- read.csv(here("analysis", "data", "raw_data", "zhang2020CG.csv"),
                      header=TRUE, sep=",", stringsAsFactors=FALSE) %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::mutate(Location = case_when(grepl("Caroline", Site) ~ "Caroline plateau"))




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


## paths to db : insert path to the sqlite files
path_to_georoc <- "~/Documents/Github/georoc-data/georoc.sqlite"
path_to_pofatu <- "~/Documents/Github/pofatu-data/dist/pofatu.sqlite"

## normalize()
## normalizes trace element values to primitive mantle,
## based on PM values from McDonough & Sun (1989,1995)
sun <- read.csv(here(
  "analysis", "data", "raw_data", "sun_mcdonough.csv"),
  header=TRUE, sep=",", stringsAsFactors=FALSE)

normalize <- function(joined_data) {
  joined_data_normalized <- joined_data %>%
    dplyr::transmute(
      Sample=Sample,Cs=Cs/sun[2,"Cs"],Rb=Rb/sun[2,"Rb"],Ba=Ba/sun[2,"Ba"],
      Th=Th/sun[2,"Th"],U=U/sun[2,"U"],Nb=Nb/sun[2,"Nb"],Ta=Ta/sun[2,"Ta"],
      La=La/sun[2,"La"],Ce=Ce/sun[2,"Ce"],Pr=Pr/sun[2,"Pr"],Pb=Pb/sun[2,"Pb"],
      Nd=Nd/sun[2,"Nd"],Sr=Sr/sun[2,"Sr"],Sm=Sm/sun[2,"Sm"],Zr=Zr/sun[2,"Zr"],
      Hf=Hf/sun[2,"Hf"],Ti=Ti/sun[2,"Ti"],Eu=Eu/sun[2,"Eu"],Gd=Gd/sun[2,"Gd"],
      Tb=Tb/sun[2,"Tb"],Dy=Dy/sun[2,"Dy"],Ho=Ho/sun[2,"Ho"],Y=Y/sun[2,"Y"],
      Er=Er/sun[2,"Er"],Li=Li/sun[2,"Li"],Yb=Yb/sun[2,"Yb"],Lu=Lu/sun[2,"Lu"])
  return(joined_data_normalized)
}
joined_data_norm <- normalize(joined_data)

normalize_to_pm <- function(df) {
  df_normalized <- df %>%
    dplyr::mutate(
      Cs=Cs/0.032,Rb=Rb/0.635,Ba=Ba/6.989,Th=Th/0.085,U=U/0.021,Nb=Nb/0.713,
      Ta=Ta/0.041,La=La/0.687,Ce=Ce/1.775,Pr=Pr/0.276,Nd=Nd/1.354,Sr=Sr/21.1,
      Sm=Sm/0.444,Zr=Zr/11.2,Ti=Ti/1300,Eu=Eu/0.168,Gd=Gd/0.596,Tb=Tb/0.108,
      Dy=Dy/0.737,Y=Y/4.55,Er=Er/0.48,Yb=Yb/0.493,Lu=Lu/0.074) %>%
    gather("var","conc",Cs:Lu)
  return(df_normalized)
}

## order_spider
order_spider <- function(df) {
  df_ordered <- df %>%
    mutate(var = fct_relevel(
      var,
      "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
      "Pb","Nd","Sr","Sm","Zr","Hf","Ti","Eu","Gd","Tb",
      "Dy","Ho","Y","Er","Li","Yb","Lu"))
  return(df_ordered)
}

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
        grepl("AUSTRAL-COOK_ISLANDS", file_id) & LONGITUDE_MIN > -155 ~ "Austral-Cook chain",
        grepl("AUSTRAL-COOK_ISLANDS", file_id) & LONGITUDE_MIN < -155 ~ "Austral-Cook chain",
        grepl("CAROLINE_ISLANDS", file_id) ~ "Caroline islands",
        grepl("EASTER_SEAMOUNT_CHAIN", file_id) ~ "Rapa Nui",
        grepl("HAWAIIAN_ISLANDS", file_id) ~ "Hawai'i islands",
        grepl("MARQUESAS", file_id) ~ "Marquesas islands",
        grepl("PITCAIRN-GAMBIER", file_id) & LONGITUDE_MIN > -132 ~ "Pitcairn-Gambier chain",
        grepl("PITCAIRN-GAMBIER", file_id) & LONGITUDE_MIN < -132 ~ "Pitcairn-Gambier chain",
        grepl("SAMOAN_ISLANDS", file_id) ~ "Samoan islands",
        grepl("SOCIETY_ISLANDS", file_id) ~ "Society islands",
        grepl("TUAMOTU_ISLANDS", file_id) ~ "Tuamotu islands",
        TRUE ~ "na"))
  return(georoc_output_with_location)
}

## georoc_archipelago_from_file_id()
georoc_archipelago_from_file_id <- function(georoc_output) {
  georoc_output_with_archipelago <- georoc_output %>%
    mutate(
      Archipelago = case_when(
        grepl("BANDA_ARC", file_id) ~ "banda arc",
        grepl("BISMARCK_ARC", file_id) ~ "bismarck arc",
        grepl("KERMADEC_ARC", file_id) ~ "kermadec arc",
        grepl("LUZON_ARC", file_id) ~ "luzon arc",
        grepl("MARIANA_ARC", file_id) ~ "mariana arc",
        grepl("NEW_CALEDONIA", file_id) ~ "new caledonia",
        grepl("NEW_HEBRIDES_ARC", file_id) ~ "vanuatu",
        grepl("NEW_ZEALAND", file_id) ~ "new zealand",
        grepl("SOLOMON_ISLAND_ARC", file_id) ~ "solomon arc",
        grepl("SULAWESI_ARC", file_id) ~ "sulawesi arc",
        grepl("SUNDA_ARC", file_id) ~ "sunda arc",
        grepl("TONGA_ARC", file_id) & LONGITUDE_MIN > -177 ~ "tongan islands",
        grepl("TONGA_ARC", file_id) & LONGITUDE_MIN < -177 ~ "fijian islands",
        grepl("TONGA_ARC", file_id) & LONGITUDE_MIN > 170 ~ "fijian islands",
        grepl("YAP_ARC", file_id) ~ "yap arc",
        grepl("AUSTRAL-COOK_ISLANDS", file_id) & LONGITUDE_MIN > -155 ~ "austral islands",
        grepl("AUSTRAL-COOK_ISLANDS", file_id) & LONGITUDE_MIN < -155 ~ "cook islands",
        grepl("CAROLINE_ISLANDS", file_id) ~ "caroline islands",
        grepl("EASTER_SEAMOUNT_CHAIN", file_id) ~ "rapa nui",
        grepl("HAWAIIAN_ISLANDS", file_id) ~ "hawai'i islands",
        grepl("MARQUESAS", file_id) ~ "marquesas islands",
        grepl("PITCAIRN-GAMBIER", file_id) & LONGITUDE_MIN < -132 ~ "pitcairn islands",
        grepl("PITCAIRN-GAMBIER", file_id) & LONGITUDE_MIN < -132 ~ "gambier islands",
        grepl("SAMOAN_ISLANDS", file_id) ~ "samoan islands",
        grepl("SOCIETY_ISLANDS", file_id) ~ "society islands",
        grepl("TUAMOTU_ISLANDS", file_id) ~ "tuamotu islands",
        TRUE ~ "na"))
  return(georoc_output_with_archipelago)
}

## georoc_vanuatu_island_from_LOCATION
get_vanuatu_location <- function(georoc_output) {
  georoc_output_with_island <- georoc_output %>%
    mutate(Location = case_when(
      grepl("ANEITYUM", LOCATION) ~ "Aneityum",
      grepl("GAUA", LOCATION) ~ "Gaua",
      grepl("VANUA LAVA", LOCATION) ~ "Vanua Lava",
      grepl("AMBRYM", LOCATION) ~ "Ambrym",
      grepl("AMBAE", LOCATION) ~ "Ambae",
      grepl("EFATE", LOCATION) ~ "Efate",
      grepl("EMAU", LOCATION) ~ "Emau",
      grepl("EPI", LOCATION) ~ "Epi",
      grepl("ERROMANGO", LOCATION) ~ "Erromango",
      grepl("FUTUNA", LOCATION) ~ "Futuna",
      grepl("MERELAVA", LOCATION) ~ "Mere Lava",
      grepl("MOTO (=MOTA)", LOCATION) ~ "Mota Lava",
      grepl("NGUNA", LOCATION) ~ "Nguna",
      grepl("MOTO LAVA", LOCATION) ~ "Mota Lava",
      grepl("PAAMA", LOCATION) ~ "Paama",
      grepl("SANTA MARIA", LOCATION) ~ "Gaua",
      grepl("ESPIRITU SANTO", LOCATION) ~ "Santo",
      grepl("TONGOA", LOCATION) ~ "Tongoa",
      grepl("LOPEVI", LOCATION) ~ "Lopevi",
      grepl("TANNA", LOCATION) ~ "Tanna",
      grepl("UREPARAPARA", LOCATION) ~ "Ureparapara",
      grepl("VOT TANDE", LOCATION) ~ "Vot Tande"
    ))
  return(georoc_output_with_island)
}

## georoc_samoa_island_from_georoc
get_samoa_location <- function(sql_output) {
  output_with_island <- sql_output %>%
    mutate(Island = case_when(
      grepl("OFU", LOCATION) ~ "Ofu",
      grepl("TAU", LOCATION) ~ "Tau",
      grepl("SAVAI^I", LOCATION) ~ "Savai'i",
      grepl("TUTUILA", LOCATION) ~ "Tutuila",
      grepl("UPOLU", LOCATION) ~ "Upolu"
    ))
  return(output_with_island)
}

## georoc_samoa_island_from_pofatu
get_samoa_location_pofatu <- function(sql_output) {
  output_with_island <- sql_output %>%
    mutate(Island = case_when(
      grepl("OFU", location_subregion) ~ "Ofu",
      grepl("SAVAI'I", location_subregion) ~ "Savai'i",
      grepl("TUTUILA", location_subregion) ~ "Tutuila",
      grepl("UPOLU", location_subregion) ~ "Upolu"
    ))
  return(output_with_island)
}

## rename_georoc_isotopes()
rename_georoc_isotopes <- function(georoc_output) {
  georoc_output_renamed <- georoc_output %>%
    rename(Sample=id, Location=file_id, lat=LATITUDE_MIN, long=LONGITUDE_MIN,
           Sr87_Sr86=SR87_SR86, Nd143_Nd144=ND143_ND144,
           Pb206_Pb204=PB206_PB204, Pb207_Pb204=PB207_PB204,
           Pb208_Pb204=PB208_PB204)
  return(georoc_output_renamed)
}

## rename_georoc_all()
rename_georoc_all <- function(georoc_output) {
  georoc_output_renamed <- georoc_output %>%
    rename(
      Location=Location, Sample=id, lat=LATITUDE_MIN, long=LONGITUDE_MIN,
      SiO2="SIO2(WT%)", TiO2="TIO2(WT%)", Al2O3="AL2O3(WT%)", MnO="MNO(WT%)",
      MgO="MGO(WT%)", CaO="CAO(WT%)",Na2O="NA2O(WT%)", K2O="K2O(WT%)",
      Li="LI(PPM)", Sc="SC(PPM)", Ti="TI(PPM)", V="V(PPM)", Cr="CR(PPM)",
      Co="CO(PPM)", Ni="NI(PPM)", Cu="CU(PPM)", Zn="ZN(PPM)", As="AS(PPM)",
      Rb="RB(PPM)", Sr="SR(PPM)", Y="Y(PPM)", Zr="ZR(PPM)", Nb="NB(PPM)",
      Cd="CD(PPM)", Cs="CS(PPM)", Ba="BA(PPM)", La="LA(PPM)", Ce="CE(PPM)",
      Pr="PR(PPM)", Nd="ND(PPM)", Sm="SM(PPM)", Eu="EU(PPM)", Gd="GD(PPM)",
      Tb="TB(PPM)", Dy="DY(PPM)", Ho="HO(PPM)", Er="ER(PPM)", Tm="TM(PPM)",
      Yb="YB(PPM)", Lu="LU(PPM)", Hf="HF(PPM)", Ta="TA(PPM)", Pb="PB(PPM)",
      Th="TH(PPM)", U="U(PPM)", Sr87_Sr86=SR87_SR86, Nd143_Nd144=ND143_ND144,
      Pb206_Pb204=PB206_PB204, Pb207_Pb204=PB207_PB204,
      Pb208_Pb204=PB208_PB204) %>%
    return(georoc_output_renamed)
}

## rename_georoc()
rename_georoc <- function(georoc_output) {
  georoc_output_renamed <- georoc_output %>%
    rename(
      Sample=id, lat=LATITUDE_MIN, long=LONGITUDE_MIN,
      SiO2="SIO2(WT%)", TiO2="TIO2(WT%)", Al2O3="AL2O3(WT%)", Fe2O3="FE2O3(WT%)",
      FeO="FEO(WT%)", MnO="MNO(WT%)",MgO="MGO(WT%)", CaO="CAO(WT%)",
      Na2O="NA2O(WT%)", K2O="K2O(WT%)",
      Li="LI(PPM)", Sc="SC(PPM)", Ti="TI(PPM)", V="V(PPM)", Cr="CR(PPM)",
      Co="CO(PPM)", Ni="NI(PPM)", Cu="CU(PPM)", Zn="ZN(PPM)", As="AS(PPM)",
      Rb="RB(PPM)", Sr="SR(PPM)", Y="Y(PPM)", Zr="ZR(PPM)", Nb="NB(PPM)",
      Cd="CD(PPM)", Cs="CS(PPM)", Ba="BA(PPM)", La="LA(PPM)", Ce="CE(PPM)",
      Pr="PR(PPM)", Nd="ND(PPM)", Sm="SM(PPM)", Eu="EU(PPM)", Gd="GD(PPM)",
      Tb="TB(PPM)", Dy="DY(PPM)", Ho="HO(PPM)", Er="ER(PPM)", Tm="TM(PPM)",
      Yb="YB(PPM)", Lu="LU(PPM)", Hf="HF(PPM)", Ta="TA(PPM)", Pb="PB(PPM)",
      Th="TH(PPM)", U="U(PPM)", Sr87_Sr86=SR87_SR86, Nd143_Nd144=ND143_ND144,
      Pb206_Pb204=PB206_PB204, Pb207_Pb204=PB207_PB204,
      Pb208_Pb204=PB208_PB204) %>%
    return(georoc_output_renamed)
}

## rename_OIB()
rename_OIB <- function(georoc_output) {
  georoc_output_renamed <- georoc_output %>%
    rename(
      Location=Location, Sample=id, lat=LATITUDE_MIN,long=LONGITUDE_MIN,
      SiO2="SIO2(WT%)", TiO2="TIO2(WT%)", Al2O3="AL2O3(WT%)", MnO="MNO(WT%)",
      MgO="MGO(WT%)", CaO="CAO(WT%)",Na2O="NA2O(WT%)", K2O="K2O(WT%)",
      Li="LI(PPM)", Sc="SC(PPM)", Ti="TI(PPM)", V="V(PPM)", Cr="CR(PPM)",
      Co="CO(PPM)", Ni="NI(PPM)", Cu="CU(PPM)", Zn="ZN(PPM)", As="AS(PPM)",
      Rb="RB(PPM)", Sr="SR(PPM)", Y="Y(PPM)", Zr="ZR(PPM)", Nb="NB(PPM)",
      Cd="CD(PPM)", Cs="CS(PPM)", Ba="BA(PPM)", La="LA(PPM)", Ce="CE(PPM)",
      Pr="PR(PPM)", Nd="ND(PPM)", Sm="SM(PPM)", Eu="EU(PPM)", Gd="GD(PPM)",
      Tb="TB(PPM)", Dy="DY(PPM)", Ho="HO(PPM)", Er="ER(PPM)", Tm="TM(PPM)",
      Yb="YB(PPM)", Lu="LU(PPM)", Hf="HF(PPM)", Ta="TA(PPM)", Pb="PB(PPM)",
      Th="TH(PPM)", U="U(PPM)", Sr87_Sr86=SR87_SR86, Nd143_Nd144=ND143_ND144,
      Pb206_Pb204=PB206_PB204, Pb207_Pb204=PB207_PB204,
      Pb208_Pb204=PB208_PB204) %>%
    return(georoc_output_renamed)
}

## pofatu_archipelago_from_location_region()
pofatu_archipelago_from_location_region <- function(pofatu_output) {
  pofatu_output_with_archipelago <- pofatu_output %>%
    mutate(
      Archipelago = case_when(
        location_region == "AUSTRAL" ~ "austral islands",
        location_region == "CAROLINE" ~ "caroline islands",
        location_region == "COOK ISLANDS" ~ "cook islands",
        location_region == "FIJI" ~ "fijian islands",
        location_region == "FIJI/LAU_GROUP" ~ "fijian islands",
        location_region == "GAMBIER" ~ "gambier islands",
        location_region == "HAWAI'I" ~ "hawai'i islands",
        location_region == "HENDERSON" ~ "pitcairn islands",
        location_region == "MARQUESAS" ~ "marquesas islands",
        location_region == "NEW GUINEA" ~ "new guinea",
        location_region == "NEW ZEALAND" ~ "new zealand",
        location_region == "PITCAIRN" ~ "pitcairn islands",
        location_region == "RAPA NUI" ~ "rapa nui",
        location_region == "ROTUMA" ~ "fijian islands",
        location_region == "SAMOA" ~ "samoan islands",
        location_region == "SOCIETY" ~ "society islands",
        location_region == "SOLOMON" ~ "solomon islands",
        location_region == "SOLOMON/STA_CRUZ_GROUP" ~ "solomon islands",
        location_region == "TIMOR" ~ "timor",
        location_region == "TOKELAU" ~ "tokelau islands",
        location_region == "TONGA" ~ "tongan islands",
        location_region == "TUAMOTU" ~ "tuamotu islands",
        location_region == "TUVALU" ~ "tuvalu islands",
        location_region == "UVEA" ~ "uvea island",
        location_region == "VANUATU" ~ "vanuatu",
        TRUE ~ "na"))
  return(pofatu_output_with_archipelago)
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

## rename_pofatu_elements
##used##
rename_pofatu_elements <- function(pofatu_output) {
  pofatu_output_renamed <- pofatu_output %>%
    rename(Sample=sample_id, lat=location_latitude,
           long=location_longitude, SiO2=`SiO2 [%]`, TiO2=`TiO2 [%]`, Al2O3=`Al2O3 [%]`,
           Fe2O3=`Fe2O3 [%]`, FeO=`FeO [%]`,  MnO=`MnO [%]`, MgO=`MgO [%]`, CaO=`CaO [%]`,
           Na2O=`Na2O [%]`, K2O=`K2O [%]`,  Li=`Li [ppm]`, Sc=`Sc [ppm]`,
           Ti=`Ti [ppm]`, V=`V [ppm]`, Cr=`Cr [ppm]`, Co=`Co [ppm]`, Ni=`Ni [ppm]`,
           Cu=`Cu [ppm]`, Zn=`Zn [ppm]`, As=`As [ppm]`, Rb=`Rb [ppm]`,
           Sr=`Sr [ppm]`, Y=`Y [ppm]`, Zr=`Zr [ppm]`, Nb=`Nb [ppm]`,
           Cd=`Cd [ppm]`, Cs=`Cs [ppm]`, Ba=`Ba [ppm]`, La=`La [ppm]`,
           Ce=`Ce [ppm]`, Pr=`Pr [ppm]`, Nd=`Nd [ppm]`, Sm=`Sm [ppm]`,
           Eu=`Eu [ppm]`, Gd=`Gd [ppm]`, Tb=`Tb [ppm]`, Dy=`Dy [ppm]`,
           Ho=`Ho [ppm]`, Er=`Er [ppm]`, Tm=`Tm [ppm]`, Yb=`Yb [ppm]`,
           Lu=`Lu [ppm]`, Hf=`Hf [ppm]`, Ta=`Ta [ppm]`, Pb=`Pb [ppm]`,
           Th=`Th [ppm]`, U=`U [ppm]`)
  return(pofatu_output_renamed)
}


