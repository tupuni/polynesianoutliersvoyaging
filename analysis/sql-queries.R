require(tidyverse)
require(RSQLite)
require(DBI)

georoc <- dbConnect(RSQLite::SQLite(), path_to_georoc)
pofatu <- dbConnect(RSQLite::SQLite(), path_to_pofatu)

#### E-11-03 ####
ranges_s_IAB[1,36:39] %>% round(digits = 6)
ranges_s_IAB[1,40:45] %>% round(digits = 3)
q1 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
(LONGITUDE_MAX > 90 OR LONGITUDE_MAX < 0) AND
LATITUDE_MAX < 20 AND TECTONIC_SETTING='CONVERGENT MARGIN' AND
(ND143_ND144 > 0.512427 AND ND143_ND144 < 0.513453 AND
SR87_SR86 > 0.703748 AND SR87_SR86 < 0.705156 AND
PB206_PB204 > 18.291 AND PB206_PB204 < 18.475 AND
PB207_PB204 > 15.457 AND PB207_PB204 < 15.613 AND
PB208_PB204 > 38.268 AND PB208_PB204 < 38.652)") %>%
  get_georoc_location() %>% dplyr::filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
q1 %>% group_by(Location) %>% tally()

ranges_s_IAB[1,1:35]
q2 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
(`SIO2(WT%)` > 68.6 AND `SIO2(WT%)` < 71.6 AND
`K2O(WT%)` > 2.62 AND `K2O(WT%)` < 5.62) AND
(file_id= '2022-06-PVFZCE_BISMARCK_ARC_NEW_BRITAIN_ARC.csv' OR
file_id= '2022-06-PVFZCE_LUZON_ARC.csv' OR
file_id= '2022-06-PVFZCE_NEW_HEBRIDES_ARC_VANUATU_ARCHIPELAGO.csv' OR
file_id= '2022-06-PVFZCE_SULAWESI_ARC.csv' OR
file_id= '2022-06-PVFZCE_SOLOMON_ISLAND_ARC.csv' OR
file_id= '2022-06-PVFZCE_SULAWESI_ARC.csv' OR
file_id= '2022-06-PVFZCE_TONGA_ARC.csv')") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
q2 %>% group_by(Location) %>% tally()

q3 <- dbGetQuery(pofatu,
"SELECT s.id AS sample_id, s.sample_category, s.location_region,
s.location_subregion, s.location_latitude, s.location_longitude,
max(CASE WHEN m.parameter='SiO2 [%]' then m.value END) AS 'SiO2 [%]',
max(CASE WHEN m.parameter='TiO2 [%]' then m.value END) AS 'TiO2 [%]',
max(CASE WHEN m.parameter='Al2O3 [%]' then m.value END) AS 'Al2O3 [%]',
max(CASE WHEN m.parameter='Fe2O3 [%]' then m.value END) AS 'Fe2O3 [%]',
max(CASE WHEN m.parameter='FeO [%]' then m.value END) AS 'FeO [%]',
max(CASE WHEN m.parameter='MnO [%]' then m.value END) AS 'MnO [%]',
max(CASE WHEN m.parameter='MgO [%]' then m.value END) AS 'MgO [%]',
max(CASE WHEN m.parameter='CaO [%]' then m.value END) AS 'CaO [%]',
max(CASE WHEN m.parameter='Na2O [%]' then m.value END) AS 'Na2O [%]',
max(CASE WHEN m.parameter='K2O [%]' then m.value END) AS 'K2O [%]',
max(CASE WHEN m.parameter='Li [ppm]' then m.value END) AS 'Li [ppm]',
max(CASE WHEN m.parameter='Sc [ppm]' then m.value END) AS 'Sc [ppm]',
max(CASE WHEN m.parameter='Ti [ppm]' then m.value END) AS 'Ti [ppm]',
max(CASE WHEN m.parameter='V [ppm]' then m.value END) AS 'V [ppm]',
max(CASE WHEN m.parameter='Cr [ppm]' then m.value END) AS 'Cr [ppm]',
max(CASE WHEN m.parameter='Co [ppm]' then m.value END) AS 'Co [ppm]',
max(CASE WHEN m.parameter='Ni [ppm]' then m.value END) AS 'Ni [ppm]',
max(CASE WHEN m.parameter='Cu [ppm]' then m.value END) AS 'Cu [ppm]',
max(CASE WHEN m.parameter='Zn [ppm]' then m.value END) AS 'Zn [ppm]',
max(CASE WHEN m.parameter='As [ppm]' then m.value END) AS 'As [ppm]',
max(CASE WHEN m.parameter='Rb [ppm]' then m.value END) AS 'Rb [ppm]',
max(CASE WHEN m.parameter='Sr [ppm]' then m.value END) AS 'Sr [ppm]',
max(CASE WHEN m.parameter='Y [ppm]' then m.value END) AS 'Y [ppm]',
max(CASE WHEN m.parameter='Zr [ppm]' then m.value END) AS 'Zr [ppm]',
max(CASE WHEN m.parameter='Nb [ppm]' then m.value END) AS 'Nb [ppm]',
max(CASE WHEN m.parameter='Cd [ppm]' then m.value END) AS 'Cd [ppm]',
max(CASE WHEN m.parameter='Cs [ppm]' then m.value END) AS 'Cs [ppm]',
max(CASE WHEN m.parameter='Ba [ppm]' then m.value END) AS 'Ba [ppm]',
max(CASE WHEN m.parameter='La [ppm]' then m.value END) AS 'La [ppm]',
max(CASE WHEN m.parameter='Ce [ppm]' then m.value END) AS 'Ce [ppm]',
max(CASE WHEN m.parameter='Pr [ppm]' then m.value END) AS 'Pr [ppm]',
max(CASE WHEN m.parameter='Nd [ppm]' then m.value END) AS 'Nd [ppm]',
max(CASE WHEN m.parameter='Sm [ppm]' then m.value END) AS 'Sm [ppm]',
max(CASE WHEN m.parameter='Eu [ppm]' then m.value END) AS 'Eu [ppm]',
max(CASE WHEN m.parameter='Gd [ppm]' then m.value END) AS 'Gd [ppm]',
max(CASE WHEN m.parameter='Tb [ppm]' then m.value END) AS 'Tb [ppm]',
max(CASE WHEN m.parameter='Dy [ppm]' then m.value END) AS 'Dy [ppm]',
max(CASE WHEN m.parameter='Ho [ppm]' then m.value END) AS 'Ho [ppm]',
max(CASE WHEN m.parameter='Er [ppm]' then m.value END) AS 'Er [ppm]',
max(CASE WHEN m.parameter='Tm [ppm]' then m.value END) AS 'Tm [ppm]',
max(CASE WHEN m.parameter='Yb [ppm]' then m.value END) AS 'Yb [ppm]',
max(CASE WHEN m.parameter='Lu [ppm]' then m.value END) AS 'Lu [ppm]',
max(CASE WHEN m.parameter='Hf [ppm]' then m.value END) AS 'Hf [ppm]',
max(CASE WHEN m.parameter='Ta [ppm]' then m.value END) AS 'Ta [ppm]',
max(CASE WHEN m.parameter='Tl [ppm]' then m.value END) AS 'Tl [ppm]',
max(CASE WHEN m.parameter='Pb [ppm]' then m.value END) AS 'Pb [ppm]',
max(CASE WHEN m.parameter='Th [ppm]' then m.value END) AS 'Th [ppm]',
max(CASE WHEN m.parameter='U [ppm]' then m.value END) AS 'U [ppm]',
max(CASE WHEN m.parameter='Nd143_Nd144' then m.value END) AS 'Nd143_Nd144',
max(CASE WHEN m.parameter='Sr87_Sr86' then m.value END) AS 'Sr87_Sr86',
max(CASE WHEN m.parameter='Pb206_Pb204' then m.value END) AS 'Pb206_Pb204',
max(CASE WHEN m.parameter='Pb207_Pb204' then m.value END) AS 'Pb207_Pb204',
max(CASE WHEN m.parameter='Pb208_Pb204' then m.value END) AS 'Pb208_Pb204'
FROM 'samples.csv' AS s JOIN 'measurements.csv' AS m ON s.id=m.sample_id
WHERE s.sample_category = 'SOURCE' AND s.location_region = 'VANUATU' AND
m.parameter IN ('SiO2 [%]', 'TiO2 [%]', 'Al2O3 [%]', 'MnO [%]', 'MgO [%]', 'Fe2O3 [%]', 'FeO [%]',
'CaO [%]','Na2O [%]', 'K2O [%]', 'Li [ppm]', 'Sc [ppm]', 'Ti [ppm]', 'V [ppm]',
'Cr [ppm]', 'Co [ppm]', 'Ni [ppm]', 'Cu [ppm]', 'Zn [ppm]', 'As [ppm]',
'Rb [ppm]', 'Sr [ppm]', 'Y [ppm]', 'Zr [ppm]', 'Nb [ppm]', 'Cd [ppm]',
'Cs [ppm]', 'Ba [ppm]', 'La [ppm]', 'Ce [ppm]', 'Pr [ppm]', 'Nd [ppm]',
'Sm [ppm]', 'Eu [ppm]', 'Gd [ppm]', 'Tb [ppm]', 'Dy [ppm]', 'Ho [ppm]',
'Er [ppm]', 'Tm [ppm]', 'Yb [ppm]', 'Lu [ppm]', 'Hf [ppm]', 'Ta [ppm]',
'Tl [ppm]', 'Pb [ppm]', 'Th [ppm]', 'U [ppm]', 'Nd143_Nd144', 'Sr87_Sr86',
'Pb206_Pb204', 'Pb207_Pb204', 'Pb208_Pb204') GROUP BY sample_id") %>%
  rename_pofatu_elements() %>% pofatu_location() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204) %>%
  filter(SiO2 > 68.6 & SiO2 < 71.6 & K2O > 2.62 & K2O < 5.62)
q3 %>% group_by(Location) %>% tally()


#### E-11-06_E-11-07 ####
ranges_s_IAB[2:3,36:39] %>% round(digits = 6)
ranges_s_IAB[2:3,40:45] %>% round(digits = 3)
q4 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='CONVERGENT MARGIN' AND
((ND143_ND144 > 0.512414 AND ND143_ND144 < 0.51344 AND
SR87_SR86 > 0.703489 AND SR87_SR86 < 0.704897 AND
PB206_PB204 > 18.464 AND PB206_PB204 < 18.65 AND
PB207_PB204 > 15.49 AND PB207_PB204 < 15.646 AND
PB208_PB204 > 38.338 AND PB208_PB204 < 38.724) OR
(ND143_ND144 > 0.512419 AND ND143_ND144 < 0.513445 AND
SR87_SR86 > 0.703451 AND SR87_SR86 < 0.704859 AND
PB206_PB204 > 18.441 AND PB206_PB204 < 18.627 AND
PB207_PB204 > 15.487 AND PB207_PB204 < 15.643 AND
PB208_PB204 > 38.313 AND PB208_PB204 < 38.699))") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
q4 %>% group_by(Location) %>% tally()

ranges_s_IAB[2:3,1:9]
q5 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='CONVERGENT MARGIN' AND
((`SIO2(WT%)` > 48.1 AND `SIO2(WT%)` < 51.1 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.26 AND
ND143_ND144 > 0.512414 AND ND143_ND144 < 0.51344 AND
SR87_SR86 > 0.703489 AND SR87_SR86 < 0.704897) OR
(`SIO2(WT%)` > 47.4 AND `SIO2(WT%)` < 50.4 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.31 AND
ND143_ND144 > 0.512419 AND ND143_ND144 < 0.513445 AND
SR87_SR86 > 0.703451 AND SR87_SR86 < 0.704859)) AND
(file_id= '2022-06-PVFZCE_BISMARCK_ARC_NEW_BRITAIN_ARC.csv' OR
file_id= '2022-06-PVFZCE_LUZON_ARC.csv' OR
file_id= '2022-06-PVFZCE_NEW_HEBRIDES_ARC_VANUATU_ARCHIPELAGO.csv' OR
file_id= '2022-06-PVFZCE_SULAWESI_ARC.csv' OR
file_id= '2022-06-PVFZCE_TONGA_ARC.csv')") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
q5 %>% group_by(Location) %>% tally()

#### K-12-28 ####
ranges_s_IAB[4,1]
ranges_s_IAB[4,40:45] %>% round(digits = 3)
q6 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
(LONGITUDE_MAX > 90 OR LONGITUDE_MAX < 0) AND
LATITUDE_MAX < 20 AND TECTONIC_SETTING='CONVERGENT MARGIN' AND
(ND143_ND144 > 0.512513 AND ND143_ND144 < 0.513539 AND
SR87_SR86 > 0.703182 AND SR87_SR86 < 0.70459 AND
PB206_PB204 > 18.147 AND PB206_PB204 < 18.329 AND
PB207_PB204 > 15.467 AND PB207_PB204 < 15.623 AND
PB208_PB204 > 38.064 AND PB208_PB204 < 38.446)") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
q6 %>% group_by(Location) %>% tally()

q7 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
(LONGITUDE_MAX > 90 OR LONGITUDE_MAX < 0) AND
LATITUDE_MAX < 20 AND TECTONIC_SETTING='CONVERGENT MARGIN' AND
(`SIO2(WT%)` > 52.1 AND `SIO2(WT%)` < 55.1 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 1.55 AND
ND143_ND144 > 0.512513 AND ND143_ND144 < 0.513539 AND
SR87_SR86 > 0.703182 AND SR87_SR86 < 0.70459) AND
(file_id= '2022-06-PVFZCE_BISMARCK_ARC_NEW_BRITAIN_ARC.csv' OR
file_id= '2022-06-PVFZCE_LUZON_ARC.csv' OR
file_id= '2022-06-PVFZCE_NEW_HEBRIDES_ARC_VANUATU_ARCHIPELAGO.csv' OR
file_id= '2022-06-PVFZCE_SULAWESI_ARC.csv')") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
q7 %>% group_by(Location) %>% tally()

#### K-12-29 ####
ranges_s_IAB[5,36:39] %>% round(digits = 6)
ranges_s_IAB[5,40:45] %>% round(digits = 3)
q8 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
(LONGITUDE_MAX > 90 OR LONGITUDE_MAX < 0) AND
LATITUDE_MAX < 20 AND TECTONIC_SETTING='CONVERGENT MARGIN' AND
(SR87_SR86 > 0.703693 AND SR87_SR86 < 0.705101 AND
ND143_ND144 > 0.512403 AND ND143_ND144 < 0.513429 AND
PB206_PB204 > 18.454 AND PB206_PB204 < 18.64 AND
PB207_PB204 > 15.492 AND PB207_PB204 < 15.648 AND
PB208_PB204 > 38.234 AND PB208_PB204 < 38.618)") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
q8 %>% group_by(Location) %>% tally() #the island arcs represented

# The Yap arc has less than 10 samples isotopically documented for Palau islands
# and only two samples documented for all 5 isotope ratios !
yap <- dbGetQuery(georoc,
"SELECT file_id, LOCATION, PB206_PB204, PB207_PB204, PB208_PB204, ND143_ND144, SR87_SR86
FROM 'sample' WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
file_id = '2022-06-PVFZCE_YAP_ARC.csv'")
yap

ranges_s_IAB[5,]
q9 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
(LONGITUDE_MAX > 90 OR LONGITUDE_MAX < 0) AND
LATITUDE_MAX < 20 AND TECTONIC_SETTING='CONVERGENT MARGIN' AND
(`K2O(WT%)` > 0.14 AND `K2O(WT%)` < 3.14 AND
`RB(PPM)` > 4.85 AND `RB(PPM)` < 14.55 AND
`SR(PPM)` > 269 AND `SR(PPM)` < 806 AND
`SM(PPM)` > 1.635 AND `SM(PPM)` < 4.905 AND
`YB(PPM)` > 1.05 AND `YB(PPM)` < 3.15)") %>%
  get_georoc_location() %>% rename_georoc() %>%
  dplyr::filter(Location != "na" & Location != "New Zealand" & Location != "Banda Arc") %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204) #%>%
  #dplyr::filter(Ti > 2388 & Ti < 7165)
q9 %>% group_by(Location) %>% tally() #the island arcs represented

# ! Overall, only 68 samples are geochemically documented for Yap arc (Palau islands) !
yap <- dbGetQuery(georoc,
"SELECT file_id,LOCATION,`SIO2(WT%)`,`TIO2(WT%)`,`AL2O3(WT%)`,`MNO(WT%)`,
`MGO(WT%)`,`CAO(WT%)`,`NA2O(WT%)`,`K2O(WT%)`,`LI(PPM)`,`SC(PPM)`,`TI(PPM)`,
`V(PPM)`,`CR(PPM)`,`CO(PPM)`,`NI(PPM)`,`CU(PPM)`,`ZN(PPM)`,`AS(PPM)`,`RB(PPM)`,
`SR(PPM)`,`Y(PPM)`,`ZR(PPM)`,`NB(PPM)`,`CD(PPM)`,`CS(PPM)`,`BA(PPM)`,`LA(PPM)`,
`CE(PPM)`,`PR(PPM)`,`ND(PPM)`,`SM(PPM)`,`EU(PPM)`,`GD(PPM)`,`TB(PPM)`,`DY(PPM)`,
`HO(PPM)`,`ER(PPM)`,`TM(PPM)`,`YB(PPM)`,`LU(PPM)`,`HF(PPM)`,`TA(PPM)`,`PB(PPM)`,
 `TH(PPM)`,`U(PPM)`
FROM 'sample' WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
file_id = '2022-06-PVFZCE_YAP_ARC.csv'")
yap


#### Emae-Taumako OIB ####
ranges_s_OIB[1:6,36:39] %>% round(digits = 6)
ranges_s_OIB[1:6,40:45] %>% round(digits = 3)
q10 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
((ND143_ND144 > 0.512341 AND ND143_ND144 < 0.513367 AND
SR87_SR86 > 0.704586 AND SR87_SR86 < 0.705996 AND
PB206_PB204 > 18.658 AND PB206_PB204 < 19.034 AND
PB207_PB204 > 15.430 AND PB207_PB204 < 15.742 AND
PB208_PB204 > 38.434 AND PB208_PB204 < 39.210) OR
(ND143_ND144 > 0.512342 AND ND143_ND144 < 0.513368 AND
SR87_SR86 > 0.704583 AND SR87_SR86 < 0.705993 AND
PB206_PB204 > 18.701 AND PB206_PB204 < 19.079 AND
PB207_PB204 > 15.431 AND PB207_PB204 < 15.743 AND
PB208_PB204 > 38.482 AND PB208_PB204 < 39.260) OR
(ND143_ND144 > 0.512358 AND ND143_ND144 < 0.513384 AND
SR87_SR86 > 0.704403 AND SR87_SR86 < 0.705813 AND
PB206_PB204 > 18.722 AND PB206_PB204 < 19.100 AND
PB207_PB204 > 15.427 AND PB207_PB204 < 15.739 AND
PB208_PB204 > 38.472 AND PB208_PB204 < 39.250) OR
(ND143_ND144 > 0.512342 AND ND143_ND144 < 0.513368 AND
SR87_SR86 > 0.704502 AND SR87_SR86 < 0.705912 AND
PB206_PB204 > 18.720 AND PB206_PB204 < 19.098 AND
PB207_PB204 > 15.445 AND PB207_PB204 < 15.757 AND
PB208_PB204 > 38.505 AND PB208_PB204 < 39.283) OR
(ND143_ND144 > 0.512359 AND ND143_ND144 < 0.513385 AND
SR87_SR86 > 0.704412 AND SR87_SR86 < 0.705822 AND
PB206_PB204 > 18.736 AND PB206_PB204 < 19.114 AND
PB207_PB204 > 15.429 AND PB207_PB204 < 15.741 AND
PB208_PB204 > 38.508 AND PB208_PB204 < 39.286) OR
(ND143_ND144 > 0.512348 AND ND143_ND144 < 0.513374 AND
SR87_SR86 > 0.704465 AND SR87_SR86 < 0.705875 AND
PB206_PB204 > 18.624 AND PB206_PB204 < 19 AND
PB207_PB204 > 15.425 AND PB207_PB204 < 15.737 AND
PB208_PB204 > 38.362 AND PB208_PB204 < 39.138))") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>% dplyr::select(
    Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
    Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
    Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
    Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

q11 <- dbGetQuery(pofatu,
"SELECT s.id AS sample_id, s.sample_category, s.location_region,
s.location_subregion, s.location_latitude, s.location_longitude,
max(CASE WHEN m.parameter='SiO2 [%]' then m.value END) AS 'SiO2 [%]',
max(CASE WHEN m.parameter='TiO2 [%]' then m.value END) AS 'TiO2 [%]',
max(CASE WHEN m.parameter='Al2O3 [%]' then m.value END) AS 'Al2O3 [%]',
max(CASE WHEN m.parameter='Fe2O3 [%]' then m.value END) AS 'Fe2O3 [%]',
max(CASE WHEN m.parameter='FeO [%]' then m.value END) AS 'FeO [%]',
max(CASE WHEN m.parameter='MnO [%]' then m.value END) AS 'MnO [%]',
max(CASE WHEN m.parameter='MgO [%]' then m.value END) AS 'MgO [%]',
max(CASE WHEN m.parameter='CaO [%]' then m.value END) AS 'CaO [%]',
max(CASE WHEN m.parameter='Na2O [%]' then m.value END) AS 'Na2O [%]',
max(CASE WHEN m.parameter='K2O [%]' then m.value END) AS 'K2O [%]',
max(CASE WHEN m.parameter='Li [ppm]' then m.value END) AS 'Li [ppm]',
max(CASE WHEN m.parameter='Sc [ppm]' then m.value END) AS 'Sc [ppm]',
max(CASE WHEN m.parameter='Ti [ppm]' then m.value END) AS 'Ti [ppm]',
max(CASE WHEN m.parameter='V [ppm]' then m.value END) AS 'V [ppm]',
max(CASE WHEN m.parameter='Cr [ppm]' then m.value END) AS 'Cr [ppm]',
max(CASE WHEN m.parameter='Co [ppm]' then m.value END) AS 'Co [ppm]',
max(CASE WHEN m.parameter='Ni [ppm]' then m.value END) AS 'Ni [ppm]',
max(CASE WHEN m.parameter='Cu [ppm]' then m.value END) AS 'Cu [ppm]',
max(CASE WHEN m.parameter='Zn [ppm]' then m.value END) AS 'Zn [ppm]',
max(CASE WHEN m.parameter='As [ppm]' then m.value END) AS 'As [ppm]',
max(CASE WHEN m.parameter='Rb [ppm]' then m.value END) AS 'Rb [ppm]',
max(CASE WHEN m.parameter='Sr [ppm]' then m.value END) AS 'Sr [ppm]',
max(CASE WHEN m.parameter='Y [ppm]' then m.value END) AS 'Y [ppm]',
max(CASE WHEN m.parameter='Zr [ppm]' then m.value END) AS 'Zr [ppm]',
max(CASE WHEN m.parameter='Nb [ppm]' then m.value END) AS 'Nb [ppm]',
max(CASE WHEN m.parameter='Cd [ppm]' then m.value END) AS 'Cd [ppm]',
max(CASE WHEN m.parameter='Cs [ppm]' then m.value END) AS 'Cs [ppm]',
max(CASE WHEN m.parameter='Ba [ppm]' then m.value END) AS 'Ba [ppm]',
max(CASE WHEN m.parameter='La [ppm]' then m.value END) AS 'La [ppm]',
max(CASE WHEN m.parameter='Ce [ppm]' then m.value END) AS 'Ce [ppm]',
max(CASE WHEN m.parameter='Pr [ppm]' then m.value END) AS 'Pr [ppm]',
max(CASE WHEN m.parameter='Nd [ppm]' then m.value END) AS 'Nd [ppm]',
max(CASE WHEN m.parameter='Sm [ppm]' then m.value END) AS 'Sm [ppm]',
max(CASE WHEN m.parameter='Eu [ppm]' then m.value END) AS 'Eu [ppm]',
max(CASE WHEN m.parameter='Gd [ppm]' then m.value END) AS 'Gd [ppm]',
max(CASE WHEN m.parameter='Tb [ppm]' then m.value END) AS 'Tb [ppm]',
max(CASE WHEN m.parameter='Dy [ppm]' then m.value END) AS 'Dy [ppm]',
max(CASE WHEN m.parameter='Ho [ppm]' then m.value END) AS 'Ho [ppm]',
max(CASE WHEN m.parameter='Er [ppm]' then m.value END) AS 'Er [ppm]',
max(CASE WHEN m.parameter='Tm [ppm]' then m.value END) AS 'Tm [ppm]',
max(CASE WHEN m.parameter='Yb [ppm]' then m.value END) AS 'Yb [ppm]',
max(CASE WHEN m.parameter='Lu [ppm]' then m.value END) AS 'Lu [ppm]',
max(CASE WHEN m.parameter='Hf [ppm]' then m.value END) AS 'Hf [ppm]',
max(CASE WHEN m.parameter='Ta [ppm]' then m.value END) AS 'Ta [ppm]',
max(CASE WHEN m.parameter='Tl [ppm]' then m.value END) AS 'Tl [ppm]',
max(CASE WHEN m.parameter='Pb [ppm]' then m.value END) AS 'Pb [ppm]',
max(CASE WHEN m.parameter='Th [ppm]' then m.value END) AS 'Th [ppm]',
max(CASE WHEN m.parameter='U [ppm]' then m.value END) AS 'U [ppm]',
max(CASE WHEN m.parameter='Nd143_Nd144' then m.value END) AS 'Nd143_Nd144',
max(CASE WHEN m.parameter='Sr87_Sr86' then m.value END) AS 'Sr87_Sr86',
max(CASE WHEN m.parameter='Pb206_Pb204' then m.value END) AS 'Pb206_Pb204',
max(CASE WHEN m.parameter='Pb207_Pb204' then m.value END) AS 'Pb207_Pb204',
max(CASE WHEN m.parameter='Pb208_Pb204' then m.value END) AS 'Pb208_Pb204'
FROM 'samples.csv' AS s JOIN 'measurements.csv' AS m ON s.id=m.sample_id
WHERE s.sample_category = 'SOURCE' AND
m.parameter IN ('SiO2 [%]', 'TiO2 [%]', 'Al2O3 [%]', 'Fe2O3 [%]', 'FeO [%]',
'MnO [%]', 'MgO [%]', 'CaO [%]', 'Na2O [%]', 'K2O [%]',
'Li [ppm]', 'Sc [ppm]', 'Ti [ppm]', 'V [ppm]',
'Cr [ppm]', 'Co [ppm]', 'Ni [ppm]', 'Cu [ppm]', 'Zn [ppm]', 'As [ppm]',
'Rb [ppm]', 'Sr [ppm]', 'Y [ppm]', 'Zr [ppm]', 'Nb [ppm]', 'Cd [ppm]',
'Cs [ppm]', 'Ba [ppm]', 'La [ppm]', 'Ce [ppm]', 'Pr [ppm]', 'Nd [ppm]',
'Sm [ppm]', 'Eu [ppm]', 'Gd [ppm]', 'Tb [ppm]', 'Dy [ppm]', 'Ho [ppm]',
'Er [ppm]', 'Tm [ppm]', 'Yb [ppm]', 'Lu [ppm]', 'Hf [ppm]', 'Ta [ppm]',
'Tl [ppm]', 'Pb [ppm]', 'Th [ppm]', 'U [ppm]', 'Nd143_Nd144', 'Sr87_Sr86',
'Pb206_Pb204', 'Pb207_Pb204', 'Pb208_Pb204') GROUP BY sample_id") %>%
  rename_pofatu_elements() %>% pofatu_location() %>% dplyr::filter((
    Nd143_Nd144 > 0.512341 & Nd143_Nd144 < 0.513367 &
      Sr87_Sr86 > 0.704586 & Sr87_Sr86 < 0.705996 &
      Pb206_Pb204 > 18.658 & Pb206_Pb204 < 19.034 &
      Pb207_Pb204 > 15.430 & Pb207_Pb204 < 15.742 &
      Pb208_Pb204 > 38.434 & Pb208_Pb204 < 39.210) |
      (Nd143_Nd144 > 0.512342 & Nd143_Nd144 < 0.513368 &
         Sr87_Sr86 > 0.704583 & Sr87_Sr86 < 0.705993 &
         Pb206_Pb204 > 18.701 & Pb206_Pb204 < 19.079 &
         Pb207_Pb204 > 15.431 & Pb207_Pb204 < 15.743 &
         Pb208_Pb204 > 38.482 & Pb208_Pb204 < 39.260) |
      (Nd143_Nd144 > 0.512358 & Nd143_Nd144 < 0.513384 &
         Sr87_Sr86 > 0.704403 & Sr87_Sr86 < 0.705813 &
         Pb206_Pb204 > 18.722 & Pb206_Pb204 < 19.1 &
         Pb207_Pb204 > 15.427 & Pb207_Pb204 < 15.739 &
         Pb208_Pb204 > 38.472 & Pb208_Pb204 < 39.25) |
      (Nd143_Nd144 > 0.512342 & Nd143_Nd144 < 0.513368 &
          Sr87_Sr86 > 0.704502 & Sr87_Sr86 < 0.705912 &
          Pb206_Pb204 > 18.72 & Pb206_Pb204 < 19.098 &
          Pb207_Pb204 > 15.445 & Pb207_Pb204 < 15.757 &
          Pb208_Pb204 > 38.505 & Pb208_Pb204 < 39.283) |
      (Nd143_Nd144 > 0.512359 & Nd143_Nd144 < 0.513385 &
         Sr87_Sr86 > 0.704412 & Sr87_Sr86 < 0.705822 &
         Pb206_Pb204 > 18.736 & Pb206_Pb204 < 19.114 &
         Pb207_Pb204 > 15.429 & Pb207_Pb204 < 15.741 &
         Pb208_Pb204 > 38.508 & Pb208_Pb204 < 39.286) |
      (Nd143_Nd144 > 0.512348 & Nd143_Nd144 < 0.513374 &
          Sr87_Sr86 > 0.704465 & Sr87_Sr86 < 0.705875 &
          Pb206_Pb204 > 18.624 & Pb206_Pb204 < 19 &
          Pb207_Pb204 > 15.425 & Pb207_Pb204 < 15.737 &
          Pb208_Pb204 > 38.362 & Pb208_Pb204 < 39.138)) %>%
  dplyr::filter(Location %in% c(
    "Caroline islands","Samoan islands","Austral-Cook chain","Society islands",
    "Hawai'i islands","Marquesas islands","Pitcairn-Gambier chain","Rapa Nui")) %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

ranges_s_OIB[1:6,2:35]
q12 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
((`SIO2(WT%)` > 45.2 AND `SIO2(WT%)` < 48.2 AND
`MGO(WT%)` > 3.08 AND `MGO(WT%)` < 6.08 AND
`NA2O(WT%)` > 2.08 AND `NA2O(WT%)` < 5.08 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.9 AND
`BA(PPM)` > 160 AND `BA(PPM)` < 480 AND
`TH(PPM)` > 1.95 AND `TH(PPM)` < 5.84 AND
`ND(PPM)` > 25.5 AND `ND(PPM)` < 76.5 AND
`NB(PPM)` > 24.8 AND `NB(PPM)` < 74.5 AND
`SM(PPM)` > 6.2 AND `SM(PPM)` < 18.6 AND
`YB(PPM)` > 1.6 AND `YB(PPM)` < 4.8) OR
(`SIO2(WT%)` > 44.3 AND `SIO2(WT%)` < 47.3 AND
`MGO(WT%)` > 3.52 AND `MGO(WT%)` < 6.52 AND
`NA2O(WT%)` > 2 AND `NA2O(WT%)` < 5 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.82 AND
`BA(PPM)` > 137 AND `BA(PPM)` < 413 AND
`TH(PPM)` > 1.86 AND `TH(PPM)` < 5.60 AND
`ND(PPM)` > 25.6 AND `ND(PPM)` < 76.9 AND
`NB(PPM)` > 24.7 AND `NB(PPM)` < 74.1 AND
`SM(PPM)` > 6 AND `SM(PPM)` < 18.1 AND
`YB(PPM)` > 1.62 AND `YB(PPM)` < 4.86) OR
(`SIO2(WT%)` > 43.5 AND `SIO2(WT%)` < 46.5 AND
`MGO(WT%)` > 3.79 AND `MGO(WT%)` < 6.79 AND
`NA2O(WT%)` > 1.9 AND `NA2O(WT%)` < 4.9 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.78 AND
`BA(PPM)` > 126 AND `BA(PPM)` < 378 AND
`TH(PPM)` > 1.58 AND `TH(PPM)` < 4.74 AND
`ND(PPM)` > 23.7 AND `ND(PPM)` < 71.1 AND
`NB(PPM)` > 21 AND `NB(PPM)` < 63.1 AND
`SM(PPM)` > 5.65 AND `SM(PPM)` < 16.95 AND
`YB(PPM)` > 1.56 AND `YB(PPM)` < 4.68) OR
(`SIO2(WT%)` > 45.9 AND `SIO2(WT%)` < 48.9 AND
`MGO(WT%)` > 3.44 AND `MGO(WT%)` < 6.44 AND
`NA2O(WT%)` > 2.18 AND `NA2O(WT%)` < 5.18 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.99 AND
`BA(PPM)` > 162 AND `BA(PPM)` < 488 AND
`TH(PPM)` > 2.13 AND `TH(PPM)` < 6.39 AND
`ND(PPM)` > 26.8 AND `ND(PPM)` < 80.5 AND
`NB(PPM)` > 24.3 AND `NB(PPM)` < 72.9 AND
`SM(PPM)` > 6.35 AND `SM(PPM)` < 19 AND
`YB(PPM)` > 1.49 AND `YB(PPM)` < 4.46) OR
(`SIO2(WT%)` > 44.1 AND `SIO2(WT%)` < 47.1 AND
`MGO(WT%)` > 4.15 AND `MGO(WT%)` < 7.15 AND
`NA2O(WT%)` > 1.8 AND `NA2O(WT%)` < 4.8 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.72 AND
`BA(PPM)` > 128 AND `BA(PPM)` < 385 AND
`TH(PPM)` > 1.58 AND `TH(PPM)` < 4.74 AND
`ND(PPM)` > 21.7 AND `ND(PPM)` < 65.1 AND
`NB(PPM)` > 20.4 AND `NB(PPM)` < 61.2 AND
`SM(PPM)` > 5.35 AND `SM(PPM)` < 16 AND
`YB(PPM)` > 1.4 AND `YB(PPM)` < 4.19) OR
(`SIO2(WT%)` > 45.7 AND `SIO2(WT%)` < 48.7 AND
`MGO(WT%)` > 3.10 AND `MGO(WT%)` < 6.10 AND
`NA2O(WT%)` > 2.08 AND `NA2O(WT%)` < 5.08 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.85 AND
`BA(PPM)` > 143 AND `BA(PPM)` < 431 AND
`TH(PPM)` > 1.84 AND `TH(PPM)` < 5.52 AND
`ND(PPM)` > 27.3 AND `ND(PPM)` < 82 AND
`NB(PPM)` > 23.5 AND `NB(PPM)` < 70.5 AND
`SM(PPM)` > 6.65 AND `SM(PPM)` < 19.95 AND
`YB(PPM)` > 1.57 AND `YB(PPM)` < 4.71))") %>%
  dplyr::filter(!grepl("MURUROA|FANGATAUFA",LOCATION)) %>%
  get_georoc_location() %>% dplyr::filter(Location != "na") %>% rename_georoc() %>%
  dplyr::filter(Location %in% c(
    "Samoan islands","Austral-Cook chain","Society islands","Marquesas islands")) %>%
  Ti_from_TiO2() %>% K_from_K2O() %>% dplyr::select(
    Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
    Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
    Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
    Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

q13 <- dbGetQuery(pofatu,
"SELECT s.id AS sample_id, s.sample_category, s.location_region,
s.location_subregion, s.location_latitude, s.location_longitude,
max(CASE WHEN m.parameter='SiO2 [%]' then m.value END) AS 'SiO2 [%]',
max(CASE WHEN m.parameter='TiO2 [%]' then m.value END) AS 'TiO2 [%]',
max(CASE WHEN m.parameter='Al2O3 [%]' then m.value END) AS 'Al2O3 [%]',
max(CASE WHEN m.parameter='Fe2O3 [%]' then m.value END) AS 'Fe2O3 [%]',
max(CASE WHEN m.parameter='FeO [%]' then m.value END) AS 'FeO [%]',
max(CASE WHEN m.parameter='MnO [%]' then m.value END) AS 'MnO [%]',
max(CASE WHEN m.parameter='MgO [%]' then m.value END) AS 'MgO [%]',
max(CASE WHEN m.parameter='CaO [%]' then m.value END) AS 'CaO [%]',
max(CASE WHEN m.parameter='Na2O [%]' then m.value END) AS 'Na2O [%]',
max(CASE WHEN m.parameter='K2O [%]' then m.value END) AS 'K2O [%]',
max(CASE WHEN m.parameter='Li [ppm]' then m.value END) AS 'Li [ppm]',
max(CASE WHEN m.parameter='Sc [ppm]' then m.value END) AS 'Sc [ppm]',
max(CASE WHEN m.parameter='Ti [ppm]' then m.value END) AS 'Ti [ppm]',
max(CASE WHEN m.parameter='V [ppm]' then m.value END) AS 'V [ppm]',
max(CASE WHEN m.parameter='Cr [ppm]' then m.value END) AS 'Cr [ppm]',
max(CASE WHEN m.parameter='Co [ppm]' then m.value END) AS 'Co [ppm]',
max(CASE WHEN m.parameter='Ni [ppm]' then m.value END) AS 'Ni [ppm]',
max(CASE WHEN m.parameter='Cu [ppm]' then m.value END) AS 'Cu [ppm]',
max(CASE WHEN m.parameter='Zn [ppm]' then m.value END) AS 'Zn [ppm]',
max(CASE WHEN m.parameter='As [ppm]' then m.value END) AS 'As [ppm]',
max(CASE WHEN m.parameter='Rb [ppm]' then m.value END) AS 'Rb [ppm]',
max(CASE WHEN m.parameter='Sr [ppm]' then m.value END) AS 'Sr [ppm]',
max(CASE WHEN m.parameter='Y [ppm]' then m.value END) AS 'Y [ppm]',
max(CASE WHEN m.parameter='Zr [ppm]' then m.value END) AS 'Zr [ppm]',
max(CASE WHEN m.parameter='Nb [ppm]' then m.value END) AS 'Nb [ppm]',
max(CASE WHEN m.parameter='Cd [ppm]' then m.value END) AS 'Cd [ppm]',
max(CASE WHEN m.parameter='Cs [ppm]' then m.value END) AS 'Cs [ppm]',
max(CASE WHEN m.parameter='Ba [ppm]' then m.value END) AS 'Ba [ppm]',
max(CASE WHEN m.parameter='La [ppm]' then m.value END) AS 'La [ppm]',
max(CASE WHEN m.parameter='Ce [ppm]' then m.value END) AS 'Ce [ppm]',
max(CASE WHEN m.parameter='Pr [ppm]' then m.value END) AS 'Pr [ppm]',
max(CASE WHEN m.parameter='Nd [ppm]' then m.value END) AS 'Nd [ppm]',
max(CASE WHEN m.parameter='Sm [ppm]' then m.value END) AS 'Sm [ppm]',
max(CASE WHEN m.parameter='Eu [ppm]' then m.value END) AS 'Eu [ppm]',
max(CASE WHEN m.parameter='Gd [ppm]' then m.value END) AS 'Gd [ppm]',
max(CASE WHEN m.parameter='Tb [ppm]' then m.value END) AS 'Tb [ppm]',
max(CASE WHEN m.parameter='Dy [ppm]' then m.value END) AS 'Dy [ppm]',
max(CASE WHEN m.parameter='Ho [ppm]' then m.value END) AS 'Ho [ppm]',
max(CASE WHEN m.parameter='Er [ppm]' then m.value END) AS 'Er [ppm]',
max(CASE WHEN m.parameter='Tm [ppm]' then m.value END) AS 'Tm [ppm]',
max(CASE WHEN m.parameter='Yb [ppm]' then m.value END) AS 'Yb [ppm]',
max(CASE WHEN m.parameter='Lu [ppm]' then m.value END) AS 'Lu [ppm]',
max(CASE WHEN m.parameter='Hf [ppm]' then m.value END) AS 'Hf [ppm]',
max(CASE WHEN m.parameter='Ta [ppm]' then m.value END) AS 'Ta [ppm]',
max(CASE WHEN m.parameter='Tl [ppm]' then m.value END) AS 'Tl [ppm]',
max(CASE WHEN m.parameter='Pb [ppm]' then m.value END) AS 'Pb [ppm]',
max(CASE WHEN m.parameter='Th [ppm]' then m.value END) AS 'Th [ppm]',
max(CASE WHEN m.parameter='U [ppm]' then m.value END) AS 'U [ppm]',
max(CASE WHEN m.parameter='Nd143_Nd144' then m.value END) AS 'Nd143_Nd144',
max(CASE WHEN m.parameter='Sr87_Sr86' then m.value END) AS 'Sr87_Sr86',
max(CASE WHEN m.parameter='Pb206_Pb204' then m.value END) AS 'Pb206_Pb204',
max(CASE WHEN m.parameter='Pb207_Pb204' then m.value END) AS 'Pb207_Pb204',
max(CASE WHEN m.parameter='Pb208_Pb204' then m.value END) AS 'Pb208_Pb204'
FROM 'samples.csv' AS s JOIN 'measurements.csv' AS m ON s.id=m.sample_id
WHERE s.sample_category = 'SOURCE' AND
m.parameter IN ('SiO2 [%]', 'TiO2 [%]', 'Al2O3 [%]', 'Fe2O3 [%]', 'FeO [%]',
'MnO [%]', 'MgO [%]', 'CaO [%]', 'Na2O [%]', 'K2O [%]',
'Li [ppm]', 'Sc [ppm]', 'Ti [ppm]', 'V [ppm]',
'Cr [ppm]', 'Co [ppm]', 'Ni [ppm]', 'Cu [ppm]', 'Zn [ppm]', 'As [ppm]',
'Rb [ppm]', 'Sr [ppm]', 'Y [ppm]', 'Zr [ppm]', 'Nb [ppm]', 'Cd [ppm]',
'Cs [ppm]', 'Ba [ppm]', 'La [ppm]', 'Ce [ppm]', 'Pr [ppm]', 'Nd [ppm]',
'Sm [ppm]', 'Eu [ppm]', 'Gd [ppm]', 'Tb [ppm]', 'Dy [ppm]', 'Ho [ppm]',
'Er [ppm]', 'Tm [ppm]', 'Yb [ppm]', 'Lu [ppm]', 'Hf [ppm]', 'Ta [ppm]',
'Tl [ppm]', 'Pb [ppm]', 'Th [ppm]', 'U [ppm]', 'Nd143_Nd144', 'Sr87_Sr86',
'Pb206_Pb204', 'Pb207_Pb204', 'Pb208_Pb204') GROUP BY sample_id") %>%
  rename_pofatu_elements() %>% pofatu_location() %>%
  dplyr::filter(Location %in% c(
    "Samoan islands","Austral-Cook chain","Society islands","Marquesas islands")) %>%
  Ti_from_TiO2() %>% K_from_K2O() %>% dplyr::select(
    Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
    Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
    Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
    Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204) %>%
  dplyr::filter((SiO2 > 45.2 & SiO2 < 48.2 &
                   MgO > 3.08 & MgO < 6.08 &
                   Na2O > 2.08 & Na2O < 5.08 &
                   K2O > 0 & K2O < 2.9 &
                   Ba > 160 & Ba < 480 &
                   Th > 1.95 & Th < 5.84 &
                   Nb > 24.8 & Nb < 74.5 &
                   Nd > 25.5 & Nd < 76.5 &
                   Sm > 6.2 & Sm < 18.6 &
                   Yb > 1.6 & Yb < 4.8) |
                  (SiO2 > 44.3 & SiO2 < 47.3 &
                     MgO > 3.52 & MgO < 6.52 &
                     Na2O > 2 & Na2O < 5 &
                     K2O > 0 & K2O < 2.82 &
                     Ba > 137 & Ba < 413 &
                     Th > 1.86 & Th < 5.60 &
                     Nb > 24.7 & Nb < 74.1 &
                     Nd > 25.6 & Nd < 76.9 &
                     Sm > 6 & Sm < 18.1 &
                     Yb > 1.62 & Yb < 4.86) |
                  (SiO2 > 43.5 & SiO2 < 46.5 &
                     MgO > 3.79 & MgO < 6.79 &
                     Na2O > 1.9 & Na2O < 4.9 &
                     K2O > 0 & K2O < 2.78 &
                     Ba > 126 & Ba < 378 &
                     Th > 1.58 & Th < 4.74 &
                     Nb > 21 & Nb < 63.1 &
                     Nd > 23.7 & Nd < 71.1 &
                     Sm > 5.65 & Sm < 16.95 &
                     Yb > 1.56 & Yb < 4.68) |
                  (SiO2 > 45.9 & SiO2 < 48.9 &
                     MgO > 3.44 & MgO < 6.44 &
                     Na2O > 2.18 & Na2O < 5.18 &
                     K2O > 0 & K2O < 2.99 &
                     Ba > 162 & Ba < 488 &
                     Th > 2.13 & Th < 6.39 &
                     Nb > 24.3 & Nb < 72.9 &
                     Nd > 26.8 & Nd < 80.5 &
                     Sm > 6.35 & Sm < 19 &
                     Yb > 1.49 & Yb < 4.46) |
                  (SiO2 > 44.1 & SiO2 < 47.1 &
                     MgO > 4.15 & MgO < 7.15 &
                     Na2O > 1.8 & Na2O < 4.8 &
                     K2O > 0 & K2O < 2.72 &
                     Ba > 128 & Ba < 385 &
                     Th > 1.58 & Th < 4.74 &
                     Nb > 20.4 & Nb < 61.2 &
                     Nd > 21.7 & Nd < 65.1 &
                     Sm > 5.35 & Sm < 16 &
                     Yb > 1.4 & Yb < 4.19) |
                  (SiO2 > 45.7 & SiO2 < 48.7 &
                     MgO > 3.10 & MgO < 6.10 &
                     Na2O > 2.08 & Na2O < 5.08 &
                     K2O > 0 & K2O < 2.85 &
                     Ba > 143 & Ba < 431 &
                     Th > 1.84 & Th < 5.52 &
                     Nb > 23.5 & Nb < 70.5 &
                     Nd > 27.3 & Nd < 82 &
                     Sm > 6.65 & Sm < 19.95 &
                     Yb > 1.57 & Yb < 4.71))

#### K-12-24 ####
ranges_s_OIB[7,]
ranges_s_OIB[7,36:39] %>% round(digits = 6)
ranges_s_OIB[7,40:45] %>% round(digits = 3)
q14 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
(ND143_ND144 > 0.512477 AND ND143_ND144 < 0.513503 AND
SR87_SR86 > 0.702761 AND SR87_SR86 < 0.704167 AND
PB206_PB204 > 18.221 AND PB206_PB204 < 18.589 AND
PB207_PB204 > 15.372 AND PB207_PB204 < 15.682 AND
PB208_PB204 > 37.927 AND PB208_PB204 < 38.693)") %>%
  get_georoc_location() %>% filter(Location != "na") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
q14 %>% group_by(Location) %>% tally()


ranges_s_OIB[7,1:35]
q15 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
(`NA2O(WT%)` > 1.61 AND `NA2O(WT%)` < 4.61 AND
`K2O(WT%)` < 2.91 AND
`NA2O(WT%)` > 1.61 AND `NA2O(WT%)` < 4.61 AND
`NB(PPM)` > 46.4 AND `NB(PPM)` < 139) AND
(file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part1.csv' OR
file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part2.csv' OR
file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv')") %>%
  get_georoc_location() %>% filter(Location != "na") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
q15 %>% group_by(Location) %>% tally()


#### K-12-25 ####
ranges_s_OIB[8,]
ranges_s_OIB[8,36:39] %>% round(digits = 6)
ranges_s_OIB[8,40:45] %>% round(digits = 3)
q16 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
(ND143_ND144 > 0.512396 AND ND143_ND144 < 0.513422 AND
SR87_SR86 > 0.703423 AND SR87_SR86 < 0.704831 AND
PB206_PB204 > 18.438 AND PB206_PB204 < 18.81 AND
PB207_PB204 > 15.427 AND PB207_PB204 < 15.739 AND
PB208_PB204 > 38.274 AND PB208_PB204 < 39.048)") %>%
  get_georoc_location() %>% filter(Location != "na") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
q16 %>% group_by(Location) %>% tally()

joined_data[17,]
ranges_s_OIB[8,1:35]
q17 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
(`SIO2(WT%)` > 43.1 AND `SIO2(WT%)` < 46.1 AND
`NA2O(WT%)` > 0.89 AND `NA2O(WT%)` < 3.89 AND
`K2O(WT%)` < 1.9 AND
`MGO(WT%)` > 10.7 AND `MGO(WT%)` < 13.7 AND
`YB(PPM)` > 0.86 AND `YB(PPM)` < 2.58)") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
q17 %>% group_by(Location) %>% tally()


#### K-12-26 ####
ranges_s_OIB[9,]
ranges_s_OIB[9,36:39] %>% round(digits = 6)
ranges_s_OIB[9,40:45] %>% round(digits = 3)
q18 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
(ND143_ND144 > 0.51248 AND ND143_ND144 < 0.513506 AND
SR87_SR86 > 0.702897 AND SR87_SR86 < 0.704305 AND
PB206_PB204 > 18.54 AND PB206_PB204 < 18.914 AND
PB207_PB204 > 15.378 AND PB207_PB204 < 15.688 AND
PB208_PB204 > 38.059 AND PB208_PB204 < 38.827)") %>%
  get_georoc_location() %>% filter(Location != "na") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
q18 %>% group_by(Location) %>% tally()

ranges_s_OIB[9,1:35]
q19 <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
(`SIO2(WT%)` > 42.9 AND `SIO2(WT%)` < 45.9 AND
`NA2O(WT%)` > 2.41 AND `NA2O(WT%)` < 5.41 AND
`K2O(WT%)` < 2.35) AND
(file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv' OR
file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part1.csv' OR
file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part2.csv')") %>%
  get_georoc_location() %>% dplyr::filter(Location != "na") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
q19 %>% group_by(Location) %>% tally()






IAB_ref <- dbGetQuery(georoc,
                      "SELECT sample_id, reference_id
FROM 'citation'
WHERE sample_id='1776564'")
IAB_ref
IAB_ref <- dbGetQuery(georoc,
                      "SELECT id, reference
FROM 'reference'
WHERE id='23842'")
IAB_ref
