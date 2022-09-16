require(tidyverse)
require(RSQLite)

georoc <- dbConnect(RSQLite::SQLite(), path_to_georoc)
pofatu <- dbConnect(RSQLite::SQLite(), path_to_pofatu)

ranges(joined_data)
q1 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
(LONGITUDE_MAX > 90 OR LONGITUDE_MAX < 0) AND
LATITUDE_MAX < 20 AND TECTONIC_SETTING='CONVERGENT MARGIN' AND
(ND143_ND144 > 0.512427 AND ND143_ND144 < 0.513453 AND
SR87_SR86 > 0.703747 AND SR87_SR86 < 0.705156 AND
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

q2 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
(`SIO2(WT%)` > 68.6 AND `SIO2(WT%)` < 71.6 AND
`K2O(WT%)` > 2.62 AND `K2O(WT%)` < 5.62) AND
(file_id= '2022-06-PVFZCE_BISMARCK_ARC_-_NEW_BRITAIN_ARC.csv' OR
file_id= '2022-06-PVFZCE_LUZON_ARC.csv' OR
file_id= '2022-06-PVFZCE_NEW_HEBRIDES_ARC_-_VANUATU_ARCHIPELAGO.csv' OR
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
PB208_PB204 > 38.313 AND PB208_PB204 < 38.698))") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

q5 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
(LONGITUDE_MAX > 90 OR LONGITUDE_MAX < 0) AND
LATITUDE_MAX < 20 AND TECTONIC_SETTING='CONVERGENT MARGIN' AND
(ND143_ND144 > 0.512513 AND ND143_ND144 < 0.513539 AND
SR87_SR86 > 0.703182 AND SR87_SR86 < 0.70459 AND
PB206_PB204 > 18.147 AND PB206_PB204 < 18.33 AND
PB207_PB204 > 15.467 AND PB207_PB204 < 15.623 AND
PB208_PB204 > 38.064 AND PB208_PB204 < 38.446)") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
q5 %>% group_by(Location) %>% tally()

q6 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
(LONGITUDE_MAX > 90 OR LONGITUDE_MAX < 0) AND
LATITUDE_MAX < 20 AND TECTONIC_SETTING='CONVERGENT MARGIN' AND
(ND143_ND144 > 0.512513 AND ND143_ND144 < 0.513539 AND
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

q6 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
(LONGITUDE_MAX > 90 OR LONGITUDE_MAX < 0) AND
LATITUDE_MAX < 20 AND TECTONIC_SETTING='CONVERGENT MARGIN' AND
(`SIO2(WT%)` > 52.1 AND `SIO2(WT%)` < 55.1 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 1.55) AND
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

joined_data_ranges[20,40:45] %>% round(digits = 3)
q7 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
(LONGITUDE_MAX > 90 OR LONGITUDE_MAX < 0) AND
LATITUDE_MAX < 20 AND TECTONIC_SETTING='CONVERGENT MARGIN' AND
(ND143_ND144 > 0.512403 AND ND143_ND144 < 0.513429 AND
SR87_SR86 > 0.703693 AND SR87_SR86 < 0.705101 AND
PB206_PB204 > 18.454 AND PB206_PB204 < 18.64 AND
PB207_PB204 > 15.492 AND PB207_PB204 < 15.648 AND
PB208_PB204 > 38.234 AND PB208_PB204 < 38.618) AND
(file_id = '2022-06-PVFZCE_BISMARCK_ARC_NEW_BRITAIN_ARC.csv' OR
file_id = '2022-06-PVFZCE_LUZON_ARC.csv' OR
file_id = '2022-06-PVFZCE_MARIANA_ARC.csv' OR
file_id = '2022-06-PVFZCE_NEW_HEBRIDES_ARC_VANUATU_ARCHIPELAGO.csv' OR
file_id = '2022-06-PVFZCE_SOLOMON_ISLAND_ARC.csv' OR
file_id = '2022-06-PVFZCE_SULAWESI_ARC.csv' OR
file_id = '2022-06-PVFZCE_SUNDA_ARC.csv' OR
file_id = '2022-06-PVFZCE_TONGA_ARC.csv')") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
q7 %>% group_by(Location) %>% tally() #the island arcs represented

joined_data_ranges[20,]
q8 <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
`SIO2(WT%)` > 48.2 AND `SIO2(WT%)` < 51.2 AND
`K2O(WT%)` > 0.14 AND `K2O(WT%)` < 3.14 AND
(file_id = '2022-06-PVFZCE_BISMARCK_ARC_NEW_BRITAIN_ARC.csv' OR
file_id = '2022-06-PVFZCE_LUZON_ARC.csv' OR
file_id = '2022-06-PVFZCE_NEW_HEBRIDES_ARC_VANUATU_ARCHIPELAGO.csv' OR
file_id = '2022-06-PVFZCE_SOLOMON_ISLAND_ARC.csv')") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
q8 %>% group_by(Location) %>% tally() #the island arcs represented

