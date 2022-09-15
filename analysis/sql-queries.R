require(RSQLite)

georoc <- dbConnect(RSQLite::SQLite(), path_to_georoc)
pofatu <- dbConnect(RSQLite::SQLite(), path_to_pofatu)

#### Emae_E_11_03 ####
E_11_03 <- joined_data_ranges[1,]

#### Emae_E_11_06 ####
E_11_06 <- joined_data_ranges[2,]

#### Emae_E_11_07 ####
E_11_07 <- joined_data_ranges[3,]

#### K_12_28 ####
K_12_28 <- joined_data_ranges[19,]

IAB <- dbGetQuery(georoc,
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

#### K_12_29 ####
K_12_29 <- joined_data_ranges[20,]

#### Emae_Taumako ####
Emae_Taumako_1 <- joined_data_ranges[4,]
Emae_Taumako_2 <- joined_data_ranges[11:15,]
Emae_Taumako <- full_join(Emae_Taumako_1,Emae_Taumako_2)
Emae_Taumako[34:43]

OIB1 <- dbGetQuery(georoc,
                   "SELECT *
                   FROM 'sample'
                   WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
                   TECTONIC_SETTING='OCEAN ISLAND' AND
                   ((ND143_ND144 > 0.512341 AND ND143_ND144 < 0.513367 AND
                   SR87_SR86 > 0.704586 AND SR87_SR86 < 0.705996 AND
                   PB206_PB204 > 18.752 AND PB206_PB204 < 18.940 AND
                   PB207_PB204 > 15.508 AND PB207_PB204 < 15.664 AND
                   PB208_PB204 > 38.628 AND PB208_PB204 < 39.016) OR
                   (ND143_ND144 > 0.512342 AND ND143_ND144 < 0.513368 AND
                   SR87_SR86 > 0.704583 AND SR87_SR86 < 0.705993 AND
                   PB206_PB204 > 18.795 AND PB206_PB204 < 18.984 AND
                   PB207_PB204 > 15.509 AND PB207_PB204 < 15.665 AND
                   PB208_PB204 > 38.677 AND PB208_PB204 < 39.065) OR
                   (ND143_ND144 > 0.512358 AND ND143_ND144 < 0.513384 AND
                   SR87_SR86 > 0.704403 AND SR87_SR86 < 0.705813 AND
                   PB206_PB204 > 18.816 AND PB206_PB204 < 19.006 AND
                   PB207_PB204 > 15.505 AND PB207_PB204 < 15.661 AND
                   PB208_PB204 > 38.667 AND PB208_PB204 < 39.055) OR
                   (ND143_ND144 > 0.512342 AND ND143_ND144 < 0.513368 AND
                   SR87_SR86 > 0.704502 AND SR87_SR86 < 0.705912 AND
                   PB206_PB204 > 18.814 AND PB206_PB204 < 19.003 AND
                   PB207_PB204 > 15.523 AND PB207_PB204 < 15.679 AND
                   PB208_PB204 > 38.699 AND PB208_PB204 < 39.088) OR
                   (ND143_ND144 > 0.512359 AND ND143_ND144 < 0.513385 AND
                   SR87_SR86 > 0.704412 AND SR87_SR86 < 0.705822 AND
                   PB206_PB204 > 18.830 AND PB206_PB204 < 19.02 AND
                   PB207_PB204 > 15.507 AND PB207_PB204 < 15.663 AND
                   PB208_PB204 > 38.702 AND PB208_PB204 < 39.091) OR
                   (ND143_ND144 > 0.512348 AND ND143_ND144 < 0.513374 AND
                   SR87_SR86 > 0.704465 AND SR87_SR86 < 0.705875 AND
                   PB206_PB204 > 18.718 AND PB206_PB204 < 18.906 AND
                   PB207_PB204 > 15.503 AND PB207_PB204 < 15.659 AND
                   PB208_PB204 > 38.556 AND PB208_PB204 < 38.944))") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB2 <- dbGetQuery(pofatu,
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
  dplyr::filter((Nd143_Nd144 > 0.512341 & Nd143_Nd144 < 0.513367 &
                   Sr87_Sr86 > 0.704586 & Sr87_Sr86 < 0.705996 &
                   Pb206_Pb204 > 18.752 & Pb206_Pb204 < 18.940 &
                   Pb207_Pb204 > 15.508 & Pb207_Pb204 < 15.664 &
                   Pb208_Pb204 > 38.628 & Pb208_Pb204 < 39.016) |
                  (Nd143_Nd144 > 0.512342 & Nd143_Nd144 < 0.513368 &
                     Sr87_Sr86 > 0.704583 & Sr87_Sr86 < 0.705993 &
                     Pb206_Pb204 > 18.795 & Pb206_Pb204 < 18.984 &
                     Pb207_Pb204 > 15.509 & Pb207_Pb204 < 15.665 &
                     Pb208_Pb204 > 38.677 & Pb208_Pb204 < 39.065) |
                  (Nd143_Nd144 > 0.512358 & Nd143_Nd144 < 0.513384 &
                     Sr87_Sr86 > 0.704403 & Sr87_Sr86 < 0.705813 &
                     Pb206_Pb204 > 18.816 & Pb206_Pb204 < 19.006 &
                     Pb207_Pb204 > 15.505 & Pb207_Pb204 < 15.661 &
                     Pb208_Pb204 > 38.667 & Pb208_Pb204 < 39.055) |
                  (Nd143_Nd144 > 0.512342 & Nd143_Nd144 < 0.513368 &
                     Sr87_Sr86 > 0.704502 & Sr87_Sr86 < 0.705912 &
                     Pb206_Pb204 > 18.814 & Pb206_Pb204 < 19.003 &
                     Pb207_Pb204 > 15.523 & Pb207_Pb204 < 15.679 &
                     Pb208_Pb204 > 38.699 & Pb208_Pb204 < 39.088) |
                  (Nd143_Nd144 > 0.512359 & Nd143_Nd144 < 0.513385 &
                     Sr87_Sr86 > 0.704412 & Sr87_Sr86 < 0.705822 &
                     Pb206_Pb204 > 18.830 & Pb206_Pb204 < 19.02 &
                     Pb207_Pb204 > 15.507 & Pb207_Pb204 < 15.663 &
                     Pb208_Pb204 > 38.702 & Pb208_Pb204 < 39.091) |
                  (Nd143_Nd144 > 0.512348 & Nd143_Nd144 < 0.513374 &
                     Sr87_Sr86 > 0.704465 & Sr87_Sr86 < 0.705875 &
                     Pb206_Pb204 > 18.718 & Pb206_Pb204 < 18.906 &
                     Pb207_Pb204 > 15.503 & Pb207_Pb204 < 15.659 &
                     Pb208_Pb204 > 38.556 & Pb208_Pb204 < 38.944)) %>%
  rename_pofatu_elements() %>% pofatu_location() %>%
  dplyr::filter(Location %in% c(
    "Caroline islands","Samoan islands","Austral-Cook chain","Society islands",
    "Hawai'i islands","Marquesas islands","Pitcairn-Gambier chain","Rapa Nui")) %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

#### Kapinga ####
Kapinga <- joined_data_ranges[16:18,]
Kapinga[34:43]

