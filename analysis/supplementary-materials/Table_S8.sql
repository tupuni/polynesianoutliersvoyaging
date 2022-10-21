#### Fig 2 ####
# q0_1 (georoc.sqlite)
SELECT TECTONIC_SETTING, id, file_id, LOCATION, LAND_OR_SEA,
ROCK_TYPE, `NB(PPM)` AS Nb, `LA(PPM)` AS La,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING = 'CONVERGENT MARGIN'

# q0_2 (georoc.sqlite)
SELECT TECTONIC_SETTING, id, file_id, LOCATION, LAND_OR_SEA,
ROCK_TYPE, `NB(PPM)` AS Nb, `LA(PPM)` AS La,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING = 'OCEAN ISLAND'

#### Fig 3 ####
#### E-11-03 ####
# q1 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
ND143_ND144 AS Nd143_Nd144, SR87_SR86 AS Sr87_Sr86,
PB206_PB204 AS Pb206_Pb204, PB207_PB204 AS Pb207_Pb204,
PB208_PB204 AS Pb208_Pb204
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
(LONGITUDE_MAX > 90 OR LONGITUDE_MAX < 0) AND
LATITUDE_MAX < 20 AND TECTONIC_SETTING='CONVERGENT MARGIN' AND
ND143_ND144 > 0.512427 AND ND143_ND144 < 0.513453 AND
SR87_SR86 > 0.703748 AND SR87_SR86 < 0.705156 AND
PB206_PB204 > 18.291 AND PB206_PB204 < 18.475 AND
PB207_PB204 > 15.457 AND PB207_PB204 < 15.613 AND
PB208_PB204 > 38.268 AND PB208_PB204 < 38.652

# q2 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
`SIO2(WT%)` AS SiO2,`TIO2(WT%)` AS TiO2, `K2O(WT%)`AS K2O, `NA2O(WT%)` AS Na2O,
`RB(PPM)` AS Rb, `BA(PPM)` AS Ba, `TH(PPM)` AS Th, `U(PPM)` AS U,
`NB(PPM)` AS Nb, `LA(PPM)` AS La, `CE(PPM)` AS Ce, `ND(PPM)` AS Nd,
`SR(PPM)` AS Sr, `SM(PPM)` AS Sm, `ZR(PPM)` AS Zr, `TI(PPM)` AS Ti,
`EU(PPM)` AS Eu, `GD(PPM)` AS Gd, `Y(PPM)` AS Y, `YB(PPM)` AS Yb
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
(SiO2 > 68.6 AND SiO2 < 71.6 AND
K2O > 2.62 AND K2O < 5.62) AND
(file_id= '2022-06-PVFZCE_BISMARCK_ARC_NEW_BRITAIN_ARC.csv' OR
file_id= '2022-06-PVFZCE_LUZON_ARC.csv' OR
file_id= '2022-06-PVFZCE_NEW_HEBRIDES_ARC_VANUATU_ARCHIPELAGO.csv' OR
file_id= '2022-06-PVFZCE_SULAWESI_ARC.csv' OR
file_id= '2022-06-PVFZCE_SOLOMON_ISLAND_ARC.csv' OR
file_id= '2022-06-PVFZCE_SULAWESI_ARC.csv' OR
file_id= '2022-06-PVFZCE_TONGA_ARC.csv')

# q2 (pofatu.sqlite)
SELECT * FROM (
SELECT s.id AS Sample, s.sample_category, s.location_region,
s.location_subregion, s.location_latitude AS lat, s.location_longitude AS long,
max(CASE WHEN m.parameter='SiO2 [%]' then m.value END) AS 'SiO2',
max(CASE WHEN m.parameter='TiO2 [%]' then m.value END) AS 'TiO2',
max(CASE WHEN m.parameter='Al2O3 [%]' then m.value END) AS 'Al2O3',
max(CASE WHEN m.parameter='Fe2O3 [%]' then m.value END) AS 'Fe2O3',
max(CASE WHEN m.parameter='FeO [%]' then m.value END) AS 'FeO',
max(CASE WHEN m.parameter='MnO [%]' then m.value END) AS 'MnO',
max(CASE WHEN m.parameter='MgO [%]' then m.value END) AS 'MgO',
max(CASE WHEN m.parameter='CaO [%]' then m.value END) AS 'CaO',
max(CASE WHEN m.parameter='Na2O [%]' then m.value END) AS 'Na2O',
max(CASE WHEN m.parameter='K2O [%]' then m.value END) AS 'K2O',
max(CASE WHEN m.parameter='Li [ppm]' then m.value END) AS 'Li',
max(CASE WHEN m.parameter='Sc [ppm]' then m.value END) AS 'Sc',
max(CASE WHEN m.parameter='Ti [ppm]' then m.value END) AS 'Ti',
max(CASE WHEN m.parameter='V [ppm]' then m.value END) AS 'V',
max(CASE WHEN m.parameter='Cr [ppm]' then m.value END) AS 'Cr',
max(CASE WHEN m.parameter='Co [ppm]' then m.value END) AS 'Co',
max(CASE WHEN m.parameter='Ni [ppm]' then m.value END) AS 'Ni',
max(CASE WHEN m.parameter='Cu [ppm]' then m.value END) AS 'Cu',
max(CASE WHEN m.parameter='Zn [ppm]' then m.value END) AS 'Zn',
max(CASE WHEN m.parameter='As [ppm]' then m.value END) AS 'As',
max(CASE WHEN m.parameter='Rb [ppm]' then m.value END) AS 'Rb',
max(CASE WHEN m.parameter='Sr [ppm]' then m.value END) AS 'Sr',
max(CASE WHEN m.parameter='Y [ppm]' then m.value END) AS 'Y',
max(CASE WHEN m.parameter='Zr [ppm]' then m.value END) AS 'Zr',
max(CASE WHEN m.parameter='Nb [ppm]' then m.value END) AS 'Nb',
max(CASE WHEN m.parameter='Cd [ppm]' then m.value END) AS 'Cd',
max(CASE WHEN m.parameter='Cs [ppm]' then m.value END) AS 'Cs',
max(CASE WHEN m.parameter='Ba [ppm]' then m.value END) AS 'Ba',
max(CASE WHEN m.parameter='La [ppm]' then m.value END) AS 'La',
max(CASE WHEN m.parameter='Ce [ppm]' then m.value END) AS 'Ce',
max(CASE WHEN m.parameter='Pr [ppm]' then m.value END) AS 'Pr',
max(CASE WHEN m.parameter='Nd [ppm]' then m.value END) AS 'Nd',
max(CASE WHEN m.parameter='Sm [ppm]' then m.value END) AS 'Sm',
max(CASE WHEN m.parameter='Eu [ppm]' then m.value END) AS 'Eu',
max(CASE WHEN m.parameter='Gd [ppm]' then m.value END) AS 'Gd',
max(CASE WHEN m.parameter='Tb [ppm]' then m.value END) AS 'Tb',
max(CASE WHEN m.parameter='Dy [ppm]' then m.value END) AS 'Dy',
max(CASE WHEN m.parameter='Ho [ppm]' then m.value END) AS 'Ho',
max(CASE WHEN m.parameter='Er [ppm]' then m.value END) AS 'Er',
max(CASE WHEN m.parameter='Tm [ppm]' then m.value END) AS 'Tm',
max(CASE WHEN m.parameter='Yb [ppm]' then m.value END) AS 'Yb',
max(CASE WHEN m.parameter='Lu [ppm]' then m.value END) AS 'Lu',
max(CASE WHEN m.parameter='Hf [ppm]' then m.value END) AS 'Hf',
max(CASE WHEN m.parameter='Ta [ppm]' then m.value END) AS 'Ta',
max(CASE WHEN m.parameter='Tl [ppm]' then m.value END) AS 'Tl',
max(CASE WHEN m.parameter='Pb [ppm]' then m.value END) AS 'Pb',
max(CASE WHEN m.parameter='Th [ppm]' then m.value END) AS 'Th',
max(CASE WHEN m.parameter='U [ppm]' then m.value END) AS 'U',
max(CASE WHEN m.parameter='Nd143_Nd144' then m.value END) AS 'Nd143_Nd144',
max(CASE WHEN m.parameter='Sr87_Sr86' then m.value END) AS 'Sr87_Sr86',
max(CASE WHEN m.parameter='Pb206_Pb204' then m.value END) AS 'Pb206_Pb204',
max(CASE WHEN m.parameter='Pb207_Pb204' then m.value END) AS 'Pb207_Pb204',
max(CASE WHEN m.parameter='Pb208_Pb204' then m.value END) AS 'Pb208_Pb204'
FROM 'samples.csv' AS s JOIN 'measurements.csv' AS m ON s.id=m.sample_id
WHERE m.parameter IN ('SiO2 [%]', 'TiO2 [%]', 'Al2O3 [%]', 'MnO [%]', 'MgO [%]',
'Fe2O3 [%]', 'FeO [%]', 'CaO [%]','Na2O [%]', 'K2O [%]',
'Li [ppm]', 'Sc [ppm]', 'Ti [ppm]', 'V [ppm]', 'Cr [ppm]', 'Co [ppm]',
'Ni [ppm]', 'Cu [ppm]', 'Zn [ppm]', 'As [ppm]', 'Rb [ppm]', 'Sr [ppm]',
'Y [ppm]', 'Zr [ppm]', 'Nb [ppm]', 'Cd [ppm]', 'Cs [ppm]', 'Ba [ppm]',
'La [ppm]', 'Ce [ppm]', 'Pr [ppm]', 'Nd [ppm]', 'Sm [ppm]', 'Eu [ppm]',
'Gd [ppm]', 'Tb [ppm]', 'Dy [ppm]', 'Ho [ppm]', 'Er [ppm]', 'Tm [ppm]',
'Yb [ppm]', 'Lu [ppm]', 'Hf [ppm]', 'Ta [ppm]', 'Tl [ppm]', 'Pb [ppm]',
'Th [ppm]', 'U [ppm]', 'Nd143_Nd144', 'Sr87_Sr86', 'Pb206_Pb204',
'Pb207_Pb204', 'Pb208_Pb204')
GROUP BY sample_id) AS Q3 WHERE
sample_category = 'SOURCE' AND
SiO2 > 68.6 AND SiO2 < 71.6 AND K2O > 2.62 AND K2O < 5.62

#### E-11-06_E-11-07 ####
# q4 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
ND143_ND144 AS Nd143_Nd144, SR87_SR86 AS Sr87_Sr86,
PB206_PB204 AS Pb206_Pb204, PB207_PB204 AS Pb207_Pb204,
PB208_PB204 AS Pb208_Pb204
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='CONVERGENT MARGIN' AND
((
Nd143_Nd144 > 0.512414 AND Nd143_Nd144 < 0.51344 AND
Sr87_Sr86 > 0.703489 AND Sr87_Sr86 < 0.704897 AND
Pb206_Pb204 > 18.464 AND Pb206_Pb204 < 18.65 AND
Pb207_Pb204 > 15.49 AND Pb207_Pb204 < 15.646 AND
Pb208_Pb204 > 38.338 AND Pb208_Pb204 < 38.724
) OR (
Nd143_Nd144 > 0.512419 AND Nd143_Nd144 < 0.513445 AND
Sr87_Sr86 > 0.703451 AND Sr87_Sr86 < 0.704859 AND
Pb206_Pb204 > 18.441 AND Pb206_Pb204 < 18.627 AND
Pb207_Pb204 > 15.487 AND Pb207_Pb204 < 15.643 AND
Pb208_Pb204 > 38.313 AND Pb208_Pb204 < 38.699
))

# q5 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
`SIO2(WT%)` AS SiO2,`TIO2(WT%)` AS TiO2, `K2O(WT%)`AS K2O, `NA2O(WT%)` AS Na2O,
`RB(PPM)` AS Rb, `BA(PPM)` AS Ba, `TH(PPM)` AS Th, `U(PPM)` AS U,
`NB(PPM)` AS Nb, `LA(PPM)` AS La, `CE(PPM)` AS Ce, `ND(PPM)` AS Nd,
`SR(PPM)` AS Sr, `SM(PPM)` AS Sm, `ZR(PPM)` AS Zr, `TI(PPM)` AS Ti,
`EU(PPM)` AS Eu, `GD(PPM)` AS Gd, `Y(PPM)` AS Y, `YB(PPM)` AS Yb
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='CONVERGENT MARGIN' AND
((
SiO2 > 48.1 AND SiO2 < 51.1 AND K2O > 0 AND K2O < 2.26
) OR (
SiO2 > 47.4 AND SiO2 < 50.4 AND K2O > 0 AND K2O < 2.31
)) AND (
file_id= '2022-06-PVFZCE_BISMARCK_ARC_NEW_BRITAIN_ARC.csv' OR
file_id= '2022-06-PVFZCE_NEW_HEBRIDES_ARC_VANUATU_ARCHIPELAGO.csv' OR
file_id= '2022-06-PVFZCE_SULAWESI_ARC.csv' OR
file_id= '2022-06-PVFZCE_TONGA_ARC.csv'
)

#### K-12-28 ####
# q6 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
ND143_ND144 AS Nd143_Nd144, SR87_SR86 AS Sr87_Sr86,
PB206_PB204 AS Pb206_Pb204, PB207_PB204 AS Pb207_Pb204,
PB208_PB204 AS Pb208_Pb204
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE = 'VOL' AND
TECTONIC_SETTING = 'CONVERGENT MARGIN' AND
lat < 20 AND (long > 90 OR long < 0) AND
Nd143_Nd144 > 0.512513 AND Nd143_Nd144 < 0.513539 AND
Sr87_Sr86 > 0.703182 AND Sr87_Sr86 < 0.70459 AND
Pb206_Pb204 > 18.147 AND Pb206_Pb204 < 18.329 AND
Pb207_Pb204 > 15.467 AND Pb207_Pb204 < 15.623 AND
Pb208_Pb204 > 38.064 AND Pb208_Pb204 < 38.446

# q7 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
`SIO2(WT%)` AS SiO2,`TIO2(WT%)` AS TiO2, `K2O(WT%)`AS K2O, `NA2O(WT%)` AS Na2O,
`RB(PPM)` AS Rb, `BA(PPM)` AS Ba, `TH(PPM)` AS Th, `U(PPM)` AS U,
`NB(PPM)` AS Nb, `LA(PPM)` AS La, `CE(PPM)` AS Ce, `ND(PPM)` AS Nd,
`SR(PPM)` AS Sr, `SM(PPM)` AS Sm, `ZR(PPM)` AS Zr, `TI(PPM)` AS Ti,
`EU(PPM)` AS Eu, `GD(PPM)` AS Gd, `Y(PPM)` AS Y, `YB(PPM)` AS Yb
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE = 'VOL' AND
(long > 90 OR long < 0) AND
lat < 20 AND TECTONIC_SETTING='CONVERGENT MARGIN' AND
(SiO2 > 52.1 AND SiO2 < 55.1 AND K2O > 0 AND K2O < 1.55) AND
(
file_id= '2022-06-PVFZCE_BISMARCK_ARC_NEW_BRITAIN_ARC.csv' OR
file_id= '2022-06-PVFZCE_LUZON_ARC.csv' OR
file_id= '2022-06-PVFZCE_NEW_HEBRIDES_ARC_VANUATU_ARCHIPELAGO.csv' OR
file_id= '2022-06-PVFZCE_SULAWESI_ARC.csv'
)

#### K-12-29 ####
# q8 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
ND143_ND144 AS Nd143_Nd144, SR87_SR86 AS Sr87_Sr86,
PB206_PB204 AS Pb206_Pb204, PB207_PB204 AS Pb207_Pb204,
PB208_PB204 AS Pb208_Pb204
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='CONVERGENT MARGIN' AND
(LONGITUDE_MAX > 90 OR LONGITUDE_MAX < 0) AND LATITUDE_MAX < 20 AND
Sr87_Sr86 > 0.703693 AND Sr87_Sr86 < 0.705101 AND
Nd143_Nd144 > 0.512403 AND Nd143_Nd144 < 0.513429 AND
Pb206_Pb204 > 18.454 AND Pb206_Pb204 < 18.64 AND
Pb207_Pb204 > 15.492 AND Pb207_Pb204 < 15.648 AND
Pb208_Pb204 > 38.234 AND Pb208_Pb204 < 38.618

# q9 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
`SIO2(WT%)` AS SiO2,`TIO2(WT%)` AS TiO2, `K2O(WT%)`AS K2O, `NA2O(WT%)` AS Na2O,
`RB(PPM)` AS Rb, `BA(PPM)` AS Ba, `TH(PPM)` AS Th, `U(PPM)` AS U,
`NB(PPM)` AS Nb, `LA(PPM)` AS La, `CE(PPM)` AS Ce, `ND(PPM)` AS Nd,
`SR(PPM)` AS Sr, `SM(PPM)` AS Sm, `ZR(PPM)` AS Zr, `TI(PPM)` AS Ti,
`EU(PPM)` AS Eu, `GD(PPM)` AS Gd, `Y(PPM)` AS Y, `YB(PPM)` AS Yb
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
(long > 90 OR long < 0) AND lat < 20 AND
TECTONIC_SETTING='CONVERGENT MARGIN' AND
SiO2 > 48.2 AND SiO2 < 51.2 AND K2O > 0.14 AND K2O < 3.14 AND
Yb > 1.05 AND Yb < 3.15 AND
(
file_id= '2022-06-PVFZCE_BISMARCK_ARC_NEW_BRITAIN_ARC.csv' OR
file_id= '2022-06-PVFZCE_LUZON_ARC.csv' OR
file_id= '2022-06-PVFZCE_NEW_HEBRIDES_ARC_VANUATU_ARCHIPELAGO.csv' OR
file_id= '2022-06-PVFZCE_SULAWESI_ARC.csv' OR
file_id= '2022-06-PVFZCE_TONGA_ARC.csv'
)

#### Fig 4 ####
#### Emae-Taumako OIB ####
# q10 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
ND143_ND144 AS Nd143_Nd144, SR87_SR86 AS Sr87_Sr86,
PB206_PB204 AS Pb206_Pb204, PB207_PB204 AS Pb207_Pb204,
PB208_PB204 AS Pb208_Pb204
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
((
ND143_ND144 > 0.512341 AND ND143_ND144 < 0.513367 AND
SR87_SR86 > 0.704586 AND SR87_SR86 < 0.705996 AND
PB206_PB204 > 18.658 AND PB206_PB204 < 19.034 AND
PB207_PB204 > 15.430 AND PB207_PB204 < 15.742 AND
PB208_PB204 > 38.434 AND PB208_PB204 < 39.210
) OR (
ND143_ND144 > 0.512342 AND ND143_ND144 < 0.513368 AND
SR87_SR86 > 0.704583 AND SR87_SR86 < 0.705993 AND
PB206_PB204 > 18.701 AND PB206_PB204 < 19.079 AND
PB207_PB204 > 15.431 AND PB207_PB204 < 15.743 AND
PB208_PB204 > 38.482 AND PB208_PB204 < 39.260
) OR (
ND143_ND144 > 0.512358 AND ND143_ND144 < 0.513384 AND
SR87_SR86 > 0.704403 AND SR87_SR86 < 0.705813 AND
PB206_PB204 > 18.722 AND PB206_PB204 < 19.100 AND
PB207_PB204 > 15.427 AND PB207_PB204 < 15.739 AND
PB208_PB204 > 38.472 AND PB208_PB204 < 39.250
) OR (
ND143_ND144 > 0.512342 AND ND143_ND144 < 0.513368 AND
SR87_SR86 > 0.704502 AND SR87_SR86 < 0.705912 AND
PB206_PB204 > 18.720 AND PB206_PB204 < 19.098 AND
PB207_PB204 > 15.445 AND PB207_PB204 < 15.757 AND
PB208_PB204 > 38.505 AND PB208_PB204 < 39.283
) OR (
ND143_ND144 > 0.512359 AND ND143_ND144 < 0.513385 AND
SR87_SR86 > 0.704412 AND SR87_SR86 < 0.705822 AND
PB206_PB204 > 18.736 AND PB206_PB204 < 19.114 AND
PB207_PB204 > 15.429 AND PB207_PB204 < 15.741 AND
PB208_PB204 > 38.508 AND PB208_PB204 < 39.286
) OR (
ND143_ND144 > 0.512348 AND ND143_ND144 < 0.513374 AND
SR87_SR86 > 0.704465 AND SR87_SR86 < 0.705875 AND
PB206_PB204 > 18.624 AND PB206_PB204 < 19 AND
PB207_PB204 > 15.425 AND PB207_PB204 < 15.737 AND
PB208_PB204 > 38.362 AND PB208_PB204 < 39.138
))

# q11 (pofatu.sqlite)
SELECT * FROM (
SELECT s.id AS Sample, s.sample_category, s.location_region,
s.location_subregion, s.location_latitude AS lat, s.location_longitude AS long,
max(CASE WHEN m.parameter='SiO2 [%]' then m.value END) AS 'SiO2',
max(CASE WHEN m.parameter='TiO2 [%]' then m.value END) AS 'TiO2',
max(CASE WHEN m.parameter='Al2O3 [%]' then m.value END) AS 'Al2O3',
max(CASE WHEN m.parameter='Fe2O3 [%]' then m.value END) AS 'Fe2O3',
max(CASE WHEN m.parameter='FeO [%]' then m.value END) AS 'FeO',
max(CASE WHEN m.parameter='MnO [%]' then m.value END) AS 'MnO',
max(CASE WHEN m.parameter='MgO [%]' then m.value END) AS 'MgO',
max(CASE WHEN m.parameter='CaO [%]' then m.value END) AS 'CaO',
max(CASE WHEN m.parameter='Na2O [%]' then m.value END) AS 'Na2O',
max(CASE WHEN m.parameter='K2O [%]' then m.value END) AS 'K2O',
max(CASE WHEN m.parameter='Li [ppm]' then m.value END) AS 'Li',
max(CASE WHEN m.parameter='Sc [ppm]' then m.value END) AS 'Sc',
max(CASE WHEN m.parameter='Ti [ppm]' then m.value END) AS 'Ti',
max(CASE WHEN m.parameter='V [ppm]' then m.value END) AS 'V',
max(CASE WHEN m.parameter='Cr [ppm]' then m.value END) AS 'Cr',
max(CASE WHEN m.parameter='Co [ppm]' then m.value END) AS 'Co',
max(CASE WHEN m.parameter='Ni [ppm]' then m.value END) AS 'Ni',
max(CASE WHEN m.parameter='Cu [ppm]' then m.value END) AS 'Cu',
max(CASE WHEN m.parameter='Zn [ppm]' then m.value END) AS 'Zn',
max(CASE WHEN m.parameter='As [ppm]' then m.value END) AS 'As',
max(CASE WHEN m.parameter='Rb [ppm]' then m.value END) AS 'Rb',
max(CASE WHEN m.parameter='Sr [ppm]' then m.value END) AS 'Sr',
max(CASE WHEN m.parameter='Y [ppm]' then m.value END) AS 'Y',
max(CASE WHEN m.parameter='Zr [ppm]' then m.value END) AS 'Zr',
max(CASE WHEN m.parameter='Nb [ppm]' then m.value END) AS 'Nb',
max(CASE WHEN m.parameter='Cd [ppm]' then m.value END) AS 'Cd',
max(CASE WHEN m.parameter='Cs [ppm]' then m.value END) AS 'Cs',
max(CASE WHEN m.parameter='Ba [ppm]' then m.value END) AS 'Ba',
max(CASE WHEN m.parameter='La [ppm]' then m.value END) AS 'La',
max(CASE WHEN m.parameter='Ce [ppm]' then m.value END) AS 'Ce',
max(CASE WHEN m.parameter='Pr [ppm]' then m.value END) AS 'Pr',
max(CASE WHEN m.parameter='Nd [ppm]' then m.value END) AS 'Nd',
max(CASE WHEN m.parameter='Sm [ppm]' then m.value END) AS 'Sm',
max(CASE WHEN m.parameter='Eu [ppm]' then m.value END) AS 'Eu',
max(CASE WHEN m.parameter='Gd [ppm]' then m.value END) AS 'Gd',
max(CASE WHEN m.parameter='Tb [ppm]' then m.value END) AS 'Tb',
max(CASE WHEN m.parameter='Dy [ppm]' then m.value END) AS 'Dy',
max(CASE WHEN m.parameter='Ho [ppm]' then m.value END) AS 'Ho',
max(CASE WHEN m.parameter='Er [ppm]' then m.value END) AS 'Er',
max(CASE WHEN m.parameter='Tm [ppm]' then m.value END) AS 'Tm',
max(CASE WHEN m.parameter='Yb [ppm]' then m.value END) AS 'Yb',
max(CASE WHEN m.parameter='Lu [ppm]' then m.value END) AS 'Lu',
max(CASE WHEN m.parameter='Hf [ppm]' then m.value END) AS 'Hf',
max(CASE WHEN m.parameter='Ta [ppm]' then m.value END) AS 'Ta',
max(CASE WHEN m.parameter='Tl [ppm]' then m.value END) AS 'Tl',
max(CASE WHEN m.parameter='Pb [ppm]' then m.value END) AS 'Pb',
max(CASE WHEN m.parameter='Th [ppm]' then m.value END) AS 'Th',
max(CASE WHEN m.parameter='U [ppm]' then m.value END) AS 'U',
max(CASE WHEN m.parameter='Nd143_Nd144' then m.value END) AS 'Nd143_Nd144',
max(CASE WHEN m.parameter='Sr87_Sr86' then m.value END) AS 'Sr87_Sr86',
max(CASE WHEN m.parameter='Pb206_Pb204' then m.value END) AS 'Pb206_Pb204',
max(CASE WHEN m.parameter='Pb207_Pb204' then m.value END) AS 'Pb207_Pb204',
max(CASE WHEN m.parameter='Pb208_Pb204' then m.value END) AS 'Pb208_Pb204'
FROM 'samples.csv' AS s JOIN 'measurements.csv' AS m ON s.id=m.sample_id
WHERE s.sample_category = 'SOURCE' AND
m.parameter IN ('SiO2 [%]', 'TiO2 [%]', 'Al2O3 [%]', 'Fe2O3 [%]',
'FeO [%]', 'MnO [%]', 'MgO [%]', 'CaO [%]', 'Na2O [%]', 'K2O [%]',
'Li [ppm]', 'Sc [ppm]', 'Ti [ppm]', 'V [ppm]', 'Cr [ppm]',
'Co [ppm]', 'Ni [ppm]', 'Cu [ppm]', 'Zn [ppm]', 'As [ppm]',
'Rb [ppm]', 'Sr [ppm]', 'Y [ppm]', 'Zr [ppm]', 'Nb [ppm]',
'Cd [ppm]', 'Cs [ppm]', 'Ba [ppm]', 'La [ppm]', 'Ce [ppm]',
'Pr [ppm]', 'Nd [ppm]', 'Sm [ppm]', 'Eu [ppm]', 'Gd [ppm]',
'Tb [ppm]', 'Dy [ppm]', 'Ho [ppm]', 'Er [ppm]', 'Tm [ppm]',
'Yb [ppm]', 'Lu [ppm]', 'Hf [ppm]', 'Ta [ppm]', 'Tl [ppm]',
'Pb [ppm]', 'Th [ppm]', 'U [ppm]', 'Nd143_Nd144',
'Sr87_Sr86', 'Pb206_Pb204', 'Pb207_Pb204', 'Pb208_Pb204')
GROUP BY sample_id) AS Q11 WHERE
(
Nd143_Nd144 > 0.512341 AND Nd143_Nd144 < 0.513367 AND
Sr87_Sr86 > 0.704586 AND Sr87_Sr86 < 0.705996 AND
Pb206_Pb204 > 18.658 AND Pb206_Pb204 < 19.034 AND
Pb207_Pb204 > 15.430 AND Pb207_Pb204 < 15.742 AND
Pb208_Pb204 > 38.434 AND Pb208_Pb204 < 39.210
) OR (
Nd143_Nd144 > 0.512342 AND Nd143_Nd144 < 0.513368 AND
Sr87_Sr86 > 0.704583 AND Sr87_Sr86 < 0.705993 AND
Pb206_Pb204 > 18.701 AND Pb206_Pb204 < 19.079 AND
Pb207_Pb204 > 15.431 AND Pb207_Pb204 < 15.743 AND
Pb208_Pb204 > 38.482 AND Pb208_Pb204 < 39.260
) OR (
Nd143_Nd144 > 0.512358 AND Nd143_Nd144 < 0.513384 AND
Sr87_Sr86 > 0.704403 AND Sr87_Sr86 < 0.705813 AND
Pb206_Pb204 > 18.722 AND Pb206_Pb204 < 19.1 AND
Pb207_Pb204 > 15.427 AND Pb207_Pb204 < 15.739 AND
Pb208_Pb204 > 38.472 AND Pb208_Pb204 < 39.25
) OR (
Nd143_Nd144 > 0.512342 AND Nd143_Nd144 < 0.513368 AND
Sr87_Sr86 > 0.704502 AND Sr87_Sr86 < 0.705912 AND
Pb206_Pb204 > 18.72 AND Pb206_Pb204 < 19.098 AND
Pb207_Pb204 > 15.445 AND Pb207_Pb204 < 15.757 AND
Pb208_Pb204 > 38.505 AND Pb208_Pb204 < 39.283
) OR (
Nd143_Nd144 > 0.512359 AND Nd143_Nd144 < 0.513385 AND
Sr87_Sr86 > 0.704412 AND Sr87_Sr86 < 0.705822 AND
Pb206_Pb204 > 18.736 AND Pb206_Pb204 < 19.114 AND
Pb207_Pb204 > 15.429 AND Pb207_Pb204 < 15.741 AND
Pb208_Pb204 > 38.508 AND Pb208_Pb204 < 39.286
) OR (
Nd143_Nd144 > 0.512348 AND Nd143_Nd144 < 0.513374 AND
Sr87_Sr86 > 0.704465 AND Sr87_Sr86 < 0.705875 AND
Pb206_Pb204 > 18.624 AND Pb206_Pb204 < 19 AND
Pb207_Pb204 > 15.425 AND Pb207_Pb204 < 15.737 AND
Pb208_Pb204 > 38.362 AND Pb208_Pb204 < 39.138
)

# q12 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
`SIO2(WT%)` AS SiO2,`TIO2(WT%)` AS TiO2, `K2O(WT%)`AS K2O, `NA2O(WT%)` AS Na2O,
`RB(PPM)` AS Rb, `BA(PPM)` AS Ba, `TH(PPM)` AS Th, `U(PPM)` AS U,
`NB(PPM)` AS Nb, `LA(PPM)` AS La, `CE(PPM)` AS Ce, `ND(PPM)` AS Nd,
`SR(PPM)` AS Sr, `SM(PPM)` AS Sm, `ZR(PPM)` AS Zr, `TI(PPM)` AS Ti,
`EU(PPM)` AS Eu, `GD(PPM)` AS Gd, `Y(PPM)` AS Y, `YB(PPM)` AS Yb
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
((
`SIO2(WT%)` > 45.2 AND `SIO2(WT%)` < 48.2 AND
`NA2O(WT%)` > 2.08 AND `NA2O(WT%)` < 5.08 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.9 AND
`YB(PPM)` > 1.6 AND `YB(PPM)` < 4.8 AND
PB206_PB204 > 18.658 AND PB206_PB204 < 19.034 AND
PB207_PB204 > 15.430 AND PB207_PB204 < 15.742 AND
PB208_PB204 > 38.434 AND PB208_PB204 < 39.210
) OR (
`SIO2(WT%)` > 44.3 AND `SIO2(WT%)` < 47.3 AND
`NA2O(WT%)` > 2 AND `NA2O(WT%)` < 5 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.82 AND
`YB(PPM)` > 1.62 AND `YB(PPM)` < 4.86 AND
PB206_PB204 > 18.701 AND PB206_PB204 < 19.079 AND
PB207_PB204 > 15.431 AND PB207_PB204 < 15.743 AND
PB208_PB204 > 38.482 AND PB208_PB204 < 39.260
) OR (
`SIO2(WT%)` > 43.5 AND `SIO2(WT%)` < 46.5 AND
`NA2O(WT%)` > 1.9 AND `NA2O(WT%)` < 4.9 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.78 AND
`YB(PPM)` > 1.56 AND `YB(PPM)` < 4.68 AND
PB206_PB204 > 18.722 AND PB206_PB204 < 19.100 AND
PB207_PB204 > 15.427 AND PB207_PB204 < 15.739 AND
PB208_PB204 > 38.472 AND PB208_PB204 < 39.250
) OR (
`SIO2(WT%)` > 45.9 AND `SIO2(WT%)` < 48.9 AND
`NA2O(WT%)` > 2.18 AND `NA2O(WT%)` < 5.18 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.99 AND
`YB(PPM)` > 1.49 AND `YB(PPM)` < 4.46 AND
PB206_PB204 > 18.720 AND PB206_PB204 < 19.098 AND
PB207_PB204 > 15.445 AND PB207_PB204 < 15.757 AND
PB208_PB204 > 38.505 AND PB208_PB204 < 39.283
) OR (
`SIO2(WT%)` > 44.1 AND `SIO2(WT%)` < 47.1 AND
`NA2O(WT%)` > 1.8 AND `NA2O(WT%)` < 4.8 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.72 AND
`YB(PPM)` > 1.4 AND `YB(PPM)` < 4.19 AND
PB206_PB204 > 18.736 AND PB206_PB204 < 19.114 AND
PB207_PB204 > 15.429 AND PB207_PB204 < 15.741 AND
PB208_PB204 > 38.508 AND PB208_PB204 < 39.286
) OR (
`SIO2(WT%)` > 45.7 AND `SIO2(WT%)` < 48.7 AND
`NA2O(WT%)` > 2.08 AND `NA2O(WT%)` < 5.08 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.85 AND
`YB(PPM)` > 1.57 AND `YB(PPM)` < 4.71 AND
PB206_PB204 > 18.624 AND PB206_PB204 < 19 AND
PB207_PB204 > 15.425 AND PB207_PB204 < 15.737 AND
PB208_PB204 > 38.362 AND PB208_PB204 < 39.138
)) AND (
file_id = '2022-06-WFJZKY_AUSTRAL-COOK_ISLANDS.csv' OR
file_id = '2022-06-WFJZKY_MARQUESAS.csv' OR
file_id = '2022-06-WFJZKY_SAMOAN_ISLANDS.csv' OR
file_id = '2022-06-WFJZKY_SOCIETY_ISLANDS.csv'
)

# q13 (pofatu.sqlite)
SELECT * FROM (
SELECT s.id AS Sample, s.sample_category, s.location_region,
s.location_subregion, s.location_latitude AS lat, s.location_longitude AS long,
max(CASE WHEN m.parameter='SiO2 [%]' then m.value END) AS 'SiO2',
max(CASE WHEN m.parameter='TiO2 [%]' then m.value END) AS 'TiO2',
max(CASE WHEN m.parameter='Al2O3 [%]' then m.value END) AS 'Al2O3',
max(CASE WHEN m.parameter='Fe2O3 [%]' then m.value END) AS 'Fe2O3',
max(CASE WHEN m.parameter='FeO [%]' then m.value END) AS 'FeO',
max(CASE WHEN m.parameter='MnO [%]' then m.value END) AS 'MnO',
max(CASE WHEN m.parameter='MgO [%]' then m.value END) AS 'MgO',
max(CASE WHEN m.parameter='CaO [%]' then m.value END) AS 'CaO',
max(CASE WHEN m.parameter='Na2O [%]' then m.value END) AS 'Na2O',
max(CASE WHEN m.parameter='K2O [%]' then m.value END) AS 'K2O',
max(CASE WHEN m.parameter='Li [ppm]' then m.value END) AS 'Li',
max(CASE WHEN m.parameter='Sc [ppm]' then m.value END) AS 'Sc',
max(CASE WHEN m.parameter='Ti [ppm]' then m.value END) AS 'Ti',
max(CASE WHEN m.parameter='V [ppm]' then m.value END) AS 'V',
max(CASE WHEN m.parameter='Cr [ppm]' then m.value END) AS 'Cr',
max(CASE WHEN m.parameter='Co [ppm]' then m.value END) AS 'Co',
max(CASE WHEN m.parameter='Ni [ppm]' then m.value END) AS 'Ni',
max(CASE WHEN m.parameter='Cu [ppm]' then m.value END) AS 'Cu',
max(CASE WHEN m.parameter='Zn [ppm]' then m.value END) AS 'Zn',
max(CASE WHEN m.parameter='As [ppm]' then m.value END) AS 'As',
max(CASE WHEN m.parameter='Rb [ppm]' then m.value END) AS 'Rb',
max(CASE WHEN m.parameter='Sr [ppm]' then m.value END) AS 'Sr',
max(CASE WHEN m.parameter='Y [ppm]' then m.value END) AS 'Y',
max(CASE WHEN m.parameter='Zr [ppm]' then m.value END) AS 'Zr',
max(CASE WHEN m.parameter='Nb [ppm]' then m.value END) AS 'Nb',
max(CASE WHEN m.parameter='Cd [ppm]' then m.value END) AS 'Cd',
max(CASE WHEN m.parameter='Cs [ppm]' then m.value END) AS 'Cs',
max(CASE WHEN m.parameter='Ba [ppm]' then m.value END) AS 'Ba',
max(CASE WHEN m.parameter='La [ppm]' then m.value END) AS 'La',
max(CASE WHEN m.parameter='Ce [ppm]' then m.value END) AS 'Ce',
max(CASE WHEN m.parameter='Pr [ppm]' then m.value END) AS 'Pr',
max(CASE WHEN m.parameter='Nd [ppm]' then m.value END) AS 'Nd',
max(CASE WHEN m.parameter='Sm [ppm]' then m.value END) AS 'Sm',
max(CASE WHEN m.parameter='Eu [ppm]' then m.value END) AS 'Eu',
max(CASE WHEN m.parameter='Gd [ppm]' then m.value END) AS 'Gd',
max(CASE WHEN m.parameter='Tb [ppm]' then m.value END) AS 'Tb',
max(CASE WHEN m.parameter='Dy [ppm]' then m.value END) AS 'Dy',
max(CASE WHEN m.parameter='Ho [ppm]' then m.value END) AS 'Ho',
max(CASE WHEN m.parameter='Er [ppm]' then m.value END) AS 'Er',
max(CASE WHEN m.parameter='Tm [ppm]' then m.value END) AS 'Tm',
max(CASE WHEN m.parameter='Yb [ppm]' then m.value END) AS 'Yb',
max(CASE WHEN m.parameter='Lu [ppm]' then m.value END) AS 'Lu',
max(CASE WHEN m.parameter='Hf [ppm]' then m.value END) AS 'Hf',
max(CASE WHEN m.parameter='Ta [ppm]' then m.value END) AS 'Ta',
max(CASE WHEN m.parameter='Tl [ppm]' then m.value END) AS 'Tl',
max(CASE WHEN m.parameter='Pb [ppm]' then m.value END) AS 'Pb',
max(CASE WHEN m.parameter='Th [ppm]' then m.value END) AS 'Th',
max(CASE WHEN m.parameter='U [ppm]' then m.value END) AS 'U',
max(CASE WHEN m.parameter='Nd143_Nd144' then m.value END) AS 'Nd143_Nd144',
max(CASE WHEN m.parameter='Sr87_Sr86' then m.value END) AS 'Sr87_Sr86',
max(CASE WHEN m.parameter='Pb206_Pb204' then m.value END) AS 'Pb206_Pb204',
max(CASE WHEN m.parameter='Pb207_Pb204' then m.value END) AS 'Pb207_Pb204',
max(CASE WHEN m.parameter='Pb208_Pb204' then m.value END) AS 'Pb208_Pb204'
FROM 'samples.csv' AS s JOIN 'measurements.csv' AS m ON s.id=m.sample_id
WHERE s.sample_category = 'SOURCE' AND
m.parameter IN ('SiO2 [%]', 'TiO2 [%]', 'Al2O3 [%]', 'Fe2O3 [%]',
'FeO [%]', 'MnO [%]', 'MgO [%]', 'CaO [%]', 'Na2O [%]', 'K2O [%]',
'Li [ppm]', 'Sc [ppm]', 'Ti [ppm]', 'V [ppm]', 'Cr [ppm]',
'Co [ppm]', 'Ni [ppm]', 'Cu [ppm]', 'Zn [ppm]', 'As [ppm]',
'Rb [ppm]', 'Sr [ppm]', 'Y [ppm]', 'Zr [ppm]', 'Nb [ppm]',
'Cd [ppm]', 'Cs [ppm]', 'Ba [ppm]', 'La [ppm]', 'Ce [ppm]',
'Pr [ppm]', 'Nd [ppm]', 'Sm [ppm]', 'Eu [ppm]', 'Gd [ppm]',
'Tb [ppm]', 'Dy [ppm]', 'Ho [ppm]', 'Er [ppm]', 'Tm [ppm]',
'Yb [ppm]', 'Lu [ppm]', 'Hf [ppm]', 'Ta [ppm]', 'Tl [ppm]',
'Pb [ppm]', 'Th [ppm]', 'U [ppm]', 'Nd143_Nd144',
'Sr87_Sr86', 'Pb206_Pb204', 'Pb207_Pb204', 'Pb208_Pb204')
GROUP BY sample_id) AS Q13 WHERE
((
SiO2 > 45.2 AND SiO2 < 48.2 AND Na2O > 2.08 AND Na2O < 5.08 AND
K2O > 0 AND K2O < 2.9 AND Yb > 1.6 AND Yb < 4.8 AND
Pb206_Pb204 > 18.658 AND Pb206_Pb204 < 19.034 AND
Pb207_Pb204 > 15.430 AND Pb207_Pb204 < 15.742 AND
Pb208_Pb204 > 38.434 AND Pb208_Pb204 < 39.210
) OR (
SiO2 > 44.3 AND SiO2 < 47.3 AND Na2O > 2 AND Na2O < 5 AND
K2O > 0 AND K2O < 2.82 AND Yb > 1.62 AND Yb < 4.86 AND
Pb206_Pb204 > 18.701 AND Pb206_Pb204 < 19.079 AND
Pb207_Pb204 > 15.431 AND Pb207_Pb204 < 15.743 AND
Pb208_Pb204 > 38.482 AND Pb208_Pb204 < 39.260
) OR (
SiO2 > 43.5 AND SiO2 < 46.5 AND Na2O > 1.9 AND Na2O < 4.9 AND
K2O > 0 AND K2O < 2.78 AND Yb > 1.56 AND Yb < 4.68 AND
Pb206_Pb204 > 18.722 AND Pb206_Pb204 < 19.1 AND
Pb207_Pb204 > 15.427 AND Pb207_Pb204 < 15.739 AND
Pb208_Pb204 > 38.472 AND Pb208_Pb204 < 39.25
) OR (
SiO2 > 45.9 AND SiO2 < 48.9 AND Na2O > 2.18 AND Na2O < 5.18 AND
K2O > 0 AND K2O < 2.99 AND Yb > 1.49 AND Yb < 4.46 AND
Pb206_Pb204 > 18.72 AND Pb206_Pb204 < 19.098 AND
Pb207_Pb204 > 15.445 AND Pb207_Pb204 < 15.757 AND
Pb208_Pb204 > 38.505 AND Pb208_Pb204 < 39.283
) OR (
SiO2 > 44.1 AND SiO2 < 47.1 AND Na2O > 1.8 AND Na2O < 4.8 AND
K2O > 0 AND K2O < 2.72 AND Yb > 1.4 AND Yb < 4.19 AND
Pb206_Pb204 > 18.736 AND Pb206_Pb204 < 19.114 AND
Pb207_Pb204 > 15.429 AND Pb207_Pb204 < 15.741 AND
Pb208_Pb204 > 38.508 AND Pb208_Pb204 < 39.286
) OR (
SiO2 > 45.7 AND SiO2 < 48.7 AND Na2O > 2.08 AND Na2O < 5.08 AND
K2O > 0 AND K2O < 2.85 AND Yb > 1.57 AND Yb < 4.71 AND
Pb206_Pb204 > 18.624 AND Pb206_Pb204 < 19 AND
Pb207_Pb204 > 15.425 AND Pb207_Pb204 < 15.737 AND
Pb208_Pb204 > 38.362 AND Pb208_Pb204 < 39.138
))

#### K-12-24 ####
# q14 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
ND143_ND144 AS Nd143_Nd144, SR87_SR86 AS Sr87_Sr86,
PB206_PB204 AS Pb206_Pb204, PB207_PB204 AS Pb207_Pb204,
PB208_PB204 AS Pb208_Pb204
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
ND143_ND144 > 0.512477 AND ND143_ND144 < 0.513503 AND
SR87_SR86 > 0.702761 AND SR87_SR86 < 0.704167 AND
PB206_PB204 > 18.221 AND PB206_PB204 < 18.589 AND
PB207_PB204 > 15.372 AND PB207_PB204 < 15.682 AND
PB208_PB204 > 37.927 AND PB208_PB204 < 38.693

# q15 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
`SIO2(WT%)` AS SiO2,`TIO2(WT%)` AS TiO2, `K2O(WT%)`AS K2O, `NA2O(WT%)` AS Na2O,
`RB(PPM)` AS Rb, `BA(PPM)` AS Ba, `TH(PPM)` AS Th, `U(PPM)` AS U,
`NB(PPM)` AS Nb, `LA(PPM)` AS La, `CE(PPM)` AS Ce, `ND(PPM)` AS Nd,
`SR(PPM)` AS Sr, `SM(PPM)` AS Sm, `ZR(PPM)` AS Zr, `TI(PPM)` AS Ti,
`EU(PPM)` AS Eu, `GD(PPM)` AS Gd, `Y(PPM)` AS Y, `YB(PPM)` AS Yb
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
`K2O(WT%)` < 2.91 AND
`NA2O(WT%)` > 1.61 AND `NA2O(WT%)` < 4.61 AND
`MGO(WT%)` > 5.84 AND `MGO(WT%)` < 8.84 AND
`YB(PPM)` > 1.26 AND `YB(PPM)` < 3.78 AND 
(
file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part1.csv' OR
file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part2.csv' OR
file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv'
)

#### K-12-25 ####
# q16 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
ND143_ND144 AS Nd143_Nd144, SR87_SR86 AS Sr87_Sr86,
PB206_PB204 AS Pb206_Pb204, PB207_PB204 AS Pb207_Pb204,
PB208_PB204 AS Pb208_Pb204
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
ND143_ND144 > 0.512396 AND ND143_ND144 < 0.513422 AND
SR87_SR86 > 0.703423 AND SR87_SR86 < 0.704831 AND
PB206_PB204 > 18.438 AND PB206_PB204 < 18.81 AND
PB207_PB204 > 15.427 AND PB207_PB204 < 15.739 AND
PB208_PB204 > 38.274 AND PB208_PB204 < 39.048

# q17 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
`SIO2(WT%)` AS SiO2,`TIO2(WT%)` AS TiO2, `K2O(WT%)`AS K2O, `NA2O(WT%)` AS Na2O,
`RB(PPM)` AS Rb, `BA(PPM)` AS Ba, `TH(PPM)` AS Th, `U(PPM)` AS U,
`NB(PPM)` AS Nb, `LA(PPM)` AS La, `CE(PPM)` AS Ce, `ND(PPM)` AS Nd,
`SR(PPM)` AS Sr, `SM(PPM)` AS Sm, `ZR(PPM)` AS Zr, `TI(PPM)` AS Ti,
`EU(PPM)` AS Eu, `GD(PPM)` AS Gd, `Y(PPM)` AS Y, `YB(PPM)` AS Yb
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
`SIO2(WT%)` > 43.1 AND `SIO2(WT%)` < 46.1 AND
`NA2O(WT%)` > 0.89 AND `NA2O(WT%)` < 3.89 AND
`K2O(WT%)` < 1.893 AND
`YB(PPM)` > 0.86 AND `YB(PPM)` < 2.58 AND
PB206_PB204 > 18.438 AND PB206_PB204 < 18.81 AND
PB207_PB204 > 15.427 AND PB207_PB204 < 15.739 AND
PB208_PB204 > 38.274 AND PB208_PB204 < 39.048 AND
(
file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part1.csv' OR
file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part2.csv' OR
file_id = '2022-06-WFJZKY_AUSTRAL-COOK_ISLANDS.csv' OR
file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv' OR
file_id = '2022-06-WFJZKY_SOCIETY_ISLANDS.csv' OR
file_id = '2022-06-WFJZKY_PITCAIRN-GAMBIER_CHAIN.csv'
)

#### K-12-26 ####
# q18 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
ND143_ND144 AS Nd143_Nd144, SR87_SR86 AS Sr87_Sr86,
PB206_PB204 AS Pb206_Pb204, PB207_PB204 AS Pb207_Pb204,
PB208_PB204 AS Pb208_Pb204
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
ND143_ND144 > 0.51248 AND ND143_ND144 < 0.513506 AND
SR87_SR86 > 0.702897 AND SR87_SR86 < 0.704305 AND
PB206_PB204 > 18.54 AND PB206_PB204 < 18.914 AND
PB207_PB204 > 15.378 AND PB207_PB204 < 15.688 AND
PB208_PB204 > 38.059 AND PB208_PB204 < 38.827

# q19 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
`SIO2(WT%)` AS SiO2,`TIO2(WT%)` AS TiO2, `K2O(WT%)`AS K2O, `NA2O(WT%)` AS Na2O,
`RB(PPM)` AS Rb, `BA(PPM)` AS Ba, `TH(PPM)` AS Th, `U(PPM)` AS U,
`NB(PPM)` AS Nb, `LA(PPM)` AS La, `CE(PPM)` AS Ce, `ND(PPM)` AS Nd,
`SR(PPM)` AS Sr, `SM(PPM)` AS Sm, `ZR(PPM)` AS Zr, `TI(PPM)` AS Ti,
`EU(PPM)` AS Eu, `GD(PPM)` AS Gd, `Y(PPM)` AS Y, `YB(PPM)` AS Yb
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
`SIO2(WT%)` > 42.9 AND `SIO2(WT%)` < 45.9 AND
PB206_PB204 > 18.54 AND PB206_PB204 < 18.914 AND
PB207_PB204 > 15.378 AND PB207_PB204 < 15.688 AND
PB208_PB204 > 38.059 AND PB208_PB204 < 38.827 AND
(
file_id = '2022-06-WFJZKY_AUSTRAL-COOK_ISLANDS.csv' OR
file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv' OR
file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part1.csv' OR
file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part2.csv' OR
file_id = '2022-06-WFJZKY_SOCIETY_ISLANDS.csv'
)

#### Fig S11 ####
# q20 (georoc.sqlite)
SELECT * FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
(LONGITUDE_MAX > 90 OR LONGITUDE_MAX < 0) AND
LATITUDE_MAX < 20 AND TECTONIC_SETTING='CONVERGENT MARGIN'

#### Fig S13 ####
# q21 (pofatu.sqlite)
SELECT * FROM (
SELECT s.id AS Sample, s.sample_category, s.location_region,
s.location_subregion, s.location_latitude AS lat, s.location_longitude AS long,
max(CASE WHEN m.parameter='SiO2 [%]' then m.value END) AS 'SiO2',
max(CASE WHEN m.parameter='TiO2 [%]' then m.value END) AS 'TiO2',
max(CASE WHEN m.parameter='Al2O3 [%]' then m.value END) AS 'Al2O3',
max(CASE WHEN m.parameter='Fe2O3 [%]' then m.value END) AS 'Fe2O3',
max(CASE WHEN m.parameter='FeO [%]' then m.value END) AS 'FeO',
max(CASE WHEN m.parameter='MnO [%]' then m.value END) AS 'MnO',
max(CASE WHEN m.parameter='MgO [%]' then m.value END) AS 'MgO',
max(CASE WHEN m.parameter='CaO [%]' then m.value END) AS 'CaO',
max(CASE WHEN m.parameter='Na2O [%]' then m.value END) AS 'Na2O',
max(CASE WHEN m.parameter='K2O [%]' then m.value END) AS 'K2O',
max(CASE WHEN m.parameter='Li [ppm]' then m.value END) AS 'Li',
max(CASE WHEN m.parameter='Sc [ppm]' then m.value END) AS 'Sc',
max(CASE WHEN m.parameter='Ti [ppm]' then m.value END) AS 'Ti',
max(CASE WHEN m.parameter='V [ppm]' then m.value END) AS 'V',
max(CASE WHEN m.parameter='Cr [ppm]' then m.value END) AS 'Cr',
max(CASE WHEN m.parameter='Co [ppm]' then m.value END) AS 'Co',
max(CASE WHEN m.parameter='Ni [ppm]' then m.value END) AS 'Ni',
max(CASE WHEN m.parameter='Cu [ppm]' then m.value END) AS 'Cu',
max(CASE WHEN m.parameter='Zn [ppm]' then m.value END) AS 'Zn',
max(CASE WHEN m.parameter='As [ppm]' then m.value END) AS 'As',
max(CASE WHEN m.parameter='Rb [ppm]' then m.value END) AS 'Rb',
max(CASE WHEN m.parameter='Sr [ppm]' then m.value END) AS 'Sr',
max(CASE WHEN m.parameter='Y [ppm]' then m.value END) AS 'Y',
max(CASE WHEN m.parameter='Zr [ppm]' then m.value END) AS 'Zr',
max(CASE WHEN m.parameter='Nb [ppm]' then m.value END) AS 'Nb',
max(CASE WHEN m.parameter='Cd [ppm]' then m.value END) AS 'Cd',
max(CASE WHEN m.parameter='Cs [ppm]' then m.value END) AS 'Cs',
max(CASE WHEN m.parameter='Ba [ppm]' then m.value END) AS 'Ba',
max(CASE WHEN m.parameter='La [ppm]' then m.value END) AS 'La',
max(CASE WHEN m.parameter='Ce [ppm]' then m.value END) AS 'Ce',
max(CASE WHEN m.parameter='Pr [ppm]' then m.value END) AS 'Pr',
max(CASE WHEN m.parameter='Nd [ppm]' then m.value END) AS 'Nd',
max(CASE WHEN m.parameter='Sm [ppm]' then m.value END) AS 'Sm',
max(CASE WHEN m.parameter='Eu [ppm]' then m.value END) AS 'Eu',
max(CASE WHEN m.parameter='Gd [ppm]' then m.value END) AS 'Gd',
max(CASE WHEN m.parameter='Tb [ppm]' then m.value END) AS 'Tb',
max(CASE WHEN m.parameter='Dy [ppm]' then m.value END) AS 'Dy',
max(CASE WHEN m.parameter='Ho [ppm]' then m.value END) AS 'Ho',
max(CASE WHEN m.parameter='Er [ppm]' then m.value END) AS 'Er',
max(CASE WHEN m.parameter='Tm [ppm]' then m.value END) AS 'Tm',
max(CASE WHEN m.parameter='Yb [ppm]' then m.value END) AS 'Yb',
max(CASE WHEN m.parameter='Lu [ppm]' then m.value END) AS 'Lu',
max(CASE WHEN m.parameter='Hf [ppm]' then m.value END) AS 'Hf',
max(CASE WHEN m.parameter='Ta [ppm]' then m.value END) AS 'Ta',
max(CASE WHEN m.parameter='Tl [ppm]' then m.value END) AS 'Tl',
max(CASE WHEN m.parameter='Pb [ppm]' then m.value END) AS 'Pb',
max(CASE WHEN m.parameter='Th [ppm]' then m.value END) AS 'Th',
max(CASE WHEN m.parameter='U [ppm]' then m.value END) AS 'U',
max(CASE WHEN m.parameter='Nd143_Nd144' then m.value END) AS 'Nd143_Nd144',
max(CASE WHEN m.parameter='Sr87_Sr86' then m.value END) AS 'Sr87_Sr86',
max(CASE WHEN m.parameter='Pb206_Pb204' then m.value END) AS 'Pb206_Pb204',
max(CASE WHEN m.parameter='Pb207_Pb204' then m.value END) AS 'Pb207_Pb204',
max(CASE WHEN m.parameter='Pb208_Pb204' then m.value END) AS 'Pb208_Pb204'
FROM 'samples.csv' AS s JOIN 'measurements.csv' AS m ON s.id=m.sample_id
WHERE m.parameter IN ('SiO2 [%]', 'TiO2 [%]', 'Al2O3 [%]', 'MnO [%]', 'MgO [%]',
'Fe2O3 [%]', 'FeO [%]', 'CaO [%]','Na2O [%]', 'K2O [%]',
'Li [ppm]', 'Sc [ppm]', 'Ti [ppm]', 'V [ppm]', 'Cr [ppm]', 'Co [ppm]',
'Ni [ppm]', 'Cu [ppm]', 'Zn [ppm]', 'As [ppm]', 'Rb [ppm]', 'Sr [ppm]',
'Y [ppm]', 'Zr [ppm]', 'Nb [ppm]', 'Cd [ppm]', 'Cs [ppm]', 'Ba [ppm]',
'La [ppm]', 'Ce [ppm]', 'Pr [ppm]', 'Nd [ppm]', 'Sm [ppm]', 'Eu [ppm]',
'Gd [ppm]', 'Tb [ppm]', 'Dy [ppm]', 'Ho [ppm]', 'Er [ppm]', 'Tm [ppm]',
'Yb [ppm]', 'Lu [ppm]', 'Hf [ppm]', 'Ta [ppm]', 'Tl [ppm]', 'Pb [ppm]',
'Th [ppm]', 'U [ppm]', 'Nd143_Nd144', 'Sr87_Sr86', 'Pb206_Pb204',
'Pb207_Pb204', 'Pb208_Pb204')
GROUP BY sample_id) AS Q21 WHERE
(sample_category = 'SOURCE' AND
location_subregion = 'VANUA LAVA' AND
(
Sample = 'reepmeyer2008_ANU9006' OR
Sample = 'reepmeyer2008_ANU9009' OR
Sample = 'reepmeyer2008_ANU9008' OR
Sample = 'reepmeyer2008_ANU9003'
))

# q22 (georoc.sqlite)
SELECT id AS Sample, file_id,
LOCATION, LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
`TIO2(WT%)` AS TiO2, `K2O(WT%)`AS K2O,
`CS(PPM)` AS Cs, `RB(PPM)` AS Rb, `BA(PPM)` AS Ba, `TH(PPM)` AS Th,
`U(PPM)` AS U, `NB(PPM)` AS Nb, `TA(PPM)` AS Ta, `LA(PPM)` AS La,
`CE(PPM)` AS Ce, `PR(PPM)` AS Pr, `ND(PPM)` AS Nd, `SR(PPM)` AS Sr,
`SM(PPM)` AS Sm, `ZR(PPM)` AS Zr, `TI(PPM)` AS Ti, `EU(PPM)` AS Eu,
`GD(PPM)` AS Gd, `TB(PPM)` AS Tb, `DY(PPM)` AS Dy, `Y(PPM)` AS Y,
`ER(PPM)` AS Er, `YB(PPM)` AS Yb, `LU(PPM)` AS Lu
FROM 'sample' WHERE
id='13423-E5/11' OR
id='13426-F7/2' OR
id='13436-2654A' OR
id='13436-2649' OR
id='13437-F5/2' OR
id='634326'

# q23 (georoc.sqlite)
SELECT id AS Sample, file_id,
LOCATION, LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
`SIO2(WT%)` AS SiO2, `K2O(WT%)`AS K2O
FROM 'sample'
WHERE file_id= '2022-06-PVFZCE_BISMARCK_ARC_NEW_BRITAIN_ARC.csv'

# q24 (georoc.sqlite)
SELECT id AS Sample, file_id,
LOCATION, LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
`SIO2(WT%)`AS SiO2, `TIO2(WT%)` AS TiO2, `K2O(WT%)`AS K2O,
`CS(PPM)` AS Cs, `RB(PPM)` AS Rb, `BA(PPM)` AS Ba, `TH(PPM)` AS Th,
`U(PPM)` AS U, `NB(PPM)` AS Nb, `TA(PPM)` AS Ta, `LA(PPM)` AS La,
`CE(PPM)` AS Ce, `PR(PPM)` AS Pr, `ND(PPM)` AS Nd, `SR(PPM)` AS Sr,
`SM(PPM)` AS Sm, `ZR(PPM)` AS Zr, `TI(PPM)` AS Ti, `EU(PPM)` AS Eu,
`GD(PPM)` AS Gd, `TB(PPM)` AS Tb, `DY(PPM)` AS Dy, `Y(PPM)` AS Y,
`ER(PPM)` AS Er, `YB(PPM)` AS Yb, `LU(PPM)` AS Lu
FROM 'sample' 
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE = 'VOL' AND
(long > 90 OR long < 0) AND
lat < 20 AND TECTONIC_SETTING='CONVERGENT MARGIN' AND
SiO2 > 52.1 AND SiO2 < 55.1 AND K2O > 0 AND K2O < 1.55 AND
(
file_id= '2022-06-PVFZCE_BISMARCK_ARC_NEW_BRITAIN_ARC.csv' OR
file_id= '2022-06-PVFZCE_LUZON_ARC.csv' OR
file_id= '2022-06-PVFZCE_NEW_HEBRIDES_ARC_VANUATU_ARCHIPELAGO.csv' OR
file_id= '2022-06-PVFZCE_SULAWESI_ARC.csv'
)

# q25 (georoc.sqlite)
SELECT id AS Sample, file_id,
LOCATION, LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
`SIO2(WT%)`AS SiO2, `TIO2(WT%)` AS TiO2, `K2O(WT%)`AS K2O,
`CS(PPM)` AS Cs, `RB(PPM)` AS Rb, `BA(PPM)` AS Ba, `TH(PPM)` AS Th,
`U(PPM)` AS U, `NB(PPM)` AS Nb, `TA(PPM)` AS Ta, `LA(PPM)` AS La,
`CE(PPM)` AS Ce, `PR(PPM)` AS Pr, `ND(PPM)` AS Nd, `SR(PPM)` AS Sr,
`SM(PPM)` AS Sm, `ZR(PPM)` AS Zr, `TI(PPM)` AS Ti, `EU(PPM)` AS Eu,
`GD(PPM)` AS Gd, `TB(PPM)` AS Tb, `DY(PPM)` AS Dy, `Y(PPM)` AS Y,
`ER(PPM)` AS Er, `YB(PPM)` AS Yb, `LU(PPM)` AS Lu
FROM 'sample'
WHERE
id = '70512' OR
id = '13306-VMAC6' OR
id = '317050' OR
id = '1871790' OR
id = '144138-KS094' OR
id = '13303-UA10'

# q26 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
`SIO2(WT%)`AS SiO2, `K2O(WT%)`AS K2O
FROM 'sample'
WHERE file_id= '2022-06-PVFZCE_LUZON_ARC.csv' OR
file_id= '2022-06-PVFZCE_NEW_HEBRIDES_ARC_VANUATU_ARCHIPELAGO.csv' OR
file_id= '2022-06-PVFZCE_SULAWESI_ARC.csv' OR
file_id= '2022-06-PVFZCE_TONGA_ARC.csv'

# q27 (georoc.sqlite)
SELECT id AS Sample, file_id,
LOCATION, LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
`SIO2(WT%)`AS SiO2, `TIO2(WT%)` AS TiO2, `K2O(WT%)`AS K2O,
`CS(PPM)` AS Cs, `RB(PPM)` AS Rb, `BA(PPM)` AS Ba, `TH(PPM)` AS Th,
`U(PPM)` AS U, `NB(PPM)` AS Nb, `TA(PPM)` AS Ta, `LA(PPM)` AS La,
`CE(PPM)` AS Ce, `PR(PPM)` AS Pr, `ND(PPM)` AS Nd, `SR(PPM)` AS Sr,
`SM(PPM)` AS Sm, `ZR(PPM)` AS Zr, `TI(PPM)` AS Ti, `EU(PPM)` AS Eu,
`GD(PPM)` AS Gd, `TB(PPM)` AS Tb, `DY(PPM)` AS Dy, `Y(PPM)` AS Y,
`ER(PPM)` AS Er, `YB(PPM)` AS Yb, `LU(PPM)` AS Lu
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
(long > 90 OR long < 0) AND lat < 20 AND
TECTONIC_SETTING='CONVERGENT MARGIN' AND
SiO2 > 48.2 AND SiO2 < 51.2 AND K2O > 0.14 AND K2O < 3.14 AND
Yb > 1.05 AND Yb < 3.15 AND
(
file_id= '2022-06-PVFZCE_BISMARCK_ARC_NEW_BRITAIN_ARC.csv' OR
file_id= '2022-06-PVFZCE_LUZON_ARC.csv' OR
file_id= '2022-06-PVFZCE_NEW_HEBRIDES_ARC_VANUATU_ARCHIPELAGO.csv' OR
file_id= '2022-06-PVFZCE_SULAWESI_ARC.csv' OR
file_id= '2022-06-PVFZCE_TONGA_ARC.csv'
)

#### Fig S15 ####
# q28 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION,
LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
ND143_ND144 AS Nd143_Nd144, SR87_SR86 AS Sr87_Sr86,
PB206_PB204 AS Pb206_Pb204, PB207_PB204 AS Pb207_Pb204,
PB208_PB204 AS Pb208_Pb204
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND'

# q29 (pofatu.sqlite)
SELECT s.id AS sample_id, s.sample_category, s.location_region,
s.location_subregion, s.location_latitude, s.location_longitude,
max(CASE WHEN m.parameter='Nd143_Nd144' then m.value END) AS 'Nd143_Nd144',
max(CASE WHEN m.parameter='Sr87_Sr86' then m.value END) AS 'Sr87_Sr86',
max(CASE WHEN m.parameter='Pb206_Pb204' then m.value END) AS 'Pb206_Pb204',
max(CASE WHEN m.parameter='Pb207_Pb204' then m.value END) AS 'Pb207_Pb204',
max(CASE WHEN m.parameter='Pb208_Pb204' then m.value END) AS 'Pb208_Pb204'
FROM 'samples.csv' AS s JOIN 'measurements.csv' AS m ON s.id=m.sample_id
WHERE s.sample_category = 'SOURCE'
AND m.parameter IN ('Nd143_Nd144', 'Sr87_Sr86','Pb206_Pb204', 'Pb207_Pb204',
'Pb208_Pb204') GROUP BY sample_id 

#### Fig S16 ####
# q30 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION, LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
`SIO2(WT%)`AS SiO2, `TIO2(WT%)` AS TiO2, `MGO(WT%)`AS MgO,
`K2O(WT%)`AS K2O, `NA2O(WT%)`AS Na2O,
`CS(PPM)` AS Cs, `RB(PPM)` AS Rb, `BA(PPM)` AS Ba, `TH(PPM)` AS Th,
`U(PPM)` AS U, `NB(PPM)` AS Nb, `TA(PPM)` AS Ta, `LA(PPM)` AS La,
`CE(PPM)` AS Ce, `PR(PPM)` AS Pr, `ND(PPM)` AS Nd, `SR(PPM)` AS Sr,
`SM(PPM)` AS Sm, `ZR(PPM)` AS Zr, `TI(PPM)` AS Ti, `EU(PPM)` AS Eu,
`GD(PPM)` AS Gd, `TB(PPM)` AS Tb, `DY(PPM)` AS Dy, `Y(PPM)` AS Y,
`ER(PPM)` AS Er, `YB(PPM)` AS Yb, `LU(PPM)` AS Lu
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
((
`SIO2(WT%)` > 45.2 AND `SIO2(WT%)` < 48.2 AND
`NA2O(WT%)` > 2.08 AND `NA2O(WT%)` < 5.08 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.9 AND
`MGO(WT%)` > 3.08 AND `MGO(WT%)` < 6.08
) OR (
`SIO2(WT%)` > 44.3 AND `SIO2(WT%)` < 47.3 AND
`NA2O(WT%)` > 2 AND `NA2O(WT%)` < 5 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.82 AND
`MGO(WT%)` > 3.52 AND `MGO(WT%)` < 6.52
) OR (
`SIO2(WT%)` > 43.5 AND `SIO2(WT%)` < 46.5 AND
`NA2O(WT%)` > 1.9 AND `NA2O(WT%)` < 4.9 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.78 AND
`MGO(WT%)` > 3.79 AND `MGO(WT%)` < 6.79
) OR (
`SIO2(WT%)` > 45.9 AND `SIO2(WT%)` < 48.9 AND
`NA2O(WT%)` > 2.18 AND `NA2O(WT%)` < 5.18 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.99 AND
`MGO(WT%)` > 3.44 AND `MGO(WT%)` < 6.44
) OR (
`SIO2(WT%)` > 44.1 AND `SIO2(WT%)` < 47.1 AND
`NA2O(WT%)` > 1.8 AND `NA2O(WT%)` < 4.8 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.72 AND
`MGO(WT%)` > 4.15 AND `MGO(WT%)` < 7.15
) OR (
`SIO2(WT%)` > 45.7 AND `SIO2(WT%)` < 48.7 AND
`NA2O(WT%)` > 2.08 AND `NA2O(WT%)` < 5.08 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.85 AND
`MGO(WT%)` > 3.10 AND `MGO(WT%)` < 6.10
))

# q31 (pofatu.sqlite)
SELECT * FROM (
SELECT s.id AS Sample, s.sample_category, s.location_region,
s.location_subregion, s.site_name, s.sample_comment,
s.location_latitude AS lat, s.location_longitude AS long,
max(CASE WHEN m.parameter='SiO2 [%]' then m.value END) AS 'SiO2',
max(CASE WHEN m.parameter='TiO2 [%]' then m.value END) AS 'TiO2',
max(CASE WHEN m.parameter='Al2O3 [%]' then m.value END) AS 'Al2O3',
max(CASE WHEN m.parameter='Fe2O3 [%]' then m.value END) AS 'Fe2O3',
max(CASE WHEN m.parameter='FeO [%]' then m.value END) AS 'FeO',
max(CASE WHEN m.parameter='MnO [%]' then m.value END) AS 'MnO',
max(CASE WHEN m.parameter='MgO [%]' then m.value END) AS 'MgO',
max(CASE WHEN m.parameter='CaO [%]' then m.value END) AS 'CaO',
max(CASE WHEN m.parameter='Na2O [%]' then m.value END) AS 'Na2O',
max(CASE WHEN m.parameter='K2O [%]' then m.value END) AS 'K2O',
max(CASE WHEN m.parameter='Li [ppm]' then m.value END) AS 'Li',
max(CASE WHEN m.parameter='Sc [ppm]' then m.value END) AS 'Sc',
max(CASE WHEN m.parameter='Ti [ppm]' then m.value END) AS 'Ti',
max(CASE WHEN m.parameter='V [ppm]' then m.value END) AS 'V',
max(CASE WHEN m.parameter='Cr [ppm]' then m.value END) AS 'Cr',
max(CASE WHEN m.parameter='Co [ppm]' then m.value END) AS 'Co',
max(CASE WHEN m.parameter='Ni [ppm]' then m.value END) AS 'Ni',
max(CASE WHEN m.parameter='Cu [ppm]' then m.value END) AS 'Cu',
max(CASE WHEN m.parameter='Zn [ppm]' then m.value END) AS 'Zn',
max(CASE WHEN m.parameter='As [ppm]' then m.value END) AS 'As',
max(CASE WHEN m.parameter='Rb [ppm]' then m.value END) AS 'Rb',
max(CASE WHEN m.parameter='Sr [ppm]' then m.value END) AS 'Sr',
max(CASE WHEN m.parameter='Y [ppm]' then m.value END) AS 'Y',
max(CASE WHEN m.parameter='Zr [ppm]' then m.value END) AS 'Zr',
max(CASE WHEN m.parameter='Nb [ppm]' then m.value END) AS 'Nb',
max(CASE WHEN m.parameter='Cd [ppm]' then m.value END) AS 'Cd',
max(CASE WHEN m.parameter='Cs [ppm]' then m.value END) AS 'Cs',
max(CASE WHEN m.parameter='Ba [ppm]' then m.value END) AS 'Ba',
max(CASE WHEN m.parameter='La [ppm]' then m.value END) AS 'La',
max(CASE WHEN m.parameter='Ce [ppm]' then m.value END) AS 'Ce',
max(CASE WHEN m.parameter='Pr [ppm]' then m.value END) AS 'Pr',
max(CASE WHEN m.parameter='Nd [ppm]' then m.value END) AS 'Nd',
max(CASE WHEN m.parameter='Sm [ppm]' then m.value END) AS 'Sm',
max(CASE WHEN m.parameter='Eu [ppm]' then m.value END) AS 'Eu',
max(CASE WHEN m.parameter='Gd [ppm]' then m.value END) AS 'Gd',
max(CASE WHEN m.parameter='Tb [ppm]' then m.value END) AS 'Tb',
max(CASE WHEN m.parameter='Dy [ppm]' then m.value END) AS 'Dy',
max(CASE WHEN m.parameter='Ho [ppm]' then m.value END) AS 'Ho',
max(CASE WHEN m.parameter='Er [ppm]' then m.value END) AS 'Er',
max(CASE WHEN m.parameter='Tm [ppm]' then m.value END) AS 'Tm',
max(CASE WHEN m.parameter='Yb [ppm]' then m.value END) AS 'Yb',
max(CASE WHEN m.parameter='Lu [ppm]' then m.value END) AS 'Lu',
max(CASE WHEN m.parameter='Hf [ppm]' then m.value END) AS 'Hf',
max(CASE WHEN m.parameter='Ta [ppm]' then m.value END) AS 'Ta',
max(CASE WHEN m.parameter='Tl [ppm]' then m.value END) AS 'Tl',
max(CASE WHEN m.parameter='Pb [ppm]' then m.value END) AS 'Pb',
max(CASE WHEN m.parameter='Th [ppm]' then m.value END) AS 'Th',
max(CASE WHEN m.parameter='U [ppm]' then m.value END) AS 'U',
max(CASE WHEN m.parameter='Nd143_Nd144' then m.value END) AS 'Nd143_Nd144',
max(CASE WHEN m.parameter='Sr87_Sr86' then m.value END) AS 'Sr87_Sr86',
max(CASE WHEN m.parameter='Pb206_Pb204' then m.value END) AS 'Pb206_Pb204',
max(CASE WHEN m.parameter='Pb207_Pb204' then m.value END) AS 'Pb207_Pb204',
max(CASE WHEN m.parameter='Pb208_Pb204' then m.value END) AS 'Pb208_Pb204'
FROM 'samples.csv' AS s JOIN 'measurements.csv' AS m ON s.id=m.sample_id
WHERE location_region = 'SAMOA' AND
m.parameter IN ('SiO2 [%]', 'TiO2 [%]', 'Al2O3 [%]', 'Fe2O3 [%]', 'FeO [%]',
'MnO [%]', 'MgO [%]', 'CaO [%]', 'Na2O [%]', 'K2O [%]',
'Li [ppm]', 'Sc [ppm]', 'Ti [ppm]', 'V [ppm]', 'Cr [ppm]',
'Co [ppm]', 'Ni [ppm]', 'Cu [ppm]', 'Zn [ppm]', 'As [ppm]',
'Rb [ppm]', 'Sr [ppm]', 'Y [ppm]', 'Zr [ppm]', 'Nb [ppm]',
'Cd [ppm]', 'Cs [ppm]', 'Ba [ppm]', 'La [ppm]', 'Ce [ppm]',
'Pr [ppm]', 'Nd [ppm]', 'Sm [ppm]', 'Eu [ppm]', 'Gd [ppm]',
'Tb [ppm]', 'Dy [ppm]', 'Ho [ppm]', 'Er [ppm]', 'Tm [ppm]',
'Yb [ppm]', 'Lu [ppm]', 'Hf [ppm]', 'Ta [ppm]', 'Tl [ppm]',
'Pb [ppm]', 'Th [ppm]', 'U [ppm]', 'Nd143_Nd144',
'Sr87_Sr86', 'Pb206_Pb204', 'Pb207_Pb204', 'Pb208_Pb204')
GROUP BY sample_id) AS Q31 WHERE
((
SiO2 > 45.2 AND SiO2 < 48.2 AND
Na2O > 2.08 AND Na2O < 5.08 AND
K2O > 0 AND K2O < 2.9 AND
MgO > 3.08 AND MgO < 6.08
) OR (
SiO2 > 44.3 AND SiO2 < 47.3 AND
Na2O > 2 AND Na2O < 5 AND
K2O > 0 AND K2O < 2.82 AND
MgO > 3.52 AND MgO < 6.52
) OR (
SiO2 > 43.5 AND SiO2 < 46.5 AND
Na2O > 1.9 AND Na2O < 4.9 AND
K2O > 0 AND K2O < 2.78 AND
MgO > 3.79 AND MgO < 6.79
) OR (
SiO2 > 45.9 AND SiO2 < 48.9 AND
Na2O > 2.18 AND Na2O < 5.18 AND
K2O > 0 AND K2O < 2.99 AND
MgO > 3.44 AND MgO < 6.44
) OR (
SiO2 > 44.1 AND SiO2 < 47.1 AND
Na2O > 1.8 AND Na2O < 4.8 AND
K2O > 0 AND K2O < 2.72 AND
MgO > 4.15 AND MgO < 7.15
) OR (
SiO2 > 45.7 AND SiO2 < 48.7 AND
Na2O > 2.08 AND Na2O < 5.08 AND
K2O > 0 AND K2O < 2.85 AND
MgO > 3.10 AND MgO < 6.10
))

# q32 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION, LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
`SIO2(WT%)`AS SiO2, `TIO2(WT%)` AS TiO2, `MGO(WT%)`AS MgO,
`K2O(WT%)`AS K2O, `NA2O(WT%)`AS Na2O,
`CS(PPM)` AS Cs, `RB(PPM)` AS Rb, `BA(PPM)` AS Ba, `TH(PPM)` AS Th,
`U(PPM)` AS U, `NB(PPM)` AS Nb, `TA(PPM)` AS Ta, `LA(PPM)` AS La,
`CE(PPM)` AS Ce, `PR(PPM)` AS Pr, `ND(PPM)` AS Nd, `SR(PPM)` AS Sr,
`SM(PPM)` AS Sm, `ZR(PPM)` AS Zr, `TI(PPM)` AS Ti, `EU(PPM)` AS Eu,
`GD(PPM)` AS Gd, `TB(PPM)` AS Tb, `DY(PPM)` AS Dy, `Y(PPM)` AS Y,
`ER(PPM)` AS Er, `YB(PPM)` AS Yb, `LU(PPM)` AS Lu
FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
`NA2O(WT%)` > 1.61 AND `NA2O(WT%)` < 4.61 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.91 AND
file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv'

# q33 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION, LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
`SIO2(WT%)`AS SiO2, `TIO2(WT%)` AS TiO2, `MGO(WT%)`AS MgO,
`K2O(WT%)`AS K2O, `NA2O(WT%)`AS Na2O,
`CS(PPM)` AS Cs, `RB(PPM)` AS Rb, `BA(PPM)` AS Ba, `TH(PPM)` AS Th,
`U(PPM)` AS U, `NB(PPM)` AS Nb, `TA(PPM)` AS Ta, `LA(PPM)` AS La,
`CE(PPM)` AS Ce, `PR(PPM)` AS Pr, `ND(PPM)` AS Nd, `SR(PPM)` AS Sr,
`SM(PPM)` AS Sm, `ZR(PPM)` AS Zr, `TI(PPM)` AS Ti, `EU(PPM)` AS Eu,
`GD(PPM)` AS Gd, `TB(PPM)` AS Tb, `DY(PPM)` AS Dy, `Y(PPM)` AS Y,
`ER(PPM)` AS Er, `YB(PPM)` AS Yb, `LU(PPM)` AS Lu
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
`SIO2(WT%)` > 43.1 AND `SIO2(WT%)` < 46.1 AND
`NA2O(WT%)` > 0.89 AND `NA2O(WT%)` < 3.89 AND
file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv'

# q34 (georoc.sqlite)
SELECT id AS Sample, file_id, LOCATION, LATITUDE_MAX AS lat, LONGITUDE_MAX AS long,
`SIO2(WT%)`AS SiO2, `TIO2(WT%)` AS TiO2, `MGO(WT%)`AS MgO,
`K2O(WT%)`AS K2O, `NA2O(WT%)`AS Na2O,
`CS(PPM)` AS Cs, `RB(PPM)` AS Rb, `BA(PPM)` AS Ba, `TH(PPM)` AS Th,
`U(PPM)` AS U, `NB(PPM)` AS Nb, `TA(PPM)` AS Ta, `LA(PPM)` AS La,
`CE(PPM)` AS Ce, `PR(PPM)` AS Pr, `ND(PPM)` AS Nd, `SR(PPM)` AS Sr,
`SM(PPM)` AS Sm, `ZR(PPM)` AS Zr, `TI(PPM)` AS Ti, `EU(PPM)` AS Eu,
`GD(PPM)` AS Gd, `TB(PPM)` AS Tb, `DY(PPM)` AS Dy, `Y(PPM)` AS Y,
`ER(PPM)` AS Er, `YB(PPM)` AS Yb, `LU(PPM)` AS Lu
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
`SIO2(WT%)` > 42.9 AND `SIO2(WT%)` < 45.9 AND
`NA2O(WT%)` > 2.41 AND `NA2O(WT%)` < 5.41 AND
file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv'


#### Fig S17 ####
# q35 (pofatu.sqlite)
SELECT id AS Sample, sample_category,
location_subregion, site_name, sample_comment,
location_latitude AS lat, location_longitude AS long
FROM 'samples.csv'
WHERE location_subregion = 'TUTUILA'


#### Fig S18 ####
# q36 (pofatu.sqlite)
SELECT s.id AS Sample, s.sample_category,
s.location_subregion, s.site_name, s.sample_comment,
s.location_latitude AS lat, s.location_longitude AS long,
max(CASE WHEN m.parameter='SiO2 [%]' then m.value END) AS 'SiO2',
max(CASE WHEN m.parameter='TiO2 [%]' then m.value END) AS 'TiO2',
max(CASE WHEN m.parameter='Al2O3 [%]' then m.value END) AS 'Al2O3',
max(CASE WHEN m.parameter='Fe2O3 [%]' then m.value END) AS 'Fe2O3',
max(CASE WHEN m.parameter='FeO [%]' then m.value END) AS 'FeO',
max(CASE WHEN m.parameter='MnO [%]' then m.value END) AS 'MnO',
max(CASE WHEN m.parameter='MgO [%]' then m.value END) AS 'MgO',
max(CASE WHEN m.parameter='CaO [%]' then m.value END) AS 'CaO',
max(CASE WHEN m.parameter='Na2O [%]' then m.value END) AS 'Na2O',
max(CASE WHEN m.parameter='K2O [%]' then m.value END) AS 'K2O',
max(CASE WHEN m.parameter='Li [ppm]' then m.value END) AS 'Li',
max(CASE WHEN m.parameter='Sc [ppm]' then m.value END) AS 'Sc',
max(CASE WHEN m.parameter='Ti [ppm]' then m.value END) AS 'Ti',
max(CASE WHEN m.parameter='V [ppm]' then m.value END) AS 'V',
max(CASE WHEN m.parameter='Cr [ppm]' then m.value END) AS 'Cr',
max(CASE WHEN m.parameter='Co [ppm]' then m.value END) AS 'Co',
max(CASE WHEN m.parameter='Ni [ppm]' then m.value END) AS 'Ni',
max(CASE WHEN m.parameter='Cu [ppm]' then m.value END) AS 'Cu',
max(CASE WHEN m.parameter='Zn [ppm]' then m.value END) AS 'Zn',
max(CASE WHEN m.parameter='As [ppm]' then m.value END) AS 'As',
max(CASE WHEN m.parameter='Rb [ppm]' then m.value END) AS 'Rb',
max(CASE WHEN m.parameter='Sr [ppm]' then m.value END) AS 'Sr',
max(CASE WHEN m.parameter='Y [ppm]' then m.value END) AS 'Y',
max(CASE WHEN m.parameter='Zr [ppm]' then m.value END) AS 'Zr',
max(CASE WHEN m.parameter='Nb [ppm]' then m.value END) AS 'Nb',
max(CASE WHEN m.parameter='Cd [ppm]' then m.value END) AS 'Cd',
max(CASE WHEN m.parameter='Cs [ppm]' then m.value END) AS 'Cs',
max(CASE WHEN m.parameter='Ba [ppm]' then m.value END) AS 'Ba',
max(CASE WHEN m.parameter='La [ppm]' then m.value END) AS 'La',
max(CASE WHEN m.parameter='Ce [ppm]' then m.value END) AS 'Ce',
max(CASE WHEN m.parameter='Pr [ppm]' then m.value END) AS 'Pr',
max(CASE WHEN m.parameter='Nd [ppm]' then m.value END) AS 'Nd',
max(CASE WHEN m.parameter='Sm [ppm]' then m.value END) AS 'Sm',
max(CASE WHEN m.parameter='Eu [ppm]' then m.value END) AS 'Eu',
max(CASE WHEN m.parameter='Gd [ppm]' then m.value END) AS 'Gd',
max(CASE WHEN m.parameter='Tb [ppm]' then m.value END) AS 'Tb',
max(CASE WHEN m.parameter='Dy [ppm]' then m.value END) AS 'Dy',
max(CASE WHEN m.parameter='Ho [ppm]' then m.value END) AS 'Ho',
max(CASE WHEN m.parameter='Er [ppm]' then m.value END) AS 'Er',
max(CASE WHEN m.parameter='Tm [ppm]' then m.value END) AS 'Tm',
max(CASE WHEN m.parameter='Yb [ppm]' then m.value END) AS 'Yb',
max(CASE WHEN m.parameter='Lu [ppm]' then m.value END) AS 'Lu',
max(CASE WHEN m.parameter='Hf [ppm]' then m.value END) AS 'Hf',
max(CASE WHEN m.parameter='Ta [ppm]' then m.value END) AS 'Ta',
max(CASE WHEN m.parameter='Tl [ppm]' then m.value END) AS 'Tl',
max(CASE WHEN m.parameter='Pb [ppm]' then m.value END) AS 'Pb',
max(CASE WHEN m.parameter='Th [ppm]' then m.value END) AS 'Th',
max(CASE WHEN m.parameter='U [ppm]' then m.value END) AS 'U',
max(CASE WHEN m.parameter='Nd143_Nd144' then m.value END) AS 'Nd143_Nd144',
max(CASE WHEN m.parameter='Sr87_Sr86' then m.value END) AS 'Sr87_Sr86',
max(CASE WHEN m.parameter='Pb206_Pb204' then m.value END) AS 'Pb206_Pb204',
max(CASE WHEN m.parameter='Pb207_Pb204' then m.value END) AS 'Pb207_Pb204',
max(CASE WHEN m.parameter='Pb208_Pb204' then m.value END) AS 'Pb208_Pb204'
FROM 'samples.csv' AS s JOIN 'measurements.csv' AS m ON s.id=m.sample_id
WHERE location_subregion = 'TUTUILA' AND
m.parameter IN ('SiO2 [%]', 'TiO2 [%]', 'Al2O3 [%]', 'Fe2O3 [%]', 'FeO [%]',
'MnO [%]', 'MgO [%]', 'CaO [%]', 'Na2O [%]', 'K2O [%]',
'Li [ppm]', 'Sc [ppm]', 'Ti [ppm]', 'V [ppm]',
'Cr [ppm]', 'Co [ppm]', 'Ni [ppm]', 'Cu [ppm]', 'Zn [ppm]', 'As [ppm]',
'Rb [ppm]', 'Sr [ppm]', 'Y [ppm]', 'Zr [ppm]', 'Nb [ppm]', 'Cd [ppm]',
'Cs [ppm]', 'Ba [ppm]', 'La [ppm]', 'Ce [ppm]', 'Pr [ppm]', 'Nd [ppm]',
'Sm [ppm]', 'Eu [ppm]', 'Gd [ppm]', 'Tb [ppm]', 'Dy [ppm]', 'Ho [ppm]',
'Er [ppm]', 'Tm [ppm]', 'Yb [ppm]', 'Lu [ppm]', 'Hf [ppm]', 'Ta [ppm]',
'Tl [ppm]', 'Pb [ppm]', 'Th [ppm]', 'U [ppm]', 'Nd143_Nd144', 'Sr87_Sr86',
'Pb206_Pb204', 'Pb207_Pb204', 'Pb208_Pb204') 
GROUP BY sample_id


#### Fig S19 ####
# q37 (pofatu.sqlite)
SELECT * FROM (
SELECT s.id AS Sample, s.sample_category,
s.location_subregion, s.site_name, s.sample_comment,
s.location_latitude AS lat, s.location_longitude AS long,
max(CASE WHEN m.parameter='SiO2 [%]' then m.value END) AS 'SiO2',
max(CASE WHEN m.parameter='TiO2 [%]' then m.value END) AS 'TiO2',
max(CASE WHEN m.parameter='Al2O3 [%]' then m.value END) AS 'Al2O3',
max(CASE WHEN m.parameter='Fe2O3 [%]' then m.value END) AS 'Fe2O3',
max(CASE WHEN m.parameter='FeO [%]' then m.value END) AS 'FeO',
max(CASE WHEN m.parameter='MnO [%]' then m.value END) AS 'MnO',
max(CASE WHEN m.parameter='MgO [%]' then m.value END) AS 'MgO',
max(CASE WHEN m.parameter='CaO [%]' then m.value END) AS 'CaO',
max(CASE WHEN m.parameter='Na2O [%]' then m.value END) AS 'Na2O',
max(CASE WHEN m.parameter='K2O [%]' then m.value END) AS 'K2O',
max(CASE WHEN m.parameter='Li [ppm]' then m.value END) AS 'Li',
max(CASE WHEN m.parameter='Sc [ppm]' then m.value END) AS 'Sc',
max(CASE WHEN m.parameter='Ti [ppm]' then m.value END) AS 'Ti',
max(CASE WHEN m.parameter='V [ppm]' then m.value END) AS 'V',
max(CASE WHEN m.parameter='Cr [ppm]' then m.value END) AS 'Cr',
max(CASE WHEN m.parameter='Co [ppm]' then m.value END) AS 'Co',
max(CASE WHEN m.parameter='Ni [ppm]' then m.value END) AS 'Ni',
max(CASE WHEN m.parameter='Cu [ppm]' then m.value END) AS 'Cu',
max(CASE WHEN m.parameter='Zn [ppm]' then m.value END) AS 'Zn',
max(CASE WHEN m.parameter='As [ppm]' then m.value END) AS 'As',
max(CASE WHEN m.parameter='Rb [ppm]' then m.value END) AS 'Rb',
max(CASE WHEN m.parameter='Sr [ppm]' then m.value END) AS 'Sr',
max(CASE WHEN m.parameter='Y [ppm]' then m.value END) AS 'Y',
max(CASE WHEN m.parameter='Zr [ppm]' then m.value END) AS 'Zr',
max(CASE WHEN m.parameter='Nb [ppm]' then m.value END) AS 'Nb',
max(CASE WHEN m.parameter='Cd [ppm]' then m.value END) AS 'Cd',
max(CASE WHEN m.parameter='Cs [ppm]' then m.value END) AS 'Cs',
max(CASE WHEN m.parameter='Ba [ppm]' then m.value END) AS 'Ba',
max(CASE WHEN m.parameter='La [ppm]' then m.value END) AS 'La',
max(CASE WHEN m.parameter='Ce [ppm]' then m.value END) AS 'Ce',
max(CASE WHEN m.parameter='Pr [ppm]' then m.value END) AS 'Pr',
max(CASE WHEN m.parameter='Nd [ppm]' then m.value END) AS 'Nd',
max(CASE WHEN m.parameter='Sm [ppm]' then m.value END) AS 'Sm',
max(CASE WHEN m.parameter='Eu [ppm]' then m.value END) AS 'Eu',
max(CASE WHEN m.parameter='Gd [ppm]' then m.value END) AS 'Gd',
max(CASE WHEN m.parameter='Tb [ppm]' then m.value END) AS 'Tb',
max(CASE WHEN m.parameter='Dy [ppm]' then m.value END) AS 'Dy',
max(CASE WHEN m.parameter='Ho [ppm]' then m.value END) AS 'Ho',
max(CASE WHEN m.parameter='Er [ppm]' then m.value END) AS 'Er',
max(CASE WHEN m.parameter='Tm [ppm]' then m.value END) AS 'Tm',
max(CASE WHEN m.parameter='Yb [ppm]' then m.value END) AS 'Yb',
max(CASE WHEN m.parameter='Lu [ppm]' then m.value END) AS 'Lu',
max(CASE WHEN m.parameter='Hf [ppm]' then m.value END) AS 'Hf',
max(CASE WHEN m.parameter='Ta [ppm]' then m.value END) AS 'Ta',
max(CASE WHEN m.parameter='Tl [ppm]' then m.value END) AS 'Tl',
max(CASE WHEN m.parameter='Pb [ppm]' then m.value END) AS 'Pb',
max(CASE WHEN m.parameter='Th [ppm]' then m.value END) AS 'Th',
max(CASE WHEN m.parameter='U [ppm]' then m.value END) AS 'U',
max(CASE WHEN m.parameter='Nd143_Nd144' then m.value END) AS 'Nd143_Nd144',
max(CASE WHEN m.parameter='Sr87_Sr86' then m.value END) AS 'Sr87_Sr86',
max(CASE WHEN m.parameter='Pb206_Pb204' then m.value END) AS 'Pb206_Pb204',
max(CASE WHEN m.parameter='Pb207_Pb204' then m.value END) AS 'Pb207_Pb204',
max(CASE WHEN m.parameter='Pb208_Pb204' then m.value END) AS 'Pb208_Pb204'
FROM 'samples.csv' AS s JOIN 'measurements.csv' AS m ON s.id=m.sample_id
WHERE m.parameter IN ('SiO2 [%]', 'TiO2 [%]', 'Al2O3 [%]', 'Fe2O3 [%]', 'FeO [%]',
'MnO [%]', 'MgO [%]', 'CaO [%]', 'Na2O [%]', 'K2O [%]',
'Li [ppm]', 'Sc [ppm]', 'Ti [ppm]', 'V [ppm]',
'Cr [ppm]', 'Co [ppm]', 'Ni [ppm]', 'Cu [ppm]', 'Zn [ppm]', 'As [ppm]',
'Rb [ppm]', 'Sr [ppm]', 'Y [ppm]', 'Zr [ppm]', 'Nb [ppm]', 'Cd [ppm]',
'Cs [ppm]', 'Ba [ppm]', 'La [ppm]', 'Ce [ppm]', 'Pr [ppm]', 'Nd [ppm]',
'Sm [ppm]', 'Eu [ppm]', 'Gd [ppm]', 'Tb [ppm]', 'Dy [ppm]', 'Ho [ppm]',
'Er [ppm]', 'Tm [ppm]', 'Yb [ppm]', 'Lu [ppm]', 'Hf [ppm]', 'Ta [ppm]',
'Tl [ppm]', 'Pb [ppm]', 'Th [ppm]', 'U [ppm]', 'Nd143_Nd144', 'Sr87_Sr86',
'Pb206_Pb204', 'Pb207_Pb204', 'Pb208_Pb204') GROUP BY sample_id) 
WHERE (
Sample = 'best1992_TAU48' OR Sample = 'best1992_TAU49' OR
Sample = 'best1992_TAU52' OR Sample = 'best1992_TAU54' OR
Sample = 'best1992_TOK-A2' OR Sample = 'best1992_COOK-1' OR
Sample = 'best1992_BB-8-4-4(A)' OR Sample = 'best1992_TOK-A2' OR
Sample = 'allen1997_5' OR Sample = 'allen1997_6' OR
Sample = 'weisler2016a_2009-369' OR Sample = 'clark2014_36' OR
Sample = 'clark2014_51' OR Sample = 'clark2014_53' OR
Sample = 'clark2014_54' OR Sample = 'clark2014_241' OR
Sample = 'clark2014_678' OR Sample = 'clark2014_683' OR
Sample = 'clark2014_707'
)