SELECT * FROM (
SELECT s.id AS sample_id, s.sample_category, s.location_region,
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
'Tl [ppm]', 'Pb [ppm]', 'Th [ppm]', 'U [ppm]', 
'Nd143_Nd144', 'Sr87_Sr86', 'Pb206_Pb204', 'Pb207_Pb204', 'Pb208_Pb204') 
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
