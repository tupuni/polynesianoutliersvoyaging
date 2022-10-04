require(here)
require(tidyverse)
require(RSQLite)
require(patchwork)

georoc <- dbConnect(RSQLite::SQLite(), path_to_georoc)
pofatu <- dbConnect(RSQLite::SQLite(), path_to_pofatu)

#### Fig S16a ####
ranges_s_OIB[1:6,1:5]
OIB1 <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
((`SIO2(WT%)` > 45.2 AND `SIO2(WT%)` < 48.2 AND
`NA2O(WT%)` > 2.08 AND `NA2O(WT%)` < 5.08 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.9 AND
`MGO(WT%)` > 3.08 AND `MGO(WT%)` < 6.08) OR
(`SIO2(WT%)` > 44.3 AND `SIO2(WT%)` < 47.3 AND
`NA2O(WT%)` > 2 AND `NA2O(WT%)` < 5 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.82 AND
`MGO(WT%)` > 3.52 AND `MGO(WT%)` < 6.52) OR
(`SIO2(WT%)` > 43.5 AND `SIO2(WT%)` < 46.5 AND
`NA2O(WT%)` > 1.9 AND `NA2O(WT%)` < 4.9 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.78 AND
`MGO(WT%)` > 3.79 AND `MGO(WT%)` < 6.79) OR
(`SIO2(WT%)` > 45.9 AND `SIO2(WT%)` < 48.9 AND
`NA2O(WT%)` > 2.18 AND `NA2O(WT%)` < 5.18 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.99 AND
`MGO(WT%)` > 3.44 AND `MGO(WT%)` < 6.44) OR
(`SIO2(WT%)` > 44.1 AND `SIO2(WT%)` < 47.1 AND
`NA2O(WT%)` > 1.8 AND `NA2O(WT%)` < 4.8 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.72 AND
`MGO(WT%)` > 4.15 AND `MGO(WT%)` < 7.15) OR
(`SIO2(WT%)` > 45.7 AND `SIO2(WT%)` < 48.7 AND
`NA2O(WT%)` > 2.08 AND `NA2O(WT%)` < 5.08 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.85 AND
`MGO(WT%)` > 3.10 AND `MGO(WT%)` < 6.10))") %>%
  dplyr::filter(grepl("SAMOAN", file_id)) %>%
  get_georoc_location() %>% filter(Location != "na") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>% Fe2O3_from_FeO() %>%
  dplyr::mutate(Location = case_when(
    grepl("MANUA ISLANDS", LOCATION) ~ "Manua Islands",
    grepl("SAVAI", LOCATION) ~ "Savai'i",
    grepl("TUTUILA", LOCATION) ~ "Tutuila",
    grepl("UPOLU", LOCATION) ~ "Upolu")) %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,Fe2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

OIB2 <- dbGetQuery(pofatu,
"SELECT s.id AS sample_id, s.sample_category,
s.location_region, s.location_subregion, s.site_name, s.sample_comment,
s.location_latitude, s.location_longitude,
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
WHERE location_region = 'SAMOA' AND
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
  rename_pofatu_elements() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>% Fe2O3_from_FeO() %>%
  dplyr::filter((SiO2 > 45.2 & SiO2 < 48.2 &
                   Na2O > 2.08 & Na2O < 5.08 &
                   K2O > 0 & K2O < 2.9 &
                   MgO > 3.08 & MgO < 6.08) |
                  (SiO2 > 44.3 & SiO2 < 47.3 &
                     Na2O > 2 & Na2O < 5 &
                     K2O > 0 & K2O < 2.82 &
                     MgO > 3.52 & MgO < 6.52) |
                  (SiO2 > 43.5 & SiO2 < 46.5 &
                     Na2O > 1.9 & Na2O < 4.9 &
                     K2O > 0 & K2O < 2.78 &
                     MgO > 3.79 & MgO < 6.79) |
                  (SiO2 > 45.9 & SiO2 < 48.9 &
                     Na2O > 2.18 & Na2O < 5.18 &
                     K2O > 0 & K2O < 2.99 &
                     MgO > 3.44 & MgO < 6.44) |
                  (SiO2 > 44.1 & SiO2 < 47.1 &
                     Na2O > 1.8 & Na2O < 4.8 &
                     K2O > 0 & K2O < 2.72 &
                     MgO > 4.15 & MgO < 7.15) |
                  (SiO2 > 45.7 & SiO2 < 48.7 &
                     Na2O > 2.08 & Na2O < 5.08 &
                     K2O > 0 & K2O < 2.85 &
                     MgO > 3.10 & MgO < 6.10)) %>%
  dplyr::mutate(Location = case_when(
    location_subregion=="TUTUILA" ~ "Tutuila",
    location_subregion=="SAVAI'I" ~ "Savai'i",
    location_subregion=="OFU" ~ "Manua Islands")) %>%
  dplyr::select(
    Sample,Location,lat,long,SiO2,TiO2,Al2O3,Fe2O3,MnO,MgO,CaO,Na2O,K2O,
    Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
    Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
    Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

OIB <- full_join(OIB1,OIB2)
OIB[OIB == 0] <- NA # Replace 0 with NA

s <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-08","T-12-06","T-12-07","T-12-08","T-12-09","T-12-10")) %>%
  mutate(Location = case_when(
    grepl("E-11-08", Sample) ~ "E-11-08",
    grepl("T-12-06", Sample) ~ "T-12-06",
    grepl("T-12-07", Sample) ~ "T-12-07",
    grepl("T-12-08", Sample) ~ "T-12-08",
    grepl("T-12-09", Sample) ~ "T-12-09",
    grepl("T-12-10", Sample) ~ "T-12-10"))

cols <- c("Savai'i"="#B262A7","Upolu"="#C77FB6","Manua Islands"="#94318E",
          "Tutuila"="#77246C","E-11-08"="red","T-12-06"="red","T-12-07"="red",
          "T-12-08"="red","T-12-09"="red","T-12-10"="red")
shapes <- c("Savai'i"=23,"Upolu"=22,"Manua Islands"=25,"Tutuila"=21,
            "E-11-08"=21,"T-12-06"=24,"T-12-07"=14,"T-12-08"=25,"T-12-09"=9,"T-12-10"=23)
contour <- c("Savai'i"="black","Upolu"="black","Manua Islands"="black",
             "Tutuila"="black","E-11-08"="black","T-12-06"="black","T-12-07"="red",
             "T-12-08"="black","T-12-09"="red","T-12-10"="black")

p1 <- OIB %>%
  ggplot(aes(x=Rb,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(10,60)) +
  scale_y_continuous(limits=c(70,200)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.title.x = element_blank(),
        axis.text = element_text(size = 8), legend.position = "none", aspect.ratio=1)
p2 <- OIB %>%
  ggplot(aes(x=Sr,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(320,950)) +
  scale_y_continuous(limits=c(70,200)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),legend.position = "none", aspect.ratio=1)
p3 <- OIB %>%
  ggplot(aes(x=Zr,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(150,500)) +
  scale_y_continuous(limits=c(70,200)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),legend.position = "none", aspect.ratio=1)
S16a <- p1|p2|p3
S16a

#### Fig S16b ####
ranges_s_OIB[7,]
OIB <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
`NA2O(WT%)` > 1.61 AND `NA2O(WT%)` < 4.61 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.91 AND
file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv'") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>% Fe2O3_from_FeO() %>%
  dplyr::mutate(Location = case_when(
    grepl("PONAPE", LOCATION) ~ "Ponape",
    grepl("KUSAIE", LOCATION) ~ "Kosrae",
    grepl("KOSRAE", LOCATION) ~ "Kosrae",
    grepl("CHUUK", LOCATION) ~ "Chuuk")) %>% dplyr::filter(Location != "NA") %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,Fe2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB %>% group_by(Location) %>% tally()
OIB[OIB == 0] <- NA # Replace 0 with NA

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-24")) %>%
  mutate(Location = case_when(grepl("K-12-24", Sample) ~ "K-12-24"))

cols <- c("Chuuk"="#6D58A5","Kosrae"="#8476B6","Ponape"="#341D59","K-12-24"="red")
shapes <- c("Chuuk"=23,"Kosrae"=22,"Ponape"=21,"K-12-24"=12)
contour <- c("Chuuk"="black","Kosrae"="black","Ponape"="black","K-12-24"="red")

p4 <- OIB %>%
  ggplot(aes(x=Rb,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(0,55)) +
  scale_y_continuous(limits=c(30,350)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.x = element_blank(), legend.position = "none", aspect.ratio=1)
p5 <- OIB %>%
  ggplot(aes(x=Sr,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(0,750)) +
  scale_y_continuous(limits=c(30,350)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)
p6 <- OIB %>%
  ggplot(aes(x=Zr,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(0,750)) +
  scale_y_continuous(limits=c(30,350)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)
S16b <- p4|p5|p6
S16b

#### Fig S16c ####
ranges_s_OIB[8,]
OIB <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
`SIO2(WT%)` > 43.1 AND `SIO2(WT%)` < 46.1 AND
`NA2O(WT%)` > 0.89 AND `NA2O(WT%)` < 3.89 AND
`K2O(WT%)` < 1.9 AND
`MGO(WT%)` > 10.7 AND `MGO(WT%)` < 13.7 AND
(file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part1.csv' OR
file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part2.csv' OR
file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv' OR
file_id = '2022-06-WFJZKY_AUSTRAL-COOK_ISLANDS.csv')") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>% Fe2O3_from_FeO() %>%
  dplyr::mutate(Location = case_when(
    grepl("CAROLINE", LOCATION) ~ "Caroline",
    grepl("AUSTRAL-COOK ISLANDS", LOCATION) ~ "Austral-Cook",
    grepl("HAWAIIAN ISLANDS / HAWAII", LOCATION) ~ "Hawai'i",
    grepl("HAWAIIAN ISLANDS / KAUAI", LOCATION) ~ "Kaua'i",
    grepl("HAWAIIAN ISLANDS / MAUI", LOCATION) ~ "Maui",
    grepl("HAWAIIAN ISLANDS / MOLOKAI", LOCATION) ~ "Moloka'i",
    grepl("HAWAIIAN ISLANDS / NIIHAU", LOCATION) ~ "Ni'ihau",
    grepl("HAWAIIAN ISLANDS / OAHU", LOCATION) ~ "O'ahu")) %>%
  dplyr::filter(Location != "NA") %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,Fe2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB %>% group_by(Location) %>% tally()
OIB[OIB == 0] <- NA # Replace 0 with NA

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-25")) %>%
  mutate(Location = case_when(grepl("K-12-25", Sample) ~ "K-12-25"))

cols <- c("Hawai'i"="#EABD00","Maui"="#F4D037","Moloka'i"="#F4D037",
          "O'ahu"="#F9E385","Kaua'i"="#F4E9BA","Ni'ihau"="#F4E9BA",
          "Caroline"="#320A5A","Austral-Cook"="#BB3654","K-12-25"="red")
shapes <- c("Hawai'i"=21,"Maui"=22,"Moloka'i"=23,"O'ahu"=24,"Kaua'i"=25,"Ni'ihau"=21,
            "Caroline"=21,"Austral-Cook"=23,"K-12-25"=13)
contour <- c("Hawai'i"="black","Maui"="black","Moloka'i"="black","O'ahu"="black",
             "Kaua'i"="black","Ni'ihau"="black","Caroline"="black",
             "Austral-Cook"="black","K-12-25"="red")

p7 <- OIB %>%
  ggplot(aes(x=Rb,y=U/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,40)) +
  scale_y_continuous(limits=c(0,1)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.x = element_blank(),legend.position = "none", aspect.ratio=1) +
  labs(x="Rb (ppm)")
p8 <- OIB %>%
  ggplot(aes(x=Sr,y=U/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(.1,2.3)) +
  scale_y_continuous(limits=c(0,1)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), legend.position = "none",
        axis.title.x = element_blank(),aspect.ratio=1) +
  labs(x="Sr (ppm)")
p9 <- OIB %>%
  ggplot(aes(x=Zr,y=U/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,300)) +
  scale_y_continuous(limits=c(0,1)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), legend.position = "none",
        axis.title.x = element_blank(),aspect.ratio=1) +
  labs(x="Zr (ppm)")
S16c <- p7|p8|p9
S16c

#### Fig S16d ####
ranges_s_OIB[9,]
OIB <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
`SIO2(WT%)` > 42.9 AND `SIO2(WT%)` < 45.9 AND
`NA2O(WT%)` > 2.41 AND `NA2O(WT%)` < 5.41 AND
`K2O(WT%)` < 2.35 AND
(file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv')") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>% Fe2O3_from_FeO() %>%
  dplyr::mutate(Location = case_when(
    grepl("PONAPE", LOCATION) ~ "Ponape",
    grepl("KUSAIE", LOCATION) ~ "Kosrae",
    grepl("KOSRAE", LOCATION) ~ "Kosrae",
    grepl("CHUUK", LOCATION) ~ "Chuuk",
    grepl("HAWAIIAN", LOCATION) ~ "Hawai'i")) %>% dplyr::filter(Location != "NA") %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,Fe2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB %>% group_by(Location) %>% tally()
OIB[OIB == 0] <- NA # Replace 0 with NA

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-26")) %>%
  mutate(Location = case_when(grepl("K-12-26", Sample) ~ "K-12-26"))

cols <- c("Chuuk"="#6D58A5","Kosrae"="#8476B6","Ponape"="#341D59",
          "Hawai'i"="#F4DD53","K-12-26"="red")
shapes <- c("Chuuk"=23,"Kosrae"=22,"Ponape"=21,"Hawai'i"=25,"K-12-26"=14)
contour <- c("Chuuk"="black","Kosrae"="black","Ponape"="black",
             "Hawai'i"="black","K-12-26"="red")

p10 <- OIB %>%
  ggplot(aes(x=Rb,y=La/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(0,55)) +
  #scale_y_continuous(limits=c(0,300)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x="Rb (ppm)")
p11 <- OIB %>%
  ggplot(aes(x=Sr,y=La/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
 # scale_x_continuous(limits=c(0,750)) +
  #scale_y_continuous(limits=c(0,300)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),legend.position = "none", aspect.ratio=1) +
  labs(x="Sr (ppm)")
p12 <- OIB %>%
  ggplot(aes(x=Zr,y=La/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  # scale_x_continuous(limits=c(0,750)) +
  #scale_y_continuous(limits=c(0,300)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),legend.position = "none", aspect.ratio=1) +
  labs(x="Zr (ppm)")
S16d <- p10|p11|p12
S16d

pdf(here("analysis","supplementary-materials","FigS16.pdf"), width=8, height=10)
S16a/S16b/S16c/S16d
dev.off()
