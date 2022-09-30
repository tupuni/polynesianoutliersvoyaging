require(here)
require(tidyverse)
require(RSQLite)
require(patchwork)

georoc <- dbConnect(RSQLite::SQLite(), path_to_georoc)
pofatu <- dbConnect(RSQLite::SQLite(), path_to_pofatu)

shapes <- c("Caroline islands"=21,"Samoan islands"=24,"Austral-Cook chain"=23,
            "Society islands"=22,"Hawai'i islands"=25,"Marquesas islands"=21,
            "Pitcairn-Gambier chain"=21,
            "E-11-08"=5,"E-11-08dup"=9,"T-12-06"=2,"T-12-06dup"=14,
            "T-12-07"=7,"T-12-08"=6,"T-12-09"=10,"T-12-10"=11,"K-12-24"=12,
            "K-12-25"=13,"K-12-26"=14)
cols <- c("Caroline islands"="#320A5A","Samoan islands"="#781B6C",
          "Austral-Cook chain"="#BB3654","Society islands"="#EC6824",
          "Marquesas islands"="#FBB41A","Hawai'i islands"="#F4DD53",
          "Pitcairn-Gambier chain"="#C96FB6",
          "E-11-08"="red","E-11-08dup"="red","T-12-06"="red","T-12-06dup"="red",
          "T-12-07"="red","T-12-08"="red","T-12-09"="red",
          "T-12-10"="red","K-12-24"="red","K-12-25"="red","K-12-26"="red")
contour <- c("Caroline islands"="black","Samoan islands"="black",
             "Austral-Cook chain"="black","Society islands"="black",
             "Hawai'i islands"="black","Marquesas islands"="black",
             "Pitcairn-Gambier chain"="black",
             "E-11-08"="red","E-11-08dup"="red","T-12-06"="red","T-12-06dup"="red",
             "T-12-07"="red","T-12-08"="red","T-12-09"="red",
             "T-12-10"="red","K-12-24"="red","K-12-25"="red","K-12-26"="red")

#### Fig 15a ####

#OIB <- full_join(q10,q11)
OIB <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
((PB206_PB204 > 18.658 AND PB206_PB204 < 19.034) OR
(PB206_PB204 > 18.701 AND PB206_PB204 < 19.079) OR
(PB206_PB204 > 18.722 AND PB206_PB204 < 19.100) OR
(PB206_PB204 > 18.720 AND PB206_PB204 < 19.098) OR
(PB206_PB204 > 18.736 AND PB206_PB204 < 19.114) OR
(PB206_PB204 > 18.624 AND PB206_PB204 < 19))") %>%
  get_georoc_location() %>% filter(Location != "na") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB <- full_join(OIB,q11)

s <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-08","T-12-06","T-12-07","T-12-08","T-12-09","T-12-10")) %>%
  mutate(Location = case_when(
    grepl("E-11-08", Sample) ~ "E-11-08",
    grepl("T-12-06", Sample) ~ "T-12-06",
    grepl("T-12-07", Sample) ~ "T-12-07",
    grepl("T-12-08", Sample) ~ "T-12-08",
    grepl("T-12-09", Sample) ~ "T-12-09",
    grepl("T-12-10", Sample) ~ "T-12-10"))

E_T <- OIB %>%
  ggplot(aes(x=Pb206_Pb204,y=Pb208_Pb204, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_y_continuous(limits=c(38.1, 39.3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^206*"Pb / "*{}^204*"Pb"),
       y=expression({}^208*"Pb / "*{}^204*"Pb"))
E_T

#### Fig 15b ####
#OIB <- q14
OIB <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
(PB206_PB204 > 18.221 AND PB206_PB204 < 18.589)") %>%
  get_georoc_location() %>% filter(Location != "na") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-24")) %>%
  mutate(Location = case_when(grepl("K-12-24", Sample) ~ "K-12-24"))

K_12_24 <- OIB %>%
  ggplot(aes(x=Pb206_Pb204,y=Pb208_Pb204, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_y_continuous(limits=c(37.8, 39.2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^206*"Pb / "*{}^204*"Pb"),
       y=expression({}^208*"Pb / "*{}^204*"Pb"))
K_12_24

#### Fig 15c ####

#OIB <- q16
OIB <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
PB206_PB204 > 18.438 AND PB206_PB204 < 18.81") %>%
  get_georoc_location() %>% filter(Location != "na") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-25")) %>%
  mutate(Location = case_when(grepl("K-12-25", Sample) ~ "K-12-25"))

K_12_25 <- OIB %>%
  ggplot(aes(x=Pb206_Pb204,y=Pb208_Pb204, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_y_continuous(limits=c(37.8, 39.2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 10),
        axis.title.x = element_blank(),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^206*"Pb / "*{}^204*"Pb"),
       y=expression({}^208*"Pb / "*{}^204*"Pb"))
K_12_25

#### Fig 15d ####
#OIB <- q18
q18 <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
(PB206_PB204 > 18.54 AND PB206_PB204 < 18.914)") %>%
  get_georoc_location() %>% filter(Location != "na") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-26")) %>%
  mutate(Location = case_when(grepl("K-12-26", Sample) ~ "K-12-26"))

K_12_26 <- OIB %>%
  ggplot(aes(x=Pb206_Pb204,y=Pb208_Pb204, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_y_continuous(limits=c(37.8, 39.2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^206*"Pb / "*{}^204*"Pb"),
       y=expression({}^208*"Pb / "*{}^204*"Pb"))
K_12_26


pdf(here("analysis","supplementary-materials","FigS15.pdf"), width=8, height=8)
(E_T/K_12_24)|(K_12_25/K_12_26)
dev.off()
