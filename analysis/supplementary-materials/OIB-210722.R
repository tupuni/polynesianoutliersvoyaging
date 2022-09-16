require(here)
require(tidyverse)
require(knitr)
require(RSQLite)
require(patchwork)
require(stats)
require(FactoMineR)
require(factoextra)
require(ggpubr)
require(ade4)
require(cluster)

cols <- c(
  "Caroline islands"="#320A5A","Samoan islands"="#781B6C",
  "Austral-Cook chain"="#BB3654","Society islands"="#EC6824",
  "Marquesas islands"="#FBB41A","Hawai'i islands"="#F4DD53",
  "E-11-08"="red","T-12-06"="red","T-12-06dup"="red","T-12-07"="red",
  "T-12-08"="red","T-12-09"="red","T-12-10"="red",
  "K-12-24"="red","K-12-25"="red","K-12-26"="red")
shapes <- c(
  "Caroline islands"=21,"Samoan islands"=24,"Austral-Cook chain"=23,
  "Society islands"=22,"Hawai'i islands"=25,"Marquesas islands"=21,"E-11-08"=8,
  "T-12-06"=21,"T-12-06dup"=12,"T-12-07"=22,"T-12-08"=23,"T-12-09"=24,"T-12-10"=25,
  "K-12-24"=2,"K-12-25"=6,"K-12-26"=11)
contour <- c(
  "Caroline islands"="black","Samoan islands"="black","Austral-Cook chain"="black",
  "Society islands"="black","Hawai'i islands"="black","Marquesas islands"="black",
  "E-11-08"="red","T-12-06"="black","T-12-06dup"="black",
  "T-12-07"="black","T-12-08"="black","T-12-09"="black","T-12-10"="black",
  "K-12-24"="red","K-12-25"="red","K-12-26"="red")

georoc <- dbConnect(RSQLite::SQLite(), path_to_georoc)
pofatu <- dbConnect(RSQLite::SQLite(), path_to_pofatu)

#### E_T joined PCA 1 ####
OIB <- dbGetQuery(georoc,
                       "SELECT *
                   FROM 'sample'
                   WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
                   TECTONIC_SETTING='OCEAN ISLAND'") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

OIB_pca1 <- OIB %>%
  dplyr::select(Sample,Location,
                Rb,Sr,Y,Zr,Nb,
                Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Sr87_Sr86,Nd143_Nd144)
samples <- joined_data %>%
  filter(Sample %in% c("K-12-24","K-12-25","K-12-26")) %>%
  mutate(Location = case_when(grepl("K-12-24", Sample) ~ "K-12-24",
                              grepl("K-12-25", Sample) ~ "K-12-25",
                              grepl("K-12-26", Sample) ~ "K-12-26")) %>%
  dplyr::select(Sample,Location,
                Rb,Sr,Y,Zr,Nb,
                Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Sr87_Sr86,Nd143_Nd144)

is.na(OIB_pca1) <- sapply(OIB_pca1, is.infinite) #replace Inf by NA
OIB_pca1[OIB_pca1 == 0] <- NA # Replace 0 with NA
OIB_pca1 <- OIB_pca1[rowSums(is.na(OIB_pca1)) == 0,] # removes rows with missing info for PCA

is.na(samples) <- sapply(samples, is.infinite) #replace Inf by NA
samples[samples == 0] <- NA # Replace 0 with NA
samples <- samples[rowSums(is.na(samples)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB_pca1[,3:11],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
res.pca <- prcomp(OIB_pca1[,8:10],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
fviz_eig(res.pca)
eig <- get_eig(res.pca)
K_PCA_1 <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB_pca1$Location, fill.ind = OIB_pca1$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  #scale_x_continuous(limits=c(-2.5, 7)) + scale_y_continuous(limits=c(-4.5,5.4)) +
  scale_x_continuous(limits=c(-2.5, 2.5)) + scale_y_continuous(limits=c(-1,1)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_PCA_1
pdf(here("analysis","work-in-progress","OIB-210722","K_joined-PCA1-a.pdf"),
    width=3.5, height=3.5)
K_PCA_1
dev.off()

res.pca.df <- cbind(OIB_pca1[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, samples[,3:11])
pred <- stats::predict(res.pca, samples[,8:10])
pred <- cbind(samples[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

K_PCA_1 <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size =2, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(-2.5, 7)) + scale_y_continuous(limits=c(-4.5,5.4)) +
  scale_x_continuous(limits=c(-2.5, 2.5)) + scale_y_continuous(limits=c(-1,1)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_PCA_1
pdf(here("analysis","work-in-progress","OIB-210722","K_joined-PCA1-b.pdf"),
    width=3.5, height=3.5)
K_PCA_1
dev.off()

#### Emae_Taumako ####
E <- joined_data_ranges[4,]
Ta <- joined_data_ranges[11:15,]
E_T_ranges <- full_join(E,Ta)

#### E_T PCA 1 ####
E_T_ranges[,2:3] %>% round(digits = 6)
E_T_ranges[,36:39] %>% round(digits = 6)
E_T_ranges[,40:45] %>% round(digits = 3)

OIB1 <- dbGetQuery(georoc,
                   "SELECT *
                   FROM 'sample'
                   WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
                   TECTONIC_SETTING='OCEAN ISLAND' AND
                   ((`SIO2(WT%)` > 45.2 AND `SIO2(WT%)` < 48.2 AND
                   ND143_ND144 > 0.512341 AND ND143_ND144 < 0.513367 AND
                   SR87_SR86 > 0.704586 AND SR87_SR86 < 0.705996 AND
                   PB206_PB204 > 18.658 AND PB206_PB204 < 19.034 AND
                   PB207_PB204 > 15.43 AND PB207_PB204 < 15.742 AND
                   PB208_PB204 > 38.434 AND PB208_PB204 < 39.21) OR
                   (`SIO2(WT%)` > 44.3 AND `SIO2(WT%)` < 47.3 AND
                   ND143_ND144 > 0.512342 AND ND143_ND144 < 0.513368 AND
                   SR87_SR86 > 0.704583 AND SR87_SR86 < 0.705993 AND
                   PB206_PB204 > 18.701 AND PB206_PB204 < 19.079 AND
                   PB207_PB204 > 15.431 AND PB207_PB204 < 15.743 AND
                   PB208_PB204 > 38.482 AND PB208_PB204 < 39.26) OR
                   (`SIO2(WT%)` > 43.5 AND `SIO2(WT%)` < 46.5 AND
                   ND143_ND144 > 0.512358 AND ND143_ND144 < 0.513384 AND
                   SR87_SR86 > 0.704403 AND SR87_SR86 < 0.705813 AND
                   PB206_PB204 > 18.722 AND PB206_PB204 < 19.1 AND
                   PB207_PB204 > 15.427 AND PB207_PB204 < 15.739 AND
                   PB208_PB204 > 38.472 AND PB208_PB204 < 39.25) OR
                   (`SIO2(WT%)` > 45.9 AND `SIO2(WT%)` < 48.9 AND
                   ND143_ND144 > 0.512342 AND ND143_ND144 < 0.513368 AND
                   SR87_SR86 > 0.704502 AND SR87_SR86 < 0.705912 AND
                   PB206_PB204 > 18.72 AND PB206_PB204 < 19.098 AND
                   PB207_PB204 > 15.445 AND PB207_PB204 < 15.757 AND
                   PB208_PB204 > 38.505 AND PB208_PB204 < 39.283) OR
                   (`SIO2(WT%)` > 44.1 AND `SIO2(WT%)` < 47.1 AND
                   ND143_ND144 > 0.512359 AND ND143_ND144 < 0.513385 AND
                   SR87_SR86 > 0.704412 AND SR87_SR86 < 0.705822 AND
                   PB206_PB204 > 18.736 AND PB206_PB204 < 19.114 AND
                   PB207_PB204 > 15.429 AND PB207_PB204 < 15.741 AND
                   PB208_PB204 > 38.508 AND PB208_PB204 < 39.286) OR
                   (`SIO2(WT%)` > 45.7 AND `SIO2(WT%)` < 48.7 AND
                   ND143_ND144 > 0.512348 AND ND143_ND144 < 0.513374 AND
                   SR87_SR86 > 0.704465 AND SR87_SR86 < 0.705875 AND
                   PB206_PB204 > 18.624 AND PB206_PB204 < 19 AND
                   PB207_PB204 > 15.425 AND PB207_PB204 < 15.737 AND
                   PB208_PB204 > 38.362 AND PB208_PB204 < 39.138))") %>%
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
  rename_pofatu_elements() %>% pofatu_location() %>%
  dplyr::filter((SiO2 > 45.2 & SiO2 < 48.2 &
                   Nd143_Nd144 > 0.512341 & Nd143_Nd144 < 0.513367 &
                   Sr87_Sr86 > 0.704586 & Sr87_Sr86 < 0.705996 &
                   Pb206_Pb204 > 18.658 & Pb206_Pb204 < 19.034 &
                   Pb207_Pb204 > 15.43 & Pb207_Pb204 < 15.742 &
                   Pb208_Pb204 > 38.434 & Pb208_Pb204 < 39.21) |
                  (SiO2 > 44.3 & SiO2 < 47.3 &
                     Nd143_Nd144 > 0.512342 & Nd143_Nd144 < 0.513368 &
                     Sr87_Sr86 > 0.704583 & Sr87_Sr86 < 0.705993 &
                     Pb206_Pb204 > 18.701 & Pb206_Pb204 < 19.079 &
                     Pb207_Pb204 > 15.431 & Pb207_Pb204 < 15.743 &
                     Pb208_Pb204 > 38.482 & Pb208_Pb204 < 39.26) |
                  (SiO2 > 43.5 & SiO2 < 46.5 &
                     Nd143_Nd144 > 0.512358 & Nd143_Nd144 < 0.513384 &
                     Sr87_Sr86 > 0.704403 & Sr87_Sr86 < 0.705813 &
                     Pb206_Pb204 > 18.722 & Pb206_Pb204 < 19.1 &
                     Pb207_Pb204 > 15.427 & Pb207_Pb204 < 15.739 &
                     Pb208_Pb204 > 38.472 & Pb208_Pb204 < 39.25) |
                  (SiO2 > 45.9 & SiO2 < 48.9 &
                     Nd143_Nd144 > 0.512342 & Nd143_Nd144 < 0.513368 &
                     Sr87_Sr86 > 0.704502 & Sr87_Sr86 < 0.705912 &
                     Pb206_Pb204 > 18.72 & Pb206_Pb204 < 19.098 &
                     Pb207_Pb204 > 15.445 & Pb207_Pb204 < 15.757 &
                     Pb208_Pb204 > 38.505 & Pb208_Pb204 < 39.283) |
                  (SiO2 > 44.1 & SiO2 < 47.1 &
                     Nd143_Nd144 > 0.512359 & Nd143_Nd144 < 0.513385 &
                     Sr87_Sr86 > 0.704412 & Sr87_Sr86 < 0.705822 &
                     Pb206_Pb204 > 18.736 & Pb206_Pb204 < 19.114 &
                     Pb207_Pb204 > 15.429 & Pb207_Pb204 < 15.741 &
                     Pb208_Pb204 > 38.508 & Pb208_Pb204 < 39.286) |
                  (SiO2 > 45.7 & SiO2 < 48.7 &
                     Nd143_Nd144 > 0.512348 & Nd143_Nd144 < 0.513374 &
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

OIB <- full_join(OIB1,OIB2)
OIB_pca <- OIB %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)

samples <- joined_data %>%
  filter(Sample %in% c("E-11-08","E-11-08dup","T-12-06","T-12-06dup",
                       "T-12-07","T-12-08","T-12-09","T-12-10")) %>%
  mutate(Location = case_when(grepl("E-11-08", Sample) ~ "E-11-08",
                              grepl("E-11-08dup", Sample) ~ "E-11-08dup",
                              grepl("T-12-06", Sample) ~ "T-12-06",
                              grepl("T-12-06dup", Sample) ~ "T-12-06dup",
                              grepl("T-12-07", Sample) ~ "T-12-07",
                              grepl("T-12-08", Sample) ~ "T-12-08",
                              grepl("T-12-09", Sample) ~ "T-12-09",
                              grepl("T-12-10", Sample) ~ "T-12-10")) %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)

is.na(OIB_pca) <- sapply(OIB_pca, is.infinite) #replace Inf by NA
OIB_pca[OIB_pca == 0] <- NA # Replace 0 with NA
OIB_pca <- OIB_pca[rowSums(is.na(OIB_pca)) == 0,] # removes rows with missing info for PCA

is.na(samples) <- sapply(samples, is.infinite) #replace Inf by NA
samples[samples == 0] <- NA # Replace 0 with NA
samples <- samples[rowSums(is.na(samples)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB_pca[,3:7],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
fviz_eig(res.pca)
eig <- get_eig(res.pca)
E_T_1 <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB_pca$Location, fill.ind = OIB_pca$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-3.8, 3.5)) + scale_y_continuous(limits=c(-3,2.3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
E_T_1
pdf(here("analysis","work-in-progress","OIB-210722","E_T-PCA1-a.pdf"),
    width=3.5, height=3.5)
E_T_1
dev.off()

res.pca.df <- cbind(OIB_pca[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, samples[,3:7])
pred <- cbind(samples[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

E_T_1 <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-3.8, 3.5)) + scale_y_continuous(limits=c(-3,2.3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
E_T_1
pdf(here("analysis","work-in-progress","OIB-210722","E_T-PCA1-b.pdf"),
    width=3.5, height=3.5)
E_T_1
dev.off()

#### E_T PCA 2 ####
E_T[,2:9] %>% round(digits=2)

E_T[2:31] %>% round(digits=2)

OIB1 <- dbGetQuery(georoc,
                   "SELECT *
                  FROM 'sample'
                  WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
                  TECTONIC_SETTING='OCEAN ISLAND' AND
                  ((`SIO2(WT%)` > 45.2 AND `SIO2(WT%)` < 48.2 AND
                  `NA2O(WT%)` > 2.08 AND `NA2O(WT%)` < 5.08 AND
                  `K2O(WT%)` > 0 AND `K2O(WT%)` < 2.9 AND
                  `RB(PPM)` > 18.3 AND `RB(PPM)` < 54.9 AND
                  `BA(PPM)` > 160 AND `BA(PPM)` < 480 AND
                  `TH(PPM)` > 1.95 AND `TH(PPM)` < 5.84 AND
                  `ND(PPM)` > 25.5 AND `ND(PPM)` < 76.5 AND
                  `SR(PPM)` > 352 AND `SR(PPM)` < 1056 AND
                  `NB(PPM)` > 24.8 AND `NB(PPM)` < 74.5 AND
                  `SM(PPM)` > 6.2 AND `SM(PPM)` < 18.6 AND
                  `ZR(PPM)` > 202 AND `ZR(PPM)` < 606 AND
                  `YB(PPM)` > 1.6 AND `YB(PPM)` < 4.8) OR
                  (`SIO2(WT%)` > 44.3 AND `SIO2(WT%)` < 47.3 AND
                  `NA2O(WT%)` > 2 AND `NA2O(WT%)` < 5 AND
                  `K2O(WT%)` > 0 AND `K2O(WT%)` < 2.82 AND
                  `RB(PPM)` > 16.8 AND `RB(PPM)` < 50.4 AND
                  `BA(PPM)` > 137 AND `BA(PPM)` < 413 AND
                  `TH(PPM)` > 1.86 AND `TH(PPM)` < 5.60 AND
                  `ND(PPM)` > 25.6 AND `ND(PPM)` < 76.9 AND
                  `SR(PPM)` > 332 AND `SR(PPM)` < 996 AND
                  `NB(PPM)` > 24.7 AND `NB(PPM)` < 74.1 AND
                  `SM(PPM)` > 6 AND `SM(PPM)` < 18.1 AND
                  `ZR(PPM)` > 196 AND `ZR(PPM)` < 589 AND
                  `YB(PPM)` > 1.62 AND `YB(PPM)` < 4.86) OR
                  (`SIO2(WT%)` > 43.5 AND `SIO2(WT%)` < 46.5 AND
                  `NA2O(WT%)` > 1.9 AND `NA2O(WT%)` < 4.9 AND
                  `K2O(WT%)` > 0 AND `K2O(WT%)` < 2.78 AND
                  `RB(PPM)` > 15.3 AND `RB(PPM)` < 46.05 AND
                  `BA(PPM)` > 126 AND `BA(PPM)` < 378 AND
                  `TH(PPM)` > 1.58 AND `TH(PPM)` < 4.74 AND
                  `ND(PPM)` > 23.7 AND `ND(PPM)` < 71.1 AND
                  `SR(PPM)` > 294 AND `SR(PPM)` < 882 AND
                  `NB(PPM)` > 21 AND `NB(PPM)` < 63.1 AND
                  `SM(PPM)` > 5.65 AND `SM(PPM)` < 16.95 AND
                  `ZR(PPM)` > 182 AND `ZR(PPM)` < 547 AND
                  `YB(PPM)` > 1.56 AND `YB(PPM)` < 4.68) OR
                  (`SIO2(WT%)` > 45.9 AND `SIO2(WT%)` < 48.9 AND
                  `NA2O(WT%)` > 2.18 AND `NA2O(WT%)` < 5.18 AND
                  `K2O(WT%)` > 0 AND `K2O(WT%)` < 2.99 AND
                  `RB(PPM)` > 20.55 AND `RB(PPM)` < 61.65 AND
                  `BA(PPM)` > 162 AND `BA(PPM)` < 488 AND
                  `TH(PPM)` > 2.13 AND `TH(PPM)` < 6.39 AND
                  `ND(PPM)` > 26.8 AND `ND(PPM)` < 80.5 AND
                  `SR(PPM)` > 365 AND `SR(PPM)` < 1095 AND
                  `NB(PPM)` > 24.3 AND `NB(PPM)` < 72.9 AND
                  `SM(PPM)` > 6.35 AND `SM(PPM)` < 19 AND
                  `ZR(PPM)` > 190 AND `ZR(PPM)` < 571 AND
                  `YB(PPM)` > 1.49 AND `YB(PPM)` < 4.46) OR
                  (`SIO2(WT%)` > 44.1 AND `SIO2(WT%)` < 47.1 AND
                  `NA2O(WT%)` > 1.8 AND `NA2O(WT%)` < 4.8 AND
                  `K2O(WT%)` > 0 AND `K2O(WT%)` < 2.72 AND
                  `RB(PPM)` > 15.6 AND `RB(PPM)` < 47 AND
                  `BA(PPM)` > 128 AND `BA(PPM)` < 385 AND
                  `TH(PPM)` > 1.58 AND `TH(PPM)` < 4.74 AND
                  `ND(PPM)` > 21.7 AND `ND(PPM)` < 65.1 AND
                  `SR(PPM)` > 296 AND `SR(PPM)` < 890 AND
                  `NB(PPM)` > 20.4 AND `NB(PPM)` < 61.2 AND
                  `SM(PPM)` > 5.35 AND `SM(PPM)` < 16 AND
                  `ZR(PPM)` > 173 AND `ZR(PPM)` < 520 AND
                  `YB(PPM)` > 1.4 AND `YB(PPM)` < 4.19) OR
                  (`SIO2(WT%)` > 45.7 AND `SIO2(WT%)` < 48.7 AND
                  `NA2O(WT%)` > 2.08 AND `NA2O(WT%)` < 5.08 AND
                  `K2O(WT%)` > 0 AND `K2O(WT%)` < 2.85 AND
                  `RB(PPM)` > 18.2 AND `RB(PPM)` < 54.6 AND
                  `BA(PPM)` > 143 AND `BA(PPM)` < 431 AND
                  `TH(PPM)` > 1.84 AND `TH(PPM)` < 5.52 AND
                  `ND(PPM)` > 27.3 AND `ND(PPM)` < 82 AND
                  `SR(PPM)` > 370 AND `SR(PPM)` < 1110 AND
                  `NB(PPM)` > 23.5 AND `NB(PPM)` < 70.5 AND
                  `SM(PPM)` > 6.65 AND `SM(PPM)` < 19.95 AND
                  `ZR(PPM)` > 200 AND `ZR(PPM)` < 601 AND
                  `YB(PPM)` > 1.57 AND `YB(PPM)` < 4.71)) AND
                  (file_id = '2022-06-WFJZKY_MARQUESAS.csv' OR
                  file_id = '2022-06-WFJZKY_SAMOAN_ISLANDS.csv' OR
                  file_id = '2022-06-WFJZKY_SOCIETY_ISLANDS.csv')") %>%
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
  rename_pofatu_elements() %>% pofatu_location() %>%
  dplyr::filter(Location %in% c(
    "Samoan islands","Society islands","Marquesas islands")) %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204) %>%
  dplyr::filter((SiO2 > 45.2 & SiO2 < 48.2 &
                   Na2O > 2.08 & Na2O < 5.08 &
                   K2O > 0 & K2O < 2.9 &
                   Rb > 18.3 & Rb < 54.9 &
                   Ba > 160 & Ba < 480 &
                   Th > 1.95 & Th < 5.84 &
                   Sr > 352 & Sr < 1056 &
                   Nb > 24.8 & Nb < 74.5 &
                   Nd > 25.5 & Nd < 76.5 &
                   Sm > 6.2 & Sm < 18.6 &
                   Zr > 202 & Zr < 606 &
                   Yb > 1.6 & Yb < 4.8) |
                  (SiO2 > 44.3 & SiO2 < 47.3 &
                     Na2O > 2 & Na2O < 5 &
                     K2O > 0 & K2O < 2.82 &
                     Rb > 16.8 & Rb < 50.4 &
                     Ba > 137 & Ba < 413 &
                     Th > 1.86 & Th < 5.60 &
                     Sr > 332 & Sr < 996 &
                     Nb > 24.7 & Nb < 74.1 &
                     Nd > 25.6 & Nd < 76.9 &
                     Sm > 6 & Sm < 18.1 &
                     Zr > 196 & Zr < 589 &
                     Yb > 1.62 & Yb < 4.86) |
                  (SiO2 > 43.5 & SiO2 < 46.5 &
                     Na2O > 1.9 & Na2O < 4.9 &
                     K2O > 0 & K2O < 2.78 &
                     Rb > 15.3 & Rb < 46.05 &
                     Ba > 126 & Ba < 378 &
                     Th > 1.58 & Th < 4.74 &
                     Sr > 294 & Sr < 882 &
                     Nb > 21 & Nb < 63.1 &
                     Nd > 23.7 & Nd < 71.1 &
                     Sm > 5.65 & Sm < 16.95 &
                     Zr > 182 & Zr < 547 &
                     Yb > 1.56 & Yb < 4.68) |
                  (SiO2 > 45.9 & SiO2 < 48.9 &
                     Na2O > 2.18 & Na2O < 5.18 &
                     K2O > 0 & K2O < 2.99 &
                     Rb > 20.55 & Rb < 61.65 &
                     Ba > 162 & Ba < 488 &
                     Th > 2.13 & Th < 6.39 &
                     Sr > 365 & Sr < 1095 &
                     Nb > 24.3 & Nb < 72.9 &
                     Nd > 26.8 & Nd < 80.5 &
                     Sm > 6.35 & Sm < 19 &
                     Zr > 190 & Zr < 571 &
                     Yb > 1.49 & Yb < 4.46) |
                  (SiO2 > 44.1 & SiO2 < 47.1 &
                     Na2O > 1.8 & Na2O < 4.8 &
                     K2O > 0 & K2O < 2.72 &
                     Rb > 15.6 & Rb < 47 &
                     Ba > 128 & Ba < 385 &
                     Th > 1.58 & Th < 4.74 &
                     Sr > 296 & Sr < 890 &
                     Nb > 20.4 & Nb < 61.2 &
                     Nd > 21.7 & Nd < 65.1 &
                     Sm > 5.35 & Sm < 16 &
                     Zr > 173 & Zr < 520 &
                     Yb > 1.4 & Yb < 4.19) |
                  (SiO2 > 45.7 & SiO2 < 48.7 &
                     Na2O > 2.08 & Na2O < 5.08 &
                     K2O > 0 & K2O < 2.85 &
                     Rb > 18.2 & Rb < 54.6 &
                     Ba > 143 & Ba < 431 &
                     Th > 1.84 & Th < 5.52 &
                     Sr > 370 & Sr < 1110 &
                     Nb > 23.5 & Nb < 70.5 &
                     Nd > 27.3 & Nd < 82 &
                     Sm > 6.65 & Sm < 19.95 &
                     Zr > 200 & Zr < 601 &
                     Yb > 1.57 & Yb < 4.71))
OIB <- full_join(OIB1,OIB2)
OIB %>% group_by(Location) %>% tally() %>% print(n=30)

OIB_pca <- OIB %>%
  dplyr::select(Sample,Location,
                TiO2,Al2O3,MgO,CaO,Na2O,K2O,
                Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Hf,Eu,Y,Yb)

samples <- joined_data %>%
  filter(Sample %in% c("E-11-08","E-11-08dup","T-12-06","T-12-06dup",
                       "T-12-07","T-12-08","T-12-09","T-12-10")) %>%
  mutate(Location = case_when(grepl("E-11-08", Sample) ~ "E-11-08",
                              grepl("E-11-08dup", Sample) ~ "E-11-08dup",
                              grepl("T-12-06", Sample) ~ "T-12-06",
                              grepl("T-12-06dup", Sample) ~ "T-12-06dup",
                              grepl("T-12-07", Sample) ~ "T-12-07",
                              grepl("T-12-08", Sample) ~ "T-12-08",
                              grepl("T-12-09", Sample) ~ "T-12-09",
                              grepl("T-12-10", Sample) ~ "T-12-10")) %>%
  dplyr::select(Sample,Location,
                TiO2,Al2O3,MgO,CaO,Na2O,K2O,
                Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Hf,Eu,Y,Yb)

is.na(OIB_pca) <- sapply(OIB_pca, is.infinite) #replace Inf by NA
OIB_pca[OIB_pca == 0] <- NA # Replace 0 with NA
OIB_pca <- OIB_pca[rowSums(is.na(OIB_pca)) == 0,] # removes rows with missing info for PCA

is.na(samples) <- sapply(samples, is.infinite) #replace Inf by NA
samples[samples == 0] <- NA # Replace 0 with NA
samples <- samples[rowSums(is.na(samples)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB_pca[,3:23], scale = TRUE, center = TRUE) # Dimension reduction using PCA
fviz_eig(res.pca)
eig <- get_eig(res.pca)
E_T_2 <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB_pca$Location, fill.ind = OIB_pca$Location,
  pointsize = 3, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-5, 8)) + scale_y_continuous(limits=c(-5,3.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
E_T_2
pdf(here("analysis","work-in-progress","OIB-210722","E_T-PCA2-a.pdf"),
    width=3.5, height=3.5)
E_T_2
dev.off()

res.pca.df <- cbind(OIB_pca[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, samples[,3:23])
pred <- cbind(samples[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

E_T_2 <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-5, 8)) + scale_y_continuous(limits=c(-5,3.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
E_T_2
pdf(here("analysis","work-in-progress","OIB-210722","E_T-PCA2-b.pdf"),
    width=3.5, height=3.5)
E_T_2
dev.off()


#### E_T SI1 ####
cols <- c(
  "Caroline islands"="#320A5A","Samoan islands"="#781B6C",
  "Austral-Cook chain"="#BB3654","Society islands"="#EC6824",
  "Marquesas islands"="#FBB41A","Hawai'i islands"="#F4DD53",
  "E-11-08"="red","T-12-06"="red","T-12-06dup"="red","T-12-07"="red",
  "T-12-08"="red","T-12-09"="red","T-12-10"="red")
shapes <- c(
  "Caroline islands"=21,"Samoan islands"=24,"Austral-Cook chain"=23,
  "Society islands"=22,"Hawai'i islands"=25,"Marquesas islands"=21,"E-11-08"=8,
  "T-12-06"=21,"T-12-06dup"=12,"T-12-07"=22,"T-12-08"=23,"T-12-09"=24,"T-12-10"=25)
contour <- c(
  "Caroline islands"="black","Samoan islands"="black","Austral-Cook chain"="black",
  "Society islands"="black","Hawai'i islands"="black","Marquesas islands"="black",
  "E-11-08"="red","T-12-06"="black","T-12-06dup"="black",
  "T-12-07"="black","T-12-08"="black","T-12-09"="black","T-12-10"="black")

samples <- joined_data %>%
  filter(Sample %in% c("E-11-08","E-11-08dup","T-12-06","T-12-06dup",
                       "T-12-07","T-12-08","T-12-09","T-12-10")) %>%
  mutate(Location = case_when(grepl("E-11-08", Sample) ~ "E-11-08",
                              grepl("E-11-08dup", Sample) ~ "E-11-08dup",
                              grepl("T-12-06", Sample) ~ "T-12-06",
                              grepl("T-12-06dup", Sample) ~ "T-12-06dup",
                              grepl("T-12-07", Sample) ~ "T-12-07",
                              grepl("T-12-08", Sample) ~ "T-12-08",
                              grepl("T-12-09", Sample) ~ "T-12-09",
                              grepl("T-12-10", Sample) ~ "T-12-10"))

E_T_ranges[,2:3] %>% round(digits = 6)
E_T_ranges[,36:39] %>% round(digits = 6)
E_T_ranges[,40:45] %>% round(digits = 3)

OIB1 <- dbGetQuery(georoc,
                   "SELECT *
                   FROM 'sample'
                   WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
                   TECTONIC_SETTING='OCEAN ISLAND' AND
                   ((`SIO2(WT%)` > 45.2 AND `SIO2(WT%)` < 48.2 AND
                   ND143_ND144 > 0.512341 AND ND143_ND144 < 0.513367 AND
                   SR87_SR86 > 0.704586 AND SR87_SR86 < 0.705996 AND
                   PB206_PB204 > 18.658 AND PB206_PB204 < 19.034 AND
                   PB207_PB204 > 15.43 AND PB207_PB204 < 15.742 AND
                   PB208_PB204 > 38.434 AND PB208_PB204 < 39.21) OR
                   (`SIO2(WT%)` > 44.3 AND `SIO2(WT%)` < 47.3 AND
                   ND143_ND144 > 0.512342 AND ND143_ND144 < 0.513368 AND
                   SR87_SR86 > 0.704583 AND SR87_SR86 < 0.705993 AND
                   PB206_PB204 > 18.701 AND PB206_PB204 < 19.079 AND
                   PB207_PB204 > 15.431 AND PB207_PB204 < 15.743 AND
                   PB208_PB204 > 38.482 AND PB208_PB204 < 39.26) OR
                   (`SIO2(WT%)` > 43.5 AND `SIO2(WT%)` < 46.5 AND
                   ND143_ND144 > 0.512358 AND ND143_ND144 < 0.513384 AND
                   SR87_SR86 > 0.704403 AND SR87_SR86 < 0.705813 AND
                   PB206_PB204 > 18.722 AND PB206_PB204 < 19.1 AND
                   PB207_PB204 > 15.427 AND PB207_PB204 < 15.739 AND
                   PB208_PB204 > 38.472 AND PB208_PB204 < 39.25) OR
                   (`SIO2(WT%)` > 45.9 AND `SIO2(WT%)` < 48.9 AND
                   ND143_ND144 > 0.512342 AND ND143_ND144 < 0.513368 AND
                   SR87_SR86 > 0.704502 AND SR87_SR86 < 0.705912 AND
                   PB206_PB204 > 18.72 AND PB206_PB204 < 19.098 AND
                   PB207_PB204 > 15.445 AND PB207_PB204 < 15.757 AND
                   PB208_PB204 > 38.505 AND PB208_PB204 < 39.283) OR
                   (`SIO2(WT%)` > 44.1 AND `SIO2(WT%)` < 47.1 AND
                   ND143_ND144 > 0.512359 AND ND143_ND144 < 0.513385 AND
                   SR87_SR86 > 0.704412 AND SR87_SR86 < 0.705822 AND
                   PB206_PB204 > 18.736 AND PB206_PB204 < 19.114 AND
                   PB207_PB204 > 15.429 AND PB207_PB204 < 15.741 AND
                   PB208_PB204 > 38.508 AND PB208_PB204 < 39.286) OR
                   (`SIO2(WT%)` > 45.7 AND `SIO2(WT%)` < 48.7 AND
                   ND143_ND144 > 0.512348 AND ND143_ND144 < 0.513374 AND
                   SR87_SR86 > 0.704465 AND SR87_SR86 < 0.705875 AND
                   PB206_PB204 > 18.624 AND PB206_PB204 < 19 AND
                   PB207_PB204 > 15.425 AND PB207_PB204 < 15.737 AND
                   PB208_PB204 > 38.362 AND PB208_PB204 < 39.138))") %>%
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
  rename_pofatu_elements() %>% pofatu_location() %>%
  dplyr::filter((SiO2 > 45.2 & SiO2 < 48.2 &
                   Nd143_Nd144 > 0.512341 & Nd143_Nd144 < 0.513367 &
                   Sr87_Sr86 > 0.704586 & Sr87_Sr86 < 0.705996 &
                   Pb206_Pb204 > 18.658 & Pb206_Pb204 < 19.034 &
                   Pb207_Pb204 > 15.43 & Pb207_Pb204 < 15.742 &
                   Pb208_Pb204 > 38.434 & Pb208_Pb204 < 39.21) |
                  (SiO2 > 44.3 & SiO2 < 47.3 &
                     Nd143_Nd144 > 0.512342 & Nd143_Nd144 < 0.513368 &
                     Sr87_Sr86 > 0.704583 & Sr87_Sr86 < 0.705993 &
                     Pb206_Pb204 > 18.701 & Pb206_Pb204 < 19.079 &
                     Pb207_Pb204 > 15.431 & Pb207_Pb204 < 15.743 &
                     Pb208_Pb204 > 38.482 & Pb208_Pb204 < 39.26) |
                  (SiO2 > 43.5 & SiO2 < 46.5 &
                     Nd143_Nd144 > 0.512358 & Nd143_Nd144 < 0.513384 &
                     Sr87_Sr86 > 0.704403 & Sr87_Sr86 < 0.705813 &
                     Pb206_Pb204 > 18.722 & Pb206_Pb204 < 19.1 &
                     Pb207_Pb204 > 15.427 & Pb207_Pb204 < 15.739 &
                     Pb208_Pb204 > 38.472 & Pb208_Pb204 < 39.25) |
                  (SiO2 > 45.9 & SiO2 < 48.9 &
                     Nd143_Nd144 > 0.512342 & Nd143_Nd144 < 0.513368 &
                     Sr87_Sr86 > 0.704502 & Sr87_Sr86 < 0.705912 &
                     Pb206_Pb204 > 18.72 & Pb206_Pb204 < 19.098 &
                     Pb207_Pb204 > 15.445 & Pb207_Pb204 < 15.757 &
                     Pb208_Pb204 > 38.505 & Pb208_Pb204 < 39.283) |
                  (SiO2 > 44.1 & SiO2 < 47.1 &
                     Nd143_Nd144 > 0.512359 & Nd143_Nd144 < 0.513385 &
                     Sr87_Sr86 > 0.704412 & Sr87_Sr86 < 0.705822 &
                     Pb206_Pb204 > 18.736 & Pb206_Pb204 < 19.114 &
                     Pb207_Pb204 > 15.429 & Pb207_Pb204 < 15.741 &
                     Pb208_Pb204 > 38.508 & Pb208_Pb204 < 39.286) |
                  (SiO2 > 45.7 & SiO2 < 48.7 &
                     Nd143_Nd144 > 0.512348 & Nd143_Nd144 < 0.513374 &
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

OIB <- full_join(OIB1,OIB2)
is.na(OIB) <- sapply(OIB, is.infinite) #replace Inf by NA

d_map <- OIB %>% dplyr::select(Sample,Location,lat,long)
d_map$long <- ifelse(d_map$long < -25, d_map$long + 360, d_map$long)

map <- ggplot() +
  geom_polygon(data = fortify(maps::map("world2", plot=FALSE, fill=TRUE)),
               aes(x=long, y=lat, group=group),
               color="black", fill="gray90", size=.25) +
  coord_equal(xlim = c(140,230), ylim = c(-45.5,23)) +
  geom_point(data=d_map, aes(x=long, y=lat,
                             shape=factor(Location),
                             fill=factor(Location),
                             colour=factor(Location)), size=3, alpha=0.7, stroke = .4) +
  geom_point(data=samples, aes(x=long, y=lat,
                               shape=factor(Location),
                               fill=factor(Location),
                               colour=factor(Location)), size=3, stroke = 1) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=contour) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.75),
        panel.background = element_rect(fill="white"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.title=element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        #legend.key=element_blank(), legend.background = element_blank(),
        #legend.text=element_text(size=8),
        legend.position="none") +
  #guides(shape = guide_legend(override.aes = list(size = 2), keyheight=0.15, default.unit="inch")) +
  annotate("text", -Inf, Inf, label = "A", size = 7, hjust = -1, vjust = 2)
map

biplot <- OIB %>% ggplot(aes(x=Sr87_Sr86,y=Nd143_Nd144, shape=factor(Location),
                             fill=factor(Location), color=factor(Location), group=Sample)) +
  geom_point(size = 3, alpha=0.7, stroke = .4) +
  geom_point(data=samples, size=3, stroke = .75) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(.7043, .7059)) +
  scale_y_continuous(limits=c(.51269, .51291)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"),
       y=expression({}^143*"Nd / "*{}^144*"Nd")) +
  annotate("text", Inf, Inf, label = "B", size = 7, hjust = 2, vjust = 2)
biplot
pdf(here("analysis","work-in-progress","OIB-210722","E_T_SI1_1.pdf"),
    width=12, height=5)
map | biplot
dev.off()


p1 <- OIB %>% ggplot(aes(x=Sr/Nb,y=Nb/La, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.8) + geom_point(data=samples, size=3, stroke=.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(position = "top", limits=c(7, 22)) +
  scale_y_continuous(limits=c(.7, 2.1), breaks=c(1,2)) +
  labs(x=expression("Sr / Nb"), y=expression("Nb / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "none", aspect.ratio=1) +
  annotate("text", -Inf, Inf, label = "C", size = 7, hjust = -1, vjust = 2)
p2 <- OIB %>% ggplot(aes(x=Sr/Nb,y=La/Yb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.8) + geom_point(data=samples, size=3, stroke=.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(7, 22)) + scale_y_continuous(limits=c(8, 28)) +
  labs(x=expression("Sr / Nb"), y=expression("La / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)

p3 <- OIB %>% ggplot(aes(x=Nd143_Nd144,y=Nb/La, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.8) + geom_point(data=samples, size=3, stroke=.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(position = "top", limits=c(.51267, .51292),
                     breaks=c(.5127,.5128,.5129)) +
  scale_y_continuous(limits=c(.7, 2.1), breaks=c(1,2)) +
  labs(x=expression({}^143*"Nd / "*{}^144*"Nd"), y=expression("Nb / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)
p4 <- OIB %>% ggplot(aes(x=Nd143_Nd144,y=La/Yb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.8) + geom_point(data=samples, size=3, stroke=.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(.51267, .51292), breaks=c(.5127,.5128,.5129)) +
  scale_y_continuous(limits=c(7, 22)) +
  labs(x=expression({}^143*"Nd / "*{}^144*"Nd"), y=expression("La / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)

p5 <- OIB %>% ggplot(aes(x=Pb206_Pb204,y=Nb/La, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.8) + geom_point(data=samples, size=3, stroke=.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(position = "top", limits=c(18.65, 19.15),
                     breaks=c(18.7,18.9,19.1)) +
  scale_y_continuous(position = "right", limits=c(.7, 2.1), breaks=c(1,2)) +
  labs(x=expression({}^206*"Pb / "*{}^204*"Pb"), y=expression("Nb / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)
p6 <- OIB %>% ggplot(aes(x=Pb206_Pb204,y=La/Yb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.8) + geom_point(data=samples, size=3, stroke=.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(18.65, 19.15), breaks=c(18.7,18.9,19.1)) +
  scale_y_continuous(position = "right", limits=c(7, 22)) +
  labs(x=expression({}^206*"Pb / "*{}^204*"Pb"), y=expression("La / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)

pdf(here("analysis","work-in-progress","OIB-210722","E_T_SI1_2.pdf"),
    width=12, height=9)
((p1/p2) | (p3/p4) | (p5/p6)) +
  plot_layout(guides = "collect") & theme(legend.position = "none")
dev.off()


#### E_T SI2 ####
OIB1 <- dbGetQuery(georoc,
                   "SELECT *
                  FROM 'sample'
                  WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
                  ((`SIO2(WT%)` > 45.2 AND `SIO2(WT%)` < 48.2 AND
                  `NA2O(WT%)` > 2.08 AND `NA2O(WT%)` < 5.08 AND
                  `K2O(WT%)` > 0 AND `K2O(WT%)` < 2.9 AND
                  `RB(PPM)` > 18.3 AND `RB(PPM)` < 54.9 AND
                  `BA(PPM)` > 160 AND `BA(PPM)` < 480 AND
                  `TH(PPM)` > 1.95 AND `TH(PPM)` < 5.84 AND
                  `SR(PPM)` > 352 AND `SR(PPM)` < 1056 AND
                  `NB(PPM)` > 24.8 AND `NB(PPM)` < 74.5) OR
                  (`SIO2(WT%)` > 44.3 AND `SIO2(WT%)` < 47.3 AND
                  `NA2O(WT%)` > 2 AND `NA2O(WT%)` < 5 AND
                  `K2O(WT%)` > 0 AND `K2O(WT%)` < 2.82 AND
                  `RB(PPM)` > 16.8 AND `RB(PPM)` < 50.4 AND
                  `BA(PPM)` > 137 AND `BA(PPM)` < 413 AND
                  `TH(PPM)` > 1.86 AND `TH(PPM)` < 5.60 AND
                  `SR(PPM)` > 332 AND `SR(PPM)` < 996 AND
                  `NB(PPM)` > 24.7 AND `NB(PPM)` < 74.1) OR
                  (`SIO2(WT%)` > 43.5 AND `SIO2(WT%)` < 46.5 AND
                  `NA2O(WT%)` > 1.9 AND `NA2O(WT%)` < 4.9 AND
                  `K2O(WT%)` > 0 AND `K2O(WT%)` < 2.78 AND
                  `RB(PPM)` > 15.3 AND `RB(PPM)` < 46.05 AND
                  `BA(PPM)` > 126 AND `BA(PPM)` < 378 AND
                  `TH(PPM)` > 1.58 AND `TH(PPM)` < 4.74 AND
                  `SR(PPM)` > 294 AND `SR(PPM)` < 882 AND
                  `NB(PPM)` > 21 AND `NB(PPM)` < 63.1) OR
                  (`SIO2(WT%)` > 45.9 AND `SIO2(WT%)` < 48.9 AND
                  `NA2O(WT%)` > 2.18 AND `NA2O(WT%)` < 5.18 AND
                  `K2O(WT%)` > 0 AND `K2O(WT%)` < 2.99 AND
                  `RB(PPM)` > 20.55 AND `RB(PPM)` < 61.65 AND
                  `BA(PPM)` > 162 AND `BA(PPM)` < 488 AND
                  `TH(PPM)` > 2.13 AND `TH(PPM)` < 6.39 AND
                  `SR(PPM)` > 365 AND `SR(PPM)` < 1095 AND
                  `NB(PPM)` > 24.3 AND `NB(PPM)` < 72.9) OR
                  (`SIO2(WT%)` > 44.1 AND `SIO2(WT%)` < 47.1 AND
                  `NA2O(WT%)` > 1.8 AND `NA2O(WT%)` < 4.8 AND
                  `K2O(WT%)` > 0 AND `K2O(WT%)` < 2.72 AND
                  `RB(PPM)` > 15.6 AND `RB(PPM)` < 47 AND
                  `BA(PPM)` > 128 AND `BA(PPM)` < 385 AND
                  `TH(PPM)` > 1.58 AND `TH(PPM)` < 4.74 AND
                  `SR(PPM)` > 296 AND `SR(PPM)` < 890 AND
                  `NB(PPM)` > 20.4 AND `NB(PPM)` < 61.2) OR
                  (`SIO2(WT%)` > 45.7 AND `SIO2(WT%)` < 48.7 AND
                  `NA2O(WT%)` > 2.08 AND `NA2O(WT%)` < 5.08 AND
                  `K2O(WT%)` > 0 AND `K2O(WT%)` < 2.85 AND
                  `RB(PPM)` > 18.2 AND `RB(PPM)` < 54.6 AND
                  `BA(PPM)` > 143 AND `BA(PPM)` < 431 AND
                  `TH(PPM)` > 1.84 AND `TH(PPM)` < 5.52 AND
                  `SR(PPM)` > 370 AND `SR(PPM)` < 1110 AND
                  `NB(PPM)` > 23.5 AND `NB(PPM)` < 70.5))") %>%
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
                   Rb > 18.3 & Rb < 54.9 &
                   Ba > 160 & Ba < 480 &
                   Th > 1.95 & Th < 5.84 &
                   Sr > 352 & Sr < 1056 &
                   Nb > 24.8 & Nb < 74.5) |
                  (SiO2 > 44.3 & SiO2 < 47.3 &
                     Na2O > 2 & Na2O < 5 &
                     K2O > 0 & K2O < 2.82 &
                     Rb > 16.8 & Rb < 50.4 &
                     Ba > 137 & Ba < 413 &
                     Th > 1.86 & Th < 5.60 &
                     Sr > 332 & Sr < 996 &
                     Nb > 24.7 & Nb < 74.1) |
                  (SiO2 > 43.5 & SiO2 < 46.5 &
                     Na2O > 1.9 & Na2O < 4.9 &
                     K2O > 0 & K2O < 2.78 &
                     Rb > 15.3 & Rb < 46.05 &
                     Ba > 126 & Ba < 378 &
                     Th > 1.58 & Th < 4.74 &
                     Sr > 294 & Sr < 882 &
                     Nb > 21 & Nb < 63.1) |
                  (SiO2 > 45.9 & SiO2 < 48.9 &
                     Na2O > 2.18 & Na2O < 5.18 &
                     K2O > 0 & K2O < 2.99 &
                     Rb > 20.55 & Rb < 61.65 &
                     Ba > 162 & Ba < 488 &
                     Th > 2.13 & Th < 6.39 &
                     Sr > 365 & Sr < 1095 &
                     Nb > 24.3 & Nb < 72.9) |
                  (SiO2 > 44.1 & SiO2 < 47.1 &
                     Na2O > 1.8 & Na2O < 4.8 &
                     K2O > 0 & K2O < 2.72 &
                     Rb > 15.6 & Rb < 47 &
                     Ba > 128 & Ba < 385 &
                     Th > 1.58 & Th < 4.74 &
                     Sr > 296 & Sr < 890 &
                     Nb > 20.4 & Nb < 61.2) |
                  (SiO2 > 45.7 & SiO2 < 48.7 &
                     Na2O > 2.08 & Na2O < 5.08 &
                     K2O > 0 & K2O < 2.85 &
                     Rb > 18.2 & Rb < 54.6 &
                     Ba > 143 & Ba < 431 &
                     Th > 1.84 & Th < 5.52 &
                     Sr > 370 & Sr < 1110 &
                     Nb > 23.5 & Nb < 70.5)) %>%
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
OIB %>% group_by(Location) %>% tally()

cols <- c("Savai'i"="#BB3654","Upolu"="#EC6824","Manua Islands"="#F4DD53",
          "Tutuila"="#781B6C","E-11-08"="red","T-12-06"="red","T-12-07"="red",
          "T-12-08"="red","T-12-09"="red","T-12-10"="red")
shapes <- c("Savai'i"=21,"Upolu"=21,"Manua Islands"=21,"Tutuila"=21,
            "E-11-08"=8,"T-12-06"=21,"T-12-07"=22,"T-12-08"=23,"T-12-09"=24,"T-12-10"=25)
contour <- c("Savai'i"="black","Upolu"="black","Manua Islands"="black",
             "Tutuila"="black","E-11-08"="red","T-12-06"="black","T-12-07"="black",
             "T-12-08"="black","T-12-09"="black","T-12-10"="black")

map <- OIB %>% ggplot() +
  geom_polygon(data = fortify(maps::map("world", plot=FALSE, fill=TRUE)),
               aes(x=long, y=lat, group=group),
               color="black", fill="gray90", size=.25) +
  coord_equal(xlim = c(-173,-169.4), ylim = c(-15.7,-12.3)) +
  geom_point(data=OIB, aes(x=long, y=lat, shape=factor(Location), fill=factor(Location),
                           color=factor(Location), group=Sample), size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=contour) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.75),
        panel.background = element_rect(fill="white"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.title=element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.key=element_blank(), legend.background = element_blank(),
        legend.text=element_text(size=8), legend.direction = "horizontal",
        legend.key.size = unit(.4, "cm"), legend.position=c(.5,.1)) +
  #guides(shape = guide_legend(override.aes = list(size = 2), keyheight=0.15, default.unit="inch")) +
  annotate("text", Inf, Inf, label = "A", size = 7,  hjust = 2, vjust = 2)
map

biplot <- OIB %>% ggplot(aes(x=Sr87_Sr86,y=Nd143_Nd144, shape=factor(Location),
                             fill=factor(Location), color=factor(Location), group=Sample)) +
  geom_point(size = 3, alpha=0.7, stroke = .4) +
  geom_point(data=samples, size=3, stroke = .5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(.7044, .707)) +
  scale_y_continuous(limits=c(.51265, .51291), breaks=c(.5127,.5128,.5129)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"),
       y=expression({}^143*"Nd / "*{}^144*"Nd")) +
  annotate("text", Inf, Inf, label = "B", size = 7, hjust = 2, vjust = 2)
biplot

pdf(here("analysis","work-in-progress","OIB-210722","E_T_SI2_1.pdf"),
    width=12, height=5)
map | biplot
dev.off()


p1 <- OIB %>% ggplot(aes(x=Sr,y=Ba/La, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.8) + geom_point(data=samples, size=3, stroke=.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(position = "top", limits=c(330, 840)) +
  scale_y_continuous(limits=c(5, 15), breaks=c(5,10,15)) +
  labs(x=expression("Sr"), y=expression("Ba / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "none", aspect.ratio=1) +
  annotate("text", -Inf, Inf, label = "C", size = 7, hjust = -1, vjust = 2)
p2 <- OIB %>% ggplot(aes(x=Sr,y=Nb/Yb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.8) + geom_point(data=samples, size=3, stroke=.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(330, 840)) +
  scale_y_continuous(limits=c(10, 40)) +
  labs(x=expression("Sr (ppm)"), y=expression("Nb / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)

p3 <- OIB %>% ggplot(aes(x=Sr87_Sr86,y=Ba/La, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.8) + geom_point(data=samples, size=3, stroke=.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(position = "top", limits=c(.7044, .707)) +
  scale_y_continuous(limits=c(5, 15), breaks=c(5,10,15)) +
  labs(x=expression({}^143*"Nd / "*{}^144*"Nd"), y=expression("Ba / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)
p4 <- OIB %>% ggplot(aes(x=Sr87_Sr86,y=Nb/Yb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.8) + geom_point(data=samples, size=3, stroke=.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(.7044, .707)) +
  scale_y_continuous(limits=c(10, 40)) +
  labs(x=expression({}^143*"Nd / "*{}^144*"Nd"), y=expression("Nb / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)

p5 <- OIB %>% ggplot(aes(x=Pb206_Pb204,y=Ba/La, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.8) + geom_point(data=samples, size=3, stroke=.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(position = "top", limits=c(18.7, 19.4)) +
  scale_y_continuous(position = "right", limits=c(5, 15), breaks=c(5,10,15)) +
  labs(x=expression({}^206*"Pb / "*{}^204*"Pb"), y=expression("Ba / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)
p6 <- OIB %>% ggplot(aes(x=Pb206_Pb204,y=Nb/Yb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.8) + geom_point(data=samples, size=3, stroke=.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous( limits=c(18.7, 19.4)) +
  scale_y_continuous(position = "right", limits=c(10, 40)) +
  labs(x=expression({}^206*"Pb / "*{}^204*"Pb"), y=expression("La / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)

pdf(here("analysis","work-in-progress","OIB-210722","E_T_SI2_2.pdf"),
    width=12, height=9)
((p1/p2) | (p3/p4) | (p5/p6)) +
  plot_layout(guides = "collect") & theme(legend.position = "none")
dev.off()


#### E_T SI3 ####
samples <- joined_data %>%
  filter(Sample %in% c("E-11-08","E-11-08dup","T-12-06","T-12-06dup",
                       "T-12-07","T-12-08","T-12-09","T-12-10")) %>%
  mutate(Location = case_when(grepl("E-11-08", Sample) ~ "E-11-08",
                              grepl("E-11-08dup", Sample) ~ "E-11-08dup",
                              grepl("T-12-06", Sample) ~ "T-12-06",
                              grepl("T-12-06dup", Sample) ~ "T-12-06dup",
                              grepl("T-12-07", Sample) ~ "T-12-07",
                              grepl("T-12-08", Sample) ~ "T-12-08",
                              grepl("T-12-09", Sample) ~ "T-12-09",
                              grepl("T-12-10", Sample) ~ "T-12-10"))

OIB <- dbGetQuery(pofatu,
                  "SELECT s.id AS sample_id, s.sample_category,
s.location_subregion, s.site_name, s.sample_comment,
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
'Pb206_Pb204', 'Pb207_Pb204', 'Pb208_Pb204') GROUP BY sample_id") %>%
  rename_pofatu_elements() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>% Fe2O3_from_FeO()

OIB %>% dplyr::filter(sample_category %in% c("SOURCE")) %>%
  group_by(site_name) %>% tally() %>% print(n=30)

OIB <- OIB %>%
  dplyr::mutate(Category = case_when(
    grepl("Artefact", sample_comment) ~ "artefact",
    sample_category == "SOURCE" ~ "source",
    sample_category == "ARTEFACT" ~ "artefact")) %>%
  dplyr::mutate(Location = case_when(
    site_name == "Tatagamatau" & Category == "source" ~ "Tatagamatau source",
    site_name == "Tatagamatau" & Category == "artefact" ~ "Tatagamatau artefact",
    grepl("MALAELOA", Sample) & Category == "source" ~ "Malaeloa source",
    grepl("MALAELOA", Sample) & Category == "artefact" ~ "Malaeloa artefact",
    grepl("MALOATA", Sample) & Category == "source" ~ "Maloata source",
    grepl("Maloata", site_name) & Category == "source" ~ "Maloata source",
    grepl("MALOATA", Sample) & Category == "artefact" ~ "Maloata artefact",
    grepl("Sa'ilele", site_name) & Category == "source" ~ "Other Tutuila sources",
    grepl("Laeano", site_name) & Category == "source" ~ "Other Tutuila sources",
    grepl("Asiapa", site_name) & Category == "source" ~ "Other Tutuila sources",
    grepl("Taputapu", site_name) & Category == "source" ~ "Other Tutuila sources",
    grepl("Lau'agae", site_name) & Category == "source" ~ "Other Tutuila sources",
    grepl("Laeano", site_name) & Category == "source" ~ "Other Tutuila sources",
    grepl("Fagasa", site_name) & Category == "source" ~ "Other Tutuila sources",
    grepl("Mapua", site_name) & Category == "source" ~ "Other Tutuila sources",
    TRUE ~ "NA")) %>%
  dplyr::select(
    Sample,Location,site_name,Category,lat,long,SiO2,TiO2,Al2O3,Fe2O3,MnO,MgO,CaO,Na2O,K2O,
    Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
    Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
    Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB %>% dplyr::filter(Category %in% c("source")) %>%
  dplyr::filter(Location %in% c("NA")) %>%
  dplyr::select(Sample, Location, site_name)

OIB %>% dplyr::filter(Category %in% c("source")) %>%
  group_by(Location) %>% tally() %>% print(n=30)

cols <- c("Tatagamatau source"="#781B6C","Tatagamatau artefact"="#781B6C",
          "Malaeloa source"="#BB3654","Malaeloa artefact"="#BB3654",
          "Maloata source"="#F4DD53","Maloata artefact"="#F4DD53",
          "Other Tutuila sources"="gray60","E-11-08"="red","T-12-06"="red","T-12-07"="red",
          "T-12-08"="red","T-12-09"="red","T-12-10"="red")
shapes <- c("Tatagamatau source"=24,"Tatagamatau artefact"=2,"Malaeloa source"=25,
            "Malaeloa artefact"=6,"Maloata source"=23,"Maloata artefact"=5,
            "Other Tutuila sources"=3,
            "E-11-08"=8,"T-12-06"=21,"T-12-07"=22,"T-12-08"=23,
            "T-12-09"=24,"T-12-10"=25)
contour <- c("Tatagamatau source"="black","Tatagamatau artefact"="#781B6C",
             "Malaeloa source"="black","Malaeloa artefact"="#BB3654",
             "Maloata source"="black","Maloata artefact"="#F4DD53",
             "Other Tutuila sources"="gray60",
             "E-11-08"="red","T-12-06"="black","T-12-07"="black",
             "T-12-08"="black","T-12-09"="black","T-12-10"="black")

library(osmdata)
coast <- getbb("Tutuila Samoa")%>% opq()%>%
  add_osm_feature(key = "natural", value = c("coastline")) %>%
  osmdata_sf()
rivers <- getbb("Tutuila Samoa")%>% opq()%>%
  add_osm_feature(key = "waterway", value = c("river","stream")) %>%
  osmdata_sf()

m <- ggplot() +
  geom_sf(data = coast$osm_lines, inherit.aes = FALSE, color = "black",
          size = .4, alpha = .8) +
  geom_sf(data = rivers$osm_lines, inherit.aes = FALSE, color = "#a6daff",
          size = .2, alpha = .8) +
  coord_sf(xlim = c(-170.85, -170.56), ylim = c(-14.39, -14.225), expand = FALSE) +
  theme_void()
tutuila <- m +
  geom_point(data=OIB,
             mapping=aes(x=long, y=lat, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group = Location), size=2.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  theme(panel.border = element_rect(colour = NA, fill= NA, size=1),
        panel.background = element_rect(colour = "black", fill= NA, size=1),
        legend.title = element_blank(), legend.text = element_text(size = 9),
        legend.key.size = unit(.35, 'cm'), legend.position = "none"#c(.83,.3)
        ) +
  #guides(color = guide_legend(override.aes = list(size = 2))) +
  annotate("text", -Inf, Inf, label = "A", size = 7, hjust = -1, vjust = 2)
tutuila

TAS <- OIB %>% ggplot(aes(x=SiO2,y=Na2O+K2O, shape=factor(Location),
                          fill=factor(Location), color=factor(Location))) +
  geom_point(stroke=.25, size=2.5) + geom_point(data=samples, stroke=.5, size=2.5) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(44.8,51)) +
  scale_y_continuous(limits=c(3.5,6), breaks=c(4,5,6)) +
  labs(y=expression(Na[2]*O + K[2]*O*~ "(wt%)"), x=expression(SiO[2]*~ "(wt%)"))+
  #annotate("segment", x=45, xend=51, y=5, yend=5, size = 0.3, linetype = "dashed", colour = "black")+
  #annotate("text", label = "Trachybasalt", x=46.4, y=5.5, size=3, colour="black")+
  #annotate("text", label = "Alkali basalt", x=48.5, y=4.4, size=3, colour="black")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.title = element_blank(),
        legend.background=element_blank(), legend.key=element_blank(),
        legend.key.size = unit(.35, "cm"), legend.text = element_text(size = 8),
        legend.position = c(.8,.25), aspect.ratio=1) +
  guides(color = guide_legend(override.aes = list(size = 1.5))) +
  annotate("text", -Inf, Inf, label = "B", size = 7, hjust = -1, vjust = 2)
TAS

pdf(here("analysis","work-in-progress","OIB-210722","E_T_SI3_1.pdf"),
    width=12, height=5)
tutuila | TAS
dev.off()


p1 <- OIB %>% ggplot(aes(x=SiO2/MgO, y=Rb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size=3, alpha=0.7) + geom_point(data=samples, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(position = "top", limits=c(6.5, 12.5)) +
  scale_y_continuous(limits=c(22, 53)) +
  labs(x=expression(SiO[2] / MgO)) + labs(y=expression("Rb (ppm)")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "none", aspect.ratio=1) +
  annotate("text", -Inf, Inf, label = "C", size = 7, hjust = -1, vjust = 2)
p2 <- OIB %>% ggplot(aes(x=SiO2/MgO, y=Sr, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size=3, alpha=0.7) + geom_point(data=samples, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(6.5, 12.5)) +
  scale_y_continuous(limits=c(450, 850)) +
  labs(x=expression(SiO[2] / MgO)) + labs(y=expression("Sr (ppm)")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), legend.position = "none", aspect.ratio=1)
p3 <- OIB %>% ggplot(aes(x=SiO2/MgO, y=Zr, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size=3, alpha=0.7) + geom_point(data=samples, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(6.5, 12.5)) +
  scale_y_continuous(limits = c(280, 480))+
  labs(x=expression(SiO[2] / MgO)) + labs(y=expression("Zr (ppm)")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)

p4 <- OIB %>% ggplot(aes(x=Al2O3, y=Rb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size=3, alpha=0.7) + geom_point(data=samples, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  labs(x=expression(Al[2]*O[3]), y=expression("Rb (ppm)")) +
  scale_x_continuous(position = "top", limits=c(13.5, 17.5)) +
  scale_y_continuous(limits=c(22, 53)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), legend.position = "none", aspect.ratio=1)
p5 <- OIB %>% ggplot(aes(x=Al2O3, y=Sr, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size=3, alpha=0.7) + geom_point(data=samples, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  labs(x=expression(Al[2]*O[3]), y=expression("Sr (ppm)")) +
  scale_x_continuous(limits = c(13.5, 17.5))+
  scale_y_continuous(limits=c(450, 850)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = "none", aspect.ratio=1)
p6 <- OIB %>% ggplot(aes(x=Al2O3, y=Zr, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size=3, alpha=0.7) + geom_point(data=samples, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(13.5, 17.5)) +
  scale_y_continuous(limits = c(280, 480))+
  labs(x=expression(Al[2]*O[3]), y=expression("Zr (ppm)")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), legend.position = "none", aspect.ratio=1)

p7 <- OIB %>% ggplot(aes(x=CaO/Na2O, y=Rb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size=3, alpha=0.7) + geom_point(data=samples, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  labs(x=expression(CaO / Na[2]*O), y=expression("Rb (ppm)")) +
  scale_x_continuous(position = "top", limits=c(1.7, 3.3)) +
  scale_y_continuous(position = "right", limits=c(22, 53)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)
p8 <- OIB %>% ggplot(aes(x=CaO/Na2O, y=Sr, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size=3, alpha=0.7) + geom_point(data=samples, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  labs(x=expression(CaO / Na[2]*O), y=expression("Sr (ppm)")) +
  scale_x_continuous(limits = c(1.7, 3.3))+
  scale_y_continuous(position = "right", limits=c(450, 850)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),legend.position = "none", aspect.ratio=1)
p9 <- OIB %>% ggplot(aes(x=CaO/Na2O, y=Zr, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size=3, alpha=0.7) + geom_point(data=samples, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(1.7, 3.3)) +
  scale_y_continuous(position = "right", limits = c(280, 480))+
  labs(x=expression(CaO / Na[2]*O), y=expression("Zr (ppm)")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)


p1 <- OIB %>% ggplot(aes(x=SiO2/MgO, y=Rb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size=3, alpha=0.7) + geom_point(data=samples, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(6.5, 12.5)) +
  scale_y_continuous(limits=c(22, 53)) +
  labs(x=expression(SiO[2] / MgO)) + labs(y=expression("Rb (ppm)")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "none", aspect.ratio=1) +
  annotate("text", -Inf, Inf, label = "C", size = 7, hjust = -1, vjust = 2)
p2 <- OIB %>% ggplot(aes(x=Al2O3, y=Rb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size=3, alpha=0.7) + geom_point(data=samples, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  labs(x=expression(Al[2]*O[3]), y=expression("Rb (ppm)")) +
  scale_x_continuous(limits=c(13.5, 17.5)) +
  scale_y_continuous(limits=c(22, 53)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), legend.position = "none", aspect.ratio=1)
p3 <- OIB %>% ggplot(aes(x=CaO/Na2O, y=Rb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size=3, alpha=0.7) + geom_point(data=samples, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  labs(x=expression(CaO / Na[2]*O), y=expression("Rb (ppm)")) +
  scale_x_continuous(limits=c(1.7, 3.3)) +
  scale_y_continuous(position = "right", limits=c(22, 53)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)
pdf(here("analysis","work-in-progress","OIB-210722","E_T_SI3_2.pdf"),
    width=12, height=4)
(p1 | p2 | p3) +
  plot_layout(guides = "collect") & theme(legend.position = "none")
dev.off()

pdf(here("analysis","work-in-progress","OIB-210722","E_T_SI3.pdf"),
    width=12, height=8)
((tutuila | TAS)/(p1 | p2 | p3)) +
  plot_layout(guides = "collect") & theme(legend.position = "none")
dev.off()

#### E_T SI4 ####
artefacts <- dbGetQuery(pofatu,
                        "SELECT s.id AS sample_id, s.sample_name, s.sample_category, s.artefact_category,
s.location_region, s.location_subregion, s.location_locality,
s.location_latitude, s.location_longitude,
substr(m.method_id, 1, instr(m.method_id, '_') - 1) AS method_id,
max(CASE WHEN m.parameter = 'SiO2 [%]' then m.value END) AS 'SiO2 [%]',
max(CASE WHEN m.parameter = 'TiO2 [%]' then m.value END) AS 'TiO2 [%]',
max(CASE WHEN m.parameter = 'Al2O3 [%]' then m.value END) AS 'Al2O3 [%]',
max(CASE WHEN m.parameter = 'Fe2O3 [%]' then m.value END) AS 'Fe2O3 [%]',
max(CASE WHEN m.parameter = 'FeO [%]' then m.value END) AS 'FeO [%]',
max(CASE WHEN m.parameter = 'MnO [%]' then m.value END) AS 'MnO [%]',
max(CASE WHEN m.parameter = 'MgO [%]' then m.value END) AS 'MgO [%]',
max(CASE WHEN m.parameter = 'CaO [%]' then m.value END) AS 'CaO [%]',
max(CASE WHEN m.parameter = 'Na2O [%]' then m.value END) AS 'Na2O [%]',
max(CASE WHEN m.parameter = 'K2O [%]' then m.value END) AS 'K2O [%]',
max(CASE WHEN m.parameter = 'P2O5 [%]' then m.value END) AS 'P2O5 [%]',
max(CASE WHEN m.parameter = 'Li [ppm]' then m.value END) AS 'Li [ppm]',
max(CASE WHEN m.parameter = 'Sc [ppm]' then m.value END) AS 'Sc [ppm]',
max(CASE WHEN m.parameter = 'Ti [ppm]' then m.value END) AS 'Ti [ppm]',
max(CASE WHEN m.parameter = 'V [ppm]' then m.value END) AS 'V [ppm]',
max(CASE WHEN m.parameter = 'Cr [ppm]' then m.value END) AS 'Cr [ppm]',
max(CASE WHEN m.parameter = 'Co [ppm]' then m.value END) AS 'Co [ppm]',
max(CASE WHEN m.parameter = 'Ni [ppm]' then m.value END) AS 'Ni [ppm]',
max(CASE WHEN m.parameter = 'Cu [ppm]' then m.value END) AS 'Cu [ppm]',
max(CASE WHEN m.parameter = 'Zn [ppm]' then m.value END) AS 'Zn [ppm]',
max(CASE WHEN m.parameter = 'As [ppm]' then m.value END) AS 'As [ppm]',
max(CASE WHEN m.parameter = 'Rb [ppm]' then m.value END) AS 'Rb [ppm]',
max(CASE WHEN m.parameter = 'Sr [ppm]' then m.value END) AS 'Sr [ppm]',
max(CASE WHEN m.parameter = 'Y [ppm]' then m.value END) AS 'Y [ppm]',
max(CASE WHEN m.parameter = 'Zr [ppm]' then m.value END) AS 'Zr [ppm]',
max(CASE WHEN m.parameter = 'Nb [ppm]' then m.value END) AS 'Nb [ppm]',
max(CASE WHEN m.parameter = 'Cd [ppm]' then m.value END) AS 'Cd [ppm]',
max(CASE WHEN m.parameter = 'Cs [ppm]' then m.value END) AS 'Cs [ppm]',
max(CASE WHEN m.parameter = 'Ba [ppm]' then m.value END) AS 'Ba [ppm]',
max(CASE WHEN m.parameter = 'La [ppm]' then m.value END) AS 'La [ppm]',
max(CASE WHEN m.parameter = 'Ce [ppm]' then m.value END) AS 'Ce [ppm]',
max(CASE WHEN m.parameter = 'Pr [ppm]' then m.value END) AS 'Pr [ppm]',
max(CASE WHEN m.parameter = 'Nd [ppm]' then m.value END) AS 'Nd [ppm]',
max(CASE WHEN m.parameter = 'Sm [ppm]' then m.value END) AS 'Sm [ppm]',
max(CASE WHEN m.parameter = 'Eu [ppm]' then m.value END) AS 'Eu [ppm]',
max(CASE WHEN m.parameter = 'Gd [ppm]' then m.value END) AS 'Gd [ppm]',
max(CASE WHEN m.parameter = 'Tb [ppm]' then m.value END) AS 'Tb [ppm]',
max(CASE WHEN m.parameter = 'Dy [ppm]' then m.value END) AS 'Dy [ppm]',
max(CASE WHEN m.parameter = 'Ho [ppm]' then m.value END) AS 'Ho [ppm]',
max(CASE WHEN m.parameter = 'Er [ppm]' then m.value END) AS 'Er [ppm]',
max(CASE WHEN m.parameter = 'Tm [ppm]' then m.value END) AS 'Tm [ppm]',
max(CASE WHEN m.parameter = 'Yb [ppm]' then m.value END) AS 'Yb [ppm]',
max(CASE WHEN m.parameter = 'Lu [ppm]' then m.value END) AS 'Lu [ppm]',
max(CASE WHEN m.parameter = 'Hf [ppm]' then m.value END) AS 'Hf [ppm]',
max(CASE WHEN m.parameter = 'Ta [ppm]' then m.value END) AS 'Ta [ppm]',
max(CASE WHEN m.parameter = 'Tl [ppm]' then m.value END) AS 'Tl [ppm]',
max(CASE WHEN m.parameter = 'Pb [ppm]' then m.value END) AS 'Pb [ppm]',
max(CASE WHEN m.parameter = 'Th [ppm]' then m.value END) AS 'Th [ppm]',
max(CASE WHEN m.parameter = 'U [ppm]' then m.value END) AS 'U [ppm]',
max(CASE WHEN m.parameter='Nd143_Nd144' then m.value END) AS 'Nd143_Nd144',
max(CASE WHEN m.parameter='Sr87_Sr86' then m.value END) AS 'Sr87_Sr86',
max(CASE WHEN m.parameter='Pb206_Pb204' then m.value END) AS 'Pb206_Pb204',
max(CASE WHEN m.parameter='Pb207_Pb204' then m.value END) AS 'Pb207_Pb204',
max(CASE WHEN m.parameter='Pb208_Pb204' then m.value END) AS 'Pb208_Pb204'
FROM 'samples.csv' AS s JOIN 'measurements.csv' AS m ON s.id = m.sample_id
WHERE (m.sample_id = 'best1992_TAU48' OR m.sample_id = 'best1992_TAU49' OR
m.sample_id = 'best1992_TAU52' OR m.sample_id = 'best1992_TAU54' OR
m.sample_id = 'best1992_TOK-A2' OR m.sample_id = 'best1992_COOK-1' OR
m.sample_id = 'best1992_BB-8-4-4(A)' OR m.sample_id = 'best1992_TOK-A2' OR
m.sample_id = 'allen1997_5' OR m.sample_id = 'allen1997_6' OR
m.sample_id = 'weisler2016a_2009-369' OR m.sample_id = 'clark2014_36' OR
m.sample_id = 'clark2014_51' OR m.sample_id = 'clark2014_53' OR
m.sample_id = 'clark2014_54' OR m.sample_id = 'clark2014_241' OR
m.sample_id = 'clark2014_678' OR m.sample_id = 'clark2014_683' OR
m.sample_id = 'clark2014_707') AND
m.parameter IN ('SiO2 [%]', 'TiO2 [%]', 'Al2O3 [%]', 'Fe2O3 [%]',
'FeO [%]','MnO [%]', 'MgO [%]', 'CaO [%]', 'Na2O [%]', 'K2O [%]', 'P2O5 [%]',
'Li [ppm]', 'Sc [ppm]', 'Ti [ppm]', 'V [ppm]',
'Cr [ppm]', 'Co [ppm]', 'Ni [ppm]', 'Cu [ppm]', 'Zn [ppm]', 'As [ppm]',
'Rb [ppm]', 'Sr [ppm]', 'Y [ppm]', 'Zr [ppm]', 'Nb [ppm]', 'Cd [ppm]',
'Cs [ppm]', 'Ba [ppm]', 'La [ppm]', 'Ce [ppm]', 'Pr [ppm]', 'Nd [ppm]',
'Sm [ppm]', 'Eu [ppm]', 'Gd [ppm]', 'Tb [ppm]', 'Dy [ppm]', 'Ho [ppm]',
'Er [ppm]', 'Tm [ppm]', 'Yb [ppm]', 'Lu [ppm]', 'Hf [ppm]', 'Ta [ppm]',
'Tl [ppm]', 'Pb [ppm]', 'Th [ppm]', 'U [ppm]', 'Nd143_Nd144', 'Sr87_Sr86',
'Pb206_Pb204', 'Pb207_Pb204', 'Pb208_Pb204') GROUP BY s.id, s.sample_name,
s.location_locality, substr(m.method_id, 1, instr(m.method_id, '_') - 1)") %>%
  rename_pofatu_elements() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>% Fe2O3_from_FeO() %>%
  group_by(Sample) %>% summarise_all(~na.omit(.)[1]) %>%
  add_column(Location = NA) %>%
  mutate_at(vars(
    one_of('Location')),
    funs(case_when(
      Sample == "allen1997_5" ~ "Moturakau (Cook Is.)",
      Sample == "allen1997_6" ~ "Moturakau (Cook Is.)",
      Sample == "best1992_BB-8-4-4(A)" ~ "San Cristobal (Solomon Is.)",
      Sample == "best1992_COOK-1" ~ "Ma'uke (Cook Is.)",
      Sample == "best1992_TAU48" ~ "Taumako (Solomon Is.)",
      Sample == "best1992_TAU49" ~ "Taumako (Solomon Is.)",
      Sample == "best1992_TAU52" ~ "Taumako (Solomon Is.)",
      Sample == "best1992_TAU54" ~ "Taumako (Solomon Is.)",
      Sample == "best1992_TOK-A2" ~ "Atafu (Tokelau Is.)",
      Sample == "weisler2016a_2009-369" ~ "Mangaia (Cook Is.)",
      Sample == "clark2014_36" ~ "Tongatapu (Tonga Is.)",
      Sample == "clark2014_51" ~ "Tongatapu (Tonga Is.)",
      Sample == "clark2014_53" ~ "Tongatapu (Tonga Is.)",
      Sample == "clark2014_54" ~ "Tongatapu (Tonga Is.)",
      Sample == "clark2014_241" ~ "Tongatapu (Tonga Is.)",
      Sample == "clark2014_678" ~ "Tongatapu (Tonga Is.)",
      Sample == "clark2014_683" ~ "Tongatapu (Tonga Is.)",
      Sample == "clark2014_707" ~ "Tongatapu (Tonga Is.)"))) %>%
  dplyr::filter(Sample %in% c(
    "allen1997_5","allen1997_6","best1992_BB-8-4-4(A)","best1992_COOK-1",
    "best1992_TOK-A2","clark2014_241","clark2014_36","clark2014_51",
    "clark2014_53","clark2014_54","clark2014_678","clark2014_683","clark2014_707",
    "weisler2016a_2009-369")) %>%
  dplyr::select(
    Sample,Location,lat,long,SiO2,TiO2,Al2O3,Fe2O3,MnO,MgO,CaO,Na2O,K2O,
    Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
    Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
    Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

#map
world <- map_data("world")
artefacts$long2 <- ifelse(artefacts$long < -25, artefacts$long + 360, artefacts$long)
#c3_q7$long2 <- ifelse(c3_q7$long < -25, c3_q7$long + 360, c3_q7$long)
world$long2 <- ifelse(world$long < -25, world$long + 360, world$long)

shapes <- c(
  "Tatagamatau-source"=20,"Moturakau (Cook Is.)"=20,
  "San Cristobal (Solomon Is.)"=20,"Ma'uke (Cook Is.)"=20,
  "Taumako (Solomon Is.)"=20,"Atafu (Tokelau Is.)"=20,
  "Mangaia (Cook Is.)"=20,"Tongatapu (Tonga Is.)"=20)
cols <- c(
  "Tatagamatau-source"="#4A64A5","Moturakau (Cook Is.)"="red",
  "San Cristobal (Solomon Is.)"="red","Ma'uke (Cook Is.)"="red",
  "Taumako (Solomon Is.)"="red","Atafu (Tokelau Is.)"="red",
  "Mangaia (Cook Is.)"="red","Tongatapu (Tonga Is.)"="red")

library(ggrepel)
m <- ggplot(world, aes(long2,lat, group=group)) +
  geom_polygon(color="black", fill="white", size=.25) +
  coord_map() +
  scale_x_continuous(expand = c(0, 0), limits = c(145, 275))+
  scale_y_continuous(expand = c(0, 0), limits = c(-50, 25))
map_tutuila_artefacts <- m + geom_point(data=artefacts,mapping=aes(
  x=long2, y=lat, shape=Location, color=Location,group=Location), size=3) +
  geom_text_repel(
    data=artefacts, mapping=aes(label = rownames(artefacts), group=Location), size=3,
    nudge_x = .15, box.padding = 0.5, nudge_y = 1, segment.curvature = -0.1,
    segment.ncp = 3, segment.angle = 20) +
  annotate("text", label = "6-13", x = 181, y = -24, size=3,
           colour = "black", hjust = 0, lineheight = .5) +
  scale_shape_manual(values=shapes) + scale_color_manual(values=cols) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), axis.text = element_blank(),
        legend.title = element_blank(), legend.text = element_text(size = 28),
        legend.position = "none") +
  annotate("text", -Inf, Inf, label = "D", size = 7, hjust = -1, vjust = 2) +
  annotate("text", label = "
  Moturakau (Cook Is.)\n
  1. allen1997_5\n
  2. allen1997_6\n
   \n
  San Cristobal (Solomon Is.)\n
  3. best1992_BB-8-4-4(A)\n
   \n
  Ma'uke (Cook Is.)\n
  4. best1992_COOK-1\n
   \n
  Atafu (Tokelau Is.)\n
  5. best1992_TOK-A2\n
   \n
  Tongatapu (Tonga Is.)\n
  6. clark2014_241\n
  7. clark2014_36\n
  8. clark2014_51\n
  9. clark2014_53\n
  10. clark2014_54\n
  11. clark2014_678\n
  12. clark2014_683\n
  13. clark2014_707\n
   \n
  Mangaia (Cook Is.)\n
  14. weisler2016a_2009-369",
  x = 230, y = -15, size=2.6, colour = "black", hjust = 0, lineheight = .5)
map_tutuila_artefacts

cols <- c("Tatagamatau source"="#781B6C","Tatagamatau artefact"="#781B6C",
          "Malaeloa source"="#BB3654","Malaeloa artefact"="#BB3654",
          "Maloata source"="#F4DD53","Maloata artefact"="#F4DD53",
          "Other Tutuila sources"="gray60","Moturakau (Cook Is.)"="red",
          "San Cristobal (Solomon Is.)"="red","Ma'uke (Cook Is.)"="red",
          "Atafu (Tokelau Is.)"="red","Tongatapu (Tonga Is.)"="red")
shapes <- c("Tatagamatau source"=24,"Tatagamatau artefact"=2,"Malaeloa source"=25,
            "Malaeloa artefact"=6,"Maloata source"=23,"Maloata artefact"=5,
            "Other Tutuila sources"=3,"Moturakau (Cook Is.)"=22,
            "San Cristobal (Solomon Is.)"=23,"Ma'uke (Cook Is.)"=24,
            "Atafu (Tokelau Is.)"=25,"Tongatapu (Tonga Is.)"=21)
contour <- c("Tatagamatau source"="black","Tatagamatau artefact"="#781B6C",
             "Malaeloa source"="black","Malaeloa artefact"="#BB3654",
             "Maloata source"="black","Maloata artefact"="#F4DD53",
             "Other Tutuila sources"="gray60","Moturakau (Cook Is.)"="#000000",
             "San Cristobal (Solomon Is.)"="#000000","Ma'uke (Cook Is.)"="#000000",
             "Atafu (Tokelau Is.)"="#000000","Tongatapu (Tonga Is.)"="#000000")


TAS <- OIB %>% ggplot(aes(x=SiO2,y=Na2O+K2O, shape=factor(Location),
                          fill=factor(Location), color=factor(Location))) +
  geom_point(stroke=.25, size=2.5) + geom_point(data=artefacts, stroke=.5, size=2.5) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(44.8,52)) +
  scale_y_continuous(limits=c(3.5,6), breaks=c(4,5,6)) +
  labs(y=expression(Na[2]*O + K[2]*O*~ "(wt%)"), x=expression(SiO[2]*~ "(wt%)"))+
  #annotate("segment", x=45, xend=51, y=5, yend=5, size = 0.3, linetype = "dashed", colour = "black")+
  #annotate("text", label = "Trachybasalt", x=46.4, y=5.5, size=3, colour="black")+
  #annotate("text", label = "Alkali basalt", x=48, y=4.4, size=3, colour="black")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.title = element_blank(),
        legend.background=element_blank(), legend.key=element_blank(),
        legend.key.size = unit(.35, "cm"), legend.text = element_text(size = 8),
        legend.position = c(.8,.25), aspect.ratio=1) +
  guides(color = guide_legend(override.aes = list(size = 1.5))) +
  annotate("text", -Inf, Inf, label = "E", size = 7, hjust = -1, vjust = 2)
TAS

pdf(here("analysis","work-in-progress","OIB-210722","E_T_SI4_1.pdf"),
    width=12, height=5)
map_tutuila_artefacts | TAS
dev.off()


p1 <- OIB %>% ggplot(aes(x=SiO2/MgO, y=Rb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =2.5) + geom_point(data=artefacts, size=2.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(position = "top", limits=c(6.5, 12.5)) +
  scale_y_continuous(limits=c(22, 53)) +
  labs(x=expression(SiO[2] / MgO)) + labs(y=expression("Rb (ppm)")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "none", aspect.ratio=1) +
  annotate("text", -Inf, Inf, label = "F", size = 7, hjust = -1, vjust = 2)
p2 <- OIB %>% ggplot(aes(x=SiO2/MgO, y=Sr, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =2.5) + geom_point(data=artefacts, size=2.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(6.5, 12.5)) +
  scale_y_continuous(limits=c(450, 850)) +
  labs(x=expression(SiO[2] / MgO)) + labs(y=expression("Sr (ppm)")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), legend.position = "none", aspect.ratio=1)
p3 <- OIB %>% ggplot(aes(x=SiO2/MgO, y=Zr, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =2.5) + geom_point(data=artefacts, size=2.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(6.5, 12.5)) +
  scale_y_continuous(limits = c(280, 480))+
  labs(x=expression(SiO[2] / MgO)) + labs(y=expression("Zr (ppm)")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)

p4 <- OIB %>% ggplot(aes(x=Al2O3, y=Rb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =2.5) + geom_point(data=artefacts, size=2.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  labs(x=expression(Al[2]*O[3]), y=expression("Rb (ppm)")) +
  scale_x_continuous(position = "top", limits=c(13.5, 17.5)) +
  scale_y_continuous(limits=c(22, 53)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), legend.position = "none", aspect.ratio=1)
p5 <- OIB %>% ggplot(aes(x=Al2O3, y=Sr, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =2.5) + geom_point(data=artefacts, size=2.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  labs(x=expression(Al[2]*O[3]), y=expression("Sr (ppm)")) +
  scale_x_continuous(limits = c(13.5, 17.5))+
  scale_y_continuous(limits=c(450, 850)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = "none", aspect.ratio=1)
p6 <- OIB %>% ggplot(aes(x=Al2O3, y=Zr, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =2.5) + geom_point(data=artefacts, size=2.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(13.5, 17.5)) +
  scale_y_continuous(limits = c(280, 480))+
  labs(x=expression(Al[2]*O[3]), y=expression("Zr (ppm)")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), legend.position = "none", aspect.ratio=1)

p7 <- OIB %>% ggplot(aes(x=CaO/Na2O, y=Rb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =2.5) + geom_point(data=artefacts, size=2.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  labs(x=expression(CaO / Na[2]*O), y=expression("Rb (ppm)")) +
  scale_x_continuous(position = "top", limits=c(1.7, 3.3)) +
  scale_y_continuous(position = "right", limits=c(22, 53)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)
p8 <- OIB %>% ggplot(aes(x=CaO/Na2O, y=Sr, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =2.5) + geom_point(data=artefacts, size=2.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  labs(x=expression(CaO / Na[2]*O), y=expression("Sr (ppm)")) +
  scale_x_continuous(limits = c(1.7, 3.3))+
  scale_y_continuous(position = "right", limits=c(450, 850)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),legend.position = "none", aspect.ratio=1)
p9 <- OIB %>% ggplot(aes(x=CaO/Na2O, y=Zr, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =2.5) + geom_point(data=artefacts, size=2.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(1.7, 3.3)) +
  scale_y_continuous(position = "right", limits = c(280, 480))+
  labs(x=expression(CaO / Na[2]*O), y=expression("Zr (ppm)")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)
((p1/p2/p3) | (p4/p5/p6) / (p7/p8/p9))

pdf(here("analysis","work-in-progress","OIB-210722","E_T_SI4_2.pdf"),
    width=12, height=12)
((p1/p2/p3) | (p4/p5/p6) | (p7/p8/p9)) +
  plot_layout(guides = "collect") & theme(legend.position = "none")
dev.off()

p1 <- OIB %>% ggplot(aes(x=SiO2/MgO, y=Rb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =2.5) + geom_point(data=artefacts, size=2.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(6.5, 12.5)) +
  scale_y_continuous(limits=c(22, 53)) +
  labs(x=expression(SiO[2] / MgO)) + labs(y=expression("Rb (ppm)")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "none", aspect.ratio=1) +
  annotate("text", -Inf, Inf, label = "F", size = 7, hjust = -1, vjust = 2)
p2 <- OIB %>% ggplot(aes(x=Al2O3, y=Rb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =2.5) + geom_point(data=artefacts, size=2.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  labs(x=expression(Al[2]*O[3]), y=expression("Rb (ppm)")) +
  scale_x_continuous(limits=c(13.5, 17.5)) +
  scale_y_continuous(limits=c(22, 53)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), legend.position = "none", aspect.ratio=1)
p3 <- OIB %>% ggplot(aes(x=CaO/Na2O, y=Rb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =2.5) + geom_point(data=artefacts, size=2.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  labs(x=expression(CaO / Na[2]*O), y=expression("Rb (ppm)")) +
  scale_x_continuous(limits=c(1.7, 3.3)) +
  scale_y_continuous(position = "right", limits=c(22, 53)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)
pdf(here("analysis","work-in-progress","OIB-210722","E_T_SI4.pdf"),
    width=12, height=8)
((map_tutuila_artefacts | TAS)/(p1 | p2 | p3)) +
  plot_layout(guides = "collect") & theme(legend.position = "none")
dev.off()

#### E_T Multielements ####
#"T-12-06"=21,"T-12-07"=22,"T-12-08"=23,"T-12-09"=24,"T-12-10"=25
s_spider_11_08 <- joined_data %>%
  filter(Sample %in% c("E-11-08")) %>%
  mutate(Location = case_when(grepl("E-11-08", Sample) ~ "E-11-08")) %>%
  normalize_to_pm()
s_spider_12_06 <- joined_data %>%
  filter(Sample %in% c("T-12-06")) %>%
  mutate(Location = case_when(grepl("T-12-06", Sample) ~ "T-12-06")) %>%
  normalize_to_pm()
s_spider_12_07 <- joined_data %>%
  filter(Sample %in% c("T-12-07")) %>%
  mutate(Location = case_when(grepl("T-12-07", Sample) ~ "T-12-07")) %>%
  normalize_to_pm()
s_spider_12_08 <- joined_data %>%
  filter(Sample %in% c("T-12-08")) %>%
  mutate(Location = case_when(grepl("T-12-08", Sample) ~ "T-12-08")) %>%
  normalize_to_pm()
s_spider_12_09 <- joined_data %>%
  filter(Sample %in% c("T-12-09")) %>%
  mutate(Location = case_when(grepl("T-12-09", Sample) ~ "T-12-09")) %>%
  normalize_to_pm()
s_spider_12_10 <- joined_data %>%
  filter(Sample %in% c("T-12-10")) %>%
  mutate(Location = case_when(grepl("T-12-10", Sample) ~ "T-12-10")) %>%
  normalize_to_pm()

OIB <- dbGetQuery(pofatu,
                   "SELECT s.id AS sample_id, s.sample_category, s.location_region,
s.location_subregion, s.site_name, s.location_latitude, s.location_longitude,
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
WHERE s.sample_category = 'SOURCE' AND s.site_name = 'Tatagamatau' AND
m.parameter IN ('SiO2 [%]', 'TiO2 [%]', 'Al2O3 [%]', 'Fe2O3 [%]','FeO [%]',
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
  rename(Island=location_subregion) %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  mutate(Location = case_when(grepl("Tatagamatau", site_name) ~ "Tatagamatau")) %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

#### E-11-08
E_T[1,] %>% dplyr::select(
  "Sample","SiO2 min","SiO2 max","K2O min","K2O max","Na2O min","Na2O max",
  "Cs min","Cs max","Rb min","Rb max","Ba min","Ba max","Th min","Th max",
  "Nb min","Nb max","Ce min","Ce max","Nd min","Nd max","Sr min","Sr max",
  "Sm min","Sm max","Zr min","Zr max","Ti min","Ti max","Yb min","Yb max")

OIB_spider <- OIB %>% dplyr::filter(
    K2O > 0 & K2O < 2.9 & Na2O > 2.08 & Na2O < 5.08 &
      Cs > 0.11 & Cs < 0.442 & Rb > 18.3 & Rb < 54.9 &
      Ba > 160 & Ba < 480 & Th > 1.94 & Th < 5.84 &
      Nb > 24.8 & Nb < 74.6 & Ce > 43.5 & Ce < 131 &
      Nd > 25.5 & Nd < 76.5 & Sr > 352 & Sr < 1056 &
      Sm > 6.2 & Sm < 18.6 & Zr > 202 & Zr < 606 &
      Ti > 10369 & Ti < 31106 & Yb > 1.6 & Yb < 4.8)
OIB_spider %>% group_by(Sample) %>% tally()

OIB_spider <- OIB_spider %>% normalize_to_pm()

shapes <- c("collerson2007_KC-05-14"=3, "collerson2007_KC-05-19"=1, "E-11-08"=8)
cols <- c("collerson2007_KC-05-14"="#781B6C", "collerson2007_KC-05-19"="#781B6C", "E-11-08"="red")

E_T_spider1 <- OIB_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Pb","Nd","Sr","Sm","Zr","Hf","Ti","Eu","Gd","Tb",
                           "Dy","Ho","Y","Er","Li","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=Sample, color=Sample, fill=Sample,
             group=Sample)) +
  geom_line(size=1) +
  geom_point(size=3, stroke=.75) +
  geom_line(data=s_spider_11_08, size=1, color="red") +
  geom_point(data=s_spider_11_08, size=2, stroke=1) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.5),
        axis.ticks.length.x = unit(.2, "cm"), axis.text = element_text(size = 12),
        axis.title = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.title = element_blank(),legend.text = element_text(size = 10),
        legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(.4, "cm"),
        legend.position = c(.83,.78), legend.direction = "vertical") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), ) +
  scale_y_log10(breaks=c(1,10,100), limits=c(1,700), labels = scales::comma)+
  annotation_logticks(sides="l", size = .5, outside = TRUE, long = unit(0.1, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
E_T_spider1

#### T-12-06
E_T[2,] %>% dplyr::select(
  "Sample","SiO2 min","SiO2 max","K2O min","K2O max","Na2O min","Na2O max",
  "Cs min","Cs max","Rb min","Rb max","Ba min","Ba max","Th min","Th max",
  "Nb min","Nb max","Ce min","Ce max","Nd min","Nd max","Sr min","Sr max",
  "Sm min","Sm max","Zr min","Zr max","Ti min","Ti max","Yb min","Yb max")

OIB_spider <- OIB %>% dplyr::filter(
  K2O > 0 & K2O < 2.82 & Na2O > 2 & Na2O < 5 &
    Rb > 16.8 & Rb < 50.4 &
    Ba > 137.5 & Ba < 412.5 & Th > 1.86 & Th < 5.59 &
    Nb > 24.7 & Nb < 74.1 & Ce > 42.4 & Ce < 127.3 &
    Nd > 25.6 & Nd < 76.9 & Sr > 332 & Sr < 996 &
    Sm > 6.05 & Sm < 18.15 & Zr > 196.5 & Zr < 589.5 &
    Ti > 11747 & Ti < 35242 & Yb > 1.62 & Yb < 4.86)

OIB_spider %>% group_by(Sample) %>% tally()

OIB_spider <- OIB_spider %>% normalize_to_pm()

shapes <- c("collerson2007_KC-05-14"=3, "collerson2007_KC-05-18"=4,
            "collerson2007_KC-05-19"=1, "T-12-06"=21)
cols <- c("collerson2007_KC-05-14"="#781B6C", "collerson2007_KC-05-18"="#781B6C",
            "collerson2007_KC-05-19"="#781B6C", "T-12-06"="red")

E_T_spider2 <- OIB_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Pb","Nd","Sr","Sm","Zr","Hf","Ti","Eu","Gd","Tb",
                           "Dy","Ho","Y","Er","Li","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=Sample, color=Sample, fill=Sample,
             group=Sample)) +
  geom_line(size=1) +
  geom_point(size=3, stroke=.75) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  geom_line(data=s_spider_12_06, size=1, color="red") +
  geom_point(data=s_spider_12_06, size=3, stroke=.25, color="black") +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.5),
        axis.ticks.length.x = unit(.2, "cm"), axis.text = element_text(size = 12),
        axis.title = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.title = element_blank(),legend.text = element_text(size = 10),
        legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(.4, "cm"),
        legend.position = c(.83,.78), legend.direction = "vertical") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), ) +
  scale_y_log10(breaks=c(1,10,100), limits=c(1,700), labels = scales::comma)+
  annotation_logticks(sides="l", size = .5, outside = TRUE, long = unit(0.1, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
E_T_spider2

#### T-12-07
E_T[3,] %>% dplyr::select(
  "Sample","SiO2 min","SiO2 max","K2O min","K2O max","Na2O min","Na2O max",
  "Cs min","Cs max","Rb min","Rb max","Ba min","Ba max","Th min","Th max",
  "Nb min","Nb max","Ce min","Ce max","Nd min","Nd max","Sr min","Sr max",
  "Sm min","Sm max","Zr min","Zr max","Ti min","Ti max","Yb min","Yb max")

OIB_spider <- OIB %>% dplyr::filter(
  K2O > 0 & K2O < 2.78 & Na2O > 1.9 & Na2O < 4.9 & Rb > 15.35 & Rb < 46.05 &
    Ba > 126 & Ba < 378 & Th > 1.58 & Th < 4.74 &
    Nb > 21.05 & Nb < 63.15 & Ce > 38.05 & Ce < 114.15 &
    Nd > 23.7 & Nd < 71.1 & Sr > 294 & Sr < 882 &
    Sm > 5.65 & Sm < 16.95 & Zr > 182.5 & Zr < 547.5 &
    Ti > 12646 & Ti < 37939 & Yb > 1.56 & Yb < 4.68)

OIB_spider %>% group_by(Sample) %>% tally()

OIB_spider <- OIB_spider %>% normalize_to_pm()

shapes <- c("collerson2007_KC-05-14"=3, "collerson2007_KC-05-19"=1, "T-12-07"=22)
cols <- c("collerson2007_KC-05-14"="#781B6C",
          "collerson2007_KC-05-19"="#781B6C", "T-12-07"="red")

E_T_spider3 <- OIB_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Pb","Nd","Sr","Sm","Zr","Hf","Ti","Eu","Gd","Tb",
                           "Dy","Ho","Y","Er","Li","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=Sample, color=Sample, fill=Sample,
             group=Sample)) +
  geom_line(size=1) +
  geom_point(size=3, stroke=.75) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  geom_line(data=s_spider_12_07, size=1, color="red") +
  geom_point(data=s_spider_12_07, size=3, stroke=.25, color="black") +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.5),
        axis.ticks.length.x = unit(.2, "cm"), axis.text = element_text(size = 12),
        axis.title = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.title = element_blank(),legend.text = element_text(size = 10),
        legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(.4, "cm"),
        legend.position = c(.83,.8), legend.direction = "vertical") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), ) +
  scale_y_log10(breaks=c(1,10,100), limits=c(1,300), labels = scales::comma)+
  annotation_logticks(sides="l", size = .5, outside = TRUE, long = unit(0.1, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
E_T_spider3

#### T-12-08
E_T[4,] %>% dplyr::select(
  "Sample","SiO2 min","SiO2 max","K2O min","K2O max","Na2O min","Na2O max",
  "Cs min","Cs max","Rb min","Rb max","Ba min","Ba max","Th min","Th max",
  "Nb min","Nb max","Ce min","Ce max","Nd min","Nd max","Sr min","Sr max",
  "Sm min","Sm max","Zr min","Zr max","Ti min","Ti max","Yb min","Yb max")

OIB_spider <- OIB %>% dplyr::filter(
  K2O > 0 & K2O < 2.99 & Na2O > 2.18 & Na2O < 5.18 &
    Cs > 0.111 & Cs < 0.334 & Rb > 20.55 & Rb < 61.65 &
    Ba > 162.5 & Ba < 487.5 & Th > 2.13 & Th < 6.39 &
    Nb > 24.3 & Nb < 72.9 & Ce > 46.9 & Ce < 140.7 &
    Nd > 26.85 & Nd < 80.55 & Sr > 365 & Sr < 1095 &
    Sm > 6.35 & Sm < 19.05 & Zr > 190.5 & Zr < 571.5 &
    Ti > 10399 & Ti < 31196 & Yb > 1.48 & Yb < 4.45)

OIB_spider %>% group_by(Sample) %>% tally()

OIB_spider <- OIB_spider %>% normalize_to_pm()

shapes <- c("collerson2007_KC-05-19"=1, "T-12-08"=23)
cols <- c("collerson2007_KC-05-19"="#781B6C","T-12-08"="red")

E_T_spider4 <- OIB_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Pb","Nd","Sr","Sm","Zr","Hf","Ti","Eu","Gd","Tb",
                           "Dy","Ho","Y","Er","Li","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=Sample, color=Sample, fill=Sample,
             group=Sample)) +
  geom_line(size=1) +
  geom_point(size=3, stroke=.75) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  geom_line(data=s_spider_12_08, size=1, color="red") +
  geom_point(data=s_spider_12_08, size=3, stroke=.25, color="black") +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.5),
        axis.ticks.length.x = unit(.2, "cm"), axis.text = element_text(size = 12),
        axis.title = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.title = element_blank(),legend.text = element_text(size = 10),
        legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(.4, "cm"),
        legend.position = c(.83,.8), legend.direction = "vertical") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), ) +
  scale_y_log10(breaks=c(1,10,100), limits=c(1,300), labels = scales::comma)+
  annotation_logticks(sides="l", size = .5, outside = TRUE, long = unit(0.1, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
E_T_spider4

#### T-12-09
E_T[5,] %>% dplyr::select(
  "Sample","SiO2 min","SiO2 max","K2O min","K2O max","Na2O min","Na2O max",
  "Cs min","Cs max","Rb min","Rb max","Ba min","Ba max","Th min","Th max",
  "Nb min","Nb max","Ce min","Ce max","Nd min","Nd max","Sr min","Sr max",
  "Sm min","Sm max","Zr min","Zr max","Ti min","Ti max","Yb min","Yb max")

OIB_spider <- OIB %>% dplyr::filter(
  K2O > 0 & K2O < 2.72 & Na2O > 1.8 & Na2O < 4.8 &
    Cs > 0.0904 & Cs < 0.3616 & Rb > 15.65 & Rb < 46.95 &
    Ba > 128.5 & Ba < 385.5 & Th > 1.58 & Th < 4.74 &
    Nb > 20.4 & Nb < 61.2 & Ce > 36.9 & Ce < 110.7 &
    Nd > 21.7 & Nd < 65.1 & Sr > 296.5 & Sr < 889.5 &
    Sm > 5.35 & Sm < 16.05 & Zr > 173.5 & Zr < 520.5 &
    Ti > 13665 & Ti < 40995 & Yb > 1.39 & Yb < 4.18)

OIB_spider %>% group_by(Sample) %>% tally()

OIB_spider <- OIB_spider %>% normalize_to_pm()

shapes <- c("collerson2007_KC-05-19"=1, "T-12-09"=24)
cols <- c("collerson2007_KC-05-19"="#781B6C", "T-12-09"="red")

E_T_spider5 <- OIB_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Pb","Nd","Sr","Sm","Zr","Hf","Ti","Eu","Gd","Tb",
                           "Dy","Ho","Y","Er","Li","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=Sample, color=Sample, fill=Sample,
             group=Sample)) +
  geom_line(size=1) +
  geom_point(size=3, stroke=.75) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  geom_line(data=s_spider_12_09, size=1, color="red") +
  geom_point(data=s_spider_12_09, size=3, stroke=.25, color="black") +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.5),
        axis.ticks.length.x = unit(.2, "cm"), axis.text = element_text(size = 12),
        axis.title = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.title = element_blank(),legend.text = element_text(size = 10),
        legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(.4, "cm"),
        legend.position = c(.83,.8), legend.direction = "vertical") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), ) +
  scale_y_log10(breaks=c(1,10,100), limits=c(1,300), labels = scales::comma)+
  annotation_logticks(sides="l", size = .5, outside = TRUE, long = unit(0.1, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
E_T_spider5


#### T-12-10
E_T[6,] %>% dplyr::select(
  "Sample","SiO2 min","SiO2 max","K2O min","K2O max","Na2O min","Na2O max",
  "Cs min","Cs max","Rb min","Rb max","Ba min","Ba max","Th min","Th max",
  "Nb min","Nb max","Ce min","Ce max","Nd min","Nd max","Sr min","Sr max",
  "Sm min","Sm max","Zr min","Zr max","Ti min","Ti max","Yb min","Yb max")

OIB_spider <- OIB %>% dplyr::filter(
  K2O > 0 & K2O < 2.85 & Na2O > 2.08 & Na2O < 5.08 &
    Cs > 0.1596 & Cs < 0.6384 & Rb > 18.2 & Rb < 54.6 &
    Ba > 143.5 & Ba < 430.5 & Th > 1.84 & Th < 5.52 &
    Nb > 23.5 & Nb < 70.5 & Ce > 44.75 & Ce < 134.25 &
    Nd > 27.35 & Nd < 82.05 & Sr > 370 & Sr < 1110 &
    Sm > 6.65 & Sm < 19.95 & Zr > 200 & Zr < 601 &
    Ti > 12586 & Ti < 37759 & Yb > 1.57 & Yb < 4.71)

OIB_spider %>% group_by(Sample) %>% tally()

OIB_spider <- OIB_spider %>% normalize_to_pm()

shapes <- c("collerson2007_KC-05-14"=3, "collerson2007_KC-05-18"=4,
            "collerson2007_KC-05-19"=1, "T-12-10"=25)
cols <- c("collerson2007_KC-05-14"="#781B6C", "collerson2007_KC-05-18"="#781B6C",
          "collerson2007_KC-05-19"="#781B6C", "T-12-10"="red")

E_T_spider6 <- OIB_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Pb","Nd","Sr","Sm","Zr","Hf","Ti","Eu","Gd","Tb",
                           "Dy","Ho","Y","Er","Li","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=Sample, color=Sample, fill=Sample,
             group=Sample)) +
  geom_line(size=1) +
  geom_point(size=3, stroke=.75) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  geom_line(data=s_spider_12_10, size=1, color="red") +
  geom_point(data=s_spider_12_10, size=3, stroke=.25, color="black") +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.5),
        axis.ticks.length.x = unit(.2, "cm"), axis.text = element_text(size = 12),
        axis.title = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.title = element_blank(),legend.text = element_text(size = 10),
        legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(.4, "cm"),
        legend.position = c(.83,.78), legend.direction = "vertical") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), ) +
  scale_y_log10(breaks=c(1,10,100), limits=c(1,700), labels = scales::comma)+
  annotation_logticks(sides="l", size = .5, outside = TRUE, long = unit(0.1, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
E_T_spider6

pdf(here("analysis","work-in-progress","OIB-210722","E_T_spiders.pdf"),
    width=15, height=8)
(E_T_spider1/E_T_spider2/E_T_spider3)|(E_T_spider4/E_T_spider5/E_T_spider6)
dev.off()


#### Kapingamarangi ####
Kapinga <- joined_data_ranges[16:18,]

#### K1224 PCA 1 ####
Kapinga[1,36:39] %>% round(digits = 6)
Kapinga[1,40:45] %>% round(digits = 3)

OIB <- dbGetQuery(georoc,
                  "SELECT *
                  FROM 'sample'
                  WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
                  TECTONIC_SETTING='OCEAN ISLAND' AND
                  (ND143_ND144 > 0.512477 AND ND143_ND144 < 0.513503 AND
                  SR87_SR86 > 0.702761 AND SR87_SR86 < 0.704167 AND
                  PB206_PB204 > 18.221 AND PB206_PB204 < 18.589 AND
                  PB207_PB204 > 15.372 AND PB207_PB204 < 15.682 AND
                  PB208_PB204 > 37.927 AND PB208_PB204 < 38.693)") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

OIB_pca <- OIB %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)

samples <- joined_data %>%
  filter(Sample %in% c("K-12-24")) %>%
  mutate(Location = case_when(grepl("K-12-24", Sample) ~ "K-12-24")) %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)

is.na(OIB_pca) <- sapply(OIB_pca, is.infinite) #replace Inf by NA
OIB_pca[OIB_pca == 0] <- NA # Replace 0 with NA
OIB_pca <- OIB_pca[rowSums(is.na(OIB_pca)) == 0,] # removes rows with missing info for PCA

is.na(samples) <- sapply(samples, is.infinite) #replace Inf by NA
samples[samples == 0] <- NA # Replace 0 with NA
samples <- samples[rowSums(is.na(samples)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB_pca[,3:7],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
fviz_eig(res.pca)
eig <- get_eig(res.pca)
K_1224_1 <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB_pca$Location, fill.ind = OIB_pca$Location,
  pointsize = 3, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-5, 4)) + scale_y_continuous(limits=c(-3.5,2.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_1224_1
pdf(here("analysis","work-in-progress","OIB-210722","K_1224-PCA1-a.pdf"),
    width=3.5, height=3.5)
K_1224_1
dev.off()

res.pca.df <- cbind(OIB_pca[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, samples[,3:7])
pred <- cbind(samples[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

K_1224_1 <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-5, 4)) + scale_y_continuous(limits=c(-3.5,2.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_1224_1
pdf(here("analysis","work-in-progress","OIB-210722","K_1224-PCA1-b.pdf"),
    width=3.5, height=3.5)
K_1224_1
dev.off()

#### K1224 PCA 2 ####
Kapinga[1,]
Kapinga[1,2:35] %>% round(digits = 6)
Kapinga[1,40:45] %>% round(digits = 3)

OIB <- dbGetQuery(georoc,
                  "SELECT *
                   FROM 'sample'
                   WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
                   TECTONIC_SETTING='OCEAN ISLAND' AND
                  (`BA(PPM)` > 244 AND `BA(PPM)` < 733 AND
                  `CE(PPM)` > 75 AND `CE(PPM)` < 225 AND
                  `ND(PPM)` > 37.6 AND `ND(PPM)` < 113 AND
                  `SR(PPM)` > 452 AND `SR(PPM)` < 1357 AND
                  `NB(PPM)` > 46.4 AND `NB(PPM)` < 139.3 AND
                  `SM(PPM)` > 7.65 AND `SM(PPM)` < 22.95 AND
                  `ZR(PPM)` > 239 AND `ZR(PPM)` < 717 AND
                  `YB(PPM)` > 1.26 AND `YB(PPM)` < 3.78) AND
                  (file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part1.csv' OR
                  file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part2.csv' OR
                  file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv')") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

OIB_pca <- OIB %>%
  dplyr::select(Sample,Location,
                TiO2,Al2O3,MgO,CaO,Na2O,K2O,
                Cs,Rb,Ba,Th,U,Nb,Ta,La,Nd,Sr,Sm,Zr,Yb)

samples <- joined_data %>%
  filter(Sample %in% c(
    "K-12-24"#,
    #"K-12-25",
    #"K-12-26"
    )) %>%
  mutate(Location = case_when(
    grepl("K-12-24", Sample) ~ "K-12-24"#,
    #grepl("K-12-25", Sample) ~ "K-12-25",
    #grepl("K-12-26", Sample) ~ "K-12-26"
    )) %>%
  dplyr::select(Sample,Location,
                TiO2,Al2O3,MgO,CaO,Na2O,K2O,
                Cs,Rb,Ba,Th,U,Nb,Ta,La,Nd,Sr,Sm,Zr,Yb)

is.na(OIB_pca) <- sapply(OIB_pca, is.infinite) #replace Inf by NA
OIB_pca[OIB_pca == 0] <- NA # Replace 0 with NA
OIB_pca <- OIB_pca[rowSums(is.na(OIB_pca)) == 0,] # removes rows with missing info for PCA

is.na(samples) <- sapply(samples, is.infinite) #replace Inf by NA
samples[samples == 0] <- NA # Replace 0 with NA
samples <- samples[rowSums(is.na(samples)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB_pca[,3:21],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
fviz_eig(res.pca)
eig <- get_eig(res.pca)
K_1224_2 <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB_pca$Location, fill.ind = OIB_pca$Location,
  pointsize = 3, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-6, 6.5)) + scale_y_continuous(limits=c(-4,5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_1224_2
pdf(here("analysis","work-in-progress","OIB-210722","K_1224-PCA2-a.pdf"),
    width=3.5, height=3.5)
K_1224_2
dev.off()

res.pca.df <- cbind(OIB_pca[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, samples[,3:21])
pred <- cbind(samples[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

K_1224_2 <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-6, 6.5)) + scale_y_continuous(limits=c(-4,5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_1224_2
pdf(here("analysis","work-in-progress","OIB-210722","K_1224-PCA2-b.pdf"),
    width=3.5, height=3.5)
K_1224_2
dev.off()


#### K1225 PCA 1 ####
Kapinga[2,36:39] %>% round(digits = 6)
Kapinga[2,40:45] %>% round(digits = 3)
OIB <- dbGetQuery(georoc,
                  "SELECT *
                  FROM 'sample'
                  WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
                  TECTONIC_SETTING='OCEAN ISLAND' AND
                  (ND143_ND144 > 0.512396 AND ND143_ND144 < 0.513422 AND
                  SR87_SR86 > 0.703423 AND SR87_SR86 < 0.704831 AND
                  PB206_PB204 > 18.438 AND PB206_PB204 < 18.81 AND
                  PB207_PB204 > 15.427 AND PB207_PB204 < 15.739 AND
                  PB208_PB204 > 38.274 AND PB208_PB204 < 39.048)") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB %>% group_by(Location) %>% tally()

OIB <- dbGetQuery(georoc,
                  "SELECT *
                  FROM 'sample'
                  WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
                  TECTONIC_SETTING='OCEAN ISLAND' AND
                  (ND143_ND144 > 0.512396 AND ND143_ND144 < 0.513422 AND
                  PB206_PB204 > 18.438 AND PB206_PB204 < 18.81 AND
                  PB207_PB204 > 15.427 AND PB207_PB204 < 15.739 AND
                  PB208_PB204 > 38.274 AND PB208_PB204 < 39.048)") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB %>% group_by(Location) %>% tally()

OIB_pca <- OIB %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)

samples <- joined_data %>%
  filter(Sample %in% c("K-12-25")) %>%
  mutate(Location = case_when(grepl("K-12-25", Sample) ~ "K-12-25")) %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)

is.na(OIB_pca) <- sapply(OIB_pca, is.infinite) #replace Inf by NA
OIB_pca[OIB_pca == 0] <- NA # Replace 0 with NA
OIB_pca <- OIB_pca[rowSums(is.na(OIB_pca)) == 0,] # removes rows with missing info for PCA

is.na(samples) <- sapply(samples, is.infinite) #replace Inf by NA
samples[samples == 0] <- NA # Replace 0 with NA
samples <- samples[rowSums(is.na(samples)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB_pca[,3:7],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
fviz_eig(res.pca)
eig <- get_eig(res.pca)
K_1225_1 <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB_pca$Location, fill.ind = OIB_pca$Location,
  pointsize = 3, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-4, 4)) + scale_y_continuous(limits=c(-2,2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_1225_1
pdf(here("analysis","work-in-progress","OIB-210722","K_1225-PCA1-a.pdf"),
    width=3.5, height=3.5)
K_1225_1
dev.off()

res.pca.df <- cbind(OIB_pca[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, samples[,3:7])
pred <- cbind(samples[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

K_1225_1 <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-4, 4)) + scale_y_continuous(limits=c(-2,2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_1225_1
pdf(here("analysis","work-in-progress","OIB-210722","K_1225-PCA1-b.pdf"),
    width=3.5, height=3.5)
K_1225_1
dev.off()

#### K1225 PCA 2 ####
Kapinga[2,2:35] %>% round(digits = 6)

OIB <- dbGetQuery(georoc,
                  "SELECT *
                   FROM 'sample'
                   WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
                   TECTONIC_SETTING='OCEAN ISLAND' AND
                  (`SIO2(WT%)` > 43.1 AND `SIO2(WT%)` < 46.1 AND
                  `SR(PPM)` > 191 AND `SR(PPM)` < 573 AND
                  `YB(PPM)` > 0.86 AND `YB(PPM)` < 2.58) AND
                  (file_id = '2022-06-WFJZKY_AUSTRAL-COOK_ISLANDS.csv' OR
                  file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv' OR
                  file_id = '2022-06-WFJZKY_SAMOAN_ISLANDS.csv')") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB %>% group_by(Location) %>% tally()

OIB_pca <- OIB %>%
  dplyr::select(Sample,Location,
                TiO2,Al2O3,MgO,CaO,Na2O,K2O,
                Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Nd,Sr,Sm,Zr,Yb)

samples <- joined_data %>%
  filter(Sample %in% c("K-12-25")) %>%
  mutate(Location = case_when(
    grepl("K-12-25", Sample) ~ "K-12-25")) %>%
  dplyr::select(Sample,Location,
                TiO2,Al2O3,MgO,CaO,Na2O,K2O,
                Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Nd,Sr,Sm,Zr,Yb)

is.na(OIB_pca) <- sapply(OIB_pca, is.infinite) #replace Inf by NA
OIB_pca[OIB_pca == 0] <- NA # Replace 0 with NA
OIB_pca <- OIB_pca[rowSums(is.na(OIB_pca)) == 0,] # removes rows with missing info for PCA

is.na(samples) <- sapply(samples, is.infinite) #replace Inf by NA
samples[samples == 0] <- NA # Replace 0 with NA
samples <- samples[rowSums(is.na(samples)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB_pca[,3:22], scale = TRUE, center = TRUE) # Dimension reduction using PCA
fviz_eig(res.pca)
eig <- get_eig(res.pca)
K_1225_2 <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB_pca$Location, fill.ind = OIB_pca$Location,
  pointsize = 3, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-8, 5.5)) + scale_y_continuous(limits=c(-3,3.3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_1225_2
pdf(here("analysis","work-in-progress","OIB-210722","K_1225-PCA2-a.pdf"),
    width=3.5, height=3.5)
K_1225_2
dev.off()

res.pca.df <- cbind(OIB_pca[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, samples[,3:22])
pred <- cbind(samples[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

K_1225_2 <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-8, 5.5)) + scale_y_continuous(limits=c(-3,3.3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_1225_2
pdf(here("analysis","work-in-progress","OIB-210722","K_1225-PCA2-b.pdf"),
    width=3.5, height=3.5)
K_1225_2
dev.off()


#### K1226 PCA 1 ####
Kapinga[3,36:39] %>% round(digits = 6)
Kapinga[3,40:45] %>% round(digits = 3)
OIB <- dbGetQuery(georoc,
                  "SELECT *
                  FROM 'sample'
                  WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
                  TECTONIC_SETTING='OCEAN ISLAND' AND
                  (ND143_ND144 > 0.51248 AND ND143_ND144 < 0.513506 AND
                  SR87_SR86 > 0.702897 AND SR87_SR86 < 0.704305 AND
                  PB206_PB204 > 18.54 AND PB206_PB204 < 18.914 AND
                  PB207_PB204 > 15.378 AND PB207_PB204 < 15.688 AND
                  PB208_PB204 > 38.059 AND PB208_PB204 < 38.827)") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB %>% group_by(Location) %>% tally()

OIB_pca <- OIB %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)

samples <- joined_data %>%
  filter(Sample %in% c("K-12-26")) %>%
  mutate(Location = case_when(grepl("K-12-26", Sample) ~ "K-12-26")) %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)

is.na(OIB_pca) <- sapply(OIB_pca, is.infinite) #replace Inf by NA
OIB_pca[OIB_pca == 0] <- NA # Replace 0 with NA
OIB_pca <- OIB_pca[rowSums(is.na(OIB_pca)) == 0,] # removes rows with missing info for PCA

is.na(samples) <- sapply(samples, is.infinite) #replace Inf by NA
samples[samples == 0] <- NA # Replace 0 with NA
samples <- samples[rowSums(is.na(samples)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB_pca[,3:7],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
fviz_eig(res.pca)
eig <- get_eig(res.pca)
K_1226_1 <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB_pca$Location, fill.ind = OIB_pca$Location,
  pointsize = 3, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-5, 3.5)) + scale_y_continuous(limits=c(-3,4)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_1226_1
pdf(here("analysis","work-in-progress","OIB-210722","K_1226-PCA1-a.pdf"),
    width=3.5, height=3.5)
K_1226_1
dev.off()

res.pca.df <- cbind(OIB_pca[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, samples[,3:7])
pred <- cbind(samples[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

K_1226_1 <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size =3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-5, 3.5)) + scale_y_continuous(limits=c(-3,4)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_1226_1
pdf(here("analysis","work-in-progress","OIB-210722","K_1226-PCA1-b.pdf"),
    width=3.5, height=3.5)
K_1226_1
dev.off()

#### K1226 PCA 2 ####
Kapinga[3,2:35] %>% round(digits = 6)
Kapinga[3,40:45] %>% round(digits = 3)

OIB <- dbGetQuery(georoc,
                  "SELECT *
                   FROM 'sample'
                   WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
                   TECTONIC_SETTING='OCEAN ISLAND' AND
                  (`SIO2(WT%)` > 42.9 AND `SIO2(WT%)` < 45.9 AND
                  `CE(PPM)` > 55.5 AND `CE(PPM)` < 166.5 AND
                  `PB(PPM)` > 1.71 AND `PB(PPM)` < 5.14 AND
                  `ND(PPM)` > 31.4 AND `ND(PPM)` < 94.2 AND
                  `SR(PPM)` > 331 AND `SR(PPM)` < 994 AND
                  `NB(PPM)` > 22.2 AND `NB(PPM)` < 66.6 AND
                  `SM(PPM)` > 6.7 AND `SM(PPM)` < 20.1 AND
                  `ZR(PPM)` > 127 AND `ZR(PPM)` < 382 AND
                  `YB(PPM)` > 1.26 AND `YB(PPM)` < 3.79) AND
                  (file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part1.csv' OR
                  file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part2.csv' OR
                  file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv')") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB %>% group_by(Location) %>% tally()

OIB_pca <- OIB %>%
  dplyr::select(Sample,Location,
                TiO2,Al2O3,Na2O,K2O,
                Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Nd,Sr,Sm,Zr,Hf,Eu)

samples <- joined_data %>%
  filter(Sample %in% c("K-12-26")) %>%
  mutate(Location = case_when(
    grepl("K-12-26", Sample) ~ "K-12-26")) %>%
  dplyr::select(Sample,Location,
                TiO2,Al2O3,Na2O,K2O,
                Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Nd,Sr,Sm,Zr,Hf,Eu)

is.na(OIB_pca) <- sapply(OIB_pca, is.infinite) #replace Inf by NA
OIB_pca[OIB_pca == 0] <- NA # Replace 0 with NA
OIB_pca <- OIB_pca[rowSums(is.na(OIB_pca)) == 0,] # removes rows with missing info for PCA

is.na(samples) <- sapply(samples, is.infinite) #replace Inf by NA
samples[samples == 0] <- NA # Replace 0 with NA
samples <- samples[rowSums(is.na(samples)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB_pca[,3:21], scale = TRUE, center = TRUE) # Dimension reduction using PCA
fviz_eig(res.pca)
eig <- get_eig(res.pca)
K_1226_2 <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB_pca$Location, fill.ind = OIB_pca$Location,
  pointsize = 3, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-7, 5)) + scale_y_continuous(limits=c(-4.5,5.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_1226_2
pdf(here("analysis","work-in-progress","OIB-210722","K_1226-PCA2-a.pdf"),
    width=3.5, height=3.5)
K_1226_2
dev.off()

res.pca.df <- cbind(OIB_pca[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, samples[,3:21])
pred <- cbind(samples[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

K_1226_2 <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-7, 5)) + scale_y_continuous(limits=c(-4.5,5.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_1226_2
pdf(here("analysis","work-in-progress","OIB-210722","K_1226-PCA2-b.pdf"),
    width=3.5, height=3.5)
K_1226_2
dev.off()


#### Kapinga SI1 ####
samples <- joined_data %>%
  filter(Sample %in% c("K-12-24",
                       "K-12-25",
                       "K-12-26")) %>%
  mutate(Location = case_when(grepl("K-12-24", Sample) ~ "K-12-24",
                              grepl("K-12-25", Sample) ~ "K-12-25",
                              grepl("K-12-26", Sample) ~ "K-12-26"))

Kapinga[2:33] %>% round(digits=2)

Kapinga[36:39] %>% round(digits=6)
Kapinga[40:45] %>% round(digits=3)

OIB <- dbGetQuery(georoc,
                  "SELECT *
                  FROM 'sample'
                  WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
                  TECTONIC_SETTING='OCEAN ISLAND' AND
                  ((ND143_ND144 > 0.512477 AND ND143_ND144 < 0.513503 AND
                  SR87_SR86 > 0.70276 AND SR87_SR86 < 0.704167 AND
                  PB206_PB204 > 18.221 AND PB206_PB204 < 18.589 AND
                  PB207_PB204 > 15.372 AND PB207_PB204 < 15.682 AND
                  PB208_PB204 > 37.927 AND PB208_PB204 < 38.693) OR
                  (ND143_ND144 > 0.512396 AND ND143_ND144 < 0.513422 AND
                  SR87_SR86 > 0.703423 AND SR87_SR86 < 0.704831 AND
                  PB206_PB204 > 18.438 AND PB206_PB204 < 18.81 AND
                  PB207_PB204 > 15.427 AND PB207_PB204 < 15.739 AND
                  PB208_PB204 > 38.274 AND PB208_PB204 < 39.048) OR
                  (ND143_ND144 > 0.51248 AND ND143_ND144 < 0.513506 AND
                  SR87_SR86 > 0.702897 AND SR87_SR86 < 0.704305 AND
                  PB206_PB204 > 18.540 AND PB206_PB204 < 18.914 AND
                  PB207_PB204 > 15.378 AND PB207_PB204 < 15.688 AND
                  PB208_PB204 > 38.059 AND PB208_PB204 < 38.827))") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
is.na(OIB) <- sapply(OIB, is.infinite) #replace Inf by NA

d_map <- OIB %>% dplyr::select(Sample,Location,lat,long)
d_map$long <- ifelse(d_map$long < -25, d_map$long + 360, d_map$long)

map <- ggplot() +
  geom_polygon(data = fortify(maps::map("world2", plot=FALSE, fill=TRUE)),
               aes(x=long, y=lat, group=group),
               color="black", fill="gray90", size=.25) +
  coord_equal(xlim = c(140,230), ylim = c(-45.5,23)) +
  geom_point(data=d_map, aes(x=long, y=lat,
                             shape=factor(Location),
                             fill=factor(Location),
                             colour=factor(Location)), size=3, alpha=0.7, stroke = .4) +
  geom_point(data=samples, aes(x=long, y=lat,
                         shape=factor(Location),
                         fill=factor(Location),
                         colour=factor(Location)), size=3, stroke = 1) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=contour) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.75),
        panel.background = element_rect(fill="white"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.title=element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        #legend.key=element_blank(), legend.background = element_blank(),
        #legend.text=element_text(size=8),
        legend.position="none") +
  #guides(shape = guide_legend(override.aes = list(size = 2), keyheight=0.15, default.unit="inch")) +
  annotate("text", -Inf, Inf, label = "A", size = 7, hjust = -1, vjust = 2)
map

biplot <- OIB %>% ggplot(aes(x=Pb208_Pb204,y=Nd143_Nd144, shape=factor(Location),
                             fill=factor(Location), color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.7, stroke = .4) +
  geom_point(data=samples, size=3, stroke = .75) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(38, 38.8)) +
  scale_y_continuous(limits=c(.5127, .51305)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^208*"Pb / "*{}^204*"Pb"),
       y=expression({}^143*"Nd / "*{}^144*"Nd")) +
  annotate("text", Inf, Inf, label = "B", size = 7, hjust = 2, vjust = 2)
biplot
pdf(here("analysis","work-in-progress","OIB-210722","K_SI1.pdf"),
    width=12, height=5)
map | biplot
dev.off()


#### Kapinga SI2 ####
p1 <- OIB %>% ggplot(aes(x=Sr/Nb,y=Nb/La, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.8) + geom_point(data=samples, size=3, stroke=.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(position = "top", limits=c(6, 34)) +
  scale_y_continuous(limits=c(.4, 2), breaks=c(1,2)) +
  labs(x=expression("Sr / Nb"), y=expression("Nb / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "none", aspect.ratio=1) +
  annotate("text", -Inf, Inf, label = "C", size = 7, hjust = -1, vjust = 2)
p2 <- OIB %>% ggplot(aes(x=Sr/Nb,y=La/Yb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.8) + geom_point(data=samples, size=3, stroke=.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(6, 34)) +
  scale_y_continuous(limits=c(0, 50)) +
  labs(x=expression("Sr / Nb"), y=expression("La / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)

p4 <- OIB %>% ggplot(aes(x=Nd143_Nd144,y=Nb/La, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.8) + geom_point(data=samples, size=3, stroke=.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(position = "top", limits=c(.5127, .51305)) +
  scale_y_continuous(limits=c(.4, 2), breaks=c(1,2)) +
  labs(x=expression({}^143*"Nd / "*{}^144*"Nd"), y=expression("Nb / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)
p5 <- OIB %>% ggplot(aes(x=Nd143_Nd144,y=La/Yb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.8) + geom_point(data=samples, size=3, stroke=.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(.5127, .51305)) +
  scale_y_continuous(limits=c(0, 50)) +
  labs(x=expression({}^143*"Nd / "*{}^144*"Nd"), y=expression("La / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)

p7 <- OIB %>% ggplot(aes(x=Pb208_Pb204,y=Nb/La, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.8) + geom_point(data=samples, size=3, stroke=.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(position = "top", limits=c(38, 38.8)) +
  scale_y_continuous(position = "right", limits=c(.4, 2), breaks=c(1,2)) +
  labs(x=expression({}^208*"Pb / "*{}^204*"Pb"), y=expression("Nb / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)
p8 <- OIB %>% ggplot(aes(x=Pb208_Pb204,y=La/Yb, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group=Sample)) +
  geom_point(size =3, alpha=0.8) + geom_point(data=samples, size=3, stroke=.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(38, 38.8)) +
  scale_y_continuous(position = "right", limits=c(0, 50)) +
  labs(x=expression({}^208*"Pb / "*{}^204*"Pb"), y=expression("La / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)

pdf(here("analysis","work-in-progress","OIB-210722","K_SI2.pdf"),
    width=12, height=9)
((p1/p2) | (p4/p5) | (p7/p8)) +
  plot_layout(guides = "collect") & theme(legend.position = "none")
dev.off()

#### Kapinga Multielement ####
s_spider_12_24 <- joined_data %>%
  filter(Sample %in% c("K-12-24")) %>%
  mutate(Location = case_when(grepl("K-12-24", Sample) ~ "K-12-24")) %>%
  normalize_to_pm()
s_spider_12_25 <- joined_data %>%
  filter(Sample %in% c("K-12-25")) %>%
  mutate(Location = case_when(grepl("K-12-25", Sample) ~ "K-12-25")) %>%
  normalize_to_pm()
s_spider_12_26 <- joined_data %>%
  filter(Sample %in% c("K-12-26")) %>%
  mutate(Location = case_when(grepl("K-12-26", Sample) ~ "K-12-26")) %>%
  normalize_to_pm()

#### K-12-24
Kapinga[1,] %>% dplyr::select(
  "Sample","SiO2 min","SiO2 max","K2O min","K2O max","Na2O min","Na2O max",
  "Cs min","Cs max","Rb min","Rb max","Ba min","Ba max","Th min","Th max",
  "U min","U max","Nb min","Nb max","Ta min","Ta max","Ce min","Ce max",
  "Nd min","Nd max","Sr min","Sr max","Sm min","Sm max","Zr min","Zr max",
  "Ti min","Ti max","Yb min","Yb max")
OIB <- dbGetQuery(georoc,
                  "SELECT * FROM 'sample'
                  WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
                  (`CS(PPM)` > 0.1545 AND `CS(PPM)` < 0.4635 AND
                  `RB(PPM)` > 12.2 AND `RB(PPM)` < 36.7 AND
                  `BA(PPM)` > 244 AND `BA(PPM)` < 733 AND
                  `TH(PPM)` > 2.73 AND `TH(PPM)` < 8.2 AND
                  `U(PPM)` > 0.925 AND `U(PPM)` < 2.77 AND
                  `NB(PPM)` > 46.4 AND `NB(PPM)` < 139 AND
                  `TA(PPM)` > 2.9 AND `TA(PPM)` < 8.7 AND
                  `CE(PPM)` > 75 AND `CE(PPM)` < 225 AND
                  `ND(PPM)` > 37.6 AND `ND(PPM)` < 113 AND
                  `SR(PPM)` > 452 AND `SR(PPM)` < 1357 AND
                  `SM(PPM)` > 7.65 AND `SM(PPM)` < 22.9 AND
                  `ZR(PPM)` > 239 AND `ZR(PPM)` < 717 AND
                  `YB(PPM)` > 1.26 AND `YB(PPM)` < 3.78) AND
                  file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv'") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::mutate(
    Location = case_when(grepl("KOSRAE", LOCATION) ~ "Kosrae",
                         grepl("CHUUK", LOCATION) ~ "Chuuk",
                         grepl("PONAPE", LOCATION) ~ "Ponape")) %>%
  dplyr::select(Sample,Location,
                lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

OIB_spider <- OIB %>% normalize_to_pm()

cols <- c("Kosrae"="#320A5A","Chuuk"="#320A5A80","Ponape"="#320A5A80","K-12-24"="red")
shapes <- c("Kosrae"=8,"Chuuk"=4,"Ponape"=3,"K-12-24"=16)

K_spider1 <- OIB_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Pb","Nd","Sr","Sm","Zr","Hf","Ti","Eu","Gd","Tb",
                           "Dy","Ho","Y","Er","Li","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=Location, color=Location, fill=Location,
             group=Sample)) +
  geom_line(size=.5) +
  geom_point(size=2, stroke=.5) +
  geom_line(data=s_spider_12_24, size=.5, color="red", #fill="red"
  ) +
  geom_point(data=s_spider_12_24, size=2, stroke=1) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=cols) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.5),
        axis.ticks.length.x = unit(.2, "cm"), axis.text = element_text(size=11),
        axis.title = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.title = element_blank(),legend.text = element_text(size = 7),
        legend.key.size = unit(.05, 'cm'),
        legend.position = c(.4,.2), legend.direction = "horizontal") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), ) +
  scale_y_log10(breaks=c(1,10,100), limits=c(1,300), labels = scales::comma)+
  #breaks = scales::trans_breaks("log10", function(x) 10^x),
  #labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides="l", size = .5, outside = TRUE, long = unit(0.1, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
K_spider1


#### K-12-25
Kapinga[2,] %>% dplyr::select(
  "Sample","SiO2 min","SiO2 max","K2O min","K2O max","Na2O min","Na2O max",
  "Cs min","Cs max","Rb min","Rb max","Ba min","Ba max","Th min","Th max",
  "U min","U max","Nb min","Nb max","Ta min","Ta max","Ce min","Ce max",
  "Nd min","Nd max","Sr min","Sr max","Sm min","Sm max","Zr min","Zr max",
  "Ti min","Ti max","Yb min","Yb max")
OIB <- dbGetQuery(georoc,
                  "SELECT * FROM 'sample'
                  WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
                  (`BA(PPM)` > 59 AND `BA(PPM)` < 177 AND
                  `NB(PPM)` > 6.45 AND `NB(PPM)` < 19.35 AND
                  `CE(PPM)` > 10.7 AND `CE(PPM)` < 32.1 AND
                  `ND(PPM)` > 6.55 AND `ND(PPM)` < 19.65 AND
                  `SR(PPM)` > 191 AND `SR(PPM)` < 573 AND
                  `SM(PPM)` > 1.71 AND `SM(PPM)` < 5.13 AND
                  `ZR(PPM)` > 46.85 AND `ZR(PPM)` < 140.55 AND
                  `YB(PPM)` > 0.86 AND `YB(PPM)` < 2.58) AND
                  file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv'") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::mutate(
    Location = case_when(grepl("KOSRAE", LOCATION) ~ "Kosrae",
                         grepl("CHUUK", LOCATION) ~ "Chuuk",
                         grepl("PONAPE", LOCATION) ~ "Ponape")) %>%
  dplyr::select(Sample,Location,
                lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

OIB_spider <- OIB %>% normalize_to_pm()

cols <- c("Chuuk"="#320A5A","K-12-25"="red")
shapes <- c("Chuuk"=4,"K-12-25"=16)

K_spider2 <- OIB_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Pb","Nd","Sr","Sm","Zr","Hf","Ti","Eu","Gd","Tb",
                           "Dy","Ho","Y","Er","Li","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=Location, color=Location, fill=Location,
             group=Sample)) +
  geom_line(size=.5) +
  geom_point(size=2, stroke=.5) +
  geom_line(data=s_spider_12_25, size=1, color="red", #fill="red"
  ) +
  geom_point(data=s_spider_12_25, size=2.5, stroke=1) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=cols) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.5),
        axis.ticks.length.x = unit(.2, "cm"), axis.text = element_text(size=11),
        axis.title = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.title = element_blank(),legend.text = element_text(size = 7),
        legend.key.size = unit(.05, 'cm'),
        legend.position = c(.4,.2), legend.direction = "horizontal") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), ) +
  scale_y_log10(breaks=c(1,10,100), limits=c(1,100), labels = scales::comma)+
  #breaks = scales::trans_breaks("log10", function(x) 10^x),
  #labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides="l", size = .5, outside = TRUE, long = unit(0.1, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
K_spider2

#### K-12-26
Kapinga[3,] %>% dplyr::select(
  "Sample","SiO2 min","SiO2 max","K2O min","K2O max","Na2O min","Na2O max",
  "Cs min","Cs max","Rb min","Rb max","Ba min","Ba max","Th min","Th max",
  "U min","U max","Nb min","Nb max","Ta min","Ta max","Ce min","Ce max",
  "Nd min","Nd max","Sr min","Sr max","Sm min","Sm max","Zr min","Zr max",
  "Ti min","Ti max","Yb min","Yb max")

OIB <- dbGetQuery(georoc,
                  "SELECT * FROM 'sample'
                  WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
                  (`SIO2(WT%)` > 42.9 AND `SIO2(WT%)` < 45.9 AND
                   `K2O(WT%)` > 0 AND `K2O(WT%)` < 2.35 AND
                   `NA2O(WT%)` > 2.41 AND `NA2O(WT%)` < 5.41 AND
                  `TH(PPM)` > 1 AND `TH(PPM)` < 3.06 AND
                  `U(PPM)` > 0.5 AND `U(PPM)` < 1.5 AND
                  `NB(PPM)` > 22.2 AND `NB(PPM)` < 66.6 AND
                  `TA(PPM)` > 1.42 AND `TA(PPM)` < 4.26 AND
                  `CE(PPM)` > 55.5 AND `CE(PPM)` < 166.5 AND
                  `ND(PPM)` > 31.4 AND `ND(PPM)` < 94.2 AND
                  `SR(PPM)` > 331 AND `SR(PPM)` < 994 AND
                  `SM(PPM)` > 6.7 AND `SM(PPM)` < 20.1 AND
                  `ZR(PPM)` > 127 AND `ZR(PPM)` < 382 AND
                  `YB(PPM)` > 1.26 AND `YB(PPM)` < 3.79) AND
                  (file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part1.csv' OR
                  file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part2.csv')") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::mutate(
    Location = case_when(grepl("MAUI", LOCATION) ~ "Maui",
                         grepl("MOLOKAI", LOCATION) ~ "Moloka'i")) %>%
  filter(Location != "na") %>%
  dplyr::select(Sample,Location,
                lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

OIB_spider <- OIB %>% normalize_to_pm()

cols <- c("Maui"="#F4DD53","Moloka'i"="#F4DD53","K-12-26"="red")
shapes <- c("Maui"=8,"Moloka'i"=3,"K-12-26"=16)

K_spider3 <- OIB_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Pb","Nd","Sr","Sm","Zr","Hf","Ti","Eu","Gd","Tb",
                           "Dy","Ho","Y","Er","Li","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=Location, color=Location, fill=Location,
             group=Sample)) +
  geom_line(size=.5) +
  geom_point(size=2, stroke=.5) +
  geom_line(data=s_spider_12_26, size=1, color="red", #fill="red"
  ) +
  geom_point(data=s_spider_12_26, size=2.5, stroke=1) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=cols) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.5),
        axis.ticks.length.x = unit(.2, "cm"), axis.text = element_text(size=11),
        axis.title = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.title = element_blank(),legend.text = element_text(size = 7),
        legend.key.size = unit(.05, 'cm'),
        legend.position = c(.4,.2), legend.direction = "horizontal") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), ) +
  scale_y_log10(breaks=c(1,10,100), limits=c(1,300), labels = scales::comma)+
  #breaks = scales::trans_breaks("log10", function(x) 10^x),
  #labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides="l", size = .5, outside = TRUE, long = unit(0.1, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
K_spider3

pdf(here("analysis","work-in-progress","OIB-210722","K_spiders.pdf"),
    width=5.5, height=6)
K_spider1/K_spider2/K_spider3
dev.off()
