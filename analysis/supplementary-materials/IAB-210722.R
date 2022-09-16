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

georoc <- dbConnect(RSQLite::SQLite(), path_to_georoc)
pofatu <- dbConnect(RSQLite::SQLite(), path_to_pofatu)

ranges(joined_data)
cols <- c("Luzon Arc"="#440154","Sulawesi Arc"="#472374","Sunda Arc"="#404386",
          "Banda Arc"="#345E8C","Yap Arc"="#29778E","Mariana Arc"="#218F8B",
          "Bismarck Arc"="#25A782","Solomon Arc"="#44BE6F","Vanuatu Arc"="#7AD04F",
          "Tonga-Fiji"="#BADD26","New Zealand"="#FDE725","E-11-03"="red",
          "E-11-06"="red","E-11-07"="red","K-12-28"="red","K-12-29"="red")
shapes <- c("Luzon Arc"=21,"Sulawesi Arc"=22,"Sunda Arc"=23,"Banda Arc"=24,
            "Yap Arc"=25,"Mariana Arc"=21,"Bismarck Arc"=22,"Solomon Arc"=23,
            "Vanuatu Arc"=24,"Tonga-Fiji"=25,"New Zealand"=21,"E-11-03"=22,
            "E-11-06"=24,"E-11-07"=25,"K-12-28"=21,"K-12-29"=23)
contour <- c("Luzon Arc"="black","Sulawesi Arc"="black","Sunda Arc"="black",
             "Banda Arc"="black","Yap Arc"="black","Mariana Arc"="black",
             "Bismarck Arc"="black","Solomon Arc"="black","Vanuatu Arc"="black",
             "Tonga-Fiji"="black","New Zealand"="black","E-11-03"="black",
             "E-11-06"="black","E-11-07"="black","K-12-28"="black","K-12-29"="black")

IAB_map <- dbGetQuery(georoc,
                    "SELECT id, file_id, LATITUDE_MIN, LONGITUDE_MIN
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND
PB206_PB204 > 1 AND PB207_PB204 > 1 AND PB208_PB204 > 1") %>%
  get_georoc_location() %>%
  rename(lat=LATITUDE_MIN, long=LONGITUDE_MIN)
IAB_map$long <- ifelse(IAB_map$long < -25, IAB_map$long + 360, IAB_map$long)
world <- map_data("world")
world$long <- ifelse(world$long < -25, world$long + 360, world$long)

map <- IAB_map %>% ggplot() +
  geom_polygon(data = fortify(maps::map("world2", plot=FALSE, fill=TRUE)),
               aes(x=long, y=lat, group=group),
               color="black", fill="gray95", size=.25) +
  coord_equal(xlim = c(95,210), ylim = c(-45.5,23)) +
  geom_point(data=IAB_map, aes(x=long, y=lat, shape=factor(Location), fill=factor(Location),
                           color=factor(Location), group=id), size=3, stroke=.25) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=contour) +
  scale_x_continuous(breaks=c(100, 130, 160, 190)) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.5),
        panel.background = element_rect(fill="white"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.title=element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        #legend.key=element_blank(), legend.background = element_blank(),
        #legend.text=element_text(size=8),
        legend.position="bottom") +
  annotate("text", Inf, Inf, label = "A", size = 7,  hjust = 2, vjust = 2)
map
pdf(here("analysis","work-in-progress","IAB-210722","IAB-map.pdf"),
    width=10, height=6)
map
dev.off()

#### E_11_03 ####
IAB <- dbGetQuery(georoc,
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
  #dplyr::filter(grepl("BISMARCK|LUZON|VANUATU|SULAWESI|SOLOMON|TONGA",file_id)) %>%
  get_georoc_location() %>% dplyr::filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
IAB %>% group_by(Location) %>% tally()

cols <- c("Luzon Arc"="#440154","Sulawesi Arc"="#472374","Bismarck Arc"="#25A782",
          "Solomon Arc"="#44BE6F","Vanuatu Arc"="#7AD04F","Tonga-Fiji"="#BADD26",
          "E-11-03"="red")
shapes <- c("Luzon Arc"=21,"Sulawesi Arc"=22,"Bismarck Arc"=22,"Solomon Arc"=23,
            "Vanuatu Arc"=24,"Tonga-Fiji"=25,"E-11-03"=22)
contour <- c("Luzon Arc"="black","Sulawesi Arc"="black",
             "Bismarck Arc"="black","Solomon Arc"="black","Vanuatu Arc"="black",
             "Tonga-Fiji"="black","E-11-03"="black")

s <- joined_data %>%
  filter(Sample %in% c(
    "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-18dup",
    "E-11-19","E-11-03")) %>%
  mutate(
    Location = case_when(
      grepl("E-11-10", Sample) ~ "Vanuatu Arc",
      grepl("E-11-11", Sample) ~ "Vanuatu Arc",
      grepl("E-11-13", Sample) ~ "Vanuatu Arc",
      grepl("E-11-16", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18dup", Sample) ~ "Vanuatu Arc",
      grepl("E-11-19", Sample) ~ "Vanuatu Arc",
      grepl("E-11-03", Sample) ~ "E-11-03"))

#### E_11_03 PCA1 ####
IAB <- IAB %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)
s <- s %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)

IAB <- full_join(IAB,s[3:6,])
s <- s[1,]

is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

is.na(s) <- sapply(s, is.infinite) #replace Inf by NA
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(IAB[,4:7],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
fviz_eig(res.pca)
eig <- get_eig(res.pca)
#fviz_pca_ind(res.pca, geom = "text") + theme(aspect.ratio=1) # plot of individual data points
E_11_03_PCA_1a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-4, 4)) + scale_y_continuous(limits=c(-1.5,2.7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
E_11_03_PCA_1a
pdf(here("analysis","work-in-progress","IAB-210722","IAB-E_11_03_PCA-1a.pdf"),
    width=3.5, height=3.5)
E_11_03_PCA_1a
dev.off()

res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, s[,4:7])
pred <- cbind(s[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

E_11_03_PCA_1b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="longdash") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="longdash") +
  geom_point(size =2, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-4, 4)) + scale_y_continuous(limits=c(-1.5,2.7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
E_11_03_PCA_1b
pdf(here("analysis","work-in-progress","IAB-210722","IAB-E_11_03_PCA-1b.pdf"),
    width=3.5, height=3.5)
E_11_03_PCA_1b
dev.off()

#### E_11_03 SI1 ####
d_map <- IAB %>% dplyr::select(Sample,Location,lat,long)
d_map$long <- ifelse(d_map$long < -25, d_map$long + 360, d_map$long)

map <- ggplot() +
  geom_polygon(data = fortify(maps::map("world2", plot=FALSE, fill=TRUE)),
               aes(x=long, y=lat, group=group),
               color="black", fill="gray90", size=.25) +
  coord_equal(xlim = c(95,185), ylim = c(-45.5,23)) +
  geom_point(data=d_map, aes(x=long, y=lat,
                             shape=factor(Location),
                             fill=factor(Location)), size=3, stroke = .4) +
  geom_point(data=s[1,], aes(x=long, y=lat,
                             shape=factor(Location),
                             fill=factor(Location)), size=3, stroke = .4) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values="black") +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.75),
        panel.background = element_rect(fill="white"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.title=element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.key=element_blank(), legend.background = element_blank(),
        legend.text=element_text(size=8), legend.position=c(.1,.2)) +
  guides(shape = guide_legend(override.aes = list(size = 2),
                              keyheight=0.15, default.unit="inch")) +
  annotate("text", Inf, Inf, label = "A", size = 7, hjust = 2, vjust = 2)
map

biplot1 <- IAB %>% ggplot(aes(x=Pb208_Pb204,y=Nd143_Nd144, shape=factor(Location),
                              fill=factor(Location), color=factor(Location), group=Sample)) +
  geom_point(size =3, stroke = .4) + geom_point(data=s, size=3, stroke = .4) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(0, 40)) + scale_y_continuous(limits=c(0, 150)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^208*"Pb / "*{}^204*"Pb"),
       y=expression({}^143*"Nd / "*{}^144*"Nd")) +
  annotate("text", Inf, Inf, label = "B", size = 7, hjust = 2, vjust = 2)
biplot1

pdf(here("analysis","work-in-progress","IAB-210722","IAB-E_11_03_SI1.pdf"),
    width=12, height=5)
map | biplot1
dev.off()

#### E_11_03 SI2 ####
IAB1 <- dbGetQuery(georoc,
                  "SELECT *
                  FROM 'sample'
                  WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
                  TECTONIC_SETTING='CONVERGENT MARGIN' AND
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

IAB2 <- dbGetQuery(pofatu,
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
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204) %>%
  filter(SiO2 > 68.6 & SiO2 < 71.6 & K2O > 2.62 & K2O < 5.62)

IAB <- full_join(IAB1,IAB2)
IAB %>% group_by(Location) %>% tally()

s <- joined_data %>%
  filter(Sample %in% c(
    "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-18dup",
    "E-11-19","E-11-03")) %>%
  mutate(
    Location = case_when(
      grepl("E-11-10", Sample) ~ "Vanuatu Arc",
      grepl("E-11-11", Sample) ~ "Vanuatu Arc",
      grepl("E-11-13", Sample) ~ "Vanuatu Arc",
      grepl("E-11-16", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18dup", Sample) ~ "Vanuatu Arc",
      grepl("E-11-19", Sample) ~ "Vanuatu Arc",
      grepl("E-11-03", Sample) ~ "E-11-03"))
s <- s[c("2","3","4","5","6","7","8","1"),]

cols <- c("Luzon Arc"="#440154","Sulawesi Arc"="#472374","Bismarck Arc"="#25A782",
          "Solomon Arc"="#44BE6F","Vanuatu Arc"="#7AD04F","Tonga-Fiji"="#BADD26",
          "E-11-03"="red")
shapes <- c("Luzon Arc"=21,"Sulawesi Arc"=22,"Bismarck Arc"=22,"Solomon Arc"=23,
            "Vanuatu Arc"=24,"Tonga-Fiji"=25,"E-11-03"=22)
contour <- c("Luzon Arc"="black","Sulawesi Arc"="black","Sunda Arc"="black",
             "Bismarck Arc"="black","Solomon Arc"="black","Vanuatu Arc"="black",
             "Tonga-Fiji"="black","E-11-03"="black")

p1 <- IAB %>% ggplot(aes(x=Sr/Yb,y=Rb/Yb, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(position = "top", limits=c(0, 350)) +
  scale_y_continuous(limits=c(0, 130)) +
  labs(x=expression("Sr / Yb"), y=expression("Rb / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "none", aspect.ratio=1) +
  annotate("text", -Inf, Inf, label = "C", size = 7, hjust = -1, vjust = 2)
p2 <- IAB %>% ggplot(aes(x=Sr/Yb,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(0, 350)) +
  scale_y_continuous(limits=c(0, 700)) +
  labs(x=expression("Sr / Yb"), y=expression("Ba / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        legend.position = "none", aspect.ratio=1)
p3 <- IAB %>% ggplot(aes(x=Sr/Yb,y=Nb/La, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(0, 350)) +
  scale_y_continuous(limits=c(0, 1.5)) +
  labs(x=expression("Sr / Yb"), y=expression("Nb / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)

p4 <- IAB %>% ggplot(aes(x=Nd143_Nd144,y=Rb/Yb, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(position = "top", limits=c(.51225, .513)) +
  scale_y_continuous(limits=c(0, 130)) +
  labs(x=expression({}^143*"Nd / "*{}^144*"Nd"), y=expression("Rb / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)
p5 <- IAB %>% ggplot(aes(x=Nd143_Nd144,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(position = "top", limits=c(.51225, .513)) +
  scale_y_continuous(limits=c(0, 700)) +
  labs(x=expression({}^143*"Nd / "*{}^144*"Nd"), y=expression("Ba / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),
        legend.position = "none", aspect.ratio=1)
p6 <- IAB %>% ggplot(aes(x=Nd143_Nd144,y=Nb/La, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(.51225, .513)) +
  scale_y_continuous(limits=c(0, 1.5)) +
  labs(x=expression({}^143*"Nd / "*{}^144*"Nd"), y=expression("Nb / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)

p7 <- IAB %>% ggplot(aes(x=Sr87_Sr86,y=Rb/Yb, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(position = "top", limits=c(.7035, .712)) +
  scale_y_continuous(position = "right", limits=c(0, 130)) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"), y=expression("Rb / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)
p8 <- IAB %>% ggplot(aes(x=Sr87_Sr86,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(.7035, .712)) +
  scale_y_continuous(position = "right", limits=c(0, 700)) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"), y=expression("Ba / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        legend.position = "none", aspect.ratio=1)
p9 <- IAB %>% ggplot(aes(x=Sr87_Sr86,y=Nb/La, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(.7035, .712)) +
  scale_y_continuous(position = "right", limits=c(0, 1.5)) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"), y=expression("Nb / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)

((p1/p2/p3) | (p4/p5/p6) | (p7/p8/p9))

pdf(here("analysis","work-in-progress","IAB-210722","IAB-E_11_03_SI2.pdf"),
    width=12, height=12)
((p1/p2/p3) | (p4/p5/p6) | (p7/p8/p9)) +
  plot_layout(guides = "collect") & theme(legend.position = "none")
dev.off()


#### E_11_03 PCA2 ####
IAB1 <- dbGetQuery(georoc,
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

IAB2 <- dbGetQuery(pofatu,
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

IAB <- full_join(IAB1,IAB2)
IAB %>% group_by(Location) %>% tally()

s <- joined_data %>%
  filter(Sample %in% c("E-11-03")) %>%
  mutate(
    Location = case_when(
      grepl("E-11-03", Sample) ~ "E-11-03"))

IAB <- IAB %>%
  dplyr::select(Sample,Location,
                TiO2,Al2O3,MgO,CaO,Na2O,
                Rb,Ba,Th,Nb,Ce,Nd,Sr,Sm,Zr,Yb)
s <- s %>%
  dplyr::select(Sample,Location,
                TiO2,Al2O3,MgO,CaO,Na2O,
                Rb,Ba,Th,Nb,Ce,Nd,Sr,Sm,Zr,Yb)
d <- full_join(IAB, s)
is.na(d) <- sapply(d, is.infinite) #replace Inf by NA
d[d == 0] <- NA # Replace 0 with NA
d <- d[rowSums(is.na(d)) == 0,] # removes rows with missing info for PCA

is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(IAB[,3:17],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
fviz_eig(res.pca)
eig <- get_eig(res.pca)
E_11_03_PCA_2a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-4, 4), breaks = c(-4,-2,0,2,4)) +
  scale_y_continuous(limits=c(-5, 5), breaks = c(-4,-2,0,2,4)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
E_11_03_PCA_2a
pdf(here("analysis","work-in-progress","IAB-210722","IAB-E_11_03_PCA-2a.pdf"),
    width=3.5, height=3.5)
E_11_03_PCA_2a
dev.off()

pred <- stats::predict(res.pca, s[,3:17])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

E_11_03_PCA_2b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="longdash") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="longdash") +
  geom_point(size =2, stroke=.25) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-4, 4), breaks = c(-4,-2,0,2,4)) +
  scale_y_continuous(limits=c(-5, 5), breaks = c(-4,-2,0,2,4)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
E_11_03_PCA_2b
pdf(here("analysis","work-in-progress","IAB-210722","IAB-E_11_03_PCA-2b.pdf"),
    width=3.5, height=3.5)
E_11_03_PCA_2b
dev.off()

#### E_11_03 Multielement ####
cols <- c("Luzon Arc"="#440154","Sulawesi Arc"="#472374",
          "Bismarck Arc"="#25A782","Solomon Arc"="#44BE6F","Vanuatu Arc"="#7AD04F",
          "Tonga-Fiji"="#BADD26","E-11-03"="red")
shapes <- c("Luzon Arc"=21,"Sulawesi Arc"=22,"Bismarck Arc"=22,"Solomon Arc"=23,
            "Vanuatu Arc"=24,"Tonga-Fiji"=25,"E-11-03"=22)
contour <- c("Luzon Arc"="black","Sulawesi Arc"="black",
             "Bismarck Arc"="black","Solomon Arc"="black","Vanuatu Arc"="black",
             "Tonga-Fiji"="black","E-11-03"="black")

joined_data_ranges[1,]

IAB1 <- dbGetQuery(georoc,
                   "SELECT *
                   FROM 'sample'
                   WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
                   (`BA(PPM)` > 480 AND `BA(PPM)` < 1441 AND
                   `TH(PPM)` > 2.77 AND `TH(PPM)` < 8.32 AND
                   `CE(PPM)` > 24.6 AND `CE(PPM)` < 73.8 AND
                   `NB(PPM)` > 2.78 AND `NB(PPM)` < 8.35 AND
                   `SM(PPM)` > 2.9 AND `SM(PPM)` < 8.7 AND
                   `ZR(PPM)` > 114 AND `ZR(PPM)` < 344 AND
                   `YB(PPM)` > 2.35 AND `YB(PPM)` < 7.05) AND
                   file_id = '2022-06-PVFZCE_NEW_HEBRIDES_ARC_VANUATU_ARCHIPELAGO.csv'") %>%
  #dplyr::filter(grepl("VANUATU",file_id)) %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K)

IAB2 <- dbGetQuery(pofatu,
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
  rename(Island=location_subregion) %>%
  Ti_from_TiO2() %>% K_from_K2O() %>% Fe2O3_from_FeO() %>%
  dplyr::select(Sample,Location,Island,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K) %>%
  dplyr::filter(Ba > 480 & Ba < 1441 & Th > 2.77 & Th < 8.32 &
                  Ce > 24.6 & Ce < 73.8 & Nb > 2.78 & Nb < 8.35 &
                  Sm > 2.9 & Sm < 8.7 & Zr > 114 & Zr < 344 &
                  Yb > 2.35 & Yb < 7.05)

IAB <- full_join(IAB1,IAB2)
IAB_spider <- IAB %>% normalize_to_pm()

s <- joined_data %>%
  filter(Sample %in% c(
    "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-18dup",
    "E-11-19","E-11-03")) %>%
  mutate(
    Location = case_when(
      grepl("E-11-10", Sample) ~ "Vanuatu Arc",
      grepl("E-11-11", Sample) ~ "Vanuatu Arc",
      grepl("E-11-13", Sample) ~ "Vanuatu Arc",
      grepl("E-11-16", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18dup", Sample) ~ "Vanuatu Arc",
      grepl("E-11-19", Sample) ~ "Vanuatu Arc",
      grepl("E-11-03", Sample) ~ "E-11-03"))
s <- s[c("2","3","4","5","6","7","8","1"),] %>%
  filter(Ba > 480 & Ba < 1441 & Th > 2.77 & Th < 8.32 &
           Ce > 24.6 & Ce < 73.8 & Nb > 2.78 & Nb < 8.35 &
           Sm > 2.9 & Sm < 8.7 & Zr > 114 & Zr < 344 &
           Yb > 2.35 & Yb < 7.05)
s_spider <- s %>% normalize_to_pm()


E_11_03_spider <- IAB_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Pb","Nd","Sr","Sm","Zr","Hf","Ti","Eu","Gd","Tb",
                           "Dy","Ho","Y","Er","Li","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=Location, color=Location, fill=Location,
             group=Sample)) +
  geom_line(size=1, color="#7AD04F") +
  geom_point(size=3, color="black", fill="#7AD04F", stroke=.25) +
  geom_line(data=s_spider, size=1, color="red") +
  geom_point(data=s_spider, size=3, color="black", fill="red", stroke=.25) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.5),
        axis.ticks.length.x = unit(-.2, "cm"), axis.text = element_text(size=11),
        axis.ticks.x.top = element_line(size=2),
        axis.title = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.title = element_blank(),legend.text = element_text(size = 7),
        legend.key.size = unit(.05, 'cm'),
        legend.position = "none", legend.direction = "horizontal") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), ) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100), limits=c(.6,300), labels = scales::comma)+
  annotation_logticks(sides="l", size = .5, outside = TRUE, long = unit(0.1, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
E_11_03_spider
pdf(here("analysis","work-in-progress","IAB-210722","IAB-E_11_03_spider.pdf"),
    width=5, height=2)
E_11_03_spider
dev.off()


#### E_11_06 & E_11_07 ####

joined_data_ranges[2:3,1]
joined_data_ranges[2:3,34:43]

E_11_06 <- dbGetQuery(georoc,
                      "SELECT *
                      FROM 'sample'
                      WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
                      (LONGITUDE_MAX > 90 OR LONGITUDE_MAX < 0) AND
                      LATITUDE_MAX < 20 AND TECTONIC_SETTING='CONVERGENT MARGIN' AND
                      (ND143_ND144 > 0.512414 AND ND143_ND144 < 0.51344 AND
                      SR87_SR86 > 0.703489 AND SR87_SR86 < 0.704897 AND
                      PB206_PB204 > 18.464 AND PB206_PB204 < 18.65 AND
                      PB207_PB204 > 15.49 AND PB207_PB204 < 15.646 AND
                      PB208_PB204 > 38.338 AND PB208_PB204 < 38.724)") %>%
  get_georoc_location() %>% filter(Location != "na")
E_11_06 %>% group_by(file_id) %>% tally()

E_11_07 <- dbGetQuery(georoc,
                      "SELECT *
                  FROM 'sample'
                  WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
                  (LONGITUDE_MAX > 90 OR LONGITUDE_MAX < 0) AND
                  LATITUDE_MAX < 20 AND TECTONIC_SETTING='CONVERGENT MARGIN' AND
                  (ND143_ND144 > 0.512419 AND ND143_ND144 < 0.513445 AND
                  SR87_SR86 > 0.703451 AND SR87_SR86 < 0.704859 AND
                  PB206_PB204 > 18.441 AND PB206_PB204 < 18.627 AND
                  PB207_PB204 > 15.487 AND PB207_PB204 < 15.643 AND
                  PB208_PB204 > 38.313 AND PB208_PB204 < 38.698)") %>%
  get_georoc_location() %>% filter(Location != "na")
E_11_07 %>% group_by(file_id) %>% tally()

cols <- c("Luzon Arc"="#440154","Sulawesi Arc"="#472374","Sunda Arc"="#404386",
          "Bismarck Arc"="#25A782","Solomon Arc"="#44BE6F","Vanuatu Arc"="#7AD04F",
          "Tonga-Fiji"="#BADD26","E-11-06"="red","E-11-07"="red")
shapes <- c("Luzon Arc"=21,"Sulawesi Arc"=22,"Sunda Arc"=23,"Bismarck Arc"=22,
            "Solomon Arc"=23,"Vanuatu Arc"=24,"Tonga-Fiji"=25,"E-11-06"=24,"E-11-07"=25)
contour <- c("Luzon Arc"="black","Sulawesi Arc"="black","Sunda Arc"="black",
             "Bismarck Arc"="black","Solomon Arc"="black","Vanuatu Arc"="black",
             "Tonga-Fiji"="black","E-11-06"="black","E-11-07"="black")


cols <- c("Luzon Arc"="#440154","Sulawesi Arc"="#472374","Sunda Arc"="#404386",
          "Banda Arc"="#345E8C","Yap Arc"="#29778E","Mariana Arc"="#218F8B",
          "Bismarck Arc"="#25A782","Solomon Arc"="#44BE6F","Vanuatu Arc"="#7AD04F",
          "Tonga-Fiji"="#BADD26","New Zealand"="#FDE725","E-11-03"="red",
          "E-11-06"="red","E-11-07"="red","K-12-28"="red","K-12-29"="red")
shapes <- c("Luzon Arc"=21,"Sulawesi Arc"=22,"Sunda Arc"=23,"Banda Arc"=24,
            "Yap Arc"=25,"Mariana Arc"=21,"Bismarck Arc"=22,"Solomon Arc"=23,
            "Vanuatu Arc"=24,"Tonga-Fiji"=25,"New Zealand"=21,"E-11-03"=22,
            "E-11-06"=24,"E-11-07"=25,"K-12-28"=21,"K-12-29"=23)
contour <- c("Luzon Arc"="black","Sulawesi Arc"="black","Sunda Arc"="black",
             "Banda Arc"="black","Yap Arc"="black","Mariana Arc"="black",
             "Bismarck Arc"="black","Solomon Arc"="black","Vanuatu Arc"="black",
             "Tonga-Fiji"="black","New Zealand"="black","E-11-03"="black",
             "E-11-06"="black","E-11-07"="black","K-12-28"="black","K-12-29"="black")

IAB <- dbGetQuery(georoc,
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

s <- joined_data %>%
  filter(Sample %in% c(
    "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-18dup",
    "E-11-19","E-11-06","E-11-07")) %>%
  mutate(
    Location = case_when(
      grepl("E-11-10", Sample) ~ "Vanuatu Arc",
      grepl("E-11-11", Sample) ~ "Vanuatu Arc",
      grepl("E-11-13", Sample) ~ "Vanuatu Arc",
      grepl("E-11-16", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18dup", Sample) ~ "Vanuatu Arc",
      grepl("E-11-19", Sample) ~ "Vanuatu Arc",
      grepl("E-11-06", Sample) ~ "E-11-06",
      grepl("E-11-07", Sample) ~ "E-11-07"))
s <- s[c("3","4","5","6","7","8","9","1","2"),]


#### E_11_06 & E_11_07 SI1 ####
d_map <- IAB %>% dplyr::select(Sample,Location,lat,long)
d_map$long <- ifelse(d_map$long < -25, d_map$long + 360, d_map$long)

map <- ggplot() +
  geom_polygon(data = fortify(maps::map("world2", plot=FALSE, fill=TRUE)),
               aes(x=long, y=lat, group=group),
               color="black", fill="gray90", size=.25) +
  coord_equal(xlim = c(95,185), ylim = c(-45.5,23)) +
  geom_point(data=d_map, aes(x=long, y=lat,
                             shape=factor(Location),
                             fill=factor(Location)), size=3, stroke = .4) +
  geom_point(data=s[8:9,], aes(x=long, y=lat,
                             shape=factor(Location),
                             fill=factor(Location)), size=3, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values="black") +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.75),
        panel.background = element_rect(fill="white"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.title=element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.key=element_blank(), legend.background = element_blank(),
        legend.text=element_text(size=8), legend.position=c(.1,.2)) +
  guides(shape = guide_legend(override.aes = list(size = 2),
                              keyheight=0.15, default.unit="inch")) +
annotate("text", Inf, Inf, label = "A", size = 7, hjust = 2, vjust = 2)
map

biplot1 <- IAB %>% ggplot(aes(x=Sr87_Sr86,y=Nd143_Nd144, shape=factor(Location),
                   fill=factor(Location), color=factor(Location), group=Sample)) +
  geom_point(size =3, stroke = .4) + geom_point(data=s[8:9,], size=3, stroke = .4) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(0, 40)) + scale_y_continuous(limits=c(0, 150)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"),
     y=expression({}^143*"Nd / "*{}^144*"Nd")) +
  annotate("text", Inf, Inf, label = "B", size = 7, hjust = 2, vjust = 2)
biplot1

pdf(here("analysis","work-in-progress","IAB-210722","IAB-E_11_06_SI1.pdf"),
    width=12, height=5)
map | biplot1
dev.off()


#### E_11_06 & E_11_07 PCA1 ####
IAB <- IAB %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)
s <- s %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)

IAB <- full_join(IAB,s[1:5,])
s <- s[8:9,]

res.pca <- prcomp(IAB[,4:7],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
fviz_eig(res.pca)
eig <- get_eig(res.pca)
fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  #scale_x_continuous(limits=c(-5, 6)) + scale_y_continuous(limits=c(-2, 4)) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols)
#fviz_pca_ind(res.pca, geom = "text") # plot of individual data points
#row.names.IAB <- c("144") #identify outliers
#IAB <- IAB[!(row.names(IAB) %in% row.names.IAB),] # remove outliers
#res.pca <- prcomp(IAB[,3:7], scale = TRUE, center = TRUE)
E_11_06_PCA_1a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-3.5, 3.5)) + scale_y_continuous(limits=c(-2, 3.3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
E_11_06_PCA_1a
pdf(here("analysis","work-in-progress","IAB-210722","IAB-E_11_06_PCA_1a.pdf"),
    width=3.5, height=3.5)
E_11_06_PCA_1a
dev.off()

res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, s[,4:7])
pred <- cbind(s[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

E_11_06_PCA_1b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="longdash") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="longdash") +
  geom_point(size =2, stroke=.25) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-3.5, 3.5)) + scale_y_continuous(limits=c(-2, 3.3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
E_11_06_PCA_1b
pdf(here("analysis","work-in-progress","IAB-210722","IAB-E_11_06_PCA_1b.pdf"),
    width=3.5, height=3.5)
E_11_06_PCA_1b
dev.off()

#### E_11_06 & E_11_07 SI2 ####
IAB <- dbGetQuery(georoc,
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

s <- joined_data %>%
  filter(Sample %in% c(
    "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-18dup",
    "E-11-19","E-11-06","E-11-07")) %>%
  mutate(
    Location = case_when(
      grepl("E-11-10", Sample) ~ "Vanuatu Arc",
      grepl("E-11-11", Sample) ~ "Vanuatu Arc",
      grepl("E-11-13", Sample) ~ "Vanuatu Arc",
      grepl("E-11-16", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18dup", Sample) ~ "Vanuatu Arc",
      grepl("E-11-19", Sample) ~ "Vanuatu Arc",
      grepl("E-11-06", Sample) ~ "E-11-06",
      grepl("E-11-07", Sample) ~ "E-11-07"))
s <- s[c("3","4","5","6","7","8","9","1","2"),]


p1 <- IAB %>% ggplot(aes(x=Sr/Yb,y=Rb/Yb, shape=factor(Location), fill=factor(Location),
                   group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(position = "top", limits=c(0, 1000)) +
  scale_y_continuous(limits=c(0, 35)) +
  labs(x=expression("Sr / Yb"), y=expression("Rb / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size = 12), axis.text = element_text(size = 10),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "none", aspect.ratio=1) +
  annotate("text", -Inf, Inf, label = "C", size = 7, hjust = -1, vjust = 2)
p2 <- IAB %>% ggplot(aes(x=Sr/Yb,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
                   group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(0, 1000)) +
  scale_y_continuous(limits=c(0, 500)) +
  labs(x=expression("Sr / Yb"), y=expression("Ba / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 12), axis.text = element_text(size = 10),
        axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        legend.position = "none", aspect.ratio=1)
p3 <- IAB %>% ggplot(aes(x=Sr/Yb,y=Nb/La, shape=factor(Location), fill=factor(Location),
                   group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(0, 1000)) +
  scale_y_continuous(limits=c(0, 1)) +
  labs(x=expression("Sr / Yb"), y=expression("Nb / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 12), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1)

p4 <- IAB %>% ggplot(aes(x=Nd143_Nd144,y=Rb/Yb, shape=factor(Location), fill=factor(Location),
                   group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(position = "top", limits=c(.5126, .51312)) +
  scale_y_continuous(limits=c(0, 35)) +
  labs(x=expression({}^143*"Nd / "*{}^144*"Nd"), y=expression("Rb / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 12), axis.text = element_text(size = 10),
        axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)
p5 <- IAB %>% ggplot(aes(x=Nd143_Nd144,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
                   group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(.5126, .51312)) +
  scale_y_continuous(limits=c(0, 500)) +
  labs(x=expression({}^143*"Nd / "*{}^144*"Nd"), y=expression("Ba / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),
        legend.position = "none", aspect.ratio=1)
p6 <- IAB %>% ggplot(aes(x=Nd143_Nd144,y=Nb/La, shape=factor(Location), fill=factor(Location),
                   group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(.5126, .51312)) +
  scale_y_continuous(limits=c(0, 1)) +
  labs(x=expression({}^143*"Nd / "*{}^144*"Nd"), y=expression("Nb / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 12), axis.text = element_text(size = 10),
        axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)

p7 <- IAB %>% ggplot(aes(x=Sr87_Sr86,y=Rb/Yb, shape=factor(Location), fill=factor(Location),
                   group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(position = "top", limits=c(.7035, .705)) +
  scale_y_continuous(position = "right", limits=c(0, 35)) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"), y=expression("Rb / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 12), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1)
p8 <- IAB %>% ggplot(aes(x=Sr87_Sr86,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
                   group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(.7035, .705)) +
  scale_y_continuous(position = "right", limits=c(0, 500)) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"), y=expression("Ba / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 12), axis.text = element_text(size = 10),
        axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        legend.position = "none", aspect.ratio=1)
p9 <- IAB %>% ggplot(aes(x=Sr87_Sr86,y=Nb/La, shape=factor(Location), fill=factor(Location),
                   group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(.7035, .705)) +
  scale_y_continuous(position = "right", limits=c(0, 1)) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"), y=expression("Nb / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 12), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1)

((p1/p2/p3) | (p4/p5/p6) | (p7/p8/p9))

pdf(here("analysis","work-in-progress","IAB-210722","IAB-E_11_06_SI2.pdf"),
    width=12, height=12)
((p1/p2/p3) | (p4/p5/p6) | (p7/p8/p9)) +
  plot_layout(guides = "collect") & theme(legend.position = "none")
dev.off()


#### E_11_06 & E_11_07 PCA2 ####
IAB <- dbGetQuery(georoc,
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

s <- joined_data %>%
  filter(Sample %in% c(
    "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-18dup",
    "E-11-19","E-11-06","E-11-07")) %>%
  mutate(
    Location = case_when(
      grepl("E-11-10", Sample) ~ "Vanuatu Arc",
      grepl("E-11-11", Sample) ~ "Vanuatu Arc",
      grepl("E-11-13", Sample) ~ "Vanuatu Arc",
      grepl("E-11-16", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18dup", Sample) ~ "Vanuatu Arc",
      grepl("E-11-19", Sample) ~ "Vanuatu Arc",
      grepl("E-11-06", Sample) ~ "E-11-06",
      grepl("E-11-07", Sample) ~ "E-11-07"))
s <- s[c("3","4","5","6","7","8","9","1","2"),]

IAB <- IAB %>%
  dplyr::select(Sample,Location,
                TiO2,Al2O3,MgO,CaO,Na2O,
                Rb,Ba,Th,Nb,Ce,Nd,Sr,Sm,Zr,Yb)
s <- s %>%
  dplyr::select(Sample,Location,
                TiO2,Al2O3,MgO,CaO,Na2O,
                Rb,Ba,Th,Nb,Ce,Nd,Sr,Sm,Zr,Yb)

is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(IAB[,3:17], scale = TRUE, center = TRUE) # Dimension reduction using PCA
fviz_eig(res.pca)
eig <- get_eig(res.pca)
E_11_06_PCA_2a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-4, 4.5), breaks=c(-6,-4,-2,0,2,4)) +
  scale_y_continuous(limits=c(-3.3, 3), breaks=c(-2,0,2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
E_11_06_PCA_2a
pdf(here("analysis","work-in-progress","IAB-210722","IAB-E_11_06_PCA_2a.pdf"),
    width=3.5, height=3.5)
E_11_06_PCA_2a
dev.off()

res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, s[,3:17])
pred <- cbind(s[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

E_11_06_PCA_2b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="longdash") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="longdash") +
  geom_point(size =2, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-4, 4.5), breaks=c(-6,-4,-2,0,2,4)) +
  scale_y_continuous(limits=c(-3.3, 3), breaks=c(-2,0,2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
E_11_06_PCA_2b
pdf(here("analysis","work-in-progress","IAB-210722","IAB-E_11_06_PCA_2b.pdf"),
    width=3.5, height=3.5)
E_11_06_PCA_2b
dev.off()


 #### E_11_06 & E_11_07 multielements ####
joined_data_ranges[2:3,]

IAB <- dbGetQuery(georoc,
                   "SELECT *
                   FROM 'sample'
                   WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
                   (`BA(PPM)` > 143 AND `BA(PPM)` < 504 AND
                   `TH(PPM)` > 1.21 AND `TH(PPM)` < 4.32 AND
                   `CE(PPM)` > 17.6 AND `CE(PPM)` < 57.3 AND
                   `NB(PPM)` > 0.52 AND `NB(PPM)` < 1.84 AND
                   `SM(PPM)` > 2.15 AND `SM(PPM)` < 6.79 AND
                   `ZR(PPM)` > 22.8 AND `ZR(PPM)` < 69 AND
                   `YB(PPM)` > 0.74 AND `YB(PPM)` < 2.29) AND
                   (file_id= '2022-06-PVFZCE_NEW_HEBRIDES_ARC_VANUATU_ARCHIPELAGO.csv')") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

s <- joined_data %>%
  filter(Sample %in% c(
    "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-18dup",
    "E-11-19","E-11-06","E-11-07")) %>%
  mutate(
    Location = case_when(
      grepl("E-11-10", Sample) ~ "Vanuatu Arc",
      grepl("E-11-11", Sample) ~ "Vanuatu Arc",
      grepl("E-11-13", Sample) ~ "Vanuatu Arc",
      grepl("E-11-16", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18dup", Sample) ~ "Vanuatu Arc",
      grepl("E-11-19", Sample) ~ "Vanuatu Arc",
      grepl("E-11-06", Sample) ~ "E-11-06",
      grepl("E-11-07", Sample) ~ "E-11-07"))
s <- s[c("3","4","5","6","7","8","9","1","2"),] %>%
  filter(Ba > 143 & Ba < 504 & Th > 1.21 & Th < 4.32 &
         Ce > 17.6 & Ce < 57.3 & Nb > 0.52 & Nb < 1.84 &
         Sm > 2.15 & Sm < 6.79 & Zr > 22.8 & Zr < 69 &
         Yb > 0.74 & Yb < 2.29) %>%
  K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

IAB <- full_join(IAB, s[1:2,])
IAB_spider <- IAB %>% normalize_to_pm()
s_spider <- s[3:4,] %>% normalize_to_pm()

E_11_06_spider <- IAB_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Pb","Nd","Sr","Sm","Zr","Hf","Ti","Eu","Gd","Tb",
                           "Dy","Ho","Y","Er","Li","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=Location, group=Sample)) +
  geom_line(size=1, color="#7AD04F", fill="#7AD04F") +
  geom_point(size=3, color="black", fill="#7AD04F", stroke=.25) +
  geom_line(data=s_spider, size=1, color="red", fill="red") +
  geom_point(data=s_spider, size=3, color="black", fill="red", stroke=.25) +
  scale_shape_manual(values=shapes) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.5),
        axis.ticks.length.x = unit(.2, "cm"), axis.text = element_text(size=11),
        axis.title = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.position = "none") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), ) +
  scale_y_log10(breaks=c(0.1,1,10,100), limits=c(0.5,100), labels = scales::comma)+
  annotation_logticks(sides="l", size = .5, outside = TRUE, long = unit(0.2, "cm"),
                      mid = unit(0.1, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
E_11_06_spider
pdf(here("analysis","work-in-progress","IAB-210722","IAB-E_11_06_spider.pdf"),
    width=5, height=2)
E_11_06_spider
dev.off()

#### K_12_28 ####

#### K_12_28 PCA1 ####

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
IAB %>% group_by(Location) %>% tally()

cols <- c("Luzon Arc"="#440154","Sulawesi Arc"="#472374","Sunda Arc"="#404386",
          "Bismarck Arc"="#25A782","Vanuatu Arc"="#7AD04F","K-12-28"="red")
shapes <- c("Luzon Arc"=21,"Sulawesi Arc"=22,"Sunda Arc"=23,"Bismarck Arc"=22,
            "Vanuatu Arc"=24,"K-12-28"=21)
contour <- c("Luzon Arc"="black","Sulawesi Arc"="black","Sunda Arc"="black",
             "Bismarck Arc"="black","Solomon Arc"="black","Vanuatu Arc"="black",
             "K-12-28"="black")

IAB <- IAB %>%
  dplyr::select(Sample,Location,
                Rb,Sr,Y,Zr,Nb,
                Nd143_Nd144,Sr87_Sr86,
                Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>%
  filter(Sample %in% c("K-12-28")) %>%
  mutate(Location = case_when(grepl("K-12-28", Sample) ~ "K-12-28")) %>%
  dplyr::select(Sample,Location,
                Rb,Sr,Y,Zr,Nb,
                Nd143_Nd144,Sr87_Sr86,
                Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

res.pca <- prcomp(IAB[,3:12],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
fviz_eig(res.pca)
eig <- get_eig(res.pca)
K_12_28_PCA_1a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  #scale_x_continuous(limits=c(-2.3, 2.8)) + scale_y_continuous(limits=c(-2.8, 3.6)) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
K_12_28_PCA_1a
pdf(here("analysis","work-in-progress","IAB-210722","IAB-K_12_28_PCA_1a.pdf"),
    width=3.5, height=3.5)
K_12_28_PCA_1a
dev.off()

res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, s[,3:12])
pred <- cbind(s[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

K_12_28_PCA_1b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="longdash") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="longdash") +
  geom_point(size =2, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(-2.3, 2.8)) + scale_y_continuous(limits=c(-2.8, 3.6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_12_28_PCA_1b
pdf(here("analysis","work-in-progress","IAB-210722","IAB-K_12_28_PCA_1b.pdf"),
    width=3.5, height=3.5)
K_12_28_PCA_1b
dev.off()

#### K_12_28 SI1 ####
d_map <- IAB %>% dplyr::select(Sample,Location,lat,long)
d_map$long <- ifelse(d_map$long < -25, d_map$long + 360, d_map$long)

map <- ggplot() +
  geom_polygon(data = fortify(maps::map("world2", plot=FALSE, fill=TRUE)),
               aes(x=long, y=lat, group=group),
               color="black", fill="gray90", size=.25) +
  coord_equal(xlim = c(95,185), ylim = c(-45.5,23)) +
  geom_point(data=d_map, aes(x=long, y=lat,
                             shape=factor(Location),
                             fill=factor(Location)), size=3, stroke = .4) +
  geom_point(data=s, aes(x=long, y=lat,
                         shape=factor(Location),
                         fill=factor(Location)), size=3, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values="black") +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.75),
        panel.background = element_rect(fill="white"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.title=element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.key=element_blank(), legend.background = element_blank(),
        legend.text=element_text(size=8), legend.position=c(.1,.2)) +
  guides(shape = guide_legend(override.aes = list(size = 2),
                              keyheight=0.15, default.unit="inch")) +
  annotate("text", Inf, Inf, label = "A", size = 7, hjust = 2, vjust = 2)
map

biplot1 <- IAB %>% ggplot(aes(x=Sr87_Sr86,y=Nd143_Nd144, shape=factor(Location),
                              fill=factor(Location), color=factor(Location), group=Sample)) +
  geom_point(size =3, stroke = .4) + geom_point(data=s, size=3, stroke = .4) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(.703, .707)) +
  scale_y_continuous(limits=c(.5125, .51312)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"),
       y=expression({}^143*"Nd / "*{}^144*"Nd")) +
  annotate("text", Inf, Inf, label = "B", size = 7, hjust = 2, vjust = 2)
biplot1

pdf(here("analysis","work-in-progress","IAB-210722","IAB-K_12_28_SI1.pdf"),
    width=12, height=5)
map | biplot1
dev.off()

#### K_12_28 SI2 ####
IAB <- dbGetQuery(georoc,
                  "SELECT *
                  FROM 'sample'
                  WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
                  (`SIO2(WT%)` > 52.1 AND `SIO2(WT%)` < 55.1 AND
                  `K2O(WT%)` > 0 AND `K2O(WT%)` < 1.55) AND
                  (file_id= '2022-06-PVFZCE_BISMARCK_ARC_NEW_BRITAIN_ARC.csv' OR
                  file_id= '2022-06-PVFZCE_LUZON_ARC.csv' OR
                  file_id= '2022-06-PVFZCE_NEW_HEBRIDES_ARC_VANUATU_ARCHIPELAGO.csv' OR
                  file_id= '2022-06-PVFZCE_SULAWESI_ARC.csv' OR
                  file_id= '2022-06-PVFZCE_SUNDA_ARC.csv')") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

s <- joined_data %>%
  filter(Sample %in% c("K-12-28")) %>%
  mutate(Location = case_when(grepl("K-12-28", Sample) ~ "K-12-28"))


p1 <- IAB %>% ggplot(aes(x=Sr/Yb,y=Rb/Yb, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(position = "top", limits=c(0, 600)) +
  scale_y_continuous(limits=c(0, 25)) +
  labs(x=expression("Sr / Yb"), y=expression("Rb / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "none", aspect.ratio=1) +
  annotate("text", -Inf, Inf, label = "C", size = 7, hjust = -1, vjust = 2)
p2 <- IAB %>% ggplot(aes(x=Sr/Yb,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(0, 600)) +
  scale_y_continuous(limits=c(20, 320)) +
  labs(x=expression("Sr / Yb"), y=expression("Ba / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        legend.position = "none", aspect.ratio=1)
p3 <- IAB %>% ggplot(aes(x=Sr/Yb,y=Nb/La, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(0, 600)) +
  scale_y_continuous(limits=c(0, 1.3)) +
  labs(x=expression("Sr / Yb"), y=expression("Nb / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)

p4 <- IAB %>% ggplot(aes(x=Nd143_Nd144,y=Rb/Yb, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(position = "top", limits=c(.5126, .5131)) +
  scale_y_continuous(limits=c(0, 25)) +
  labs(x=expression({}^143*"Nd / "*{}^144*"Nd"), y=expression("Rb / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)
p5 <- IAB %>% ggplot(aes(x=Nd143_Nd144,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(.5126, .5131)) +
  scale_y_continuous(limits=c(20, 320)) +
  labs(x=expression({}^143*"Nd / "*{}^144*"Nd"), y=expression("Ba / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),
        legend.position = "none", aspect.ratio=1)
p6 <- IAB %>% ggplot(aes(x=Nd143_Nd144,y=Nb/La, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(.5126, .5131)) +
  scale_y_continuous(limits=c(0, 1.3)) +
  labs(x=expression({}^143*"Nd / "*{}^144*"Nd"), y=expression("Nb / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)

p7 <- IAB %>% ggplot(aes(x=Sr87_Sr86,y=Rb/Yb, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(position = "top", limits=c(.703, .7055)) +
  scale_y_continuous(position = "right", limits=c(0, 25)) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"), y=expression("Rb / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)
p8 <- IAB %>% ggplot(aes(x=Sr87_Sr86,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(.703, .7055)) +
  scale_y_continuous(position = "right", limits=c(20, 320)) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"), y=expression("Ba / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        legend.position = "none", aspect.ratio=1)
p9 <- IAB %>% ggplot(aes(x=Sr87_Sr86,y=Nb/La, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(.703, .7055)) +
  scale_y_continuous(position = "right", limits=c(0, 1.3)) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"), y=expression("Nb / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)

pdf(here("analysis","work-in-progress","IAB-210722","IAB-K_12_28_SI2.pdf"),
    width=12, height=12)
((p1/p2/p3) | (p4/p5/p6) | (p7/p8/p9)) +
  plot_layout(guides = "collect") & theme(legend.position = "none")
dev.off()

#### K_12_28 PCA2 ####
IAB <- dbGetQuery(georoc,
                  "SELECT *
                  FROM 'sample'
                  WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
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

IAB <- IAB %>%
  dplyr::select(Sample,Location,
                TiO2,Al2O3,MgO,CaO,Na2O,
                Rb,Ba,Th,Nb,Ce,Nd,Sr,Sm,Zr,Yb,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
s <- joined_data %>%
  filter(Sample %in% c("K-12-28")) %>%
  mutate(Location = case_when(grepl("K-12-28", Sample) ~ "K-12-28")) %>%
  dplyr::select(Sample,Location,
                TiO2,Al2O3,MgO,CaO,Na2O,
                Rb,Ba,Th,Nb,Ce,Nd,Sr,Sm,Zr,Yb,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(IAB[,3:22], scale = TRUE, center = TRUE) # Dimension reduction using PCA
fviz_eig(res.pca)
eig <- get_eig(res.pca)
fviz_pca_ind(res.pca, geom = "text") # plot of individual data points
#row.names.IAB <- c("409","260","252","411","5") #identify outliers
#IAB <- IAB[!(row.names(IAB) %in% row.names.IAB),] # remove outliers
#res.pca <- prcomp(IAB[,3:17], scale = TRUE, center = TRUE) # Dimension reduction using PCA

K_12_28_PCA_2a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  #scale_x_continuous(limits=c(-5.5, 6)) + scale_y_continuous(limits=c(-3.5, 5)) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
K_12_28_PCA_2a
pdf(here("analysis","work-in-progress","IAB-210722","IAB-K_12_28_PCA_2a.pdf"),
    width=3.5, height=3.5)
K_12_28_PCA_2a
dev.off()

pred <- stats::predict(res.pca, s[,3:22])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

K_12_28_PCA_2b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="longdash") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="longdash") +
  geom_point(size =2, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-5.5, 6)) + scale_y_continuous(limits=c(-4, 4)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_12_28_PCA_2b
pdf(here("analysis","work-in-progress","IAB-210722","IAB-K_12_28_PCA_2b.pdf"),
    width=3.5, height=3.5)
K_12_28_PCA_2b
dev.off()


#### K_12_28 multielements ####
joined_data_ranges[19,]
IAB <- dbGetQuery(georoc,
                  "SELECT *
                   FROM 'sample'
                   WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
                   (`SIO2(WT%)` > 52.1 AND `SIO2(WT%)` < 55.1 AND
                   `TH(PPM)` > 0.26 AND `TH(PPM)` < 0.78 AND
                   `CE(PPM)` > 5.65 AND `CE(PPM)` < 16.95 AND
                   `NB(PPM)` > 0.33 AND `NB(PPM)` < 1 AND
                   `SM(PPM)` > 0.97 AND `SM(PPM)` < 2.91 AND
                   `ZR(PPM)` > 11.1 AND `ZR(PPM)` < 33.3 AND
                   `YB(PPM)` > 0.62 AND `YB(PPM)` < 1.86) AND
                   (file_id= '2022-06-PVFZCE_BISMARCK_ARC_NEW_BRITAIN_ARC.csv' OR
                   file_id= '2022-06-PVFZCE_LUZON_ARC.csv' OR
                   file_id= '2022-06-PVFZCE_NEW_HEBRIDES_ARC_VANUATU_ARCHIPELAGO.csv' OR
                   file_id= '2022-06-PVFZCE_SULAWESI_ARC.csv' OR
                  file_id= '2022-06-PVFZCE_SUNDA_ARC.csv')") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB %>% group_by(Location) %>% tally()

IAB_spider <- IAB %>% normalize_to_pm()

s_spider <- joined_data %>%
  filter(Sample %in% c("K-12-28")) %>% normalize_to_pm() %>%
  mutate(Location = case_when(grepl("K-12-28", Sample) ~ "K-12-28"))

K_12_28_spider <- IAB_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Pb","Nd","Sr","Sm","Zr","Hf","Ti","Eu","Gd","Tb",
                           "Dy","Ho","Y","Er","Li","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, color=Location, shape=Location, fill=Location,
             group=Sample)) +
  scale_shape_manual(values=shapes) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols) +
  geom_line(size=1) +
  geom_point(color="black", size=3, stroke=.25) +
  geom_line(data=s_spider, size=1) +
  geom_point(data=s_spider, color="black", size=3, stroke=.25) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.5),
        axis.ticks.length.x = unit(.2, "cm"), axis.text = element_text(size=11),
        axis.title = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.position = "none") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), ) +
  scale_y_log10(breaks=c(0.1,1,10,100), limits=c(0.3,100), labels = scales::comma)+
  annotation_logticks(sides="l", size = .5, outside = TRUE, long = unit(0.2, "cm"),
                      mid = unit(0.1, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")

pdf(here("analysis","work-in-progress","IAB-210722","IAB-K_12_28_spider.pdf"),
    width=5, height=2)
K_12_28_spider
dev.off()


#### K_12_29 ####
joined_data_ranges[20,]
IAB <- dbGetQuery(georoc,
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
  #dplyr::filter(grepl(
  #  "BISMARCK|LUZON|MARIANA|VANUATU|SULAWESI|SOLOMON|SUNDA|TONGA",file_id)) %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
IAB %>% group_by(Location) %>% tally() #the island arcs represented

cols <- c("Luzon Arc"="#440154","Sulawesi Arc"="#472374","Sunda Arc"="#404386",
          "Mariana Arc"="#218F8B","Bismarck Arc"="#25A782","Solomon Arc"="#44BE6F",
          "Vanuatu Arc"="#7AD04F","Tonga-Fiji"="#BADD26","K-12-29"="red")
shapes <- c("Luzon Arc"=21,"Sulawesi Arc"=22,"Sunda Arc"=23,"Mariana Arc"=21,
            "Bismarck Arc"=22,"Solomon Arc"=23,"Vanuatu Arc"=24,"Tonga-Fiji"=25,
            "K-12-29"=23)
contour <- c("Luzon Arc"="black","Sulawesi Arc"="black","Sunda Arc"="black",
             "Mariana Arc"="black","Bismarck Arc"="black","Solomon Arc"="black",
             "Vanuatu Arc"="black","Tonga-Fiji"="black","K-12-29"="black")

s <- joined_data %>%
  filter(Sample %in% c("K-12-29")) %>%
  mutate(Location = case_when(grepl("K-12-29", Sample) ~ "K-12-29"))

#### K_12_29 SI1 ####
d_map <- IAB %>% dplyr::select(Sample,Location,lat,long)
d_map$long <- ifelse(d_map$long < -25, d_map$long + 360, d_map$long)

map <- ggplot() +
  geom_polygon(data = fortify(maps::map("world2", plot=FALSE, fill=TRUE)),
               aes(x=long, y=lat, group=group),
               color="black", fill="gray90", size=.25) +
  coord_equal(xlim = c(95,185), ylim = c(-45.5,23)) +
  geom_point(data=d_map, aes(x=long, y=lat,
                             shape=factor(Location),
                             fill=factor(Location)), size=3, stroke = .4) +
  geom_point(data=s, aes(x=long, y=lat,
                         shape=factor(Location),
                         fill=factor(Location)), size=3, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values="black") +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.75),
        panel.background = element_rect(fill="white"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.title=element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.key=element_blank(), legend.background = element_blank(),
        legend.text=element_text(size=8), legend.position=c(.1,.2)) +
  guides(shape = guide_legend(override.aes = list(size = 2),
                              keyheight=0.15, default.unit="inch")) +
  annotate("text", Inf, Inf, label = "A", size = 7, hjust = 2, vjust = 2)
map

biplot1 <- IAB %>% ggplot(aes(x=Pb208_Pb204,y=Nd143_Nd144, shape=factor(Location),
                              fill=factor(Location), color=factor(Location), group=Sample)) +
  geom_point(size =3, stroke = .4) + geom_point(data=s, size=3, stroke = .4) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(0, 40)) +
  #scale_y_continuous(limits=c(.5127, .51312)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^208*"Pb / "*{}^204*"Pb"),
       y=expression({}^143*"Nd / "*{}^144*"Nd")) +
  annotate("text", Inf, Inf, label = "B", size = 7, hjust = 2, vjust = 2)
biplot1

pdf(here("analysis","work-in-progress","IAB-210722","IAB-K_12_29_SI1.pdf"),
    width=12, height=5)
map | biplot1
dev.off()

#### K_12_29 PCA1 ####
IAB <- IAB %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)
s <- s %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)
d <- full_join(IAB, s) %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)

is.na(d) <- sapply(d, is.infinite) #replace Inf by NA
d[d == 0] <- NA # Replace 0 with NA
d <- d[rowSums(is.na(d)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(d[,3:7], scale = TRUE, center = TRUE)
fviz_eig(res.pca)
fviz_pca_var(res.pca, geom = c("arrow", "text")) # plot of variables
fviz_pca_ind(res.pca, geom = "text") # plot of individual data points
#row.names.d <- c("") #identify outliers
#d <- d[!(row.names(d) %in% row.names.d),] # remove outliers
#res.pca <- prcomp(d[,3:7], scale = TRUE, center = TRUE)
fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = d$Location, fill.ind = d$Location,
  pointsize = 3, invisible = "quali", labelsize = 3, repel = T) +
  #scale_x_continuous(limits=c(-5, 5)) + scale_y_continuous(limits=c(-2, 2)) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 12), axis.text = element_text(size = 12),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)

is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(IAB[,3:7],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
fviz_eig(res.pca)
eig <- get_eig(res.pca)
K_12_29_PCA_1a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_x_continuous(limits=c(-3.5, 3.1)) + scale_y_continuous(limits=c(-2.5, 2.5)) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
K_12_29_PCA_1a
pdf(here("analysis","work-in-progress","IAB-210722","IAB-K_12_29_PCA_1a.pdf"),
    width=3.5, height=3.5)
K_12_29_PCA_1a
dev.off()

res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, s[,3:7])
pred <- cbind(s[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

K_12_29_PCA_1b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="longdash") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="longdash") +
  geom_point(size =2, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-3.5, 3.1)) + scale_y_continuous(limits=c(-2.5, 2.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_12_29_PCA_1b
pdf(here("analysis","work-in-progress","IAB-210722","IAB-K_12_29_PCA_1b.pdf"),
    width=3.5, height=3.5)
K_12_29_PCA_1b
dev.off()


#### E_12_29 SI2 ####
joined_data_ranges[20,]
IAB <- dbGetQuery(georoc,
                  "SELECT *
                  FROM 'sample'
                  WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
                  `SIO2(WT%)` > 48.2 AND `SIO2(WT%)` < 51.2 AND
                  `K2O(WT%)` > 0.14 AND `K2O(WT%)` < 3.14 AND
                  (file_id = '2022-06-PVFZCE_BISMARCK_ARC_NEW_BRITAIN_ARC.csv' OR
                  file_id = '2022-06-PVFZCE_LUZON_ARC.csv' OR
                  file_id = '2022-06-PVFZCE_MARIANA_ARC.csv' OR
                  file_id = '2022-06-PVFZCE_NEW_HEBRIDES_ARC_VANUATU_ARCHIPELAGO.csv' OR
                  file_id = '2022-06-PVFZCE_SOLOMON_ISLAND_ARC.csv' OR
                  file_id = '2022-06-PVFZCE_SULAWESI_ARC.csv' OR
                  file_id = '2022-06-PVFZCE_SUNDA_ARC.csv' OR
                  file_id = '2022-06-PVFZCE_TONGA_ARC.csv')")
IAB <- dbGetQuery(georoc,
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

s <- joined_data %>%
  filter(Sample %in% c("K-12-29")) %>%
  mutate(Location = case_when(grepl("K-12-29", Sample) ~ "K-12-29"))

p1 <- IAB %>% ggplot(aes(x=Sr/Yb,y=Rb/Yb, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(position = "top", limits=c(0, 700)) +
  scale_y_continuous(limits=c(0, 45)) +
  labs(x=expression("Sr / Yb"), y=expression("Rb / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "none", aspect.ratio=1) +
  annotate("text", -Inf, Inf, label = "C", size = 7, hjust = -1, vjust = 2)
p2 <- IAB %>% ggplot(aes(x=Sr/Yb,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(0, 700)) +
  scale_y_continuous(limits=c(0, 500)) +
  labs(x=expression("Sr / Yb"), y=expression("Ba / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        legend.position = "none", aspect.ratio=1)
p3 <- IAB %>% ggplot(aes(x=Sr/Yb,y=Nb/La, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(0, 700)) +
  scale_y_continuous(limits=c(0, 1.5)) +
  labs(x=expression("Sr / Yb"), y=expression("Nb / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)

p4 <- IAB %>% ggplot(aes(x=Nd143_Nd144,y=Rb/Yb, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(position = "top", limits=c(.5126, .5131)) +
  scale_y_continuous(limits=c(0, 45)) +
  labs(x=expression({}^143*"Nd / "*{}^144*"Nd"), y=expression("Rb / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)
p5 <- IAB %>% ggplot(aes(x=Nd143_Nd144,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(.5126, .5131)) +
  scale_y_continuous(limits=c(0, 500)) +
  labs(x=expression({}^143*"Nd / "*{}^144*"Nd"), y=expression("Ba / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),
        legend.position = "none", aspect.ratio=1)
p6 <- IAB %>% ggplot(aes(x=Nd143_Nd144,y=Nb/La, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(.5126, .5131)) +
  scale_y_continuous(limits=c(0, 1.5)) +
  labs(x=expression({}^143*"Nd / "*{}^144*"Nd"), y=expression("Nb / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)

p7 <- IAB %>% ggplot(aes(x=Sr87_Sr86,y=Rb/Yb, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(position = "top", limits=c(.7028, .7069)) +
  scale_y_continuous(position = "right", limits=c(0, 45)) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"), y=expression("Rb / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)
p8 <- IAB %>% ggplot(aes(x=Sr87_Sr86,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(.7028, .7069)) +
  scale_y_continuous(position = "right", limits=c(0, 500)) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"), y=expression("Ba / Yb")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        axis.title.x = element_blank(), axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        legend.position = "none", aspect.ratio=1)
p9 <- IAB %>% ggplot(aes(x=Sr87_Sr86,y=Nb/La, shape=factor(Location), fill=factor(Location),
                         group=Sample)) + geom_point(size =2.5) +
  geom_point(data=s, size=2.5, stroke = .4) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_x_continuous(limits=c(.7028, .7069)) +
  scale_y_continuous(position = "right", limits=c(0, 1.5)) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"), y=expression("Nb / La")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none", aspect.ratio=1)

pdf(here("analysis","work-in-progress","IAB-210722","IAB-K_12_29_SI2.pdf"),
    width=12, height=12)
((p1/p2/p3) | (p4/p5/p6) | (p7/p8/p9)) +
  plot_layout(guides = "collect") & theme(legend.position = "none")
dev.off()


#### K_12_29 PCA2 ####
IAB <- IAB %>%
  dplyr::select(Sample,Location,
                TiO2,Al2O3,MgO,CaO,Na2O,Rb,Ba,Th,Nb,Ce,Nd,Sr,Sm,Zr,Yb)
s <- s %>%
  dplyr::select(Sample,Location,
                TiO2,Al2O3,MgO,CaO,Na2O,Rb,Ba,Th,Nb,Ce,Nd,Sr,Sm,Zr,Yb)
d <- full_join(IAB, s) %>%
  dplyr::select(Sample,Location,
                TiO2,Al2O3,MgO,CaO,Na2O,Rb,Ba,Th,Nb,Ce,Nd,Sr,Sm,Zr,Yb)

is.na(d) <- sapply(d, is.infinite) #replace Inf by NA
d[d == 0] <- NA # Replace 0 with NA
d <- d[rowSums(is.na(d)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(d[,3:17], scale = TRUE, center = TRUE)
fviz_eig(res.pca)
fviz_pca_var(res.pca, geom = c("arrow", "text")) # plot of variables
fviz_pca_ind(res.pca, geom = "text") # plot of individual data points
row.names.d <- c("756","755","754","1111","848") #identify outliers
d <- d[!(row.names(d) %in% row.names.d),] # remove outliers
res.pca <- prcomp(d[,3:17], scale = TRUE, center = TRUE)
fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = d$Location, fill.ind = d$Location,
  pointsize = 3, invisible = "quali", labelsize = 3, repel = T) +
  #scale_x_continuous(limits=c(-10, 10)) + scale_y_continuous(limits=c(-10, 10)) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols)

is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

cols <- c("Luzon Arc"="#440154","Sulawesi Arc"="#472374","Sunda Arc"="#404386",
          "Mariana Arc"="#218F8B","Bismarck Arc"="#25A782","Solomon Arc"="#44BE6F",
          "Vanuatu Arc"="#7AD04F","Tonga-Fiji"="#BADD26","K-12-29"="red")
shapes <- c("Luzon Arc"=21,"Sulawesi Arc"=22,"Sunda Arc"=23,"Mariana Arc"=21,
            "Bismarck Arc"=22,"Solomon Arc"=23,"Vanuatu Arc"=24,"Tonga-Fiji"=25,
            "K-12-29"=23)
contour <- c("Luzon Arc"="black","Sulawesi Arc"="black","Sunda Arc"="black",
             "Mariana Arc"="black","Bismarck Arc"="black","Solomon Arc"="black",
             "Vanuatu Arc"="black","Tonga-Fiji"="black","K-12-29"="black")

res.pca <- prcomp(IAB[,3:17], scale = TRUE, center = TRUE) # Dimension reduction using PCA
fviz_pca_ind(res.pca, geom = "text") # plot of individual data points
row.names.IAB <- c("756","755","754","1111","848") #identify outliers
IAB <- IAB[!(row.names(IAB) %in% row.names.IAB),] # remove outliers
res.pca <- prcomp(IAB[,3:17], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
K_12_29_PCA_2a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  #scale_x_continuous(limits=c(-7, 5), breaks=c(-6,-4,-2,0,2,4)) +
  #scale_y_continuous(limits=c(-4.5, 4.3), breaks=c(-4,-2,0,2,4)) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
K_12_29_PCA_2a
pdf(here("analysis","work-in-progress","IAB-210722","IAB-K_12_29_PCA_2a.pdf"),
    width=3.5, height=3.5)
K_12_29_PCA_2a
dev.off()

pred <- stats::predict(res.pca, s[,3:17])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

K_12_29_PCA_2b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="longdash") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="longdash") +
  geom_point(size =2, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(-7, 5), breaks=c(-6,-4,-2,0,2,4)) +
  #scale_y_continuous(limits=c(-4.5, 4.3), breaks=c(-4,-2,0,2,4)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)+
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_12_29_PCA_2b
pdf(here("analysis","work-in-progress","IAB-210722","IAB-K_12_29_PCA_2b.pdf"),
    width=3.5, height=3.5)
K_12_29_PCA_2b
dev.off()


#### K_12_29 multielements ####
joined_data_ranges[20,]

IAB <- dbGetQuery(georoc,
                  "SELECT *
                  FROM 'sample'
                  WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
                  (`SIO2(WT%)` > 48.2 AND `SIO2(WT%)` < 51.2 AND
                  `CS(PPM)` > 0.044 AND `CS(PPM)` < 0.176 AND
                  `RB(PPM)` > 4.85 AND `RB(PPM)` < 14.55 AND
                  `BA(PPM)` > 53.5 AND `BA(PPM)` < 160.5 AND
                  `TH(PPM)` > 0.6 AND `TH(PPM)` < 1.8 AND
                  `NB(PPM)` > 0.665 AND `NB(PPM)` < 1.99 AND
                  `CE(PPM)` > 8.9 AND `CE(PPM)` < 26.7 AND
                  `ND(PPM)` > 6.4 AND `ND(PPM)` < 19.2 AND
                  `SR(PPM)` > 269 AND `SR(PPM)` < 806 AND
                  `SM(PPM)` > 1.63 AND `SM(PPM)` < 4.9 AND
                  `ZR(PPM)` > 25.25 AND `ZR(PPM)` < 75.75 AND
                  `YB(PPM)` > 1.05 AND `YB(PPM)` < 3.15) AND
                  (file_id= '2022-06-PVFZCE_BISMARCK_ARC_-_NEW_BRITAIN_ARC.csv' OR
                  file_id= '2022-06-PVFZCE_LUZON_ARC.csv' OR
                  file_id= '2022-06-PVFZCE_MARIANA_ARC.csv' OR
                  file_id= '2022-06-PVFZCE_NEW_HEBRIDES_ARC_-_VANUATU_ARCHIPELAGO.csv' OR
                  file_id= '2022-06-PVFZCE_SOLOMON_ISLAND_ARC.csv' OR
                  file_id= '2022-06-PVFZCE_SULAWESI_ARC.csv' OR
                  file_id= '2022-06-PVFZCE_SUNDA_ARC.csv' OR
                  file_id= '2022-06-PVFZCE_TONGA_ARC.csv')") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
IAB[IAB == 0] <- NA # Replace 0 with NA

IAB_spider <- IAB %>% normalize_to_pm()

s_spider <- joined_data %>%
  filter(Sample %in% c("K-12-29")) %>%
  mutate(Location = case_when(grepl("K-12-29", Sample) ~ "K-12-29")) %>%
  normalize_to_pm()


E_12_29_spider <- IAB_spider %>%
  filter(Location %in% c("Bismarck Arc","Mariana Arc","Vanuatu Arc",
                         "Solomon Arc","Tonga-Fiji",
                         "Luzon Arc","Sulawesi Arc","Sunda Arc")) %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Pb","Nd","Sr","Sm","Zr","Hf","Ti","Eu","Gd","Tb",
                           "Dy","Ho","Y","Er","Li","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, color=Location, shape=Location, fill=Location,
             group=Sample)) +
  geom_line(size=1) +
  geom_point(color="black", size=3, stroke=.25) +
  geom_line(data=s_spider, size=1) +
  geom_point(data=s_spider, color="black", size=3, stroke=.25) +
  scale_shape_manual(values=shapes) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.5),
        axis.ticks.length.x = unit(.2, "cm"), axis.text = element_text(size=11),
        axis.title = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.position = "none") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), ) +
  scale_y_log10(breaks=c(0.1,1,10,100), limits=c(0.7,100), labels = scales::comma)+
  annotation_logticks(sides="l", size = .5, outside = TRUE, long = unit(0.2, "cm"),
                      mid = unit(0.1, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
E_12_29_spider
pdf(here("analysis","work-in-progress","IAB-210722","IAB-K_12_29_spider.pdf"),
    width=5, height=2)
E_12_29_spider
dev.off()


E_12_29_spider1 <- IAB_spider %>%
  filter(Location %in% c("Bismarck Arc","Mariana Arc","Vanuatu Arc",
                         "Solomon Arc","Tonga-Fiji")) %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Pb","Nd","Sr","Sm","Zr","Hf","Ti","Eu","Gd","Tb",
                           "Dy","Ho","Y","Er","Li","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, color=Location, shape=Location, fill=Location,
             group=Sample)) +
  geom_line(size=1) + geom_point(color="black", size=2) +
  geom_line(data=s_spider, size=1) + geom_point(data=s_spider, color="black", size=2) +
  scale_shape_manual(values=shapes) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols) +
  theme_classic() + theme(axis.line=element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 1), ) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.5),
        axis.ticks.length.x = unit(.2, "cm"), axis.text = element_text(size=11),
        axis.title = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.position = "none") +
  scale_y_log10(breaks=c(0.1,1,10,100), limits=c(0.7,50), labels = scales::comma)+
  #breaks = scales::trans_breaks("log10", function(x) 10^x),
  #labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides="l", size = .5, outside = TRUE, long = unit(0.2, "cm"),
                      mid = unit(0.1, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
E_12_29_spider1

  E_12_29_spider2 <- IAB_spider %>%
    filter(Location %in% c("Luzon Arc","Sulawesi Arc","Sunda Arc")) %>%
    mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Pb","Nd","Sr","Sm","Zr","Hf","Ti","Eu","Gd","Tb",
                           "Dy","Ho","Y","Er","Li","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, color=Location, shape=Location, fill=Location,
             group=Sample)) +
  geom_line(size=1) + geom_point(color="black", size=2) +
  geom_line(data=s_spider, size=1) + geom_point(data=s_spider, color="black", size=2) +
  scale_shape_manual(values=shapes) +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols) +
  theme_classic() + theme(axis.line=element_blank()) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2), ) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.5),
        axis.ticks.length.x = unit(.2, "cm"), axis.text = element_text(size=11),
        axis.title = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.position = "none") +
  scale_y_log10(breaks=c(0.1,1,10,100), limits=c(0.7,50), labels = scales::comma)+
  #breaks = scales::trans_breaks("log10", function(x) 10^x),
  #labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(sides="l", size = .5, outside = TRUE, long = unit(0.2, "cm"),
                      mid = unit(0.1, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")

E_12_29_spider2
