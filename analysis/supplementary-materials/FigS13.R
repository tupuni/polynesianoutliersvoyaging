require(here)
require(tidyverse)
require(RSQLite)
require(patchwork)
require(FactoMineR)
require(factoextra)
require(stats)

georoc <- dbConnect(RSQLite::SQLite(), path_to_georoc)
pofatu <- dbConnect(RSQLite::SQLite(), path_to_pofatu)


shapes <- c("Luzon Arc"=21,"Sulawesi Arc"=22,"Sunda Arc"=23,"Banda Arc"=24,
            "Yap Arc"=25,"Mariana Arc"=21,"Bismarck Arc"=22,"Solomon Arc"=23,
            "Vanuatu Arc"=24,"Tonga-Fiji"=25,"New Zealand"=21,
            "E-11-03"=0,"E-11-06"=1,"E-11-07"=8,"E-11-10"=0,"E-11-11"=1,
            "E-11-13"=2,"E-11-16"=5,"E-11-18"=6,"E-11-19"=8,"K-12-28"=3,"K-12-29"=4)
cols <- c("Luzon Arc"="#440154","Sulawesi Arc"="#345E8C","Sunda Arc"="#404386",
          "Banda Arc"="#472374","Yap Arc"="#29778E","Mariana Arc"="#218F8B",
          "Bismarck Arc"="#25A782","Solomon Arc"="#44BE6F","Vanuatu Arc"="#7AD04F",
          "Tonga-Fiji"="#BADD26","New Zealand"="#FDE725",
          "E-11-03"="red","E-11-06"="red","E-11-07"="red","E-11-10"="#7AD04F",
          "E-11-11"="#7AD04F","E-11-13"="#7AD04F","E-11-16"="#7AD04F",
          "E-11-18"="#7AD04F","E-11-19"="#7AD04F","K-12-28"="red","K-12-29"="red")
contour <- c("Luzon Arc"="black","Sulawesi Arc"="black","Sunda Arc"="black",
             "Banda Arc"="black","Yap Arc"="black","Mariana Arc"="black",
             "Bismarck Arc"="black","Solomon Arc"="black","Vanuatu Arc"="black",
             "Tonga-Fiji"="black","New Zealand"="black",
             "E-11-03"="red","E-11-06"="red","E-11-07"="red","E-11-10"="#7AD04F",
             "E-11-11"="#7AD04F","E-11-13"="#7AD04F","E-11-16"="#7AD04F",
             "E-11-18"="#7AD04F","E-11-19"="#7AD04F","K-12-28"="red","K-12-29"="red")

dir.create(here("analysis","supplementary-materials","FigS13"))

#### E_11_03 ####
IAB <- full_join(q2,q3) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>%
  filter(Sample %in% c("E-11-03")) %>%
  mutate(Location = case_when(grepl("E-11-03", Sample) ~ "E-11-03")) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)

res.pca <- prcomp(IAB[,3:21],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
pred <- stats::predict(res.pca, s[,3:21])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:33,"Sample"]),
  Location = c(d_pca[1:33,"Location"]),
  PC1 = c(sqrt(((d_pca[39,"PC1"])-d_pca[1:33,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[39,"PC2"])-d_pca[1:33,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[39,"PC3"])-d_pca[1:33,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[39,"PC4"])-d_pca[1:33,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[39,"PC5"])-d_pca[1:33,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[39,"PC6"])-d_pca[1:33,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[39,"PC7"])-d_pca[1:33,"PC7"])^2)),
  PC8 = c(sqrt(((d_pca[39,"PC8"])-d_pca[1:33,"PC8"])^2)),
  PC9 = c(sqrt(((d_pca[39,"PC9"])-d_pca[1:33,"PC9"])^2)),
  PC10 = c(sqrt(((d_pca[39,"PC10"])-d_pca[1:33,"PC10"])^2)),
  PC11 = c(sqrt(((d_pca[39,"PC11"])-d_pca[1:33,"PC11"])^2)),
  PC12 = c(sqrt(((d_pca[39,"PC12"])-d_pca[1:33,"PC12"])^2)),
  PC13 = c(sqrt(((d_pca[39,"PC13"])-d_pca[1:33,"PC13"])^2)),
  PC14 = c(sqrt(((d_pca[39,"PC14"])-d_pca[1:33,"PC14"])^2)),
  PC15 = c(sqrt(((d_pca[39,"PC15"])-d_pca[1:33,"PC15"])^2)),
  PC16 = c(sqrt(((d_pca[39,"PC16"])-d_pca[1:33,"PC16"])^2)),
  PC17 = c(sqrt(((d_pca[39,"PC17"])-d_pca[1:33,"PC17"])^2)),
  PC18 = c(sqrt(((d_pca[39,"PC18"])-d_pca[1:33,"PC18"])^2)),
  PC19 = c(sqrt(((d_pca[39,"PC19"])-d_pca[1:33,"PC19"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])+(PC8*eig[8,2])+(PC9*eig[9,2])+
      (PC10*eig[10,2])+(PC11*eig[11,2])+(PC12*eig[12,2])+(PC13*eig[13,2])+
      (PC14*eig[14,2])+(PC15*eig[15,2])+(PC16*eig[16,2])+(PC17*eig[17,2])+
      (PC18*eig[18,2])+(PC19*eig[19,2])) / (sum(eig[1:19,2])))

head(arrange(dist,weight_mean))

d <- dbGetQuery(pofatu,
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
WHERE (sample_id = 'reepmeyer2008_ANU9006' OR sample_id = 'reepmeyer2008_ANU9009'
OR sample_id = 'reepmeyer2008_ANU9008' OR sample_id = 'reepmeyer2008_ANU9003') AND
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
  mutate(Location = case_when(
    grepl("VANUATU", location_region) ~ "Vanuatu")) %>%
  mutate(Island = case_when(
    grepl("VANUA LAVA", location_subregion) ~ "Vanua Lava")) %>%
  dplyr::select(Sample,Location,Island,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu)

d_spider <- d %>%
  mutate(Location = case_when(
    grepl("reepmeyer2008_ANU9006", Sample) ~ "[ANU9006] Vanua Lava (Vanuatu)",
    grepl("reepmeyer2008_ANU9009", Sample) ~ "[ANU9009] Vanua Lava (Vanuatu)",
    grepl("reepmeyer2008_ANU9008", Sample) ~ "[ANU9008] Vanua Lava (Vanuatu)",
    grepl("reepmeyer2008_ANU9003", Sample) ~ "[ANU9003] Vanua Lava (Vanuatu)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>% filter(Sample %in% c("E-11-03")) %>%
  mutate(Location = case_when(grepl("E-11-03", Sample) ~ "E-11-03")) %>%
  mutate(Island = case_when(grepl("E-11-03", Sample) ~ "E-11-03")) %>%
  dplyr::select(Sample,Location,Island,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()


shapes <- c("[ANU9006] Vanua Lava (Vanuatu)"=0,
            "[ANU9009] Vanua Lava (Vanuatu)"=1,
            "[ANU9008] Vanua Lava (Vanuatu)"=2,
            "[ANU9003] Vanua Lava (Vanuatu)"=5,
            "E-11-03"=22)
cols <- c("[ANU9006] Vanua Lava (Vanuatu)"="#7AD04F",
          "[ANU9009] Vanua Lava (Vanuatu)"="#7AD04F",
          "[ANU9008] Vanua Lava (Vanuatu)"="#7AD04F",
          "[ANU9003] Vanua Lava (Vanuatu)"="#7AD04F",
          "E-11-03"="red")

E_11_03_spider <- d_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Nd","Sr","Sm","Zr","Ti","Eu","Gd","Tb",
                           "Dy","Y","Er","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=factor(Location), color=factor(Location),
             fill=factor(Location), group=Sample)) +
  geom_line(size=.5) + geom_point(size=2, stroke=.5) +
  geom_line(data=s_spider, size=.5) + geom_point(data=s_spider, size=2, stroke=.5) +
  scale_shape_manual(values=shapes) + scale_color_manual(values=cols) +
  scale_fill_manual(values=cols) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.25),
        axis.ticks.length.x = unit(-.15, "cm"), axis.text = element_text(size=8),
        axis.ticks.x.top = element_line(size=.25),
        axis.title = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.title = element_blank(),legend.text = element_text(size = 5),
        legend.key.size = unit(.2, 'cm'),
        legend.position = c(.74,.89), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.3,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")

pdf(here("analysis","supplementary-materials","FigS13","FigS13-a.pdf"), width=5, height=2)
E_11_03_spider
dev.off()


#### E_11_06 & E_11_07 ####
s <- joined_data %>% filter(Sample %in% c(
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
      grepl("E-11-07", Sample) ~ "E-11-07")) %>% dplyr::select(
        Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

IAB <- q5 %>% dplyr::select(
  Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
IAB <- full_join(IAB,s[3:7,])
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- s[1:2,]

res.pca <- prcomp(IAB[,3:21], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, s[,3:21])
pred <- cbind(s[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:191,"Sample"]),
  Location = c(d_pca[1:191,"Location"]),
  PC1 = c(sqrt(((median(d_pca[192:193,"PC1"]))-d_pca[1:191,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[192:193,"PC2"]))-d_pca[1:191,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[192:193,"PC3"]))-d_pca[1:191,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[192:193,"PC4"]))-d_pca[1:191,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[192:193,"PC5"]))-d_pca[1:191,"PC5"])^2)),
  PC6 = c(sqrt(((median(d_pca[192:193,"PC6"]))-d_pca[1:191,"PC6"])^2)),
  PC7 = c(sqrt(((median(d_pca[192:193,"PC7"]))-d_pca[1:191,"PC7"])^2)),
  PC8 = c(sqrt(((median(d_pca[192:193,"PC8"]))-d_pca[1:191,"PC8"])^2)),
  PC9 = c(sqrt(((median(d_pca[192:193,"PC9"]))-d_pca[1:191,"PC9"])^2)),
  PC10 = c(sqrt(((median(d_pca[192:193,"PC10"]))-d_pca[1:191,"PC10"])^2)),
  PC11 = c(sqrt(((median(d_pca[192:193,"PC11"]))-d_pca[1:191,"PC11"])^2)),
  PC12 = c(sqrt(((median(d_pca[192:193,"PC12"]))-d_pca[1:191,"PC12"])^2)),
  PC13 = c(sqrt(((median(d_pca[192:193,"PC13"]))-d_pca[1:191,"PC13"])^2)),
  PC14 = c(sqrt(((median(d_pca[192:193,"PC14"]))-d_pca[1:191,"PC14"])^2)),
  PC15 = c(sqrt(((median(d_pca[192:193,"PC15"]))-d_pca[1:191,"PC15"])^2)),
  PC16 = c(sqrt(((median(d_pca[192:193,"PC16"]))-d_pca[1:191,"PC16"])^2)),
  PC17 = c(sqrt(((median(d_pca[192:193,"PC17"]))-d_pca[1:191,"PC17"])^2)),
  PC18 = c(sqrt(((median(d_pca[192:193,"PC18"]))-d_pca[1:191,"PC18"])^2)),
  PC19 = c(sqrt(((median(d_pca[192:193,"PC19"]))-d_pca[1:191,"PC19"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])+(PC8*eig[8,2])+(PC9*eig[9,2])+
      (PC10*eig[10,2])+(PC11*eig[11,2])+(PC12*eig[12,2])+(PC13*eig[13,2])+
      (PC14*eig[14,2])+(PC15*eig[15,2])+(PC16*eig[16,2])+(PC17*eig[17,2])+
      (PC18*eig[18,2])+(PC19*eig[19,2])) / (sum(eig[1:19,2])))

head(arrange(dist,weight_mean))

d <- dbGetQuery(georoc,
"SELECT * FROM 'sample' WHERE id='1871808' OR id='115854-AMB 88'
OR id='1141527' OR id='70517'") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  mutate(Location = case_when(
    grepl("SULAWESI", file_id) ~ "Sulawesi",
    grepl("NEW_HEBRIDES_ARC", file_id) ~ "Vanuatu")) %>%
  mutate(Island = case_when(grepl("MANADO TUA", LOCATION) ~ "Manadotua",
                            grepl("GAUA", LOCATION) ~ "Gaua",
                            grepl("AMBRYM", LOCATION) ~ "Ambrym",
                            grepl("AMBAE", LOCATION) ~ "Ambae")) %>%
  dplyr::select(Sample,Location,Island,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
    Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu)
d_s <- joined_data %>% filter(Sample %in% c("E-11-11")) %>%
  mutate(Location = case_when(grepl("E-11-11", Sample) ~ "Vanuatu")) %>%
  mutate(Island = case_when(grepl("E-11-11", Sample) ~ "Emae")) %>%
  dplyr::select(Sample,Location,Island,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu)

d[,c("Sample","Location","Island")]
d_spider <- full_join(d,d_s) %>%
  mutate(Location = case_when(
    grepl("1141527", Sample) ~ "[1141527] Gaua (Vanuatu)",
    grepl("115854-AMB 88", Sample) ~ "[AMB 88] Ambrym (Vanuatu)",
    grepl("1871808", Sample) ~ "[1871808] Manadotua (Sulawesi)",
    grepl("70517", Sample) ~ "[70517] Ambae (Vanuatu)",
    grepl("E-11-11", Sample) ~ "[E-11-11] Emae (Vanuatu)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>% filter(Sample %in% c("E-11-06","E-11-07")) %>%
  mutate(Location = case_when(grepl("E-11-06", Sample) ~ "E-11-06",
                              grepl("E-11-07", Sample) ~ "E-11-07")) %>%
  mutate(Island = case_when(grepl("E-11-06", Sample) ~ "E-11-06",
                              grepl("E-11-07", Sample) ~ "E-11-07")) %>%
  dplyr::select(Sample,Location,Island,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()


shapes <- c("[E-11-11] Emae (Vanuatu)"=1,
            "[1141527] Gaua (Vanuatu)"=0,
            "[AMB 88] Ambrym (Vanuatu)"=2,
            "[1871808] Manadotua (Sulawesi)"=3,
            "[70517] Ambae (Vanuatu)"=5,
            "E-11-06"=1,"E-11-07"=8)
cols <- c("[E-11-11] Emae (Vanuatu)"="#7AD04F",
          "[1141527] Gaua (Vanuatu)"="#7AD04F",
          "[AMB 88] Ambrym (Vanuatu)"="#7AD04F",
          "[1871808] Manadotua (Sulawesi)"="#345E8C",
          "[70517] Ambae (Vanuatu)"="#7AD04F",
          "E-11-06"="red","E-11-07"="red")

E_11_06_07_spider <- d_spider %>%
  #dplyr::filter(Sample %in% "E-11-11") %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Nd","Sr","Sm","Zr","Ti","Eu","Gd","Tb",
                           "Dy","Y","Er","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=factor(Location), color=factor(Location),
             fill=factor(Location), group=Sample)) +
  geom_line(size=.5) + geom_point(size=2, stroke=.5) +
  geom_line(data=s_spider, size=.5) + geom_point(data=s_spider, size=2, stroke=.5) +
  scale_shape_manual(values=shapes) + scale_color_manual(values=cols) +
  scale_fill_manual(values=cols) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.25),
        axis.ticks.length.x = unit(-.15, "cm"), axis.text = element_text(size=8),
        axis.ticks.x.top = element_line(size=.25),
        axis.title = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.title = element_blank(),legend.text = element_text(size = 5),
        legend.key.size = unit(.2, 'cm'),
        legend.position = c(.74,.89), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")

pdf(here("analysis","supplementary-materials","FigS13","FigS13-b.pdf"), width=5, height=2)
E_11_06_07_spider
dev.off()


#### K_12_28 ####
IAB <- q7 %>% dplyr::select(
  Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% filter(Sample %in% c("K-12-28")) %>%
  mutate(Location = case_when(grepl("K-12-28", Sample) ~ "K-12-28")) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)

res.pca <- prcomp(IAB[,3:21], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
pred <- stats::predict(res.pca, s[,3:21])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:97,"Sample"]),
  Location = c(d_pca[1:97,"Location"]),
  PC1 = c(sqrt(((d_pca[98,"PC1"])-d_pca[1:97,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[98,"PC2"])-d_pca[1:97,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[98,"PC3"])-d_pca[1:97,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[98,"PC4"])-d_pca[1:97,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[98,"PC5"])-d_pca[1:97,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[98,"PC6"])-d_pca[1:97,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[98,"PC7"])-d_pca[1:97,"PC7"])^2)),
  PC8 = c(sqrt(((d_pca[98,"PC8"])-d_pca[1:97,"PC8"])^2)),
  PC9 = c(sqrt(((d_pca[98,"PC9"])-d_pca[1:97,"PC9"])^2)),
  PC10 = c(sqrt(((d_pca[98,"PC10"])-d_pca[1:97,"PC10"])^2)),
  PC11 = c(sqrt(((d_pca[98,"PC11"])-d_pca[1:97,"PC11"])^2)),
  PC12 = c(sqrt(((d_pca[98,"PC12"])-d_pca[1:97,"PC12"])^2)),
  PC13 = c(sqrt(((d_pca[98,"PC13"])-d_pca[1:97,"PC13"])^2)),
  PC14 = c(sqrt(((d_pca[98,"PC14"])-d_pca[1:97,"PC14"])^2)),
  PC15 = c(sqrt(((d_pca[98,"PC15"])-d_pca[1:97,"PC15"])^2)),
  PC16 = c(sqrt(((d_pca[98,"PC16"])-d_pca[1:97,"PC16"])^2)),
  PC17 = c(sqrt(((d_pca[98,"PC17"])-d_pca[1:97,"PC17"])^2)),
  PC18 = c(sqrt(((d_pca[98,"PC18"])-d_pca[1:97,"PC18"])^2)),
  PC19 = c(sqrt(((d_pca[98,"PC19"])-d_pca[1:97,"PC19"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])+(PC8*eig[8,2])+(PC9*eig[9,2])+
      (PC10*eig[10,2])+(PC11*eig[11,2])+(PC12*eig[12,2])+(PC13*eig[13,2])+
      (PC14*eig[14,2])+(PC15*eig[15,2])+(PC16*eig[16,2])+(PC17*eig[17,2])+
      (PC18*eig[18,2])+(PC19*eig[19,2])) / (sum(eig[1:19,2])))

head(arrange(dist,weight_mean))

d <- dbGetQuery(georoc,
 "SELECT * FROM 'sample'
WHERE id='13423-E5/11' OR id='13426-F7/2' OR id='13436-2654A'
OR id='13436-2649' OR id='13437-F5/2' OR id='634326'") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  mutate_at("LOCATION", str_replace,
            "BISMARCK ARC - NEW BRITAIN ARC / BISMARCK ARC - ", "") %>%
  rename(Location=LOCATION)%>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
d[,1:4]
d_spider <- d %>%
  mutate(Location = case_when(
    grepl("ULAWUN", Location) ~ "Ulawun volcano (New Britain)",
    grepl("LOLOBAU", Location) ~ "Lolobau Is. (New Britain)",
    grepl("SULU", Location) ~ "Sulu (New Britain)",
    grepl("WULAI", Location) ~ "Wulai Is. (New Britain)",
    grepl("MANAM", Location) ~ "Manam Is. (New Britain)")) %>%
  mutate(Sample = case_when(
    grepl("13423-E5/11", Sample) ~ "[E5/11] Ulawun volcano (New Britain)",
    grepl("13426-F7/2", Sample) ~ "[F7/2] Lolobau Is. (New Britain)",
    grepl("13436-2649", Sample) ~ "[2649] Sulu (New Britain)",
    grepl("13436-2654A", Sample) ~ "[2654A] Sulu (New Britain)",
    grepl("13437-F5/2", Sample) ~ "[F5/2] Wulai Is.(New Britain)",
    grepl("634326", Sample) ~ "[634326] Manam Is. (New Britain)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

d_spider %>% group_by(Location) %>% tally()

s_spider <- joined_data %>% filter(Sample %in% c("K-12-28")) %>%
  mutate(Location = case_when(grepl("K-12-28", Sample) ~ "K-12-28")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()


shapes <- c("[2649] Sulu (New Britain)"=0,
            "[2654A] Sulu (New Britain)"=1,
            "[E5/11] Ulawun volcano (New Britain)"=3,
            "[F7/2] Lolobau Is. (New Britain)"=5,
            "[F5/2] Wulai Is.(New Britain)"=2,
            "[634326] Manam Is. (New Britain)"=6,
            "K-12-28"=3)
cols <- c("[2649] Sulu (New Britain)"="#25A782",
          "[2654A] Sulu (New Britain)"="#25A782",
          "[E5/11] Ulawun volcano (New Britain)"="#25A782",
          "[F7/2] Lolobau Is. (New Britain)"="#25A782",
          "[F5/2] Wulai Is.(New Britain)"="#25A782",
          "[634326] Manam Is. (New Britain)"="#25A782",
          "K-12-28"="red")

K_12_28_spider <- d_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Nd","Sr","Sm","Zr","Ti","Eu","Gd","Tb",
                           "Dy","Y","Er","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=factor(Sample), color=factor(Sample),
             fill=factor(Sample), group=Sample)) +
  geom_line(size=.5) + geom_point(size=2, stroke=.5) +
  geom_line(data=s_spider, size=.5) + geom_point(data=s_spider, size=2, stroke=.5) +
  scale_shape_manual(values=shapes) + scale_color_manual(values=cols) +
  scale_fill_manual(values=cols) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.25),
        axis.ticks.length.x = unit(-.15, "cm"), axis.text = element_text(size=8),
        axis.ticks.x.top = element_line(size=.25),
        axis.title = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.title = element_blank(),legend.text = element_text(size = 5),
        legend.key.size = unit(.2, 'cm'),
        legend.position = c(.75,.84), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
K_12_28_spider

pdf(here("analysis","supplementary-materials","FigS13","FigS13-c.pdf"), width=5, height=2)
K_12_28_spider
dev.off()

citation <- dbGetQuery(georoc,
"SELECT sample_id, reference_id
FROM 'citation'
WHERE sample_id='13423-E5/11' OR sample_id='13437-F5/2'
OR sample_id='13436-2654A' OR sample_id='13436-2649'
OR sample_id='13437-F5/2' OR sample_id='634326'")
citation
reference <- dbGetQuery(georoc,
"SELECT id, reference
FROM 'reference'
WHERE id='2703' OR id='2496' OR id='4590' OR id='6900' OR id='16728'") %>%
  rename(reference_id=id)
reference
cite <- full_join(citation,reference)
cite

#### K_12_29 ####
IAB <- q9 %>%  dplyr::select(
  Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% filter(Sample %in% c("K-12-29")) %>%
  mutate(Location = case_when(grepl("K-12-29", Sample) ~ "K-12-29")) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)

res.pca <- prcomp(IAB[,3:21], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
pred <- stats::predict(res.pca, s[,3:21])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:28,"Sample"]),
  Location = c(d_pca[1:28,"Location"]),
  PC1 = c(sqrt(((d_pca[29,"PC1"])-d_pca[1:28,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[29,"PC2"])-d_pca[1:28,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[29,"PC3"])-d_pca[1:28,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[29,"PC4"])-d_pca[1:28,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[29,"PC5"])-d_pca[1:28,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[29,"PC6"])-d_pca[1:28,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[29,"PC7"])-d_pca[1:28,"PC7"])^2)),
  PC8 = c(sqrt(((d_pca[29,"PC8"])-d_pca[1:28,"PC8"])^2)),
  PC9 = c(sqrt(((d_pca[29,"PC9"])-d_pca[1:28,"PC9"])^2)),
  PC10 = c(sqrt(((d_pca[29,"PC10"])-d_pca[1:28,"PC10"])^2)),
  PC11 = c(sqrt(((d_pca[29,"PC11"])-d_pca[1:28,"PC11"])^2)),
  PC12 = c(sqrt(((d_pca[29,"PC12"])-d_pca[1:28,"PC12"])^2)),
  PC13 = c(sqrt(((d_pca[29,"PC13"])-d_pca[1:28,"PC13"])^2)),
  PC14 = c(sqrt(((d_pca[29,"PC14"])-d_pca[1:28,"PC14"])^2)),
  PC15 = c(sqrt(((d_pca[29,"PC15"])-d_pca[1:28,"PC15"])^2)),
  PC16 = c(sqrt(((d_pca[29,"PC16"])-d_pca[1:28,"PC16"])^2)),
  PC17 = c(sqrt(((d_pca[29,"PC17"])-d_pca[1:28,"PC17"])^2)),
  PC18 = c(sqrt(((d_pca[29,"PC18"])-d_pca[1:28,"PC18"])^2)),
  PC19 = c(sqrt(((d_pca[29,"PC19"])-d_pca[1:28,"PC19"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])+(PC8*eig[8,2])+(PC9*eig[9,2])+
      (PC10*eig[10,2])+(PC11*eig[11,2])+(PC12*eig[12,2])+(PC13*eig[13,2])+
      (PC14*eig[14,2])+(PC15*eig[15,2])+(PC16*eig[16,2])+(PC17*eig[17,2])+
      (PC18*eig[18,2])+(PC19*eig[19,2])) / (sum(eig[1:19,2])))

head(dist[order(dist$weight_mean),] %>%
       dplyr::select("Sample","Location","weight_mean"), 10)

### A MODIFIER
d <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE id = '1871790' OR id = '317050' OR id = '144138-KS094' OR id = '13306-VMAC6'
OR id = '1871790' OR id = '13303-UA10'") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  rename(Location=LOCATION)%>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
d[,1:4]
d_spider <- d %>%
  mutate(Location = case_when(
    grepl("70512", Sample) ~ "[70512] Vanua Lava (Vanuatu)",
    grepl("317050", Sample) ~ "[317050_AK9] Kibobo Is. (Lau, Fiji)",
    grepl("1871790", Sample) ~ "[1871790] Buhias Is. (Indonesia)",
    grepl("144138-KS094", Sample) ~ "[KS094] Cebu (Philippines)",
    grepl("13306-VMAC6", Sample) ~ "[VMAC6] Vanua Lava (Vanuatu)",
    grepl("13303-UA10", Sample) ~ "[UA10] Ureparapara (Vanuatu)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>% filter(Sample %in% c("K-12-29")) %>%
  mutate(Location = case_when(grepl("K-12-29", Sample) ~ "K-12-29")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()


shapes <- c("[70512] Vanua Lava (Vanuatu)"=0,
            "[VMAC6] Vanua Lava (Vanuatu)"=1,
            "[UA10] Ureparapara (Vanuatu)"=5,
            "[317050_AK9] Kibobo Is. (Lau, Fiji)"=3,
            "[1871790] Buhias Is. (Indonesia)"=6,
            "[KS094] Cebu (Philippines)"=2,
            "K-12-29"=4)
cols <- c("[70512] Vanua Lava (Vanuatu)"="#7AD04F",
          "[VMAC6] Vanua Lava (Vanuatu)"="#7AD04F",
          "[UA10] Ureparapara (Vanuatu)"="#7AD04F",
          "[317050_AK9] Kibobo Is. (Lau, Fiji)"="#BADD26",
          "[1871790] Buhias Is. (Indonesia)"="#345E8C",
          "[KS094] Cebu (Philippines)"="#440154",
          "K-12-29"="red")
K_12_29_spider <- d_spider %>%
  mutate(var = fct_relevel(var,
                         "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                         "Nd","Sr","Sm","Zr","Ti","Eu","Gd","Tb",
                         "Dy","Y","Er","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=factor(Location), color=factor(Location),
             fill=factor(Location), group=Sample)) +
  geom_line(size=.5) + geom_point(size=2, stroke=.5) +
  geom_line(data=s_spider, size=.5) + geom_point(data=s_spider, size=2, stroke=.5) +
  scale_shape_manual(values=shapes) + scale_color_manual(values=cols) +
  scale_fill_manual(values=cols) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.25),
        axis.ticks.length.x = unit(-.15, "cm"), axis.text = element_text(size=8),
        axis.ticks.x.top = element_line(size=.25),
        axis.title = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.title = element_blank(),legend.text = element_text(size = 5),
        legend.key.size = unit(.2, 'cm'),
        legend.position = c(.75,.84), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
K_12_29_spider

citation <- dbGetQuery(georoc,
"SELECT sample_id, reference_id
FROM 'citation'
WHERE sample_id = '70512' OR sample_id = '317050'
OR sample_id = '144138-KS094' OR sample_id = '13306-VMAC6'
OR sample_id = '1871790' OR sample_id = '13303-UA10'")
citation
reference <- dbGetQuery(georoc,
"SELECT id, reference
FROM 'reference'
WHERE id='24095' OR id='19607' OR id='10608' OR id='4032'
OR id='16735' OR id='10898'") %>% rename(reference_id=id)
reference
cite <- full_join(citation,reference)
cite


#### fig min max ####
V <- q9 %>% dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                           Ce,Pr,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  dplyr::filter(Location %in% c("Vanuatu Arc")) %>% dplyr::na_if(0)
V_minmax <- data.frame (Sample  = c("Vanuatu_min", "Vanuatu_max"),
                        Location = c("Vanuatu Arc", "Vanuatu Arc"),
                        Cs = c(min(V[,"Cs"],na.rm=TRUE),max(V[,"Cs"],na.rm=TRUE)),
                        Rb = c(min(V[,"Rb"],na.rm=TRUE),max(V[,"Rb"],na.rm=TRUE)),
                        Ba = c(min(V[,"Ba"],na.rm=TRUE),max(V[,"Ba"],na.rm=TRUE)),
                        Th = c(min(V[,"Th"],na.rm=TRUE),max(V[,"Th"],na.rm=TRUE)),
                        U = c(min(V[,"U"],na.rm=TRUE),max(V[,"U"],na.rm=TRUE)),
                        Nb = c(min(V[,"Nb"],na.rm=TRUE),max(V[,"Nb"],na.rm=TRUE)),
                        Ta = c(min(V[,"Ta"],na.rm=TRUE),max(V[,"Ta"],na.rm=TRUE)),
                        La = c(min(V[,"La"],na.rm=TRUE),max(V[,"La"],na.rm=TRUE)),
                        Ce = c(min(V[,"Ce"],na.rm=TRUE),max(V[,"Ce"],na.rm=TRUE)),
                        Pr = c(min(V[,"Pr"],na.rm=TRUE),max(V[,"Pr"],na.rm=TRUE)),
                        Nd = c(min(V[,"Nd"],na.rm=TRUE),max(V[,"Nd"],na.rm=TRUE)),
                        Sr = c(min(V[,"Sr"],na.rm=TRUE),max(V[,"Sr"],na.rm=TRUE)),
                        Sm = c(min(V[,"Sm"],na.rm=TRUE),max(V[,"Sm"],na.rm=TRUE)),
                        Zr = c(min(V[,"Zr"],na.rm=TRUE),max(V[,"Zr"],na.rm=TRUE)),
                        Ti = c(min(V[,"Ti"],na.rm=TRUE),max(V[,"Ti"],na.rm=TRUE)),
                        Eu = c(min(V[,"Eu"],na.rm=TRUE),max(V[,"Eu"],na.rm=TRUE)),
                        Gd = c(min(V[,"Gd"],na.rm=TRUE),max(V[,"Gd"],na.rm=TRUE)),
                        Tb = c(min(V[,"Tb"],na.rm=TRUE),max(V[,"Tb"],na.rm=TRUE)),
                        Dy = c(min(V[,"Dy"],na.rm=TRUE),max(V[,"Dy"],na.rm=TRUE)),
                        Y = c(min(V[,"Y"],na.rm=TRUE),max(V[,"Y"],na.rm=TRUE)),
                        Er = c(min(V[,"Er"],na.rm=TRUE),max(V[,"Er"],na.rm=TRUE)),
                        Yb = c(min(V[,"Yb"],na.rm=TRUE),max(V[,"Yb"],na.rm=TRUE)),
                        Lu = c(min(V[,"Lu"],na.rm=TRUE),max(V[,"Lu"],na.rm=TRUE)))

TF <- q9 %>% dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                          Ce,Pr,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  dplyr::filter(Location %in% c("Tonga-Fiji")) %>% dplyr::na_if(0)
TF_minmax <- data.frame (Sample  = c("Tonga-Fiji_min", "Tonga-Fiji_max"),
                        Location = c("Tonga-Fiji", "Tonga-Fiji"),
                        Cs = c(min(TF[,"Cs"],na.rm=TRUE),max(TF[,"Cs"],na.rm=TRUE)),
                        Rb = c(min(TF[,"Rb"],na.rm=TRUE),max(TF[,"Rb"],na.rm=TRUE)),
                        Ba = c(min(TF[,"Ba"],na.rm=TRUE),max(TF[,"Ba"],na.rm=TRUE)),
                        Th = c(min(TF[,"Th"],na.rm=TRUE),max(TF[,"Th"],na.rm=TRUE)),
                        U = c(min(TF[,"U"],na.rm=TRUE),max(TF[,"U"],na.rm=TRUE)),
                        Nb = c(min(TF[,"Nb"],na.rm=TRUE),max(TF[,"Nb"],na.rm=TRUE)),
                        Ta = c(min(TF[,"Ta"],na.rm=TRUE),max(TF[,"Ta"],na.rm=TRUE)),
                        La = c(min(TF[,"La"],na.rm=TRUE),max(TF[,"La"],na.rm=TRUE)),
                        Ce = c(min(TF[,"Ce"],na.rm=TRUE),max(TF[,"Ce"],na.rm=TRUE)),
                        Pr = c(min(TF[,"Pr"],na.rm=TRUE),max(TF[,"Pr"],na.rm=TRUE)),
                        Nd = c(min(TF[,"Nd"],na.rm=TRUE),max(TF[,"Nd"],na.rm=TRUE)),
                        Sr = c(min(TF[,"Sr"],na.rm=TRUE),max(TF[,"Sr"],na.rm=TRUE)),
                        Sm = c(min(TF[,"Sm"],na.rm=TRUE),max(TF[,"Sm"],na.rm=TRUE)),
                        Zr = c(min(TF[,"Zr"],na.rm=TRUE),max(TF[,"Zr"],na.rm=TRUE)),
                        Ti = c(min(TF[,"Ti"],na.rm=TRUE),max(TF[,"Ti"],na.rm=TRUE)),
                        Eu = c(min(TF[,"Eu"],na.rm=TRUE),max(TF[,"Eu"],na.rm=TRUE)),
                        Gd = c(min(TF[,"Gd"],na.rm=TRUE),max(TF[,"Gd"],na.rm=TRUE)),
                        Tb = c(min(TF[,"Tb"],na.rm=TRUE),max(TF[,"Tb"],na.rm=TRUE)),
                        Dy = c(min(TF[,"Dy"],na.rm=TRUE),max(TF[,"Dy"],na.rm=TRUE)),
                        Y = c(min(TF[,"Y"],na.rm=TRUE),max(TF[,"Y"],na.rm=TRUE)),
                        Er = c(min(TF[,"Er"],na.rm=TRUE),max(TF[,"Er"],na.rm=TRUE)),
                        Yb = c(min(TF[,"Yb"],na.rm=TRUE),max(TF[,"Yb"],na.rm=TRUE)),
                        Lu = c(min(TF[,"Lu"],na.rm=TRUE),max(TF[,"Lu"],na.rm=TRUE)))

L <- q9 %>% dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                           Ce,Pr,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  dplyr::filter(Location %in% c("Luzon Arc")) %>% dplyr::na_if(0)
L_minmax <- data.frame (Sample  = c("Luzon_min", "Luzon_max"),
                         Location = c("Luzon Arc", "Luzon Arc"),
                         Cs = c(min(L[,"Cs"],na.rm=TRUE),max(L[,"Cs"],na.rm=TRUE)),
                         Rb = c(min(L[,"Rb"],na.rm=TRUE),max(L[,"Rb"],na.rm=TRUE)),
                         Ba = c(min(L[,"Ba"],na.rm=TRUE),max(L[,"Ba"],na.rm=TRUE)),
                         Th = c(min(L[,"Th"],na.rm=TRUE),max(L[,"Th"],na.rm=TRUE)),
                         U = c(min(L[,"U"],na.rm=TRUE),max(L[,"U"],na.rm=TRUE)),
                         Nb = c(min(L[,"Nb"],na.rm=TRUE),max(L[,"Nb"],na.rm=TRUE)),
                         Ta = c(min(L[,"Ta"],na.rm=TRUE),max(L[,"Ta"],na.rm=TRUE)),
                         La = c(min(L[,"La"],na.rm=TRUE),max(L[,"La"],na.rm=TRUE)),
                         Ce = c(min(L[,"Ce"],na.rm=TRUE),max(L[,"Ce"],na.rm=TRUE)),
                         Pr = c(min(L[,"Pr"],na.rm=TRUE),max(L[,"Pr"],na.rm=TRUE)),
                         Nd = c(min(L[,"Nd"],na.rm=TRUE),max(L[,"Nd"],na.rm=TRUE)),
                         Sr = c(min(L[,"Sr"],na.rm=TRUE),max(L[,"Sr"],na.rm=TRUE)),
                         Sm = c(min(L[,"Sm"],na.rm=TRUE),max(L[,"Sm"],na.rm=TRUE)),
                         Zr = c(min(L[,"Zr"],na.rm=TRUE),max(L[,"Zr"],na.rm=TRUE)),
                         Ti = c(min(L[,"Ti"],na.rm=TRUE),max(L[,"Ti"],na.rm=TRUE)),
                         Eu = c(min(L[,"Eu"],na.rm=TRUE),max(L[,"Eu"],na.rm=TRUE)),
                         Gd = c(min(L[,"Gd"],na.rm=TRUE),max(L[,"Gd"],na.rm=TRUE)),
                         Tb = c(min(L[,"Tb"],na.rm=TRUE),max(L[,"Tb"],na.rm=TRUE)),
                         Dy = c(min(L[,"Dy"],na.rm=TRUE),max(L[,"Dy"],na.rm=TRUE)),
                         Y = c(min(L[,"Y"],na.rm=TRUE),max(L[,"Y"],na.rm=TRUE)),
                         Er = c(min(L[,"Er"],na.rm=TRUE),max(L[,"Er"],na.rm=TRUE)),
                         Yb = c(min(L[,"Yb"],na.rm=TRUE),max(L[,"Yb"],na.rm=TRUE)),
                         Lu = c(min(L[,"Lu"],na.rm=TRUE),max(L[,"Lu"],na.rm=TRUE)))

d <- full_join(V_minmax,TF_minmax)
d <- full_join(d,L_minmax)
d_spider <- d %>% normalize_to_pm()

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("K-12-29")) %>%
  mutate(Location = case_when(grepl("K-12-29", Sample) ~ "K-12-29")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

shapes <- c("Vanuatu Arc"=0,
            "Tonga-Fiji"=1,
            "Luzon Arc"=2,
            "K-12-29"=4)
cols <- c("Vanuatu Arc"="#7AD04F",
          "Tonga-Fiji"="#BADD26",
          "Luzon Arc"="#440154",
          "K-12-29"="red")

K_12_29_spider_minmax <- d_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Nd","Sr","Sm","Zr","Ti","Eu","Gd","Tb",
                           "Dy","Y","Er","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=factor(Location), color=factor(Location),
             fill=factor(Location), group=Sample)) +
  geom_line(size=.5) +
  geom_line(data=s_spider, size=.5) + geom_point(data=s_spider, size=1, stroke=.5) +
  scale_shape_manual(values=shapes) + scale_color_manual(values=cols) +
  scale_fill_manual(values=cols) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.25),
        axis.ticks.length.x = unit(-.15, "cm"), axis.text = element_text(size=8),
        axis.ticks.x.top = element_line(size=.25),
        axis.title = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust=1, margin = margin(r=5)),
        legend.title = element_blank(),legend.text = element_text(size = 5),
        legend.key.size = unit(.2, 'cm'),
        legend.position = c(.7,.86), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.01,10000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
K_12_29_spider_minmax

pdf(here("analysis","supplementary-materials","FigS20","FigS20-i.pdf"), width=3.5, height=2)
K_12_26_spider_minmax
dev.off()


pdf(here("analysis","supplementary-materials","FigS13","FigS13-d.pdf"), width=5, height=2)
K_12_29_spider
dev.off()


