require(here)
require(tidyverse)
require(RSQLite)
require(patchwork)
require(FactoMineR)
require(factoextra)
require(stats)

georoc <- dbConnect(RSQLite::SQLite(), path_to_georoc)
pofatu <- dbConnect(RSQLite::SQLite(), path_to_pofatu)

dir.create(here("analysis","supplementary-materials","FigS13"))

#### IAB ####

#### E_11_03 ####
IAB <- full_join(q2,q3) %>%
  dplyr::select(
    Sample,Location,Cs,Rb,Ba,Nb,La,Zr,Ti)
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>%
  filter(Sample %in% c("E-11-03")) %>%
  mutate(Location = case_when(grepl("E-11-03", Sample) ~ "E-11-03")) %>%
  dplyr::select(
    Sample,Location,Cs,Rb,Ba,Nb,La,Zr,Ti)

res.pca <- prcomp(IAB[,3:9],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
pred <- stats::predict(res.pca, s[,3:9])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

dist <- data.frame(
  Sample = c(d_pca[1:24,"Sample"]),
  Location = c(d_pca[1:24,"Location"]),
  PC1 = c(sqrt(((d_pca[25,"PC1"])-d_pca[1:24,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[25,"PC2"])-d_pca[1:24,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[25,"PC3"])-d_pca[1:24,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[25,"PC4"])-d_pca[1:24,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[25,"PC5"])-d_pca[1:24,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[25,"PC6"])-d_pca[1:24,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[25,"PC7"])-d_pca[1:24,"PC7"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])) / (sum(eig[1:7,2])))
head(dist[order(dist$weight_mean),] %>% dplyr::select("Sample","Location","weight_mean"))

#### plot ####
d <- dbGetQuery(pofatu,
"SELECT s.id AS sample_id, s.location_region, s.location_subregion,
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
max(CASE WHEN m.parameter='U [ppm]' then m.value END) AS 'U [ppm]'
FROM 'samples.csv' AS s JOIN 'measurements.csv' AS m ON s.id=m.sample_id
WHERE (sample_id = 'reepmeyer2008_ANU9005' OR
sample_id = 'reepmeyer2008_ANU9014' OR
sample_id = 'reepmeyer2008_ANU9007') AND
m.parameter IN ('SiO2 [%]', 'TiO2 [%]', 'Al2O3 [%]', 'Fe2O3 [%]', 'FeO [%]',
'MnO [%]', 'MgO [%]', 'CaO [%]', 'Na2O [%]', 'K2O [%]',
'Li [ppm]', 'Sc [ppm]', 'Ti [ppm]', 'V [ppm]',
'Cr [ppm]', 'Co [ppm]', 'Ni [ppm]', 'Cu [ppm]', 'Zn [ppm]', 'As [ppm]',
'Rb [ppm]', 'Sr [ppm]', 'Y [ppm]', 'Zr [ppm]', 'Nb [ppm]', 'Cd [ppm]',
'Cs [ppm]', 'Ba [ppm]', 'La [ppm]', 'Ce [ppm]', 'Pr [ppm]', 'Nd [ppm]',
'Sm [ppm]', 'Eu [ppm]', 'Gd [ppm]', 'Tb [ppm]', 'Dy [ppm]', 'Ho [ppm]',
'Er [ppm]', 'Tm [ppm]', 'Yb [ppm]', 'Lu [ppm]', 'Hf [ppm]', 'Ta [ppm]',
'Pb [ppm]', 'Th [ppm]', 'U [ppm]') GROUP BY sample_id") %>%
  rename_pofatu_elements() %>%
  rename(Location=location_region, Island=location_subregion) %>%
  Ti_from_TiO2() %>% K_from_K2O() %>% Fe2O3_from_FeO() %>% dplyr::select(
    Sample,Location,Island,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
    Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu)

d_spider <- d %>%
  mutate(Location = case_when(
    grepl("reepmeyer2008_ANU9005", Sample) ~ "[ANU9005] Vanua Lava (Vanuatu)",
    grepl("reepmeyer2008_ANU9007", Sample) ~ "[ANU9007] Vanua Lava (Vanuatu)",
    grepl("reepmeyer2008_ANU9014", Sample) ~ "[ANU9014] Vanua Lava (Vanuatu)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>% filter(Sample %in% c("E-11-03")) %>%
  mutate(Location = case_when(grepl("E-11-03", Sample) ~ "E-11-03")) %>%
  mutate(Island = case_when(grepl("E-11-03", Sample) ~ "E-11-03")) %>%
  dplyr::select(Sample,Location,Island,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()


shapes <- c("[ANU9005] Vanua Lava (Vanuatu)"=0,
            "[ANU9007] Vanua Lava (Vanuatu)"=1,
            "[ANU9014] Vanua Lava (Vanuatu)"=5,
            "E-11-03"=22)
cols <- c("[ANU9005] Vanua Lava (Vanuatu)"="#7AD04F",
            "[ANU9007] Vanua Lava (Vanuatu)"="#7AD04F",
            "[ANU9014] Vanua Lava (Vanuatu)"="#7AD04F",
            "E-11-03"="red")

E_11_03_spider <- d_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Nd","Sr","Sm","Zr","Ti","Eu","Gd","Tb",
                           "Dy","Y","Er","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=factor(Location), color=factor(Location),
             fill=factor(Location), group=Sample)) +
  geom_line(size=.5) + geom_point(size=2, stroke=.5) +
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
        legend.position = c(.74,.89), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.3,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")

pdf(here("analysis","supplementary-materials","FigS13","FigS13-a.pdf"), width=3, height=2)
E_11_03_spider
dev.off()


#### E_11_06 & E_11_07 ####
s <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-18dup",
  "E-11-19","E-11-06","E-11-07")) %>%
  mutate(
    Location = case_when(
      grepl("E-11-10", Sample) ~ "Vanuatu Arc",
      grepl("E-11-11", Sample) ~ "Vanuatu Arc",
      grepl("E-11-13", Sample) ~ "Vanuatu Arc",
      grepl("E-11-16", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18", Sample) ~ "Vanuatu Arc",
      grepl("E-11-06", Sample) ~ "E-11-06",
      grepl("E-11-07", Sample) ~ "E-11-07")) %>%
  dplyr::select(Sample,Location,Cs,Rb,Ba,Nb,La,Zr,Ti)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA
s <- s[c("3","4","5","6","7","1","2"),]

IAB <- q5 %>% dplyr::select(Sample,Location,Cs,Rb,Ba,Nb,La,Zr,Ti)
IAB <- full_join(IAB,s[1:5,])
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- s[6:7,]

res.pca <- prcomp(IAB[,3:9],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
pred <- stats::predict(res.pca, s[,3:9])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

dist <- data.frame(
  Sample = c(d_pca[1:86,"Sample"]),
  Location = c(d_pca[1:86,"Location"]),
  PC1 = c(sqrt(((d_pca[87:88,"PC1"])-d_pca[1:86,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[87:88,"PC2"])-d_pca[1:86,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[87:88,"PC3"])-d_pca[1:86,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[87:88,"PC4"])-d_pca[1:86,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[87:88,"PC5"])-d_pca[1:86,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[87:88,"PC6"])-d_pca[1:86,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[87:88,"PC7"])-d_pca[1:86,"PC7"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])) / (sum(eig[1:7,2])))
head(dist[order(dist$weight_mean),] %>% dplyr::select("Sample","Location","weight_mean"))

#### plot ####
d <- dbGetQuery(georoc,
"SELECT * FROM 'sample' WHERE id='70518'") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  mutate(Location = case_when(
    grepl("NEW_HEBRIDES_ARC", file_id) ~ "Vanuatu")) %>%
  mutate(Island = case_when(grepl("AMBAE", LOCATION) ~ "Ambae")) %>%
  dplyr::select(Sample,Location,Island,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
    Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu)
d_s <- joined_data %>% filter(Sample %in% c("E-11-11")) %>%
  mutate(Location = case_when(grepl("E-11-11", Sample) ~ "Vanuatu")) %>%
  mutate(Island = case_when(grepl("E-11-11", Sample) ~ "Emae")) %>%
  dplyr::select(Sample,Location,Island,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu)
d_spider <- full_join(d,d_s) %>%
  mutate(Location = case_when(
    grepl("70518", Sample) ~ "[70518] Ambae (Vanuatu)",
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


shapes <- c("[E-11-11] Emae (Vanuatu)"=1,"E-11-06"=1,"E-11-07"=8)
cols <- c("[E-11-11] Emae (Vanuatu)"="#7AD04F","E-11-06"="red","E-11-07"="red")

E_11_06_07_spider <- d_spider %>%
  dplyr::filter(Sample %in% "E-11-11") %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Nd","Sr","Sm","Zr","Ti","Eu","Gd","Tb",
                           "Dy","Y","Er","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=factor(Location), color=factor(Location),
             fill=factor(Location), group=Sample)) +
  geom_line(size=.5) + geom_point(size=2, stroke=.5) +
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
        legend.position = c(.75,.88), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")

pdf(here("analysis","supplementary-materials","FigS13","FigS13-b.pdf"), width=3, height=2)
E_11_06_07_spider
dev.off()


#### K_12_28 ####
ranges_s_IAB[4,1:35]
IAB <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
`SIO2(WT%)` > 52.1 AND `SIO2(WT%)` < 55.1 AND
file_id= '2022-06-PVFZCE_BISMARCK_ARC_NEW_BRITAIN_ARC.csv'") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  mutate_at("LOCATION", str_replace,
            "BISMARCK ARC - NEW BRITAIN ARC / BISMARCK ARC - ", "") %>%
  rename(Location=LOCATION)%>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
IAB %>% group_by(Location) %>% tally()

IAB <- IAB %>% dplyr::select(Sample,Location,Th,Nb,La,Nd,Sr,Zr,Eu)
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% filter(Sample %in% c("K-12-28")) %>%
  mutate(Location = case_when(grepl("K-12-28", Sample) ~ "K-12-28")) %>%
  dplyr::select(Sample,Location,Th,Nb,La,Nd,Sr,Zr,Eu)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(IAB[,3:9], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
pred <- stats::predict(res.pca, s[,3:9])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

dist <- data.frame(
  Sample = c(d_pca[1:56,"Sample"]),
  Location = c(d_pca[1:56,"Location"]),
  PC1 = c(sqrt(((d_pca[57,"PC1"])-d_pca[1:56,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[57,"PC2"])-d_pca[1:56,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[57,"PC3"])-d_pca[1:56,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[57,"PC4"])-d_pca[1:56,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[57,"PC5"])-d_pca[1:56,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[57,"PC6"])-d_pca[1:56,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[57,"PC7"])-d_pca[1:56,"PC7"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])) / (sum(eig[1:7,2])))

head(dist[order(dist$weight_mean),] %>%
       dplyr::select("Sample","Location","weight_mean"), 10)

#### plot ####
ranges_s_IAB[4,1:35]
d <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE id='13440-GN1/4' OR id='634326' OR
id='634330' OR id='634323' OR id='634331'") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  mutate_at("LOCATION", str_replace,
            "BISMARCK ARC - NEW BRITAIN ARC / BISMARCK ARC - ", "") %>%
  rename(Location=LOCATION)%>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
d[,1:2]
d_spider <- d %>%
  mutate(Location = case_when(
    grepl("DAKATAUA", Location) ~ "Dakatau (New Britain)",
    grepl("MANAM", Location) ~ "Manam Is. (New Britain)")) %>%
  mutate(Sample = case_when(
    grepl("13440-GN1/4", Sample) ~ "[GN1/4] Dakatau (New Britain)",
    grepl("634323", Sample) ~ "[634323] Manam Is. (New Britain)",
    grepl("634326", Sample) ~ "[634326] Manam Is. (New Britain)",
    grepl("634330", Sample) ~ "[634330] Manam Is. (New Britain)",
    grepl("634331", Sample) ~ "[634331] Manam Is. (New Britain)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

d_spider %>% group_by(Location) %>% tally()

s_spider <- joined_data %>% filter(Sample %in% c("K-12-28")) %>%
  mutate(Location = case_when(grepl("K-12-28", Sample) ~ "K-12-28")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()


shapes <- c("[GN1/4] Dakatau (New Britain)"=0,
            "[634323] Manam Is. (New Britain)"=1,
            "[634326] Manam Is. (New Britain)"=2,
            "[634330] Manam Is. (New Britain)"=5,
            "[634331] Manam Is. (New Britain)"=6,
            "K-12-28"=3)
cols <- c("[GN1/4] Dakatau (New Britain)"="#25A782",
          "[634323] Manam Is. (New Britain)"="#25A782",
          "[634326] Manam Is. (New Britain)"="#25A782",
          "[634330] Manam Is. (New Britain)"="#25A782",
          "[634331] Manam Is. (New Britain)"="#25A782",
          "K-12-28"="red")

K_12_28_spider <- d_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Nd","Sr","Sm","Zr","Ti","Eu","Gd","Tb",
                           "Dy","Y","Er","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=factor(Sample), color=factor(Sample),
             fill=factor(Sample), group=Sample)) +
  geom_line(size=.5) + geom_point(size=2, stroke=.5) +
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
        legend.position = c(.75,.84), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
K_12_28_spider

pdf(here("analysis","supplementary-materials","FigS13","FigS13-c.pdf"), width=3, height=2)
K_12_28_spider
dev.off()

#### K_12_29 ####
ranges_s_IAB[5,1:35]
IAB <- dbGetQuery(georoc,
"SELECT *
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
`SIO2(WT%)` > 48.2 AND `SIO2(WT%)` < 51.2 AND
`K2O(WT%)` > 0.14 AND `K2O(WT%)` < 3.14 AND
`RB(PPM)` > 4.85 AND `RB(PPM)` < 14.55 AND
`NB(PPM)` > 0.665 AND `NB(PPM)` < 1.995 AND
`SR(PPM)` > 269 AND `SR(PPM)` < 806 AND
`SM(PPM)` > 1.635 AND `SM(PPM)` < 4.905 AND
`YB(PPM)` > 1.05 AND `YB(PPM)` < 3.15 AND
(file_id= '2022-06-PVFZCE_YAP_ARC.csv' OR
file_id= '2022-06-PVFZCE_BISMARCK_ARC_NEW_BRITAIN_ARC.csv' OR
file_id= '2022-06-PVFZCE_LUZON_ARC.csv' OR
file_id= '2022-06-PVFZCE_NEW_HEBRIDES_ARC_VANUATU_ARCHIPELAGO.csv' OR
file_id= '2022-06-PVFZCE_SULAWESI_ARC.csv')") %>%
  get_georoc_location() %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
IAB %>% group_by(Location) %>% tally()

IAB <- IAB %>% dplyr::select(Sample,Location,Cs,Rb,Nb,La,Nd,Sr,Zr)
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% filter(Sample %in% c("K-12-29")) %>%
  mutate(Location = case_when(grepl("K-12-29", Sample) ~ "K-12-29")) %>%
  dplyr::select(Sample,Location,Cs,Rb,Nb,La,Nd,Sr,Zr)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(IAB[,3:9], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
pred <- stats::predict(res.pca, s[,3:9])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

dist <- data.frame(
  Sample = c(d_pca[1:30,"Sample"]),
  Location = c(d_pca[1:30,"Location"]),
  PC1 = c(sqrt(((d_pca[31,"PC1"])-d_pca[1:30,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[31,"PC2"])-d_pca[1:30,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[31,"PC3"])-d_pca[1:30,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[31,"PC4"])-d_pca[1:30,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[31,"PC5"])-d_pca[1:30,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[31,"PC6"])-d_pca[1:30,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[31,"PC7"])-d_pca[1:30,"PC7"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])) / (sum(eig[1:7,2])))

head(dist[order(dist$weight_mean),] %>%
       dplyr::select("Sample","Location","weight_mean"), 10)

#### plot ####
ranges_s_IAB[4,1:35]
d <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE id='144138-KS094' OR id='527524' OR
id='70512' OR id='527523'") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  rename(Location=LOCATION)%>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
d[,1:3]
d_spider <- d %>%
  mutate(Location = case_when(
    grepl("144138-KS094", Sample) ~ "[KS094] Cebu (Philippines)",
    grepl("527523", Sample) ~ "[527523] (Palau)",
    grepl("527524", Sample) ~ "[527524] (Palau)",
    grepl("70512", Sample) ~ "[70512] Vanua Lava (Vanuatu)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>% filter(Sample %in% c("K-12-29")) %>%
  mutate(Location = case_when(grepl("K-12-29", Sample) ~ "K-12-29")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()


shapes <- c("[KS094] Cebu (Philippines)"=0,
            "[527523] (Palau)"=1,"[527524] (Palau)"=2,
            "[70512] Vanua Lava (Vanuatu)"=5,
            "K-12-29"=4)
cols <- c("[KS094] Cebu (Philippines)"="#440154",
          "[527523] (Palau)"="#29778E","[527524] (Palau)"="#29778E",
          "[70512] Vanua Lava (Vanuatu)"="#7AD04F",
          "K-12-29"="red")

K_12_29_spider <- d_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Nd","Sr","Sm","Zr","Ti","Eu","Gd","Tb",
                           "Dy","Y","Er","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=factor(Location), color=factor(Location),
             fill=factor(Location), group=Sample)) +
  geom_line(size=.5) + geom_point(size=2, stroke=.5) +
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
        legend.position = c(.75,.84), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
K_12_29_spider

pdf(here("analysis","supplementary-materials","FigS13","FigS13-d.pdf"), width=3, height=2)
K_12_29_spider
dev.off()


