require(here)
require(tidyverse)
require(RSQLite)
require(FactoMineR)
require(factoextra)
require(patchwork)

georoc <- dbConnect(RSQLite::SQLite(), path_to_georoc)
pofatu <- dbConnect(RSQLite::SQLite(), path_to_pofatu)

dir.create(here("analysis","supplementary-materials","FigS20"))

#### E_11_08 ####
ranges_s_OIB[1,1:35]
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
WHERE s.sample_category = 'SOURCE' AND location_subregion = 'TUTUILA' AND
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
  rename_pofatu_elements() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::rename(Location=site_name) %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204) %>%
  dplyr::filter(Location %in% c("Tatagamatau"))

d <- OIB %>% dplyr::select(Sample,Location,Rb,Nb,La,Nd,Sr,Zr,Ti)
is.na(d) <- sapply(d, is.infinite) #replace Inf by NA
d[d == 0] <- NA # Replace 0 with NA
d <- d[rowSums(is.na(d)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% dplyr::filter(Sample %in% c("E-11-08")) %>%
  mutate(Location = case_when(grepl("E-11-08", Sample) ~ "E-11-08")) %>%
  dplyr::select(Sample,Location,Rb,Nb,La,Nd,Sr,Zr,Ti)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(d[,3:9], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
pred <- stats::predict(res.pca, s[,3:9])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(d[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

dist <- data.frame(
  Sample = c(d_pca[1:12,"Sample"]),
  Location = c(d_pca[1:12,"Location"]),
  PC1 = c(sqrt(((d_pca[13,"PC1"])-d_pca[1:12,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[13,"PC2"])-d_pca[1:12,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[13,"PC3"])-d_pca[1:12,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[13,"PC4"])-d_pca[1:12,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[13,"PC5"])-d_pca[1:12,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[13,"PC6"])-d_pca[1:12,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[13,"PC7"])-d_pca[1:12,"PC7"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])) / (sum(eig[1:7,2])))

head(dist[order(dist$weight_mean),] %>%
       dplyr::select("Sample","Location","weight_mean"), 10)

#### plot ####
d_spider <- OIB %>%
  dplyr::filter(Sample %in% c("collerson2007_KC-05-19",
                              "collerson2007_KC-05-14")) %>%
    mutate(Location = case_when(
    grepl("collerson2007_KC-05-19", Sample) ~ "[KC-05-19] Tatagamatau (Tutuila)",
    grepl("collerson2007_KC-05-14", Sample) ~ "[KC-05-14] Tatagamatau (Tutuila)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("E-11-08")) %>%
  mutate(Location = case_when(grepl("E-11-08", Sample) ~ "E-11-08")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()


shapes <- c("[KC-05-19] Tatagamatau (Tutuila)"=0,
            "[KC-05-14] Tatagamatau (Tutuila)"=1,
            "E-11-08"=5)
cols <- c("[KC-05-19] Tatagamatau (Tutuila)"="#781B6C",
          "[KC-05-14] Tatagamatau (Tutuila)"="#781B6C",
          "E-11-08"="red")

E_11_08_spider <- d_spider %>%
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
        legend.position = c(.7,.86), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
E_11_08_spider

pdf(here("analysis","supplementary-materials","FigS20","FigS20-a.pdf"), width=3.5, height=2)
E_11_08_spider
dev.off()


#### T_12_06_spider ####
s <- joined_data %>% dplyr::filter(Sample %in% c("T-12-06")) %>%
  mutate(Location = case_when(grepl("T-12-06", Sample) ~ "T-12-06")) %>%
  dplyr::select(Sample,Location,Rb,Nb,La,Nd,Sr,Zr,Ti)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(d[,3:9], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
pred <- stats::predict(res.pca, s[,3:9])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(d[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

dist <- data.frame(
  Sample = c(d_pca[1:12,"Sample"]),
  Location = c(d_pca[1:12,"Location"]),
  PC1 = c(sqrt(((d_pca[13,"PC1"])-d_pca[1:12,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[13,"PC2"])-d_pca[1:12,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[13,"PC3"])-d_pca[1:12,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[13,"PC4"])-d_pca[1:12,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[13,"PC5"])-d_pca[1:12,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[13,"PC6"])-d_pca[1:12,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[13,"PC7"])-d_pca[1:12,"PC7"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])) / (sum(eig[1:7,2])))

head(dist[order(dist$weight_mean),] %>%
       dplyr::select("Sample","Location","weight_mean"), 10)

#### plot ####
d_spider <- OIB %>%
  dplyr::filter(Sample %in% c("collerson2007_KC-05-19",
                              "collerson2007_KC-05-14")) %>%
  mutate(Location = case_when(
    grepl("collerson2007_KC-05-19", Sample) ~ "[KC-05-19] Tatagamatau (Tutuila)",
    grepl("collerson2007_KC-05-14", Sample) ~ "[KC-05-14] Tatagamatau (Tutuila)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("T-12-06")) %>%
  mutate(Location = case_when(grepl("T-12-06", Sample) ~ "T-12-06")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()


shapes <- c("[KC-05-19] Tatagamatau (Tutuila)"=0,
            "[KC-05-14] Tatagamatau (Tutuila)"=1,
            "T-12-06"=2)
cols <- c("[KC-05-19] Tatagamatau (Tutuila)"="#781B6C",
          "[KC-05-14] Tatagamatau (Tutuila)"="#781B6C",
          "T-12-06"="red")

T_12_06_spider <- d_spider %>%
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
        legend.position = c(.7,.86), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
T_12_06_spider

pdf(here("analysis","supplementary-materials","FigS20","FigS20-b.pdf"), width=3.5, height=2)
T_12_06_spider
dev.off()


#### T_12_07_spider ####
s <- joined_data %>% dplyr::filter(Sample %in% c("T-12-07")) %>%
  mutate(Location = case_when(grepl("T-12-07", Sample) ~ "T-12-07")) %>%
  dplyr::select(Sample,Location,Rb,Nb,La,Nd,Sr,Zr,Ti)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(d[,3:9], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
pred <- stats::predict(res.pca, s[,3:9])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(d[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

dist <- data.frame(
  Sample = c(d_pca[1:12,"Sample"]),
  Location = c(d_pca[1:12,"Location"]),
  PC1 = c(sqrt(((d_pca[13,"PC1"])-d_pca[1:12,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[13,"PC2"])-d_pca[1:12,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[13,"PC3"])-d_pca[1:12,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[13,"PC4"])-d_pca[1:12,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[13,"PC5"])-d_pca[1:12,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[13,"PC6"])-d_pca[1:12,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[13,"PC7"])-d_pca[1:12,"PC7"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])) / (sum(eig[1:7,2])))

head(dist[order(dist$weight_mean),] %>%
       dplyr::select("Sample","Location","weight_mean"), 10)

#### plot ####
d_spider <- OIB %>%
  dplyr::filter(Sample %in% c("collerson2007_KC-05-19",
                              "collerson2007_KC-05-14")) %>%
  mutate(Location = case_when(
    grepl("collerson2007_KC-05-19", Sample) ~ "[KC-05-19] Tatagamatau (Tutuila)",
    grepl("collerson2007_KC-05-14", Sample) ~ "[KC-05-14] Tatagamatau (Tutuila)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("T-12-07")) %>%
  mutate(Location = case_when(grepl("T-12-07", Sample) ~ "T-12-07")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

shapes <- c("[KC-05-19] Tatagamatau (Tutuila)"=0,
            "[KC-05-14] Tatagamatau (Tutuila)"=1, "T-12-07"=7)
cols <- c("[KC-05-19] Tatagamatau (Tutuila)"="#781B6C",
          "[KC-05-14] Tatagamatau (Tutuila)"="#781B6C", "T-12-07"="red")

T_12_07_spider <- d_spider %>%
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
        legend.position = c(.7,.86), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
T_12_07_spider

pdf(here("analysis","supplementary-materials","FigS20","FigS20-c.pdf"), width=3.5, height=2)
T_12_07_spider
dev.off()


#### T_12_08_spider ####
s <- joined_data %>% dplyr::filter(Sample %in% c("T-12-08")) %>%
  mutate(Location = case_when(grepl("T-12-08", Sample) ~ "T-12-08")) %>%
  dplyr::select(Sample,Location,Rb,Nb,La,Nd,Sr,Zr,Ti)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(d[,3:9], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
pred <- stats::predict(res.pca, s[,3:9])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(d[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

dist <- data.frame(
  Sample = c(d_pca[1:12,"Sample"]),
  Location = c(d_pca[1:12,"Location"]),
  PC1 = c(sqrt(((d_pca[13,"PC1"])-d_pca[1:12,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[13,"PC2"])-d_pca[1:12,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[13,"PC3"])-d_pca[1:12,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[13,"PC4"])-d_pca[1:12,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[13,"PC5"])-d_pca[1:12,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[13,"PC6"])-d_pca[1:12,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[13,"PC7"])-d_pca[1:12,"PC7"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])) / (sum(eig[1:7,2])))

head(dist[order(dist$weight_mean),] %>%
       dplyr::select("Sample","Location","weight_mean"), 10)

#### plot ####
d_spider <- OIB %>%
  dplyr::filter(Sample %in% c(
    "collerson2007_KC-05-19","collerson2007_KC-05-18")) %>%
  mutate(Location = case_when(
    grepl("collerson2007_KC-05-19", Sample) ~ "[KC-05-19] Tatagamatau (Tutuila)",
    grepl("collerson2007_KC-05-18", Sample) ~ "[KC-05-18] Tatagamatau (Tutuila)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("T-12-08")) %>%
  mutate(Location = case_when(grepl("T-12-08", Sample) ~ "T-12-08")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

shapes <- c("[KC-05-19] Tatagamatau (Tutuila)"=0,
            "[KC-05-18] Tatagamatau (Tutuila)"=1,
            "T-12-08"=6)
cols <- c("[KC-05-19] Tatagamatau (Tutuila)"="#781B6C",
          "[KC-05-18] Tatagamatau (Tutuila)"="#781B6C",
          "T-12-08"="red")

T_12_08_spider <- d_spider %>%
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
        legend.position = c(.7,.86), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
T_12_08_spider

pdf(here("analysis","supplementary-materials","FigS20","FigS20-d.pdf"), width=3.5, height=2)
T_12_08_spider
dev.off()


#### T_12_09_spider ####
s <- joined_data %>% dplyr::filter(Sample %in% c("T-12-09")) %>%
  mutate(Location = case_when(grepl("T-12-09", Sample) ~ "T-12-09")) %>%
  dplyr::select(Sample,Location,Rb,Nb,La,Nd,Sr,Zr,Ti)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(d[,3:9], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
pred <- stats::predict(res.pca, s[,3:9])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(d[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

dist <- data.frame(
  Sample = c(d_pca[1:12,"Sample"]),
  Location = c(d_pca[1:12,"Location"]),
  PC1 = c(sqrt(((d_pca[13,"PC1"])-d_pca[1:12,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[13,"PC2"])-d_pca[1:12,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[13,"PC3"])-d_pca[1:12,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[13,"PC4"])-d_pca[1:12,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[13,"PC5"])-d_pca[1:12,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[13,"PC6"])-d_pca[1:12,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[13,"PC7"])-d_pca[1:12,"PC7"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])) / (sum(eig[1:7,2])))

head(dist[order(dist$weight_mean),] %>%
       dplyr::select("Sample","Location","weight_mean"), 10)

#### plot ####
d_spider <- OIB %>%
  dplyr::filter(Sample %in% c(
    "collerson2007_KC-05-19","collerson2007_KC-05-14")) %>%
  mutate(Location = case_when(
    grepl("collerson2007_KC-05-19", Sample) ~ "[KC-05-19] Tatagamatau (Tutuila)",
    grepl("collerson2007_KC-05-14", Sample) ~ "[KC-05-14] Tatagamatau (Tutuila)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("T-12-09")) %>%
  mutate(Location = case_when(grepl("T-12-09", Sample) ~ "T-12-09")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

shapes <- c("[KC-05-19] Tatagamatau (Tutuila)"=0,
            "[KC-05-14] Tatagamatau (Tutuila)"=1,
            "T-12-09"=10)
cols <- c("[KC-05-19] Tatagamatau (Tutuila)"="#781B6C",
          "[KC-05-14] Tatagamatau (Tutuila)"="#781B6C",
          "T-12-09"="red")

T_12_09_spider <- d_spider %>%
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
        legend.position = c(.7,.86), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
T_12_09_spider

pdf(here("analysis","supplementary-materials","FigS20","FigS20-e.pdf"), width=3.5, height=2)
T_12_09_spider
dev.off()


#### T_12_10_spider ####
s <- joined_data %>% dplyr::filter(Sample %in% c("T-12-10")) %>%
  mutate(Location = case_when(grepl("T-12-10", Sample) ~ "T-12-10")) %>%
  dplyr::select(Sample,Location,Rb,Nb,La,Nd,Sr,Zr,Ti)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(d[,3:9], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
pred <- stats::predict(res.pca, s[,3:9])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(d[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

dist <- data.frame(
  Sample = c(d_pca[1:12,"Sample"]),
  Location = c(d_pca[1:12,"Location"]),
  PC1 = c(sqrt(((d_pca[13,"PC1"])-d_pca[1:12,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[13,"PC2"])-d_pca[1:12,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[13,"PC3"])-d_pca[1:12,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[13,"PC4"])-d_pca[1:12,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[13,"PC5"])-d_pca[1:12,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[13,"PC6"])-d_pca[1:12,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[13,"PC7"])-d_pca[1:12,"PC7"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])) / (sum(eig[1:7,2])))

head(dist[order(dist$weight_mean),] %>%
       dplyr::select("Sample","Location","weight_mean"), 10)

#### plot ####
d_spider <- OIB %>%
  dplyr::filter(Sample %in% c(
    "collerson2007_KC-05-19","collerson2007_KC-05-14")) %>%
  mutate(Location = case_when(
    grepl("collerson2007_KC-05-19", Sample) ~ "[KC-05-19] Tatagamatau (Tutuila)",
    grepl("collerson2007_KC-05-14", Sample) ~ "[KC-05-14] Tatagamatau (Tutuila)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("T-12-10")) %>%
  mutate(Location = case_when(grepl("T-12-10", Sample) ~ "T-12-10")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

shapes <- c("[KC-05-19] Tatagamatau (Tutuila)"=0,
            "[KC-05-14] Tatagamatau (Tutuila)"=1,
            "T-12-10"=11)
cols <- c("[KC-05-19] Tatagamatau (Tutuila)"="#781B6C",
          "[KC-05-14] Tatagamatau (Tutuila)"="#781B6C",
          "T-12-10"="red")

T_12_10_spider <- d_spider %>%
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
        legend.position = c(.7,.86), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
T_12_10_spider

pdf(here("analysis","supplementary-materials","FigS20","FigS20-f.pdf"), width=3.5, height=2)
T_12_10_spider
dev.off()

reference <- dbGetQuery(pofatu,
"SELECT Source_ID, Sample_ID FROM 'references.csv'
WHERE sample_ID = 'collerson2007_KC-05-19' OR
sample_ID = 'collerson2007_KC-05-14' OR
sample_ID = 'collerson2007_KC-05-18'")
reference
citation <- dbGetQuery(pofatu,
"SELECT ID, author, date, journaltitle
FROM 'sources.csv'
WHERE ID = 'collerson2007'") %>% rename (Source_ID=ID)
cite <- full_join(reference,citation)
cite

#### K_12_24 ####
OIB <- q15 %>% dplyr::select(
  Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
is.na(OIB) <- sapply(OIB, is.infinite) #replace Inf by NA
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-24")) %>%
  mutate(Location = case_when(grepl("K-12-24", Sample) ~ "K-12-24")) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB[,3:21], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
pred <- stats::predict(res.pca, s[,3:21])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(OIB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:499,"Sample"]),
  Location = c(d_pca[1:499,"Location"]),
  PC1 = c(sqrt(((d_pca[500,"PC1"])-d_pca[1:499,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[500,"PC2"])-d_pca[1:499,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[500,"PC3"])-d_pca[1:499,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[500,"PC4"])-d_pca[1:499,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[500,"PC5"])-d_pca[1:499,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[500,"PC6"])-d_pca[1:499,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[500,"PC7"])-d_pca[1:499,"PC7"])^2)),
  PC8 = c(sqrt(((d_pca[500,"PC8"])-d_pca[1:499,"PC8"])^2)),
  PC9 = c(sqrt(((d_pca[500,"PC9"])-d_pca[1:499,"PC9"])^2)),
  PC10 = c(sqrt(((d_pca[500,"PC10"])-d_pca[1:499,"PC10"])^2)),
  PC11 = c(sqrt(((d_pca[500,"PC11"])-d_pca[1:499,"PC11"])^2)),
  PC12 = c(sqrt(((d_pca[500,"PC12"])-d_pca[1:499,"PC12"])^2)),
  PC13 = c(sqrt(((d_pca[500,"PC13"])-d_pca[1:499,"PC13"])^2)),
  PC14 = c(sqrt(((d_pca[500,"PC14"])-d_pca[1:499,"PC14"])^2)),
  PC15 = c(sqrt(((d_pca[500,"PC15"])-d_pca[1:499,"PC15"])^2)),
  PC16 = c(sqrt(((d_pca[500,"PC16"])-d_pca[1:499,"PC16"])^2)),
  PC17 = c(sqrt(((d_pca[500,"PC17"])-d_pca[1:499,"PC17"])^2)),
  PC18 = c(sqrt(((d_pca[500,"PC18"])-d_pca[1:499,"PC18"])^2)),
  PC19 = c(sqrt(((d_pca[500,"PC19"])-d_pca[1:499,"PC19"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])+(PC8*eig[8,2])+(PC9*eig[9,2])+
      (PC10*eig[10,2])+(PC11*eig[11,2])+(PC12*eig[12,2])+(PC13*eig[13,2])+
      (PC14*eig[14,2])+(PC15*eig[15,2])+(PC16*eig[16,2])+(PC17*eig[17,2])+
      (PC18*eig[18,2])+(PC19*eig[19,2])) / (sum(eig[1:19,2])))

head(dist[order(dist$weight_mean),] %>%
       dplyr::select("Sample","Location","weight_mean"), 10)

#### plot ####
d <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE id='120903-KOS 13-4' OR id='1867354' OR
id='1867352' OR id='1867350'") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  rename(Location=LOCATION)%>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
d
d_spider <- d %>%
  mutate(Location = case_when(
    grepl("120903-KOS 13-4", Sample) ~ "[KOS 13-4] Kosrae (Caroline)",
    grepl("1867354", Sample) ~ "[1867354] Ponape (Caroline)",
    grepl("1867352", Sample) ~ "[1867352] Ponape (Caroline)",
    grepl("1867350", Sample) ~ "[1867350] Ponape (Caroline)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("K-12-24")) %>%
  mutate(Location = case_when(grepl("K-12-24", Sample) ~ "K-12-24")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()


shapes <- c("[1867354] Ponape (Caroline)"=0,
            "[1867352] Ponape (Caroline)"=1,
            "[1867350] Ponape (Caroline)"=2,
            "[KOS 13-4] Kosrae (Caroline)"=5,
            "K-12-24"=12)
cols <- c("[1867354] Ponape (Caroline)"="#320A5A",
          "[1867352] Ponape (Caroline)"="#320A5A",
          "[1867350] Ponape (Caroline)"="#320A5A",
          "[KOS 13-4] Kosrae (Caroline)"="#320A5A",
          "K-12-24"="red")

K_12_24_spider <- d_spider %>%
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
        legend.position = c(.7,.86), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
K_12_24_spider

pdf(here("analysis","supplementary-materials","FigS20","FigS20-g.pdf"), width=3.5, height=2)
K_12_24_spider
dev.off()

citation <- dbGetQuery(georoc,
"SELECT sample_id, reference_id
FROM 'citation'
WHERE sample_id='120903-KOS 13-4' OR
sample_id='1867354' OR
sample_id='1867352' OR
sample_id='1867350'")
citation
reference <- dbGetQuery(georoc,
"SELECT id, reference
FROM 'reference'
WHERE id='23592' OR
id='21069' OR
id='24239'") %>% rename(reference_id=id)
cite <- full_join(citation,reference)
cite

#### K_12_25 ####
OIB <- q17 %>% dplyr::select(
  Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)

d_price2014 <- price2014 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::select(Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
OIB <- full_join(OIB,d_price2014)

d_price2017 <- price2017 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::select(Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
OIB <- full_join(OIB,d_price2017)

d_jeanvoine2021 <- jeanvoine2021 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::select(Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
OIB <- full_join(OIB,d_jeanvoine2021)

is.na(OIB) <- sapply(OIB, is.infinite) #replace Inf by NA
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA
OIB %>% group_by(Location) %>% tally()

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-25")) %>%
  mutate(Location = case_when(grepl("K-12-25", Sample) ~ "K-12-25")) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB[,3:21], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
pred <- stats::predict(res.pca, s[,3:21])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(OIB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:179,"Sample"]),
  Location = c(d_pca[1:179,"Location"]),
  PC1 = c(sqrt(((d_pca[180,"PC1"])-d_pca[1:179,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[180,"PC2"])-d_pca[1:179,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[180,"PC3"])-d_pca[1:179,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[180,"PC4"])-d_pca[1:179,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[180,"PC5"])-d_pca[1:179,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[180,"PC6"])-d_pca[1:179,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[180,"PC7"])-d_pca[1:179,"PC7"])^2)),
  PC8 = c(sqrt(((d_pca[180,"PC8"])-d_pca[1:179,"PC8"])^2)),
  PC9 = c(sqrt(((d_pca[180,"PC9"])-d_pca[1:179,"PC9"])^2)),
  PC10 = c(sqrt(((d_pca[180,"PC10"])-d_pca[1:179,"PC10"])^2)),
  PC11 = c(sqrt(((d_pca[180,"PC11"])-d_pca[1:179,"PC11"])^2)),
  PC12 = c(sqrt(((d_pca[180,"PC12"])-d_pca[1:179,"PC12"])^2)),
  PC13 = c(sqrt(((d_pca[180,"PC13"])-d_pca[1:179,"PC13"])^2)),
  PC14 = c(sqrt(((d_pca[180,"PC14"])-d_pca[1:179,"PC14"])^2)),
  PC15 = c(sqrt(((d_pca[180,"PC15"])-d_pca[1:179,"PC15"])^2)),
  PC16 = c(sqrt(((d_pca[180,"PC16"])-d_pca[1:179,"PC16"])^2)),
  PC17 = c(sqrt(((d_pca[180,"PC17"])-d_pca[1:179,"PC17"])^2)),
  PC18 = c(sqrt(((d_pca[180,"PC18"])-d_pca[1:179,"PC18"])^2)),
  PC19 = c(sqrt(((d_pca[180,"PC19"])-d_pca[1:179,"PC19"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])+(PC8*eig[8,2])+(PC9*eig[9,2])+
      (PC10*eig[10,2])+(PC11*eig[11,2])+(PC12*eig[12,2])+(PC13*eig[13,2])+
      (PC14*eig[14,2])+(PC15*eig[15,2])+(PC16*eig[16,2])+(PC17*eig[17,2])+
      (PC18*eig[18,2])+(PC19*eig[19,2])) / (sum(eig[1:19,2])))

head(dist[order(dist$weight_mean),] %>%
       dplyr::select("Sample","Location","weight_mean"), 20)

#### plot ####
d <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE id='370016' OR id='370013' OR id='370021' OR
id='370023' OR id='370018' OR id='370024'") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  rename(Location=LOCATION) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu)
d[,1:2]

d_spider <- d %>%
  mutate(Location = case_when(
    grepl("370016", Sample) ~ "[370016] Ni'ihau (Hawai'i)",
    grepl("370013", Sample) ~ "[370013] Ni'ihau (Hawai'i)",
    grepl("370021", Sample) ~ "[370021] Ni'ihau (Hawai'i)",
    grepl("370023", Sample) ~ "[370023] Ni'ihau (Hawai'i)",
    grepl("370018", Sample) ~ "[370018] Ni'ihau (Hawai'i)",
    grepl("370024", Sample) ~ "[370024] Ni'ihau (Hawai'i)")) %>%
  normalize_to_pm()
d_spider[d_spider == 0] <- NA # Replace 0 with NA

price2014 %>% dplyr::filter(Sample %in% c("s127-05g","s127-11g"))
price2014_spider <- price2014 %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::mutate(Location = "North Fiji Basin") %>%
  dplyr::filter(Sample %in% c("s127-05g","s127-11g")) %>%
  mutate(Location = case_when(
    grepl("s127-05g", Sample) ~ "[s127-05g] 'Uvea (Wallis)",
    grepl("s127-11g", Sample) ~ "[s127-11g] 'Uvea (Wallis)",
    grepl("s142-1", Sample) ~ "s142-1")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("K-12-25")) %>%
  mutate(Location = case_when(grepl("K-12-25", Sample) ~ "K-12-25")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

shapes <- c("[s127-05g] 'Uvea (Wallis)"=8,
            "[s127-11g] 'Uvea (Wallis)"=8,
            "[s142-1] N Fiji"=8,
            "[370016] Ni'ihau (Hawai'i)"=0,
            "[370013] Ni'ihau (Hawai'i)"=1,
            "[370021] Ni'ihau (Hawai'i)"=2,
            "[370023] Ni'ihau (Hawai'i)"=3,
            "[370018] Ni'ihau (Hawai'i)"=4,
            "[370024] Ni'ihau (Hawai'i)"=5,
            "K-12-25"=13)
cols <- c("[s127-05g] 'Uvea (Wallis)"="#B4C630",
          "[s127-11g] 'Uvea (Wallis)"="#B4C630",
          "[s142-1] N Fiji"="#B4C630",
          "[370016] Ni'ihau (Hawai'i)"="#F4DD53",
          "[370013] Ni'ihau (Hawai'i)"="#F4DD53",
          "[370021] Ni'ihau (Hawai'i)"="#F4DD53",
          "[370023] Ni'ihau (Hawai'i)"="#F4DD53",
          "[370018] Ni'ihau (Hawai'i)"="#F4DD53",
          "[370024] Ni'ihau (Hawai'i)"="#F4DD53",
          "K-12-25"="red")

K_12_25_spider <- d_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Nd","Sr","Sm","Zr","Ti","Eu","Gd","Tb",
                           "Dy","Y","Er","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=factor(Location), color=factor(Location),
             fill=factor(Location), group=Sample)) +
  geom_line(size=.5) + geom_point(size=2, stroke=.5) +
  geom_line(data=s_spider, size=.5) + geom_point(data=s_spider, size=1, stroke=.5) +
  geom_line(data=price2014_spider, size=.5) + geom_point(data=price2014_spider, size=1, stroke=.5) +
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
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
K_12_25_spider

citation <- dbGetQuery(georoc,
"SELECT sample_id, reference_id
FROM 'citation'
WHERE sample_id='370017' OR
sample_id='370023' OR
sample_id='370009' OR
sample_id='370016' OR
sample_id='370021' OR
sample_id='370024'")
citation
reference <- dbGetQuery(georoc,
                        "SELECT id, reference
FROM 'reference'
WHERE id='12504'") %>% rename(reference_id=id)
cite <- full_join(citation,reference)
cite

#### fig min max ####
OIB <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE ((LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
`YB(PPM)` > 0.86 AND `YB(PPM)` < 2.58 AND
(file_id = '2022-06-WFJZKY_AUSTRAL-COOK_ISLANDS.csv' OR
file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv' OR
file_id = '2022-06-WFJZKY_PITCAIRN-GAMBIER_CHAIN.csv' OR
file_id = '2022-06-WFJZKY_SOCIETY_ISLANDS.csv')) OR
(ROCK_TYPE='VOL' AND
file_id = '2022-06-PVFZCE_TONGA_ARC.csv' AND
`YB(PPM)` > 0.86 AND `YB(PPM)` < 2.58 AND
LATITUDE_MAX > -16))") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

C <- OIB %>% dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                             Ce,Pr,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  dplyr::filter(Location %in% c("Caroline islands")) %>% dplyr::na_if(0)
C_minmax <- data.frame (Sample  = c("Caroline_min", "Caroline_max"),
                        Location = c("Caroline islands", "Caroline islands"),
                        Cs = c(min(C[,"Cs"],na.rm=TRUE),max(C[,"Cs"],na.rm=TRUE)),
                        Rb = c(min(C[,"Rb"],na.rm=TRUE),max(C[,"Rb"],na.rm=TRUE)),
                        Ba = c(min(C[,"Ba"],na.rm=TRUE),max(C[,"Ba"],na.rm=TRUE)),
                        Th = c(min(C[,"Th"],na.rm=TRUE),max(C[,"Th"],na.rm=TRUE)),
                        U = c(min(C[,"U"],na.rm=TRUE),max(C[,"U"],na.rm=TRUE)),
                        Nb = c(min(C[,"Nb"],na.rm=TRUE),max(C[,"Nb"],na.rm=TRUE)),
                        Ta = c(min(C[,"Ta"],na.rm=TRUE),max(C[,"Ta"],na.rm=TRUE)),
                        La = c(min(C[,"La"],na.rm=TRUE),max(C[,"La"],na.rm=TRUE)),
                        Ce = c(min(C[,"Ce"],na.rm=TRUE),max(C[,"Ce"],na.rm=TRUE)),
                        Pr = c(min(C[,"Pr"],na.rm=TRUE),max(C[,"Pr"],na.rm=TRUE)),
                        Nd = c(min(C[,"Nd"],na.rm=TRUE),max(C[,"Nd"],na.rm=TRUE)),
                        Sr = c(min(C[,"Sr"],na.rm=TRUE),max(C[,"Sr"],na.rm=TRUE)),
                        Sm = c(min(C[,"Sm"],na.rm=TRUE),max(C[,"Sm"],na.rm=TRUE)),
                        Zr = c(min(C[,"Zr"],na.rm=TRUE),max(C[,"Zr"],na.rm=TRUE)),
                        Ti = c(min(C[,"Ti"],na.rm=TRUE),max(C[,"Ti"],na.rm=TRUE)),
                        Eu = c(min(C[,"Eu"],na.rm=TRUE),max(C[,"Eu"],na.rm=TRUE)),
                        Gd = c(min(C[,"Gd"],na.rm=TRUE),max(C[,"Gd"],na.rm=TRUE)),
                        Tb = c(min(C[,"Tb"],na.rm=TRUE),max(C[,"Tb"],na.rm=TRUE)),
                        Dy = c(min(C[,"Dy"],na.rm=TRUE),max(C[,"Dy"],na.rm=TRUE)),
                        Y = c(min(C[,"Y"],na.rm=TRUE),max(C[,"Y"],na.rm=TRUE)),
                        Er = c(min(C[,"Er"],na.rm=TRUE),max(C[,"Er"],na.rm=TRUE)),
                        Yb = c(min(C[,"Yb"],na.rm=TRUE),max(C[,"Yb"],na.rm=TRUE)),
                        Lu = c(min(C[,"Lu"],na.rm=TRUE),max(C[,"Lu"],na.rm=TRUE)))

AC <- OIB %>% dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                            Ce,Pr,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  dplyr::filter(Location %in% c("Austral-Cook chain")) %>% dplyr::na_if(0)
AC_minmax <- data.frame (Sample  = c("Austral-Cook_min", "Austral-Cook_max"),
                        Location = c("Austral-Cook chain", "Austral-Cook chain"),
                        Cs = c(min(AC[,"Cs"],na.rm=TRUE),max(AC[,"Cs"],na.rm=TRUE)),
                        Rb = c(min(AC[,"Rb"],na.rm=TRUE),max(AC[,"Rb"],na.rm=TRUE)),
                        Ba = c(min(AC[,"Ba"],na.rm=TRUE),max(AC[,"Ba"],na.rm=TRUE)),
                        Th = c(min(AC[,"Th"],na.rm=TRUE),max(AC[,"Th"],na.rm=TRUE)),
                        U = c(min(AC[,"U"],na.rm=TRUE),max(AC[,"U"],na.rm=TRUE)),
                        Nb = c(min(AC[,"Nb"],na.rm=TRUE),max(AC[,"Nb"],na.rm=TRUE)),
                        Ta = c(min(AC[,"Ta"],na.rm=TRUE),max(AC[,"Ta"],na.rm=TRUE)),
                        La = c(min(AC[,"La"],na.rm=TRUE),max(AC[,"La"],na.rm=TRUE)),
                        Ce = c(min(AC[,"Ce"],na.rm=TRUE),max(AC[,"Ce"],na.rm=TRUE)),
                        Pr = c(min(AC[,"Pr"],na.rm=TRUE),max(AC[,"Pr"],na.rm=TRUE)),
                        Nd = c(min(AC[,"Nd"],na.rm=TRUE),max(AC[,"Nd"],na.rm=TRUE)),
                        Sr = c(min(AC[,"Sr"],na.rm=TRUE),max(AC[,"Sr"],na.rm=TRUE)),
                        Sm = c(min(AC[,"Sm"],na.rm=TRUE),max(AC[,"Sm"],na.rm=TRUE)),
                        Zr = c(min(AC[,"Zr"],na.rm=TRUE),max(AC[,"Zr"],na.rm=TRUE)),
                        Ti = c(min(AC[,"Ti"],na.rm=TRUE),max(AC[,"Ti"],na.rm=TRUE)),
                        Eu = c(min(AC[,"Eu"],na.rm=TRUE),max(AC[,"Eu"],na.rm=TRUE)),
                        Gd = c(min(AC[,"Gd"],na.rm=TRUE),max(AC[,"Gd"],na.rm=TRUE)),
                        Tb = c(min(AC[,"Tb"],na.rm=TRUE),max(AC[,"Tb"],na.rm=TRUE)),
                        Dy = c(min(AC[,"Dy"],na.rm=TRUE),max(AC[,"Dy"],na.rm=TRUE)),
                        Y = c(min(AC[,"Y"],na.rm=TRUE),max(AC[,"Y"],na.rm=TRUE)),
                        Er = c(min(AC[,"Er"],na.rm=TRUE),max(AC[,"Er"],na.rm=TRUE)),
                        Yb = c(min(AC[,"Yb"],na.rm=TRUE),max(AC[,"Yb"],na.rm=TRUE)),
                        Lu = c(min(AC[,"Lu"],na.rm=TRUE),max(AC[,"Lu"],na.rm=TRUE)))

PG <- OIB %>% dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                            Ce,Pr,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  dplyr::filter(Location %in% c("Pitcairn-Gambier chain")) %>% dplyr::na_if(0)
PG_minmax <- data.frame (Sample  = c("Pitcairn-Gambier_min", "Pitcairn-Gambier_max"),
                         Location = c("Pitcairn-Gambier chain", "Pitcairn-Gambier chain"),
                         Cs = c(min(PG[,"Cs"],na.rm=TRUE),max(PG[,"Cs"],na.rm=TRUE)),
                         Rb = c(min(PG[,"Rb"],na.rm=TRUE),max(PG[,"Rb"],na.rm=TRUE)),
                         Ba = c(min(PG[,"Ba"],na.rm=TRUE),max(PG[,"Ba"],na.rm=TRUE)),
                         Th = c(min(PG[,"Th"],na.rm=TRUE),max(PG[,"Th"],na.rm=TRUE)),
                         U = c(min(PG[,"U"],na.rm=TRUE),max(PG[,"U"],na.rm=TRUE)),
                         Nb = c(min(PG[,"Nb"],na.rm=TRUE),max(PG[,"Nb"],na.rm=TRUE)),
                         Ta = c(min(PG[,"Ta"],na.rm=TRUE),max(PG[,"Ta"],na.rm=TRUE)),
                         La = c(min(PG[,"La"],na.rm=TRUE),max(PG[,"La"],na.rm=TRUE)),
                         Ce = c(min(PG[,"Ce"],na.rm=TRUE),max(PG[,"Ce"],na.rm=TRUE)),
                         Pr = c(min(PG[,"Pr"],na.rm=TRUE),max(PG[,"Pr"],na.rm=TRUE)),
                         Nd = c(min(PG[,"Nd"],na.rm=TRUE),max(PG[,"Nd"],na.rm=TRUE)),
                         Sr = c(min(PG[,"Sr"],na.rm=TRUE),max(PG[,"Sr"],na.rm=TRUE)),
                         Sm = c(min(PG[,"Sm"],na.rm=TRUE),max(PG[,"Sm"],na.rm=TRUE)),
                         Zr = c(min(PG[,"Zr"],na.rm=TRUE),max(PG[,"Zr"],na.rm=TRUE)),
                         Ti = c(min(PG[,"Ti"],na.rm=TRUE),max(PG[,"Ti"],na.rm=TRUE)),
                         Eu = c(min(PG[,"Eu"],na.rm=TRUE),max(PG[,"Eu"],na.rm=TRUE)),
                         Gd = c(min(PG[,"Gd"],na.rm=TRUE),max(PG[,"Gd"],na.rm=TRUE)),
                         Tb = c(min(PG[,"Tb"],na.rm=TRUE),max(PG[,"Tb"],na.rm=TRUE)),
                         Dy = c(min(PG[,"Dy"],na.rm=TRUE),max(PG[,"Dy"],na.rm=TRUE)),
                         Y = c(min(PG[,"Y"],na.rm=TRUE),max(PG[,"Y"],na.rm=TRUE)),
                         Er = c(min(PG[,"Er"],na.rm=TRUE),max(PG[,"Er"],na.rm=TRUE)),
                         Yb = c(min(PG[,"Yb"],na.rm=TRUE),max(PG[,"Yb"],na.rm=TRUE)),
                         Lu = c(min(PG[,"Lu"],na.rm=TRUE),max(PG[,"Lu"],na.rm=TRUE)))

NFB <- OIB %>% dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                            Ce,Pr,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  dplyr::filter(Location %in% c("Tonga-Fiji")) %>% dplyr::na_if(0) %>%
  mutate(Location=recode(Location,"Tonga-Fiji"="North Fiji Basin"))
d_price2014 <- price2014 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(Yb > min(ranges_s_OIB[8,"Yb min"]) &
                  Yb < max(ranges_s_OIB[8,"Yb max"])) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu)
d_price2017 <- price2017 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(Yb > min(ranges_s_OIB[8,"Yb min"]) &
                  Yb < max(ranges_s_OIB[8,"Yb max"])) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu)
NFB <- full_join(NFB,d_price2017) %>% dplyr::na_if(0)
d_jeanvoine2021 <- jeanvoine2021 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(Yb > min(ranges_s_OIB[8,"Yb min"]) &
                  Yb < max(ranges_s_OIB[8,"Yb max"])) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu)
NFB <- full_join(NFB,jeanvoine2021) %>% dplyr::na_if(0)

NFB_minmax <- data.frame (Sample  = c("North_Fiji_Basin_min", "North_Fiji_Basin_max"),
                         Location = c("North Fiji Basin", "North Fiji Basin"),
                         Cs = c(min(NFB[,"Cs"],na.rm=TRUE),max(NFB[,"Cs"],na.rm=TRUE)),
                         Rb = c(min(NFB[,"Rb"],na.rm=TRUE),max(NFB[,"Rb"],na.rm=TRUE)),
                         Ba = c(min(NFB[,"Ba"],na.rm=TRUE),max(NFB[,"Ba"],na.rm=TRUE)),
                         Th = c(min(NFB[,"Th"],na.rm=TRUE),max(NFB[,"Th"],na.rm=TRUE)),
                         U = c(min(NFB[,"U"],na.rm=TRUE),max(NFB[,"U"],na.rm=TRUE)),
                         Nb = c(min(NFB[,"Nb"],na.rm=TRUE),max(NFB[,"Nb"],na.rm=TRUE)),
                         Ta = c(min(NFB[,"Ta"],na.rm=TRUE),max(NFB[,"Ta"],na.rm=TRUE)),
                         La = c(min(NFB[,"La"],na.rm=TRUE),max(NFB[,"La"],na.rm=TRUE)),
                         Ce = c(min(NFB[,"Ce"],na.rm=TRUE),max(NFB[,"Ce"],na.rm=TRUE)),
                         Pr = c(min(NFB[,"Pr"],na.rm=TRUE),max(NFB[,"Pr"],na.rm=TRUE)),
                         Nd = c(min(NFB[,"Nd"],na.rm=TRUE),max(NFB[,"Nd"],na.rm=TRUE)),
                         Sr = c(min(NFB[,"Sr"],na.rm=TRUE),max(NFB[,"Sr"],na.rm=TRUE)),
                         Sm = c(min(NFB[,"Sm"],na.rm=TRUE),max(NFB[,"Sm"],na.rm=TRUE)),
                         Zr = c(min(NFB[,"Zr"],na.rm=TRUE),max(NFB[,"Zr"],na.rm=TRUE)),
                         Ti = c(min(NFB[,"Ti"],na.rm=TRUE),max(NFB[,"Ti"],na.rm=TRUE)),
                         Eu = c(min(NFB[,"Eu"],na.rm=TRUE),max(NFB[,"Eu"],na.rm=TRUE)),
                         Gd = c(min(NFB[,"Gd"],na.rm=TRUE),max(NFB[,"Gd"],na.rm=TRUE)),
                         Tb = c(min(NFB[,"Tb"],na.rm=TRUE),max(NFB[,"Tb"],na.rm=TRUE)),
                         Dy = c(min(NFB[,"Dy"],na.rm=TRUE),max(NFB[,"Dy"],na.rm=TRUE)),
                         Y = c(min(NFB[,"Y"],na.rm=TRUE),max(NFB[,"Y"],na.rm=TRUE)),
                         Er = c(min(NFB[,"Er"],na.rm=TRUE),max(NFB[,"Er"],na.rm=TRUE)),
                         Yb = c(min(NFB[,"Yb"],na.rm=TRUE),max(NFB[,"Yb"],na.rm=TRUE)),
                         Lu = c(min(NFB[,"Lu"],na.rm=TRUE),max(NFB[,"Lu"],na.rm=TRUE)))

d <- full_join(C_minmax,AC_minmax)
d <- full_join(d,PG_minmax)
d <- full_join(d,NFB_minmax)
d_spider <- d %>% normalize_to_pm()

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("K-12-25")) %>%
  mutate(Location = case_when(grepl("K-12-25", Sample) ~ "K-12-25")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

shapes <- c("Caroline islands"=21,"Austral-Cook chain"=23,
            "Pitcairn-Gambier chain"=21,"North Fiji Basin"=8,"K-12-25"=13)
cols <- c("Caroline islands"="#320A5A","Austral-Cook chain"="#BB3654",
          "Pitcairn-Gambier chain"="#C96FB6","North Fiji Basin"="#B4C630",
          "K-12-25"="red")

K_12_25_spider_minmax <- d_spider %>%
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
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
K_12_25_spider_minmax

pdf(here("analysis","supplementary-materials","FigS20","FigS20-h.pdf"), width=3.5, height=2)
K_12_25_spider_minmax
dev.off()


#### K_12_26 ####
OIB <- q19 %>% dplyr::select(
  Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
is.na(OIB) <- sapply(OIB, is.infinite) #replace Inf by NA
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>%
  dplyr::filter(Sample %in% c("K-12-26")) %>%
  mutate(Location = case_when(grepl("K-12-26", Sample) ~ "K-12-26")) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB[,3:21], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
pred <- stats::predict(res.pca, s[,3:21])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(OIB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:79,"Sample"]),
  Location = c(d_pca[1:79,"Location"]),
  PC1 = c(sqrt(((d_pca[80,"PC1"])-d_pca[1:79,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[80,"PC2"])-d_pca[1:79,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[80,"PC3"])-d_pca[1:79,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[80,"PC4"])-d_pca[1:79,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[80,"PC5"])-d_pca[1:79,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[80,"PC6"])-d_pca[1:79,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[80,"PC7"])-d_pca[1:79,"PC7"])^2)),
  PC8 = c(sqrt(((d_pca[80,"PC8"])-d_pca[1:79,"PC8"])^2)),
  PC9 = c(sqrt(((d_pca[80,"PC9"])-d_pca[1:79,"PC9"])^2)),
  PC10 = c(sqrt(((d_pca[80,"PC10"])-d_pca[1:79,"PC10"])^2)),
  PC11 = c(sqrt(((d_pca[80,"PC11"])-d_pca[1:79,"PC11"])^2)),
  PC12 = c(sqrt(((d_pca[80,"PC12"])-d_pca[1:79,"PC12"])^2)),
  PC13 = c(sqrt(((d_pca[80,"PC13"])-d_pca[1:79,"PC13"])^2)),
  PC14 = c(sqrt(((d_pca[80,"PC14"])-d_pca[1:79,"PC14"])^2)),
  PC15 = c(sqrt(((d_pca[80,"PC15"])-d_pca[1:79,"PC15"])^2)),
  PC16 = c(sqrt(((d_pca[80,"PC16"])-d_pca[1:79,"PC16"])^2)),
  PC17 = c(sqrt(((d_pca[80,"PC17"])-d_pca[1:79,"PC17"])^2)),
  PC18 = c(sqrt(((d_pca[80,"PC18"])-d_pca[1:79,"PC18"])^2)),
  PC19 = c(sqrt(((d_pca[80,"PC19"])-d_pca[1:79,"PC19"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])+(PC8*eig[8,2])+(PC9*eig[9,2])+
      (PC10*eig[10,2])+(PC11*eig[11,2])+(PC12*eig[12,2])+(PC13*eig[13,2])+
      (PC14*eig[14,2])+(PC15*eig[15,2])+(PC16*eig[16,2])+(PC17*eig[17,2])+
      (PC18*eig[18,2])+(PC19*eig[19,2])) / (sum(eig[1:19,2])))

head(dist[order(dist$weight_mean),] %>%
       dplyr::select("Sample","Location","weight_mean"), 10)

#### plot ####
d <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE id='1956574' OR id='447-HK-11' OR id='1956571' OR
id='1104168' OR id='1956569' OR id='313150' OR id='1956577' OR
id='1104171' OR id='1867325' OR id='1867326'") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  rename(Location=LOCATION)%>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K)
d[,1:2]

d_spider <- d %>%
  dplyr::filter(Sample %in% c("1867325","1867326")) %>%
  mutate(Location = case_when(
    grepl("1867325", Sample) ~ "[1867325] Ponape (Caroline)",
    grepl("1867326", Sample) ~ "[1867326] Ponape (Caroline)",
    grepl("1956574", Sample) ~ "[1956574] Maui (Hawai'i)",
    grepl("447-HK-11", Sample) ~ "[447-HK-11] Maui (Hawai'i)",
    grepl("1956571", Sample) ~ "[1956571] Maui (Hawai'i)",
    grepl("1104168", Sample) ~ "[1104168] Maui (Hawai'i)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()
d_spider[d_spider == 0] <- NA # Replace 0 with NA

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("K-12-26")) %>%
  mutate(Location = case_when(grepl("K-12-26", Sample) ~ "K-12-26")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()


shapes <- c("[1867325] Ponape (Caroline)"=0,
            "[1867326] Ponape (Caroline)"=1,
            "[1956574] Maui (Hawai'i)"=2,
            "[447-HK-11] Maui (Hawai'i)"=3,
            "[1956571] Maui (Hawai'i)"=4,
            "[1104168] Maui (Hawai'i)"=5,
            "K-12-26"=14)
cols <- c("[1867325] Ponape (Caroline)"="#320A5A",
            "[1867326] Ponape (Caroline)"="#320A5A",
            "[1956574] Maui (Hawai'i)"="#F4DD53",
            "[447-HK-11] Maui (Hawai'i)"="#F4DD53",
            "[1956571] Maui (Hawai'i)"="#F4DD53",
            "[1104168] Maui (Hawai'i)"="#F4DD53",
            "K-12-26"="red")

K_12_26_spider <- d_spider %>%
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
        legend.position = c(.7,.86), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
K_12_26_spider

pdf(here("analysis","supplementary-materials","FigS20","FigS20-i(bis).pdf"), width=3.5, height=2)
K_12_26_spider
dev.off()

citation <- dbGetQuery(georoc,
"SELECT sample_id, reference_id
FROM 'citation'
WHERE sample_id='1867325' OR
sample_id='1867326' OR
sample_id='1956574' OR
sample_id='447-HK-11' OR
sample_id='1956571' OR
sample_id='1104168'")
citation%>%group_by(reference_id)%>%tally()
reference <- dbGetQuery(georoc,
"SELECT id, reference
FROM 'reference'
WHERE id='83' OR
id='317' OR
id='1496' OR
id='2661' OR
id='20654' OR
id='24239' OR
id='24498'") %>% rename(reference_id=id)
cite <- full_join(citation,reference)
cite


#### fig min max ####
C <- q19 %>% dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                           Ce,Pr,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  dplyr::filter(Location %in% c("Caroline islands")) %>% dplyr::na_if(0)
C_minmax <- data.frame (Sample  = c("Caroline_min", "Caroline_max"),
                        Location = c("Caroline islands", "Caroline islands"),
                        Cs = c(min(C[,"Cs"],na.rm=TRUE),max(C[,"Cs"],na.rm=TRUE)),
                        Rb = c(min(C[,"Rb"],na.rm=TRUE),max(C[,"Rb"],na.rm=TRUE)),
                        Ba = c(min(C[,"Ba"],na.rm=TRUE),max(C[,"Ba"],na.rm=TRUE)),
                        Th = c(min(C[,"Th"],na.rm=TRUE),max(C[,"Th"],na.rm=TRUE)),
                        U = c(min(C[,"U"],na.rm=TRUE),max(C[,"U"],na.rm=TRUE)),
                        Nb = c(min(C[,"Nb"],na.rm=TRUE),max(C[,"Nb"],na.rm=TRUE)),
                        Ta = c(min(C[,"Ta"],na.rm=TRUE),max(C[,"Ta"],na.rm=TRUE)),
                        La = c(min(C[,"La"],na.rm=TRUE),max(C[,"La"],na.rm=TRUE)),
                        Ce = c(min(C[,"Ce"],na.rm=TRUE),max(C[,"Ce"],na.rm=TRUE)),
                        Pr = c(min(C[,"Pr"],na.rm=TRUE),max(C[,"Pr"],na.rm=TRUE)),
                        Nd = c(min(C[,"Nd"],na.rm=TRUE),max(C[,"Nd"],na.rm=TRUE)),
                        Sr = c(min(C[,"Sr"],na.rm=TRUE),max(C[,"Sr"],na.rm=TRUE)),
                        Sm = c(min(C[,"Sm"],na.rm=TRUE),max(C[,"Sm"],na.rm=TRUE)),
                        Zr = c(min(C[,"Zr"],na.rm=TRUE),max(C[,"Zr"],na.rm=TRUE)),
                        Ti = c(min(C[,"Ti"],na.rm=TRUE),max(C[,"Ti"],na.rm=TRUE)),
                        Eu = c(min(C[,"Eu"],na.rm=TRUE),max(C[,"Eu"],na.rm=TRUE)),
                        Gd = c(min(C[,"Gd"],na.rm=TRUE),max(C[,"Gd"],na.rm=TRUE)),
                        Tb = c(min(C[,"Tb"],na.rm=TRUE),max(C[,"Tb"],na.rm=TRUE)),
                        Dy = c(min(C[,"Dy"],na.rm=TRUE),max(C[,"Dy"],na.rm=TRUE)),
                        Y = c(min(C[,"Y"],na.rm=TRUE),max(C[,"Y"],na.rm=TRUE)),
                        Er = c(min(C[,"Er"],na.rm=TRUE),max(C[,"Er"],na.rm=TRUE)),
                        Yb = c(min(C[,"Yb"],na.rm=TRUE),max(C[,"Yb"],na.rm=TRUE)),
                        Lu = c(min(C[,"Lu"],na.rm=TRUE),max(C[,"Lu"],na.rm=TRUE)))

HW <- q19 %>% dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                            Ce,Pr,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  dplyr::filter(Location %in% c("Hawai'i islands")) %>% dplyr::na_if(0)
HW_minmax <- data.frame (Sample  = c("Hawaii_min", "Hawaii_max"),
                         Location = c("Hawai'i islands", "Hawai'i islands"),
                         Cs = c(min(HW[,"Cs"],na.rm=TRUE),max(HW[,"Cs"],na.rm=TRUE)),
                         Rb = c(min(HW[,"Rb"],na.rm=TRUE),max(HW[,"Rb"],na.rm=TRUE)),
                         Ba = c(min(HW[,"Ba"],na.rm=TRUE),max(HW[,"Ba"],na.rm=TRUE)),
                         Th = c(min(HW[,"Th"],na.rm=TRUE),max(HW[,"Th"],na.rm=TRUE)),
                         U = c(min(HW[,"U"],na.rm=TRUE),max(HW[,"U"],na.rm=TRUE)),
                         Nb = c(min(HW[,"Nb"],na.rm=TRUE),max(HW[,"Nb"],na.rm=TRUE)),
                         Ta = c(min(HW[,"Ta"],na.rm=TRUE),max(HW[,"Ta"],na.rm=TRUE)),
                         La = c(min(HW[,"La"],na.rm=TRUE),max(HW[,"La"],na.rm=TRUE)),
                         Ce = c(min(HW[,"Ce"],na.rm=TRUE),max(HW[,"Ce"],na.rm=TRUE)),
                         Pr = c(min(HW[,"Pr"],na.rm=TRUE),max(HW[,"Pr"],na.rm=TRUE)),
                         Nd = c(min(HW[,"Nd"],na.rm=TRUE),max(HW[,"Nd"],na.rm=TRUE)),
                         Sr = c(min(HW[,"Sr"],na.rm=TRUE),max(HW[,"Sr"],na.rm=TRUE)),
                         Sm = c(min(HW[,"Sm"],na.rm=TRUE),max(HW[,"Sm"],na.rm=TRUE)),
                         Zr = c(min(HW[,"Zr"],na.rm=TRUE),max(HW[,"Zr"],na.rm=TRUE)),
                         Ti = c(min(HW[,"Ti"],na.rm=TRUE),max(HW[,"Ti"],na.rm=TRUE)),
                         Eu = c(min(HW[,"Eu"],na.rm=TRUE),max(HW[,"Eu"],na.rm=TRUE)),
                         Gd = c(min(HW[,"Gd"],na.rm=TRUE),max(HW[,"Gd"],na.rm=TRUE)),
                         Tb = c(min(HW[,"Tb"],na.rm=TRUE),max(HW[,"Tb"],na.rm=TRUE)),
                         Dy = c(min(HW[,"Dy"],na.rm=TRUE),max(HW[,"Dy"],na.rm=TRUE)),
                         Y = c(min(HW[,"Y"],na.rm=TRUE),max(HW[,"Y"],na.rm=TRUE)),
                         Er = c(min(HW[,"Er"],na.rm=TRUE),max(HW[,"Er"],na.rm=TRUE)),
                         Yb = c(min(HW[,"Yb"],na.rm=TRUE),max(HW[,"Yb"],na.rm=TRUE)),
                         Lu = c(min(HW[,"Lu"],na.rm=TRUE),max(HW[,"Lu"],na.rm=TRUE)))

NFB <- OIB %>% dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                             Ce,Pr,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  dplyr::filter(Location %in% c("Tonga-Fiji")) %>% dplyr::na_if(0) %>%
  mutate(Location=recode(Location,"Tonga-Fiji"="North Fiji Basin"))
d_price2014 <- price2014 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(SiO2 > min(ranges_s_OIB[9,"SiO2 min"]) &
                  SiO2 < max(ranges_s_OIB[9,"SiO2 max"]) &
                  Na2O > min(ranges_s_OIB[9,"Na2O min"]) &
                  Na2O < max(ranges_s_OIB[9,"Na2O max"]) &
                  Yb > min(ranges_s_OIB[9,"Yb min"]) &
                  Yb < max(ranges_s_OIB[9,"Yb max"])) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu)
d_price2017 <- price2017 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(SiO2 > min(ranges_s_OIB[9,"SiO2 min"]) &
                  SiO2 < max(ranges_s_OIB[9,"SiO2 max"]) &
                  Na2O > min(ranges_s_OIB[9,"Na2O min"]) &
                  Na2O < max(ranges_s_OIB[9,"Na2O max"]) &
                  Yb > min(ranges_s_OIB[9,"Yb min"]) &
                  Yb < max(ranges_s_OIB[9,"Yb max"])) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu)
NFB <- full_join(NFB,d_price2017) %>% dplyr::na_if(0)
d_jeanvoine2021 <- jeanvoine2021 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(SiO2 > min(ranges_s_OIB[9,"SiO2 min"]) &
                  SiO2 < max(ranges_s_OIB[9,"SiO2 max"]) &
                  Na2O > min(ranges_s_OIB[9,"Na2O min"]) &
                  Na2O < max(ranges_s_OIB[9,"Na2O max"]) &
                  Yb > min(ranges_s_OIB[9,"Yb min"]) &
                  Yb < max(ranges_s_OIB[9,"Yb max"])) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu)
NFB <- full_join(NFB,jeanvoine2021) %>% dplyr::na_if(0)

NFB_minmax <- data.frame (Sample  = c("North_Fiji_Basin_min", "North_Fiji_Basin_max"),
                          Location = c("North Fiji Basin", "North Fiji Basin"),
                          Cs = c(min(NFB[,"Cs"],na.rm=TRUE),max(NFB[,"Cs"],na.rm=TRUE)),
                          Rb = c(min(NFB[,"Rb"],na.rm=TRUE),max(NFB[,"Rb"],na.rm=TRUE)),
                          Ba = c(min(NFB[,"Ba"],na.rm=TRUE),max(NFB[,"Ba"],na.rm=TRUE)),
                          Th = c(min(NFB[,"Th"],na.rm=TRUE),max(NFB[,"Th"],na.rm=TRUE)),
                          U = c(min(NFB[,"U"],na.rm=TRUE),max(NFB[,"U"],na.rm=TRUE)),
                          Nb = c(min(NFB[,"Nb"],na.rm=TRUE),max(NFB[,"Nb"],na.rm=TRUE)),
                          Ta = c(min(NFB[,"Ta"],na.rm=TRUE),max(NFB[,"Ta"],na.rm=TRUE)),
                          La = c(min(NFB[,"La"],na.rm=TRUE),max(NFB[,"La"],na.rm=TRUE)),
                          Ce = c(min(NFB[,"Ce"],na.rm=TRUE),max(NFB[,"Ce"],na.rm=TRUE)),
                          Pr = c(min(NFB[,"Pr"],na.rm=TRUE),max(NFB[,"Pr"],na.rm=TRUE)),
                          Nd = c(min(NFB[,"Nd"],na.rm=TRUE),max(NFB[,"Nd"],na.rm=TRUE)),
                          Sr = c(min(NFB[,"Sr"],na.rm=TRUE),max(NFB[,"Sr"],na.rm=TRUE)),
                          Sm = c(min(NFB[,"Sm"],na.rm=TRUE),max(NFB[,"Sm"],na.rm=TRUE)),
                          Zr = c(min(NFB[,"Zr"],na.rm=TRUE),max(NFB[,"Zr"],na.rm=TRUE)),
                          Ti = c(min(NFB[,"Ti"],na.rm=TRUE),max(NFB[,"Ti"],na.rm=TRUE)),
                          Eu = c(min(NFB[,"Eu"],na.rm=TRUE),max(NFB[,"Eu"],na.rm=TRUE)),
                          Gd = c(min(NFB[,"Gd"],na.rm=TRUE),max(NFB[,"Gd"],na.rm=TRUE)),
                          Tb = c(min(NFB[,"Tb"],na.rm=TRUE),max(NFB[,"Tb"],na.rm=TRUE)),
                          Dy = c(min(NFB[,"Dy"],na.rm=TRUE),max(NFB[,"Dy"],na.rm=TRUE)),
                          Y = c(min(NFB[,"Y"],na.rm=TRUE),max(NFB[,"Y"],na.rm=TRUE)),
                          Er = c(min(NFB[,"Er"],na.rm=TRUE),max(NFB[,"Er"],na.rm=TRUE)),
                          Yb = c(min(NFB[,"Yb"],na.rm=TRUE),max(NFB[,"Yb"],na.rm=TRUE)),
                          Lu = c(min(NFB[,"Lu"],na.rm=TRUE),max(NFB[,"Lu"],na.rm=TRUE)))

d <- full_join(C_minmax,HW_minmax)
d <- full_join(d,NFB_minmax)
d_spider <- d %>% normalize_to_pm()

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("K-12-26")) %>%
  mutate(Location = case_when(grepl("K-12-26", Sample) ~ "K-12-26")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

shapes <- c("Hawai'i islands"=5,"Caroline islands"=0,"North Fiji Basin"=8,
            "K-12-26"=14)
cols <- c("Hawai'i islands"="#F4DD53","Caroline islands"="#320A5A",
          "North Fiji Basin"="#B4C630","K-12-26"="red")

K_12_26_spider_minmax <- d_spider %>%
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
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
K_12_26_spider_minmax

pdf(here("analysis","supplementary-materials","FigS20","FigS20-i.pdf"), width=3.5, height=2)
K_12_26_spider_minmax
dev.off()
