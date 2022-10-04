require(here)
require(tidyverse)
require(RSQLite)
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
ranges_s_OIB[7,1:35]
OIB <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
`NA2O(WT%)` > 1.61 AND `NA2O(WT%)` < 4.61 AND
`K2O(WT%)` > 0 AND `K2O(WT%)` < 2.91 AND
file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv'") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::rename(Location=LOCATION) %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

d <- OIB %>% dplyr::select(Sample,Location,Rb,Nb,La,Nd,Sr,Zr,Ti)
is.na(d) <- sapply(d, is.infinite) #replace Inf by NA
d[d == 0] <- NA # Replace 0 with NA
d <- d[rowSums(is.na(d)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-24")) %>%
  mutate(Location = case_when(grepl("K-12-24", Sample) ~ "K-12-24")) %>%
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
  Sample = c(d_pca[1:80,"Sample"]),
  Location = c(d_pca[1:80,"Location"]),
  PC1 = c(sqrt(((d_pca[81,"PC1"])-d_pca[1:80,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[81,"PC2"])-d_pca[1:80,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[81,"PC3"])-d_pca[1:80,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[81,"PC4"])-d_pca[1:80,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[81,"PC5"])-d_pca[1:80,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[81,"PC6"])-d_pca[1:80,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[81,"PC7"])-d_pca[1:80,"PC7"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])) / (sum(eig[1:7,2])))

head(dist[order(dist$weight_mean),] %>%
       dplyr::select("Sample","Location","weight_mean"), 10)

#### plot ####
d_spider <- OIB %>%
  dplyr::filter(Sample %in% c("34227","120903-KOS 13-4","1867349","1867355")) %>%
  mutate(Location = case_when(
    grepl("34227", Sample) ~ "[34227] Kosrae (Caroline)",
    grepl("120903-KOS 13-4", Sample) ~ "[13-4] Kosrae (Caroline)",
    grepl("1867349", Sample) ~ "[1867349] Ponape (Caroline)",
    grepl("1867355", Sample) ~ "[1867355] Ponape (Caroline)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("K-12-24")) %>%
  mutate(Location = case_when(grepl("K-12-24", Sample) ~ "K-12-24")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()


shapes <- c("[34227] Kosrae (Caroline)"=0,
            "[13-4] Kosrae (Caroline)"=1,
            "[1867349] Ponape (Caroline)"=2,
            "[1867355] Ponape (Caroline)"=3,
            "K-12-24"=12)
cols <- c("[34227] Kosrae (Caroline)"="#320A5A",
          "[13-4] Kosrae (Caroline)"="#320A5A",
          "[1867349] Ponape (Caroline)"="#320A5A",
          "[1867355] Ponape (Caroline)"="#320A5A",
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

dplyr::filter(Sample %in% c("34227","120903-KOS 13-4","1867349","1867355")) %>%

citation <- dbGetQuery(georoc,
"SELECT sample_id, reference_id
FROM 'citation'
WHERE sample_id='34227' OR
sample_id='120903-KOS 13-4' OR
sample_id='1867349' OR
sample_id='1867355'")
citation
reference <- dbGetQuery(georoc,
"SELECT id, reference
FROM 'reference'
WHERE id='23592' OR
id='21069' OR
id='24239' OR
id='767'") %>% rename(reference_id=id)
cite <- full_join(citation,reference)
cite

#### K_12_25 ####
ranges_s_OIB[8,1:35]

OIB1 <- dbGetQuery(pofatu,
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
WHERE s.sample_category = 'SOURCE' AND location_region LIKE '%HAWAI%' AND
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
  dplyr::mutate(Location = case_when(
    grepl("HAWAI'I", location_subregion) ~ "Hawai'i",
    grepl("KAHO'OLAWE", location_subregion) ~ "Kaho'olawe",
    grepl("KAUA'I", location_subregion) ~ "Kaua'i",
    grepl("MAUI", location_subregion) ~ "Maui",
    grepl("MOLOKAI", location_subregion) ~ "Moloka'i",
    grepl("MOLOKA'I", location_subregion) ~ "Moloka'i",
    grepl("LANA'I", location_subregion) ~ "Lana'i",
    grepl("O'AHU", location_subregion) ~ "O'ahu")) %>%
  dplyr::filter(Location != "NA") %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB1 %>% group_by(Location) %>% tally()

OIB2 <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE LAND_OR_SEA='SAE' AND ROCK_TYPE='VOL' AND
`SIO2(WT%)` > 43.1 AND `SIO2(WT%)` < 46.1 AND
`NA2O(WT%)` > 0.89 AND `NA2O(WT%)` < 3.89 AND
`K2O(WT%)` < 1.9 AND
`MGO(WT%)` > 10.7 AND `MGO(WT%)` < 13.7 AND
(file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part1.csv' OR
file_id = '2022-06-WFJZKY_HAWAIIAN_ISLANDS_part2.csv' OR
file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv')") %>%
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
OIB2 %>% group_by(Location) %>% tally()
OIB <- full_join(OIB1,OIB2)
OIB[OIB == 0] <- NA # Replace 0 with NA

d <- OIB %>% dplyr::select(Sample,Location,Rb,La,Nd,Sr,Zr,Ti,Yb)
is.na(d) <- sapply(d, is.infinite) #replace Inf by NA
d[d == 0] <- NA # Replace 0 with NA
d <- d[rowSums(is.na(d)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-25")) %>%
  mutate(Location = case_when(grepl("K-12-25", Sample) ~ "K-12-25")) %>%
  dplyr::select(Sample,Location,Rb,La,Nd,Sr,Zr,Ti,Yb)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(d[,3:9], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
pred <- stats::predict(res.pca, s[,3:9])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(d[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

dist <- data.frame(
  Sample = c(d_pca[1:82,"Sample"]),
  Location = c(d_pca[1:82,"Location"]),
  PC1 = c(sqrt(((d_pca[83,"PC1"])-d_pca[1:82,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[83,"PC2"])-d_pca[1:82,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[83,"PC3"])-d_pca[1:82,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[83,"PC4"])-d_pca[1:82,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[83,"PC5"])-d_pca[1:82,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[83,"PC6"])-d_pca[1:82,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[83,"PC7"])-d_pca[1:82,"PC7"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])) / (sum(eig[1:7,2])))

head(dist[order(dist$weight_mean),] %>%
       dplyr::select("Sample","Location","weight_mean"), 10)

#### plot ####
d_spider <- OIB %>%
  dplyr::filter(Sample %in% c("34330-71KAUH-2","370024","370018","370017","370023")) %>%
  mutate(Location = case_when(
    grepl("34330-71KAUH-2", Sample) ~ "[71KAUH-2] Moloka'i (Hawai'i)",
    grepl("370024", Sample) ~ "[370024] Ni'ihau (Hawai'i)",
    grepl("370018", Sample) ~ "[370018] Ni'ihau (Hawai'i)",
    grepl("370017", Sample) ~ "[370017] Ni'ihau (Hawai'i)",
    grepl("370023", Sample) ~ "[370023] Ni'ihau (Hawai'i)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()
d_spider[d_spider == 0] <- NA # Replace 0 with NA

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("K-12-25")) %>%
  mutate(Location = case_when(grepl("K-12-25", Sample) ~ "K-12-25")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

shapes <- c("[71KAUH-2] Moloka'i (Hawai'i)"=0,
            "[370024] Ni'ihau (Hawai'i)"=1,
            "[370018] Ni'ihau (Hawai'i)"=2,
            "[370017] Ni'ihau (Hawai'i)"=3,
            "[370023] Ni'ihau (Hawai'i)"=5,
            "K-12-25"=13)
cols <- c("[71KAUH-2] Moloka'i (Hawai'i)"="#F4DD53",
          "[370024] Ni'ihau (Hawai'i)"="#F4DD53",
          "[370018] Ni'ihau (Hawai'i)"="#F4DD53",
          "[370017] Ni'ihau (Hawai'i)"="#F4DD53",
          "[370023] Ni'ihau (Hawai'i)"="#F4DD53",
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

pdf(here("analysis","supplementary-materials","FigS20","FigS20-h.pdf"), width=3.5, height=2)
K_12_25_spider
dev.off()

citation <- dbGetQuery(georoc,
"SELECT sample_id, reference_id
FROM 'citation'
WHERE sample_id='34330-71KAUH-2' OR
sample_id='370024' OR
sample_id='370018' OR
sample_id='370017' OR
sample_id='370023'")
citation
reference <- dbGetQuery(georoc,
"SELECT id, reference
FROM 'reference'
WHERE id='7517' OR id='668' OR id='12504'") %>% rename(reference_id=id)
cite <- full_join(citation,reference)
cite


#### K_12_26 ####
ranges_s_OIB[9,1:35]
OIB <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND' AND
`K2O(WT%)` < 2.351 AND
file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv'") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::rename(Location=LOCATION) %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

d <- OIB %>% dplyr::select(Sample,Location,Rb,La,Nd,Sr,Zr,Ti,Yb)
is.na(d) <- sapply(d, is.infinite) #replace Inf by NA
d[d == 0] <- NA # Replace 0 with NA
d <- d[rowSums(is.na(d)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-26")) %>%
  mutate(Location = case_when(grepl("K-12-26", Sample) ~ "K-12-26")) %>%
  dplyr::select(Sample,Location,Rb,La,Nd,Sr,Zr,Ti,Yb)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(d[,3:9], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
pred <- stats::predict(res.pca, s[,3:9])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(d[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

dist <- data.frame(
  Sample = c(d_pca[1:70,"Sample"]),
  Location = c(d_pca[1:70,"Location"]),
  PC1 = c(sqrt(((d_pca[71,"PC1"])-d_pca[1:70,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[71,"PC2"])-d_pca[1:70,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[71,"PC3"])-d_pca[1:70,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[71,"PC4"])-d_pca[1:70,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[71,"PC5"])-d_pca[1:70,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[71,"PC6"])-d_pca[1:70,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[71,"PC7"])-d_pca[1:70,"PC7"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])) / (sum(eig[1:7,2])))

head(dist[order(dist$weight_mean),] %>%
       dplyr::select("Sample","Location","weight_mean"), 10)

#### plot ####
d_spider <- OIB %>%
  dplyr::filter(Sample %in% c("1867336","1867337","1175189","1175187")) %>%
  mutate(Location = case_when(
    grepl("1867336", Sample) ~ "[1867336] Ponape (Caroline)",
    grepl("1867337", Sample) ~ "[1867337] Ponape (Caroline)",
    grepl("1175189", Sample) ~ "[1175189] Ponape (Caroline)",
    grepl("1175187", Sample) ~ "[1175187] Ponape (Caroline)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()
d_spider[d_spider == 0] <- NA # Replace 0 with NA

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("K-12-26")) %>%
  mutate(Location = case_when(grepl("K-12-26", Sample) ~ "K-12-26")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()


shapes <- c("[1867336] Ponape (Caroline)"=0,
            "[1867337] Ponape (Caroline)"=1,
            "[1175189] Ponape (Caroline)"=2,
            "[1175187] Ponape (Caroline)"=5,
            "K-12-26"=14)
cols <- c("[1867336] Ponape (Caroline)"="#320A5A",
          "[1867337] Ponape (Caroline)"="#320A5A",
          "[1175189] Ponape (Caroline)"="#320A5A",
          "[1175187] Ponape (Caroline)"="#320A5A",
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

pdf(here("analysis","supplementary-materials","FigS20","FigS20-i.pdf"), width=3.5, height=2)
K_12_26_spider
dev.off()

dplyr::filter(Sample %in% c("1867336","1867337","1175189","1175187")) %>%

citation <- dbGetQuery(georoc,
"SELECT sample_id, reference_id
FROM 'citation'
WHERE sample_id='1867336' OR
sample_id='1867337' OR
sample_id='1175189' OR
sample_id='1175187'")
citation
reference <- dbGetQuery(georoc,
"SELECT id, reference
FROM 'reference'
WHERE id='21069' OR id='24239'") %>% rename(reference_id=id)
cite <- full_join(citation,reference)
cite


