require(here)
require(tidyverse)
require(RSQLite)
require(FactoMineR)
require(factoextra)
require(patchwork)

georoc <- dbConnect(RSQLite::SQLite(), path_to_georoc)
pofatu <- dbConnect(RSQLite::SQLite(), path_to_pofatu)

dir.create(here("analysis","supplementary-materials","FigS20"))

#TAS template
df = data.frame(x = c(40,77), y = c(0,14))
theme_set(theme_bw(base_size=14))
tas <- ggplot(data=df, mapping=aes(x=x, y=y)) +
  geom_blank() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(limits=c(0.5,10), breaks = c(2,4,6,8,10), expand = c(0, 0)) +
  scale_x_continuous(limits=c(37,57), breaks = c(41,45,49,53,57), expand = c(0, 0)) +
  labs(y=expression(Na[2]*O + K[2]*O*~ "(wt%)"), x=expression(SiO[2]*~ "(wt%)"))+
  annotate("segment", x=45, xend=45, y=1, yend=5, size = 0.2, colour = "gray50")+
  annotate("segment", x=45, xend=52, y=5, yend=5, size = 0.2, colour = "gray50")+
  annotate("segment", x=52, xend=57, y=5, yend=5.9, size = 0.2, colour = "gray50")+
  annotate("segment", x=45, xend=49.4, y=5, yend= 7.3, size = 0.2, colour = "gray50")+
  annotate("segment", x=49.4, xend=53, y=7.3, yend=9.3, size = 0.2, colour = "gray50")+
  annotate("segment", x=52, xend=69, y=5, yend=8, size = 0.2, colour = "gray50")+
  annotate("segment", x=76.5, xend=69, y=1, yend=8, size = 0.2, colour = "gray50")+
  annotate("segment", x=69, xend=69, y=8, yend=13, size = 0.2, colour = "gray50")+
  annotate("segment", x=45, xend=61.32, y=5, yend=13.7, size = 0.2, colour = "gray50")+
  annotate("segment", x=52, xend=52, y=1, yend=5, size = 0.2, colour = "gray50")+
  annotate("segment", x=57, xend=57, y=1, yend=5.9, size = 0.2, colour = "gray50")+
  annotate("segment", x=63, xend=63, y=1, yend=6.9, size = 0.2, colour = "gray50")+
  annotate("segment", x=52, xend=49.4, y=5, yend=7.3, size = 0.2, colour = "gray50")+
  annotate("segment", x=57, xend=53.05, y=5.9, yend=9.25, size = 0.2, colour = "gray50")+
  annotate("segment", x=63, xend=57.6, y=6.9, yend=11.7, size = 0.2, colour = "gray50")+
  annotate("segment", x=41, xend=45, y=3, yend=3, size = 0.2, colour = "gray50")+
  annotate("segment", x=41, xend=41, y=1, yend=3, size = 0.2, colour = "gray50")+
  annotate("segment", x=41, xend=41, y=3, yend=7, size = 0.2, colour = "gray50")+
  annotate("segment", x=41, xend=45, y=7, yend=9.4, size = 0.2, colour = "gray50")+
  annotate("segment", x=45, xend=52.5, y=9.4, yend=14, size = 0.2, colour = "gray50")+
  annotate("segment", x=49.4, xend=45, y=7.3, yend=9.4, size = 0.2, colour = "gray50")+
  annotate("segment", x=53, xend=48.4, y=9.3, yend=11.5, size = 0.2, colour = "gray50")+
  annotate("segment", x=57.6, xend=52.5, y=11.7, yend=14, size = 0.2, colour = "gray50")

#### Tatagamatau ####
ranges_s_OIB[1,1:35]
d <- dbGetQuery(pofatu,
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
  dplyr::filter(Location %in% c("Tatagamatau")) %>%
  dplyr::mutate(Location = case_when(
    grepl("collerson2007_KC-05-19", Sample) ~ "[KC-05-19] Tatagamatau (Tutuila)",
    grepl("collerson2007_KC-05-18", Sample) ~ "[KC-05-18] Tatagamatau (Tutuila)",
    grepl("collerson2007_KC-05-14", Sample) ~ "[KC-05-14] Tatagamatau (Tutuila)",
    TRUE ~ "Tatagamatau"))

s <- joined_data %>% dplyr::mutate(Location=Sample)

shapes <- c("[KC-05-19] Tatagamatau (Tutuila)"=0,
            "[KC-05-14] Tatagamatau (Tutuila)"=1,
            "[KC-05-18] Tatagamatau (Tutuila)"=2,
            "Tatagamatau"=3,"E-11-08"=5,"T-12-06"=2,"T-12-07"=7,
            "T-12-08"=6,"T-12-09"=10,"T-12-10"=11)
cols <- c("[KC-05-19] Tatagamatau (Tutuila)"="#781B6C",
          "[KC-05-14] Tatagamatau (Tutuila)"="#781B6C",
          "[KC-05-18] Tatagamatau (Tutuila)"="#781B6C",
          "Tatagamatau"="#781B6C","E-11-08"="red","T-12-06"="red",
          "T-12-07"="red","T-12-08"="red","T-12-09"="red","T-12-10"="red")

tutuila <- tas +
  geom_point(data=d,
             aes(x=SiO2, y=Na2O+K2O, shape=factor(Location), color=factor(Location),
                 group=Sample), size=1, stroke=.25) +
  geom_point(data=s,aes(x=SiO2, y=Na2O+K2O, shape=factor(Location),
                        color=factor(Location), group=Sample), size=1, stroke=.5) +
  scale_shape_manual(values = shapes) + scale_color_manual(values = cols) +
  scale_y_continuous(position = "right") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.title = element_blank(), legend.text = element_text(size = 5),
        legend.position="none", aspect.ratio=1)
tutuila


d_spider <- d %>%
  dplyr::filter(Sample %in% c("collerson2007_KC-05-19",
                              "collerson2007_KC-05-18",
                              "collerson2007_KC-05-14")) %>%
  mutate(Location = case_when(
    grepl("collerson2007_KC-05-19", Sample) ~ "[KC-05-19] Tatagamatau (Tutuila)",
    grepl("collerson2007_KC-05-18", Sample) ~ "[KC-05-18] Tatagamatau (Tutuila)",
    grepl("collerson2007_KC-05-14", Sample) ~ "[KC-05-14] Tatagamatau (Tutuila)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>%
  dplyr::filter(Sample %in% c("E-11-08","T-12-06","T-12-07","T-12-08",
                              "T-12-09","T-12-10")) %>%
  dplyr::mutate(Location = Sample) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

tutuila_spider <- d_spider %>%
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
        legend.position = c(.3,.4), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
tutuila_spider

pdf(here("analysis","supplementary-materials","FigS20","FigS20-a.pdf"), width=3.5, height=2)
tutuila_spider
dev.off()

pdf(here("analysis","supplementary-materials","FigS20","FigS20-a-class.pdf"), width=6, height=2)
tutuila_spider|tutuila
dev.off()


#### K_12_24 ####
OIB <- q15 %>% dplyr::select(
  Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
is.na(OIB) <- sapply(OIB, is.infinite) #replace Inf by NA
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

d <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE (LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
`CS(PPM)` > 0 AND `YB(PPM)` < 100 AND
file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv')") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::mutate(Location = case_when(
    grepl("KOSRAE", LOCATION) ~ "Kosrae",
    grepl("CHUUK", LOCATION) ~ "Chuuk",
    grepl("PONAPE", LOCATION) ~ "Ponape")) %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

s <- joined_data %>% dplyr::mutate(Location=Sample) %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

cols <- c("Chuuk"="#6D58A5","Kosrae"="#8476B6","Ponape"="#341D59","K-12-24"="red")
shapes <- c("Chuuk"=23,"Kosrae"=22,"Ponape"=21, "K-12-24"=12)
contour <- c("Chuuk"="black","Kosrae"="black","Ponape"="black","K-12-24"="red")

K_12_24_TAS <- tas +
  geom_point(data=d,
             aes(x=SiO2, y=Na2O+K2O, shape=factor(Location), fill=factor(Location),
                 color=factor(Location),group=Sample), size=1, stroke=.25) +
  geom_point(data=s,aes(x=SiO2, y=Na2O+K2O, shape=factor(Location), fill=factor(Location),
                        color=factor(Location), group=Sample), size=1, stroke=.5) +
  scale_shape_manual(values = shapes) + scale_fill_manual(values = cols) +
  scale_color_manual(values = contour) + scale_y_continuous(position = "right") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.title = element_blank(), legend.text = element_text(size = 5),
        legend.position=c(.1,.8), aspect.ratio=1)
K_12_24_TAS


d <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv'") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  mutate(Location = case_when(
    grepl("1867354", Sample) ~ "[1867354] Ponape (Caroline)",
    grepl("1867352", Sample) ~ "[1867352] Ponape (Caroline)",
    grepl("1867350", Sample) ~ "[1867350] Ponape (Caroline)",
    grepl("KOS 13-4", Sample) ~ "[KOS 13-4] Kosrae (Caroline)")) %>%
  dplyr::select(Sample,Location,LOCATION,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

d_spider <- d %>%
  dplyr::filter(Location %in% c("[1867354] Ponape (Caroline)",
                              "[1867352] Ponape (Caroline)",
                              "[1867350] Ponape (Caroline)",
                              "[KOS 13-4] Kosrae (Caroline)")) %>%
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
             group=Sample)) +
  geom_line(size=.5) + geom_point(size=2, stroke=.5) +
  geom_line(data=s_spider, size=.5) + geom_point(data=s_spider, size=1, stroke=.5) +
  scale_shape_manual(values=shapes) + scale_color_manual(values=cols) +
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

pdf(here("analysis","supplementary-materials","FigS20","FigS20-b.pdf"), width=3.5, height=2)
K_12_24_spider
dev.off()

pdf(here("analysis","supplementary-materials","FigS20","FigS20-b-class.pdf"), width=6, height=2)
K_12_24_spider|K_12_24_TAS
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
d <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE (LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
`CS(PPM)` > 0 AND `YB(PPM)` < 100 AND
file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv')") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::mutate(Location = case_when(
    grepl("KOSRAE", LOCATION) ~ "Kosrae",
    grepl("CHUUK", LOCATION) ~ "Chuuk",
    grepl("PONAPE", LOCATION) ~ "Ponape")) %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

s <- joined_data %>% dplyr::mutate(Location=Sample) %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

cols <- c("Chuuk"="#6D58A5","Kosrae"="#8476B6","Ponape"="#341D59","K-12-25"="red")
shapes <- c("Chuuk"=23,"Kosrae"=22,"Ponape"=21,"K-12-25"=13)
contour <- c("Chuuk"="black","Kosrae"="black","Ponape"="black","K-12-25"="red")

K_12_25_TAS_2 <- tas +
  geom_point(data=d,
             aes(x=SiO2, y=Na2O+K2O, shape=factor(Location), fill=factor(Location),
                 color=factor(Location),group=Sample), size=1, stroke=.25) +
  geom_point(data=s,aes(x=SiO2, y=Na2O+K2O, shape=factor(Location), fill=factor(Location),
                        color=factor(Location), group=Sample), size=1, stroke=.5) +
  scale_shape_manual(values = shapes) + scale_fill_manual(values = cols) +
  scale_color_manual(values = contour) + scale_y_continuous(position = "right") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.title = element_blank(), legend.text = element_text(size = 5),
        legend.position=c(.1,.8), aspect.ratio=1)
K_12_25_TAS_2

d_spider <- d  %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()
s_spider <- joined_data %>% dplyr::filter(Sample %in% c("K-12-25")) %>%
  mutate(Location = case_when(grepl("K-12-25", Sample) ~ "K-12-25")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

K_12_25_spider_2 <- d_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Nd","Sr","Sm","Zr","Ti","Eu","Gd","Tb",
                           "Dy","Y","Er","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=factor(Location), color=factor(Location),
             fill=factor(Location), group=Sample)) +
  geom_line(size=.5) + geom_point(size=1, stroke=.5) +
  geom_line(data=s_spider, size=.5) + geom_point(data=s_spider, size=1, stroke=.5) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
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
K_12_25_spider_2

pdf(here("analysis","supplementary-materials","FigS20","FigS20-c-class(2).pdf"), width=6, height=2)
K_12_25_spider_2|K_12_25_TAS_2
dev.off()


citation <- dbGetQuery(georoc,
"SELECT sample_id, reference_id
FROM 'citation'
WHERE sample_id = '143075' OR sample_id = '143077'")
citation
reference <- dbGetQuery(georoc,
"SELECT id, reference
FROM 'reference'
WHERE id='6333'") %>% rename(reference_id=id)
reference
cite <- full_join(citation,reference)
cite


#### K_12_26 ####
ranges_s_OIB[9,1:35]
OIB <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE (LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
`NB(PPM)` > 22.2 AND `NB(PPM)` < 66.6 AND
`YB(PPM)` < 100 AND
file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv') OR
(ROCK_TYPE='VOL' AND
file_id = '2022-06-PVFZCE_TONGA_ARC.csv' AND
`NB(PPM)` > 22.2 AND `NB(PPM)` < 66.6 AND
LATITUDE_MAX > -16)") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

d_price2014 <- price2014 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(Nb > 22.2 & Nb < 66.6) %>%
  dplyr::select(Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
OIB <- full_join(OIB,d_price2014)
d_price2017 <- price2017 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(Nb > 22.2 & Nb < 66.6) %>%
  dplyr::select(Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
OIB <- full_join(OIB,d_price2017)
d_jeanvoine2021 <- jeanvoine2021 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(Nb > 22.2 & Nb < 66.6) %>%
  dplyr::select(Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
d <- full_join(OIB,d_jeanvoine2021)
is.na(d) <- sapply(d, is.infinite) #replace Inf by NA
d[d == 0] <- NA # Replace 0 with NA
d %>% group_by(Location) %>% tally()

s <- joined_data %>% dplyr::mutate(Location=Sample)

shapes <- c("Caroline islands"=21,"North Fiji Basin"=25,"Rotuma"=21,
            "Futuna"=22,"Cikobia"=23,"Uvea"=24,"K-12-26"=14)
cols <- c("Caroline islands"="#320A5A","North Fiji Basin"="#B4C630",
          "Rotuma"="#6EA002","Futuna"="#6EA002","Cikobia"="#6EA002",
          "Uvea"="#6EA002","K-12-26"="red")
contour <- c("Caroline islands"="black","North Fiji Basin"="black",
             "Rotuma"="black","Futuna"="black","Cikobia"="black","Uvea"="black",
             "K-12-26"="red")

K_12_26_TAS <- tas +
  geom_point(data=d,
             aes(x=SiO2, y=Na2O+K2O, shape=factor(Location),
                 color=factor(Location),fill=factor(Location),
                 group=Sample), size=1, stroke=.25) +
  geom_point(data=s,aes(x=SiO2, y=Na2O+K2O, shape=factor(Location),
                        color=factor(Location), group=Sample), size=1, stroke=.5) +
  scale_shape_manual(values = shapes) + scale_color_manual(values = contour) +
  scale_fill_manual(values = cols) + scale_y_continuous(position = "right") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.title = element_blank(), legend.text = element_text(size = 5),
        legend.position="none", aspect.ratio=1)
K_12_26_TAS



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

NFB <- OIB %>% dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                             Ce,Pr,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  dplyr::filter(Location %in% c("Tonga-Fiji")) %>% dplyr::na_if(0) %>%
  mutate(Location=recode(Location,"Tonga-Fiji"="North Fiji Basin"))
d_price2014 <- price2014 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(Nb > min(ranges_s_OIB[9,"Nb min"]) &
                  Nb < max(ranges_s_OIB[9,"Nb max"])) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu)
d_price2017 <- price2017 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(Nb > min(ranges_s_OIB[9,"Nb min"]) &
                  Nb < max(ranges_s_OIB[9,"Nb max"])) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu)
NFB <- full_join(NFB,d_price2017) %>% dplyr::na_if(0)
d_jeanvoine2021 <- jeanvoine2021 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(Nb > min(ranges_s_OIB[9,"Nb min"]) &
                  Nb < max(ranges_s_OIB[9,"Nb max"])) %>%
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

d <- full_join(C_minmax,NFB_minmax)
d_spider <- d %>% normalize_to_pm()

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("K-12-26")) %>%
  mutate(Location = case_when(grepl("K-12-26", Sample) ~ "K-12-26")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()


shapes <- c("Caroline islands"=21,"North Fiji Basin"=25,"Rotuma"=21,
            "Futuna"=22,"Cikobia"=23,"Uvea"=24,"K-12-26"=14)
cols <- c("Caroline islands"="#320A5A","North Fiji Basin"="#B4C630",
          "Rotuma"="#6EA002","Futuna"="#6EA002","Cikobia"="#6EA002",
          "Uvea"="#6EA002","K-12-26"="red")
contour <- c("Caroline islands"="black","North Fiji Basin"="black",
             "Rotuma"="black","Futuna"="black","Cikobia"="black","Uvea"="black",
             "K-12-26"="red")

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

pdf(here("analysis","supplementary-materials","FigS20","FigS20-d.pdf"), width=3.5, height=2)
K_12_26_spider_minmax
dev.off()

pdf(here("analysis","supplementary-materials","FigS20","FigS20-d-class.pdf"), width=6, height=2)
K_12_26_spider_minmax|K_12_26_TAS
dev.off()

#### K_12_26 bis ####
d <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE (LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
`CS(PPM)` > 0 AND `YB(PPM)` < 100 AND
file_id = '2022-06-WFJZKY_CAROLINE_ISLANDS.csv')") %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::mutate(Location = case_when(
    grepl("KOSRAE", LOCATION) ~ "Kosrae",
    grepl("CHUUK", LOCATION) ~ "Chuuk",
    grepl("PONAPE", LOCATION) ~ "Ponape")) %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

s <- joined_data %>% dplyr::mutate(Location=Sample) %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

cols <- c("Chuuk"="#6D58A5","Kosrae"="#8476B6","Ponape"="#341D59","K-12-26"="red")
shapes <- c("Chuuk"=23,"Kosrae"=22,"Ponape"=21,"K-12-26"=14)
contour <- c("Chuuk"="black","Kosrae"="black","Ponape"="black","K-12-26"="red")

K_12_26_TAS_2 <- tas +
  geom_point(data=d,
             aes(x=SiO2, y=Na2O+K2O, shape=factor(Location), fill=factor(Location),
                 color=factor(Location),group=Sample), size=1, stroke=.25) +
  geom_point(data=s,aes(x=SiO2, y=Na2O+K2O, shape=factor(Location), fill=factor(Location),
                        color=factor(Location), group=Sample), size=1, stroke=.5) +
  scale_shape_manual(values = shapes) + scale_fill_manual(values = cols) +
  scale_color_manual(values = contour) + scale_y_continuous(position = "right") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.title = element_blank(), legend.text = element_text(size = 5),
        legend.position=c(.1,.8), aspect.ratio=1)
K_12_26_TAS_2

d_spider <- d  %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()
s_spider <- joined_data %>% dplyr::filter(Sample %in% c("K-12-26")) %>%
  mutate(Location = case_when(grepl("K-12-26", Sample) ~ "K-12-26")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

K_12_26_spider_2 <- d_spider %>%
  mutate(var = fct_relevel(var,
                           "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr",
                           "Nd","Sr","Sm","Zr","Ti","Eu","Gd","Tb",
                           "Dy","Y","Er","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, shape=factor(Location), color=factor(Location),
             fill=factor(Location), group=Sample)) +
  geom_line(size=.5) + geom_point(size=1, stroke=.5) +
  geom_line(data=s_spider, size=.5) + geom_point(data=s_spider, size=1, stroke=.5) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
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
K_12_26_spider_2

pdf(here("analysis","supplementary-materials","FigS20","FigS20-d-class(2).pdf"), width=6, height=2)
K_12_26_spider_2|K_12_26_TAS_2
dev.off()



dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE id = '1175178' OR id = '1867324' OR id = '1867344'")

citation <- dbGetQuery(georoc,
"SELECT sample_id, reference_id
FROM 'citation'
WHERE sample_id = '1175178' OR sample_id = '1867324' OR sample_id = '1867344'")
citation
reference <- dbGetQuery(georoc,
"SELECT id, reference
FROM 'reference'
WHERE id='21069' OR id='24239'") %>% rename(reference_id=id)
reference
cite <- full_join(citation,reference)
cite
