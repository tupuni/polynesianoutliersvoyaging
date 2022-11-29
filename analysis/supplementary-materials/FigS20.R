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
d <- q38
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
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                Ce,Pr,Pb,Nd,Sr,Sm,Zr,Hf,Ti,Eu,Gd,Tb,Dy,Ho,Y,Er,
                Li,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>%
  dplyr::filter(Sample %in% c("E-11-08","T-12-06","T-12-07","T-12-08",
                              "T-12-09","T-12-10")) %>%
  dplyr::mutate(Location = Sample) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                Ce,Pr,Pb,Nd,Sr,Sm,Zr,Hf,Ti,Eu,Gd,Tb,Dy,Ho,Y,Er,
                Li,Yb,Lu) %>%
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
        legend.title = element_blank(),legend.text = element_text(size = 4),
        legend.key.size = unit(.1, 'cm'), legend.background=element_blank(),
        legend.position = c(.3,.35), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
tutuila_spider

pdf(here("analysis","supplementary-materials","FigS20","FigS20-a.pdf"), width=6, height=2)
tutuila_spider|tutuila
dev.off()


#### K_12_24 ####
OIB <- q15 %>% dplyr::select(
  Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
is.na(OIB) <- sapply(OIB, is.infinite) #replace Inf by NA
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

d <- q39
s <- joined_data %>% dplyr::mutate(Location=Sample)

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
        legend.title = element_blank(), legend.text = element_text(size = 4),
        legend.key.size = unit(.2, 'cm'), legend.background=element_blank(),
        legend.position=c(.2,.85), aspect.ratio=1)
K_12_24_TAS

d <- q40
d_spider <- d %>%
  dplyr::filter(Location %in% c("[1867354] Ponape (Caroline)",
                              "[1867352] Ponape (Caroline)",
                              "[1867350] Ponape (Caroline)",
                              "[KOS 13-4] Kosrae (Caroline)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                Ce,Pr,Pb,Nd,Sr,Sm,Zr,Hf,Ti,Eu,Gd,Tb,Dy,Ho,Y,Er,
                Li,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("K-12-24")) %>%
  mutate(Location = case_when(grepl("K-12-24", Sample) ~ "K-12-24")) %>%
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
        legend.title = element_blank(),legend.text = element_text(size = 4),
        legend.key.size = unit(.2, 'cm'), legend.background=element_blank(),
        legend.position = c(.3,.35), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
K_12_24_spider

pdf(here("analysis","supplementary-materials","FigS20","FigS20-b.pdf"), width=6, height=2)
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
d <- q41
s <- joined_data %>% dplyr::mutate(Location=Sample)

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
        legend.title = element_blank(), legend.text = element_text(size = 4),
        legend.key.size = unit(.2, 'cm'), legend.background=element_blank(),
        legend.position=c(.2,.85), aspect.ratio=1)
K_12_25_TAS_2

d_spider <- d  %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                Ce,Pr,Pb,Nd,Sr,Sm,Zr,Hf,Ti,Eu,Gd,Tb,Dy,Ho,Y,Er,
                Li,Yb,Lu) %>%
  normalize_to_pm()
s_spider <- joined_data %>% dplyr::filter(Sample %in% c("K-12-25")) %>%
  mutate(Location = case_when(grepl("K-12-25", Sample) ~ "K-12-25")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                Ce,Pr,Pb,Nd,Sr,Sm,Zr,Hf,Ti,Eu,Gd,Tb,Dy,Ho,Y,Er,
                Li,Yb,Lu) %>%
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
        legend.title = element_blank(),legend.text = element_text(size = 4),
        legend.key.size = unit(.2, 'cm'), legend.background=element_blank(),
        legend.position = c(.3,.3), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
K_12_25_spider_2

pdf(here("analysis","supplementary-materials","FigS20","FigS20-c.pdf"), width=6, height=2)
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
d <- q42
s <- joined_data %>% dplyr::mutate(Location=Sample)

cols <- c("Chuuk"="#6D58A5","Kosrae"="#8476B6","Ponape"="#341D59","K-12-26"="red")
shapes <- c("Chuuk"=23,"Kosrae"=22,"Ponape"=21,"K-12-26"=14)
contour <- c("Chuuk"="black","Kosrae"="black","Ponape"="black","K-12-26"="red")

K_12_26_TAS <- tas +
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
        legend.title = element_blank(), legend.text = element_text(size = 4),
        legend.key.size = unit(.2, 'cm'), legend.background=element_blank(),
        legend.position=c(.2,.85), aspect.ratio=1)
K_12_26_TAS

d_spider <- d  %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                Ce,Pr,Pb,Nd,Sr,Sm,Zr,Hf,Ti,Eu,Gd,Tb,Dy,Ho,Y,Er,
                Li,Yb,Lu) %>%
  normalize_to_pm()
s_spider <- joined_data %>% dplyr::filter(Sample %in% c("K-12-26")) %>%
  mutate(Location = case_when(grepl("K-12-26", Sample) ~ "K-12-26")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                Ce,Pr,Pb,Nd,Sr,Sm,Zr,Hf,Ti,Eu,Gd,Tb,Dy,Ho,Y,Er,
                Li,Yb,Lu) %>%
  normalize_to_pm()

K_12_26_spider <- d_spider %>%
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
        legend.title = element_blank(),legend.text = element_text(size = 4),
        legend.key.size = unit(.2, 'cm'), legend.background=element_blank(),
        legend.position = c(.3,.3), legend.direction = "vertical") +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # dodge = 2 to stagger
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
K_12_26_spider

pdf(here("analysis","supplementary-materials","FigS20","FigS20-d.pdf"), width=6, height=2)
K_12_26_spider|K_12_26_TAS
dev.off()


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
