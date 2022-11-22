require(here)
require(tidyverse)
require(patchwork)
require(FactoMineR)
require(factoextra)
require(stats)

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

#### SIO2 vs K2O ####
df = data.frame(x = c(43,78), y = c(0,4))
theme_set(theme_bw(base_size=14))
k2o_sio2 <- ggplot(data=df, mapping=aes(x=x, y=y)) + geom_blank() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(limits=c(0,5.5), breaks = c(1,2,3,4,5)) +
  scale_x_continuous(limits=c(43,78), breaks = c(45,52,56,63,70,78)) +
  labs(y=expression(K[2]*O*~ "(wt%)"), x=expression(SiO[2]*~ "(wt%)"))+
  annotate("segment", x=52, xend=52, y=0, yend=5.5, size=0.2, linetype="dashed", colour="gray70")+
  annotate("segment", x=56, xend=56, y=0, yend=5.5, size=0.2, linetype="dashed", colour="gray70")+
  annotate("segment", x=63, xend=63, y=0, yend=5.5, size=0.2, linetype="dashed", colour="gray70")+
  annotate("segment", x=70, xend=70, y=0, yend=5.5, size=0.2, linetype="dashed", colour="gray70")+
  annotate("segment", x=48, xend=52, y=0.3, yend=0.5, size=0.2, colour="gray70")+
  annotate("segment", x=52, xend=56, y=0.5, yend=0.7, size=0.2, colour="gray70")+
  annotate("segment", x=56, xend=63, y=0.7, yend=1, size=0.2, colour="gray70")+
  annotate("segment", x=63, xend=70, y=1, yend=1.3, size=0.2, colour="gray70")+
  annotate("segment", x=70, xend=78, y=1.3, yend=1.6, size=0.2, colour="gray70")+
  annotate("segment", x=48, xend=52, y=1.2, yend=1.5, size=0.2, colour="gray70")+
  annotate("segment", x=52, xend=56, y=1.5, yend=1.8, size=0.2, colour="gray70")+
  annotate("segment", x=56, xend=63, y=1.8, yend=2.4, size=0.2, colour="gray70")+
  annotate("segment", x=63, xend=70, y=2.4, yend=3, size=0.2, colour="gray70")+
  annotate("segment", x=70, xend=78, y=3, yend=3.7, size=0.2, colour="gray70")+
  annotate("segment", x=48, xend=52, y=1.7, yend=2.4, size=0.2, colour="gray70")+
  annotate("segment", x=52, xend=56, y=2.4, yend=3.2, size=0.2, colour="gray70")+
  annotate("segment", x=56, xend=63, y=3.2, yend=4, size=0.2, colour="gray70")+
  annotate("segment", x=45, xend=48, y=1.4, yend=1.7, size=0.2, colour="gray70")+
  annotate("segment", x=45, xend=48, y=0.9, yend=1.2, size=0.2, colour="gray70")+
  annotate("segment", x=45, xend=48, y=0.2, yend=0.3, size=0.2, colour="gray70")+
  annotate("text", label="Basalt", x=47.5, y=5, size=1, colour="black")+
  annotate("text", label="Basaltic\n andesite", x=54, y=5, size=1, colour="black")+
  annotate("text", label="Andesite", x=59.5, y=5, size=1, colour="black")+
  annotate("text", label="Dacite", x=66.5, y=5, size=1, colour="black")+
  annotate("text", label="Rhyolite", x=74, y=5, size=1, colour="black")+
  annotate("text", label="Alkaline series", x=51, y=3.3, size=1, colour="gray50", angle = 50)+
  annotate("text", label="High-K calc-alkaline series", x=59.5, y=2.8, size=1, colour="gray50", angle = 35)+
  annotate("text", label="Calc-alkaline series", x=63, y=1.7, size=1, colour="gray50", angle = 17)+
  annotate("text", label="Low-K series", x=65, y=0.6, size=1, colour="gray50", angle = 4)

#### E_11_03 spider ####
d <- q21
d_spider <- d %>%
  dplyr::mutate(Location = case_when(
    grepl("reepmeyer2008_ANU9006", Sample) ~ "[ANU9006] Vanua Lava (Vanuatu)",
    grepl("reepmeyer2008_ANU9009", Sample) ~ "[ANU9009] Vanua Lava (Vanuatu)",
    grepl("reepmeyer2008_ANU9008", Sample) ~ "[ANU9008] Vanua Lava (Vanuatu)",
    grepl("reepmeyer2008_ANU9003", Sample) ~ "[ANU9003] Vanua Lava (Vanuatu)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("E-11-03")) %>%
  dplyr::mutate(Location = Sample) %>%
  dplyr::mutate(Island = Sample) %>%
  dplyr::select(Sample,Location,Island,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()


shapes <- c("[ANU9006] Vanua Lava (Vanuatu)"=0,
            "[ANU9009] Vanua Lava (Vanuatu)"=1,
            "[ANU9008] Vanua Lava (Vanuatu)"=2,
            "[ANU9003] Vanua Lava (Vanuatu)"=5,
            "E-11-03"=0)
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

#### E_11_03 classification ####
d <- d %>% dplyr::mutate(Location = case_when(
  grepl("reepmeyer2008_ANU9006", Sample) ~ "[ANU9006] Vanua Lava (Vanuatu)",
  grepl("reepmeyer2008_ANU9009", Sample) ~ "[ANU9009] Vanua Lava (Vanuatu)",
  grepl("reepmeyer2008_ANU9008", Sample) ~ "[ANU9008] Vanua Lava (Vanuatu)",
  grepl("reepmeyer2008_ANU9003", Sample) ~ "[ANU9003] Vanua Lava (Vanuatu)"))

s <- joined_data %>% dplyr::mutate(Location=Sample)

shapes <- c("[ANU9006] Vanua Lava (Vanuatu)"=0,
            "[ANU9009] Vanua Lava (Vanuatu)"=1,
            "[ANU9008] Vanua Lava (Vanuatu)"=2,
            "[ANU9003] Vanua Lava (Vanuatu)"=5,"E-11-03"=0)
cols <- c("[ANU9006] Vanua Lava (Vanuatu)"="#7AD04F",
          "[ANU9009] Vanua Lava (Vanuatu)"="#7AD04F",
          "[ANU9008] Vanua Lava (Vanuatu)"="#7AD04F",
          "[ANU9003] Vanua Lava (Vanuatu)"="#7AD04F","E-11-03"="red")

E_11_03 <- k2o_sio2 +
  geom_point(data=d,
             aes(x=SiO2, y=K2O, shape=factor(Location), color=factor(Location),
                 group=Sample), size=1, stroke=.25) +
  geom_point(data=s,aes(x=SiO2, y=K2O, shape=factor(Location),
                        color=factor(Location), group=Sample), size=1, stroke=.5) +
  scale_shape_manual(values = shapes) + scale_color_manual(values = cols) +
  scale_y_continuous(position = "right") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.title = element_blank(), legend.text = element_text(size = 5),
        legend.position="none", aspect.ratio=1)
E_11_03
pdf(here("analysis","supplementary-materials","FigS13","FigS13-a-class.pdf"), width=6, height=2)
E_11_03_spider|E_11_03
dev.off()


#### E_11_06 & E_11_07 spider ####
s <- joined_data %>% dplyr::filter(Sample %in% c("E-11-06","E-11-07")) %>%
  dplyr::mutate(Location = Sample) %>% dplyr::select(
        Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA
d <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-19")) %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204) %>%
  dplyr::mutate(Location = case_when(
    grepl("1141527", Sample) ~ "[1141527] Gaua (Vanuatu)",
    grepl("115854-AMB 88", Sample) ~ "[AMB 88] Ambrym (Vanuatu)",
    grepl("1871808", Sample) ~ "[1871808] Manadotua (Sulawesi)",
    grepl("70517", Sample) ~ "[70517] Ambae (Vanuatu)",
    grepl("E-11-10", Sample) ~ "[E-11-10] Emae (Vanuatu)",
    grepl("E-11-11", Sample) ~ "[E-11-11] Emae (Vanuatu)",
    grepl("E-11-13", Sample) ~ "[E-11-13] Emae (Vanuatu)",
    grepl("E-11-16", Sample) ~ "[E-11-16] Emae (Vanuatu)",
    grepl("E-11-18", Sample) ~ "[E-11-18] Emae (Vanuatu)",
    grepl("E-11-19", Sample) ~ "[E-11-19] Emae (Vanuatu)"))

d_spider <- d %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("E-11-06","E-11-07")) %>%
  dplyr::mutate(Location = Sample) %>%
  dplyr::mutate(Island = Sample) %>%
  dplyr::select(Sample,Location,Island,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

shapes <- c("[E-11-10] Emae (Vanuatu)"=0,
            "[E-11-11] Emae (Vanuatu)"=1,
            "[E-11-13] Emae (Vanuatu)"=2,
            "[E-11-16] Emae (Vanuatu)"=5,
            "[E-11-18] Emae (Vanuatu)"=6,
            "[E-11-19] Emae (Vanuatu)"=8,
            "E-11-06"=1,"E-11-07"=8)
cols <- c("[E-11-10] Emae (Vanuatu)"="#7AD04F",
          "[E-11-11] Emae (Vanuatu)"="#7AD04F",
          "[E-11-13] Emae (Vanuatu)"="#7AD04F",
          "[E-11-16] Emae (Vanuatu)"="#7AD04F",
          "[E-11-18] Emae (Vanuatu)"="#7AD04F",
          "[E-11-19] Emae (Vanuatu)"="#7AD04F",
          "E-11-06"="red","E-11-07"="red")

E_11_06_07_spider <- d_spider %>%
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
E_11_06_07_spider
pdf(here("analysis","supplementary-materials","FigS13","FigS13-b.pdf"), width=5, height=2)
E_11_06_07_spider
dev.off()

#### E_11_06 & E_11_07 classification ####
s <- joined_data %>% dplyr::mutate(Location=Sample)

E_11_0607 <- k2o_sio2 +
  geom_point(data=d,
             aes(x=SiO2, y=K2O, shape=factor(Location), color=factor(Location),
                 group=Sample), size=1, stroke=.25) +
  geom_point(data=s,aes(x=SiO2, y=K2O, shape=factor(Location),
                        color=factor(Location), group=Sample), size=1, stroke=.5) +
  scale_shape_manual(values = shapes) + scale_color_manual(values = cols) +
  scale_y_continuous(position = "right") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.title = element_blank(), legend.text = element_text(size = 5),
        legend.position="none", aspect.ratio=1)
E_11_0607
pdf(here("analysis","supplementary-materials","FigS13","FigS13-b-class.pdf"), width=6, height=2)
E_11_06_07_spider|E_11_0607
dev.off()


#### K_12_28 spider ####
q22[,1:4]
d_spider <- q22 %>%
  dplyr::mutate(Location = case_when(
    grepl("ULAWUN", Location) ~ "Ulawun volcano (New Britain)",
    grepl("LOLOBAU", Location) ~ "Lolobau Is. (New Britain)",
    grepl("SULU", Location) ~ "Sulu (New Britain)",
    grepl("WULAI", Location) ~ "Wulai Is. (New Britain)",
    grepl("MANAM", Location) ~ "Manam Is. (New Britain)")) %>%
  dplyr::mutate(Sample = case_when(
    grepl("13423-E5/11", Sample) ~ "[E5/11] Ulawun volcano (New Britain)",
    grepl("13426-F7/2", Sample) ~ "[F7/2] Lolobau Is. (New Britain)",
    grepl("13436-2649", Sample) ~ "[2649] Sulu (New Britain)",
    grepl("13436-2654A", Sample) ~ "[2654A] Sulu (New Britain)",
    grepl("13437-F5/2", Sample) ~ "[F5/2] Wulai Is.(New Britain)",
    grepl("634326", Sample) ~ "[634326] Manam Is. (New Britain)")) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("K-12-28")) %>%
  dplyr::mutate(Location = Sample) %>%
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
  dplyr::mutate(var = fct_relevel(var,
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


#### K_12_28 classification ####
d <- q23
d[,1:2]

s <- joined_data %>% dplyr::mutate(Location=Sample)

shapes <- c("Ulawun"=3,"Lolobau"=5,"Sulu"=0,"Wulai"=2,"Manam"=6,"K-12-28"=3)
cols <- c("Ulawun"="#25A782","Lolobau"="#25A782","Sulu"="#25A782",
          "Wulai"="#25A782","Manam"="#25A782","K-12-28"="red")
K_12_28 <- k2o_sio2 +
  geom_point(data=d,
             aes(x=SiO2, y=K2O, shape=factor(Location), color=factor(Location),
                 group=Sample), size=1, stroke=.25) +
  geom_point(data=s,aes(x=SiO2, y=K2O, shape=factor(Location),
                        color=factor(Location), group=Sample), size=1, stroke=.5) +
  scale_shape_manual(values = shapes) + scale_color_manual(values = cols) +
  scale_y_continuous(position = "right") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.title = element_blank(), legend.text = element_text(size = 5),
        legend.position="none", aspect.ratio=1)
K_12_28

pdf(here("analysis","supplementary-materials","FigS13","FigS13-c-class.pdf"), width=6, height=2)
K_12_28_spider|K_12_28
dev.off()

#### K_12_28 min max ####
V <- q24 %>% dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
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

B <- q24 %>% dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
                           Ce,Pr,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  dplyr::filter(Location %in% c("Bismarck Arc")) %>% dplyr::na_if(0)
B_minmax <- data.frame (Sample  = c("Bismarck_Arc_min", "Bismarck_Arc_max"),
                         Location = c("Bismarck Arc", "Bismarck Arc"),
                         Cs = c(min(B[,"Cs"],na.rm=TRUE),max(B[,"Cs"],na.rm=TRUE)),
                         Rb = c(min(B[,"Rb"],na.rm=TRUE),max(B[,"Rb"],na.rm=TRUE)),
                         Ba = c(min(B[,"Ba"],na.rm=TRUE),max(B[,"Ba"],na.rm=TRUE)),
                         Th = c(min(B[,"Th"],na.rm=TRUE),max(B[,"Th"],na.rm=TRUE)),
                         U = c(min(B[,"U"],na.rm=TRUE),max(B[,"U"],na.rm=TRUE)),
                         Nb = c(min(B[,"Nb"],na.rm=TRUE),max(B[,"Nb"],na.rm=TRUE)),
                         Ta = c(min(B[,"Ta"],na.rm=TRUE),max(B[,"Ta"],na.rm=TRUE)),
                         La = c(min(B[,"La"],na.rm=TRUE),max(B[,"La"],na.rm=TRUE)),
                         Ce = c(min(B[,"Ce"],na.rm=TRUE),max(B[,"Ce"],na.rm=TRUE)),
                         Pr = c(min(B[,"Pr"],na.rm=TRUE),max(B[,"Pr"],na.rm=TRUE)),
                         Nd = c(min(B[,"Nd"],na.rm=TRUE),max(B[,"Nd"],na.rm=TRUE)),
                         Sr = c(min(B[,"Sr"],na.rm=TRUE),max(B[,"Sr"],na.rm=TRUE)),
                         Sm = c(min(B[,"Sm"],na.rm=TRUE),max(B[,"Sm"],na.rm=TRUE)),
                         Zr = c(min(B[,"Zr"],na.rm=TRUE),max(B[,"Zr"],na.rm=TRUE)),
                         Ti = c(min(B[,"Ti"],na.rm=TRUE),max(B[,"Ti"],na.rm=TRUE)),
                         Eu = c(min(B[,"Eu"],na.rm=TRUE),max(B[,"Eu"],na.rm=TRUE)),
                         Gd = c(min(B[,"Gd"],na.rm=TRUE),max(B[,"Gd"],na.rm=TRUE)),
                         Tb = c(min(B[,"Tb"],na.rm=TRUE),max(B[,"Tb"],na.rm=TRUE)),
                         Dy = c(min(B[,"Dy"],na.rm=TRUE),max(B[,"Dy"],na.rm=TRUE)),
                         Y = c(min(B[,"Y"],na.rm=TRUE),max(B[,"Y"],na.rm=TRUE)),
                         Er = c(min(B[,"Er"],na.rm=TRUE),max(B[,"Er"],na.rm=TRUE)),
                         Yb = c(min(B[,"Yb"],na.rm=TRUE),max(B[,"Yb"],na.rm=TRUE)),
                         Lu = c(min(B[,"Lu"],na.rm=TRUE),max(B[,"Lu"],na.rm=TRUE)))

L <- q24 %>% dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
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

d <- full_join(V_minmax,B_minmax)
d <- full_join(d,L_minmax)
d_spider <- d %>% normalize_to_pm()

s_spider <- joined_data %>% dplyr::filter(Sample %in% c("K-12-28")) %>%
  dplyr::mutate(Location = Sample) %>%
  dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,Ce,Pr,
                Nd,Sr,Sm,Zr,Ti,Eu,Gd,Tb,Dy,Y,Er,Yb,Lu) %>%
  normalize_to_pm()

shapes <- c("Vanuatu Arc"=0,
            "Bismarck Arc"=1,
            "Luzon Arc"=2,
            "K-12-28"=3)
cols <- c("Vanuatu Arc"="#7AD04F",
          "Bismarck Arc"="#25A782",
          "Luzon Arc"="#440154",
          "K-12-28"="red")

K_12_28_spider_minmax <- d_spider %>%
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
K_12_28_spider_minmax

pdf(here("analysis","supplementary-materials","FigS13","FigS13-c(bis).pdf"), width=6, height=2)
K_12_28_spider_minmax|K_12_28
dev.off()


#### K_12_29 spider ####
d <- q25
d[,1:2]
s_d <- joined_data %>%
  dplyr::filter(Sample %in% c("E-11-13"))

d_spider <- full_join(d,s_d) %>%
  mutate(Location = case_when(
    grepl("13303-UA10", Sample) ~ "[UA10] Ureparapara (Vanuatu)",
    grepl("70512", Sample) ~ "[70512] Vanua Lava (Vanuatu)",
    grepl("13306-VMAC6", Sample) ~ "[VMAC6] Vanua Lava (Vanuatu)",
    grepl("317050", Sample) ~ "[317050] Kibobo Is. (Lau, Fiji)",
    grepl("1871790", Sample) ~ "[1871790] Buhias Is. (Indonesia)",
    grepl("144138-KS094", Sample) ~ "[KS094] Cebu (Philippines)",
    grepl("E-11-13", Sample) ~ "[E-11-13] Emae (Vanuatu)")) %>%
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
            "[E-11-13] Emae (Vanuatu)"=4,
            "[317050] Kibobo Is. (Lau, Fiji)"=3,
            "[1871790] Buhias Is. (Indonesia)"=6,
            "[KS094] Cebu (Philippines)"=2,
            "K-12-29"=4)
cols <- c("[70512] Vanua Lava (Vanuatu)"="#7AD04F",
          "[VMAC6] Vanua Lava (Vanuatu)"="#7AD04F",
          "[UA10] Ureparapara (Vanuatu)"="#7AD04F",
          "[E-11-13] Emae (Vanuatu)"="#7AD04F",
          "[317050] Kibobo Is. (Lau, Fiji)"="#BADD26",
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

pdf(here("analysis","supplementary-materials","FigS13","FigS13-d.pdf"), width=5, height=2)
K_12_29_spider
dev.off()

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


#### K_12_29 classification ####
emae <- joined_data %>% filter(Sample %in% c(
  "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-19")) %>%
  dplyr::mutate(Location = Sample) %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

d <- full_join(q26,emae)
s <- joined_data %>% dplyr::mutate(Location=Sample)

shapes <- c("Vanua Lava "=0,"Ureparapara"=5,"Emae"=4,
            "Kibobo"=3,"Buhias"=6,"Cebu"=2,"K-12-29"=4)
cols <- c("Vanua Lava "="#7AD04F","Ureparapara"="#7AD04F","Emae"="#7AD04F",
          "Kibobo"="#BADD26","Buhias"="#345E8C","Cebu"="#440154","K-12-29"="red")

K_12_29 <- k2o_sio2 +
  geom_point(data=d,
             aes(x=SiO2, y=K2O, shape=factor(Location), color=factor(Location),
                 group=Sample), size=1, stroke=.25) +
  geom_point(data=s,aes(x=SiO2, y=K2O, shape=factor(Location),
                        color=factor(Location), group=Sample), size=1, stroke=.5) +
  scale_shape_manual(values = shapes) + scale_color_manual(values = cols) +
  scale_y_continuous(position = "right") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.title = element_text(size = 9),
        axis.text = element_text(size = 7),
        legend.title = element_blank(), legend.text = element_text(size = 5),
        legend.position="none", aspect.ratio=1)
K_12_29

pdf(here("analysis","supplementary-materials","FigS13","FigS13-d-class.pdf"), width=6, height=2)
K_12_29_spider|K_12_29
dev.off()

#### K_12_29 min max ####
V <- q27 %>% dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
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

TF <- q27 %>% dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
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

L <- q27 %>% dplyr::select(Sample,Location,lat,long,Cs,Rb,Ba,Th,U,Nb,Ta,La,
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
  dplyr::mutate(Location = case_when(grepl("K-12-29", Sample) ~ "K-12-29")) %>%
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
  scale_y_log10(breaks=c(1,10,100,1000), limits=c(.11,1000),
                expand = c(0, 0), labels = scales::comma_format(big.mark = ""))+
  annotation_logticks(sides="l", size = .25, outside = TRUE, long = unit(0.15, "cm"),
                      mid = unit(0, "cm"), short = unit(0, "cm"))+
  coord_cartesian(clip = "off")
K_12_29_spider_minmax

pdf(here("analysis","supplementary-materials","FigS13","FigS13-d(bis).pdf"), width=6, height=2)
K_12_29_spider_minmax|K_12_29
dev.off()






