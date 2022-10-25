require(here)
require(tidyverse)
require(patchwork)

#### Fig S19 ####
OIB <- q36
OIB %>% group_by(site_name) %>% tally() %>% print(n=30)

OIB <- OIB %>%
  dplyr::mutate(Category = case_when(
    grepl("Artefact", sample_comment) ~ "artefact",
    sample_category == "SOURCE" ~ "source",
    sample_category == "ARTEFACT" ~ "artefact")) %>%
  dplyr::mutate(Location = case_when(
    site_name == "Tatagamatau" & Category == "source" ~ "Tatagamatau-source",
    site_name == "Tatagamatau" & Category == "artefact" ~ "Tatagamatau-artefact",
    grepl("MALAELOA", Sample) & Category == "source" ~ "Malaeloa-source",
    grepl("MALAELOA", Sample) & Category == "artefact" ~ "Malaeloa-artefact",
    grepl("MALOATA", Sample) & Category == "source" ~ "Maloata-source",
    grepl("MALOATA", Sample) & Category == "artefact" ~ "Maloata-artefact",
    TRUE ~ "Tutuila"))
OIB %>% dplyr::select(Sample, Location, Category)
OIB[OIB == 0] <- NA # Replace 0 with NA


#### add samples ####
s <- q37 %>%
  #group_by(Sample) %>% summarise_all(~na.omit(.)[1]) %>%
  #add_column(Location2 = NA) %>%
  dplyr::mutate(Location = case_when(
    grepl("allen1997_5", Sample) ~ "[allen1997_5] Moturakau (Cook Is.)",
    grepl("allen1997_6", Sample) ~ "[allen1997_6] Moturakau (Cook Is.)",
    grepl("best1992_BB-8-4-4(A)", Sample) ~ "[BB-8-4-4(A)] San Cristobal (Solomon Is.)",
    grepl("best1992_COOK-1", Sample) ~ "[COOK-1] Ma'uke (Cook Is.)",
    grepl("best1992_TAU48", Sample) ~ "[TAU48] Taumako (Solomon Is.)",
    grepl("best1992_TAU49", Sample) ~ "[TAU49] Taumako (Solomon Is.)",
    grepl("best1992_TAU52", Sample) ~ "[TAU52] Taumako (Solomon Is.)",
    grepl("best1992_TAU54", Sample) ~ "[TAU54] Taumako (Solomon Is.)",
    grepl("best1992_TOK-A2", Sample) ~ "[TOK-A2] Atafu (Tokelau Is.)",
    grepl("weisler2016a_2009-369", Sample) ~ "[2009-369] Mangaia (Cook Is.)",
    grepl("clark2014_36", Sample) ~ "[clark2014_36] Tongatapu (Tonga Is.)",
    grepl("clark2014_51", Sample) ~ "[clark2014_51] Tongatapu (Tonga Is.)",
    grepl("clark2014_53", Sample) ~ "[clark2014_53] Tongatapu (Tonga Is.)",
    grepl("clark2014_54", Sample) ~ "[clark2014_54] Tongatapu (Tonga Is.)",
    grepl("clark2014_241", Sample) ~ "[clark2014_241] Tongatapu (Tonga Is.)",
    grepl("clark2014_678", Sample) ~ "[clark2014_678] Tongatapu (Tonga Is.)",
    grepl("clark2014_683", Sample) ~ "[clark2014_683] Tongatapu (Tonga Is.)",
    grepl("clark2014_707", Sample) ~ "[clark2014_707] Tongatapu (Tonga Is.)"))

#biplots
shapes <- c(
  "Tatagamatau-source"=21,"Tatagamatau-artefact"=1,
  "Malaeloa-source"=25,"Malaeloa-artefact"=6,
  "Maloata-source"=23,"Maloata-artefact"=5,
  "[allen1997_5] Moturakau (Cook Is.)"=2,
  "[allen1997_6] Moturakau (Cook Is.)"=6,
  "[COOK-1] Ma'uke (Cook Is.)"=11,
  "[2009-369] Mangaia (Cook Is.)"=8,
  "[BB-8-4-4(A)] San Cristobal (Solomon Is.)"=0,
  "[TOK-A2] Atafu (Tokelau Is.)"=5,
  "[clark2014_51] Tongatapu (Tonga Is.)"=7,
  "[clark2014_53] Tongatapu (Tonga Is.)"=9,
  "[clark2014_54] Tongatapu (Tonga Is.)"=10,
  "[clark2014_241] Tongatapu (Tonga Is.)"=12,
  "[clark2014_678] Tongatapu (Tonga Is.)"=13,
  "[clark2014_683] Tongatapu (Tonga Is.)"=14,
  "[clark2014_707] Tongatapu (Tonga Is.)"=3)
cols <- c(
  "Tatagamatau-source"="#77246C","Tatagamatau-artefact"="#77246C",
  "Malaeloa-source"="#D694C9","Malaeloa-artefact"="#D694C9",
  "Maloata-source"="#B262A7","Maloata-artefact"="#B262A7",
  "[allen1997_5] Moturakau (Cook Is.)"="red",
  "[allen1997_6] Moturakau (Cook Is.)"="red",
  "[COOK-1] Ma'uke (Cook Is.)"="red",
  "[2009-369] Mangaia (Cook Is.)"="red",
  "[BB-8-4-4(A)] San Cristobal (Solomon Is.)"="red",
  "[TOK-A2] Atafu (Tokelau Is.)"="red",
  "[clark2014_51] Tongatapu (Tonga Is.)"="red",
  "[clark2014_53] Tongatapu (Tonga Is.)"="red",
  "[clark2014_54] Tongatapu (Tonga Is.)"="red",
  "[clark2014_241] Tongatapu (Tonga Is.)"="red",
  "[clark2014_678] Tongatapu (Tonga Is.)"="red",
  "[clark2014_683] Tongatapu (Tonga Is.)"="red",
  "[clark2014_707] Tongatapu (Tonga Is.)"="red")
contour <- c(
  "Tatagamatau-source"="black","Tatagamatau-artefact"="#77246C",
  "Malaeloa-source"="black","Malaeloa-artefact"="#D694C9",
  "Maloata-source"="black","Maloata-artefact"="#B262A7",
  "[allen1997_5] Moturakau (Cook Is.)"="red",
  "[allen1997_6] Moturakau (Cook Is.)"="red",
  "[COOK-1] Ma'uke (Cook Is.)"="red",
  "[2009-369] Mangaia (Cook Is.)"="red",
  "[BB-8-4-4(A)] San Cristobal (Solomon Is.)"="red",
  "[TOK-A2] Atafu (Tokelau Is.)"="red",
  "[clark2014_51] Tongatapu (Tonga Is.)"="red",
  "[clark2014_53] Tongatapu (Tonga Is.)"="red",
  "[clark2014_54] Tongatapu (Tonga Is.)"="red",
  "[clark2014_241] Tongatapu (Tonga Is.)"="red",
  "[clark2014_678] Tongatapu (Tonga Is.)"="red",
  "[clark2014_683] Tongatapu (Tonga Is.)"="red",
  "[clark2014_707] Tongatapu (Tonga Is.)"="red")


p1 <- OIB %>%
  ggplot(aes(x=TiO2/Fe2O3,y=Al2O3, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(.23,.36)) + scale_y_continuous(limits=c(13.6,16.3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.title.x = element_blank(),
        axis.text = element_text(size = 8), legend.position = "none", aspect.ratio=1) +
  labs(x=expression(paste(TiO[2]*" / Fe"[2]*"O"[3]))) +
  labs(y=expression(Al[2]*O[3]*~ "(wt%)"))
p2 <- OIB %>%
  ggplot(aes(x=TiO2/Fe2O3,y=MgO, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(.23,.36)) + scale_y_continuous(limits=c(4,7.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.title.x = element_blank(),
        axis.text = element_text(size = 8), legend.position = "none", aspect.ratio=1) +
  labs(x=expression(paste(TiO[2]*" / Fe"[2]*"O"[3]))) +
  labs(y=expression("MgO (wt%)"))
p3 <- OIB %>%
  ggplot(aes(x=TiO2/Fe2O3,y=Na2O+K2O, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(.23,.36)) + scale_y_continuous(limits=c(3.5,6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression(paste(TiO[2]*" / Fe"[2]*"O"[3]))) +
  labs(y=expression(paste(Na[2]*"O + K"[2]*~"O (wt%)")))
p4 <- OIB %>%
  ggplot(aes(x=Ti/Zr,y=Sr, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(40,110)) + scale_y_continuous(limits=c(450,850)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.title.x = element_blank(),
        axis.text = element_text(size = 8), legend.position = "none", aspect.ratio=1) +
  labs(x="Ti / Zr", y="Sr (ppm)")
p5 <- OIB %>%
  ggplot(aes(x=Ti/Zr,y=Rb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(40,110)) + scale_y_continuous(limits=c(20,60)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.title.x = element_blank(),
        axis.text = element_text(size = 8), legend.position = "none", aspect.ratio=1) +
  labs(x="Ti / Zr", y="Rb (ppm)")
p6 <- OIB %>%
  ggplot(aes(x=Ti/Zr,y=Nb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(40,110)) + scale_y_continuous(limits=c(30,60)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x="Ti / Zr", y="Nb (ppm)")

pdf(here("analysis","supplementary-materials","FigS19.pdf"), width=8, height=10)
(p1/p2/p3) | (p4/p5/p6)
dev.off()

