require(here)
require(tidyverse)
require(RSQLite)
require(patchwork)

#### Fig S18 ####
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

OIB[order(OIB$Nb, decreasing = TRUE), ] %>% select("Sample","Location","Nb")

s <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-08","T-12-06","T-12-07","T-12-08","T-12-09","T-12-10")) %>%
  mutate(Location = case_when(
    grepl("E-11-08", Sample) ~ "E-11-08",
    grepl("T-12-06", Sample) ~ "T-12-06",
    grepl("T-12-07", Sample) ~ "T-12-07",
    grepl("T-12-08", Sample) ~ "T-12-08",
    grepl("T-12-09", Sample) ~ "T-12-09",
    grepl("T-12-10", Sample) ~ "T-12-10"))

cols <- c("Tatagamatau-source"="#77246C","Tatagamatau-artefact"="#77246C",
          "Malaeloa-source"="#D694C9","Malaeloa-artefact"="#D694C9",
          "Maloata-source"="#B262A7","Maloata-artefact"="#B262A7",
          "E-11-08"="red","T-12-06"="red","T-12-07"="red",
          "T-12-08"="red","T-12-09"="red","T-12-10"="red")
shapes <- c("Tatagamatau-source"=21,"Tatagamatau-artefact"=1,
            "Malaeloa-source"=25,"Malaeloa-artefact"=6,
            "Maloata-source"=23,"Maloata-artefact"=5,"E-11-08"=21,"T-12-06"=24,
            "T-12-07"=14,"T-12-08"=25,"T-12-09"=9,"T-12-10"=23)
contour <- c("Tatagamatau-source"="black","Tatagamatau-artefact"="#77246C",
             "Malaeloa-source"="black","Malaeloa-artefact"="#D694C9",
             "Maloata-source"="black","Maloata-artefact"="#B262A7",
             "E-11-08"="black","T-12-06"="black","T-12-07"="red",
             "T-12-08"="black","T-12-09"="red","T-12-10"="black")

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

pdf(here("analysis","supplementary-materials","FigS18.pdf"), width=8, height=10)
(p1/p2/p3) | (p4/p5/p6)
dev.off()
