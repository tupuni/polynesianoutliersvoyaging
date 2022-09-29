library(here)
library(tidyverse)
library(patchwork)

#### Fig 2 A ####
shapes <- c("E-11-03"=0,"E-11-06"=1,"E-11-07"=8,"E-11-08"=5,"E-11-08dup"=9,
            "E-11-10"=0,"E-11-11"=1,"E-11-13"=2,"E-11-16"=5,"E-11-18"=6,
            "E-11-19"=8,"T-12-06"=2,"T-12-06dup"=14,"T-12-07"=7,
            "T-12-08"=6,"T-12-09"=10,"T-12-10"=11,"K-12-24"=12,"K-12-25"=13,
            "K-12-26"=14,"K-12-28"=3,"K-12-29"=4)
cols <- c("E-11-03"="red","E-11-06"="red","E-11-07"="red","E-11-08"="red",
          "E-11-08dup"="red","E-11-10"="#7AD04F","E-11-11"="#7AD04F",
          "E-11-13"="#7AD04F","E-11-16"="#7AD04F","E-11-18"="#7AD04F",
          "E-11-19"="#7AD04F","T-12-06"="red","T-12-06dup"="red",
          "T-12-07"="red","T-12-08"="red","T-12-09"="red",
          "T-12-10"="red","K-12-24"="red","K-12-25"="red","K-12-26"="red",
          "K-12-28"="red","K-12-29"="red")

A <- traces %>% dplyr::filter(Type %in% c("Artefact", "Source")) %>%
  ggplot() + geom_point(
    aes(x=Nb/La, y=Sample, shape=factor(Sample), color=factor(Sample)), size=4) +
  scale_shape_manual(values=shapes) +
  scale_color_manual(values=cols) +
  theme_bw(base_size=12, base_rect_size=1) +
  scale_x_continuous(limits=c(0,2), breaks = c(0,.86,1,2)) +
  theme(line=element_blank(), axis.ticks=element_line(),
        legend.title = element_blank(),legend.text = element_text(size=9),
        axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),
        legend.direction="horizontal",legend.position="none") + #aspect.ratio=1
  annotate("segment", x=.86, xend=.86, y=0, yend=23, size = 0.5, linetype = "dashed") +
  annotate("text", Inf, Inf, label = "A", size = 7, hjust = 2, vjust = 2)
A


#### Fig 2 B ####
library(RSQLite)
library(stats)
library(gridExtra)

georoc <- dbConnect(RSQLite::SQLite(), path_to_georoc)
pofatu <- dbConnect(RSQLite::SQLite(), path_to_pofatu)

IAB <- dbGetQuery(georoc,
"SELECT TECTONIC_SETTING, id, file_id, LOCATION, LAND_OR_SEA,
ROCK_TYPE, `NB(PPM)`, `LA(PPM)`, LATITUDE_MAX, LONGITUDE_MAX
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING = 'CONVERGENT MARGIN'") %>%
  rename(Nb="NB(PPM)",La="LA(PPM)",lat="LATITUDE_MAX",long="LONGITUDE_MAX") %>%
  mutate(Nb_La = Nb/La) %>%
  dplyr::select(file_id, id, LOCATION, Nb, La, Nb_La, lat, long)
is.na(IAB) <- sapply(IAB, is.infinite) # replaces Inf or -Inf by NA
IAB[IAB==0] <- NA # replaces 0 by NA
IAB <- IAB[complete.cases(IAB),] # removes rows with NA
IAB <- IAB %>% dplyr::filter(Nb_La < 3)

summary(IAB$Nb_La)
sd <- (sqrt(sum((IAB$Nb_La - mean(IAB$Nb_La)) ^ 2 / (length(IAB$Nb_La) - 1))))
stat_IAB <- data.frame(
  name = c("mean", "sd min", "sd max", "median"),
  value = c(mean(IAB$Nb_La),
            mean(IAB$Nb_La)-sd,
            mean(IAB$Nb_La)+sd(IAB$Nb_La),
            median(IAB$Nb_La))) %>% mutate(across(where(is.numeric), round, 3))


OIB <- dbGetQuery(georoc,
"SELECT TECTONIC_SETTING, id, file_id, LOCATION, LAND_OR_SEA,
ROCK_TYPE, `NB(PPM)`, `LA(PPM)`, LATITUDE_MAX, LONGITUDE_MAX
FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING = 'OCEAN ISLAND'") %>%
  rename(Nb="NB(PPM)",La="LA(PPM)",lat="LATITUDE_MAX",long="LONGITUDE_MAX") %>%
  mutate(Nb_La = Nb/La) %>%
  dplyr::select(file_id, id, LOCATION, Nb, La, Nb_La, lat, long)

is.na(OIB) <- sapply(OIB, is.infinite) # replaces Inf or -Inf by NA
OIB[OIB==0] <- NA # replaces 0 by NA
OIB <- OIB[complete.cases(OIB),] # removes rows with NA
OIB <- OIB %>% filter(Nb_La < 3)

summary(OIB$Nb_La)
sd <- (sqrt(sum((OIB$Nb_La - mean(OIB$Nb_La)) ^ 2 / (length(OIB$Nb_La) - 1))))
stat_OIB <- data.frame(
  name = c("mean", "sd min", "sd max", "median"),
  value = c(mean(OIB$Nb_La),
            mean(OIB$Nb_La)-sd,
            mean(OIB$Nb_La)+sd,
            median(OIB$Nb_La))) %>% mutate(across(where(is.numeric), round, 3))

B <- ggplot(OIB, aes(Nb_La)) +
  geom_histogram(binwidth=.02, col="black", fill="red", size=.2, alpha=.4) + #OIB
  annotate("rect",xmin=stat_OIB[2,2],xmax=stat_OIB[3,2],
           ymin=-Inf,ymax=Inf,fill="red",alpha=.1) + #OIB
  geom_histogram(data=IAB, binwidth=.02, col="black", fill="blue", size=.2, alpha=.4) + #IAB
  annotate("rect",xmin=stat_IAB[2,2],xmax=stat_IAB[3,2],
           ymin=-Inf,ymax=Inf,fill="blue",alpha=.1) + #IAB
  annotate("point", x=stat_OIB[1,2], y=0, shape=21, col="red", fill="red", size=3) + #mean OIB
  annotate("text",x=stat_OIB[1,2], y=-50, label = "bar(x)", col="red", parse=T) + #mean OIB
  annotate("point", x=stat_OIB[4,2], y=0, shape=21, col="red", fill="white", stroke=1, size=3) + #median OIB
  annotate("text",x=stat_OIB[4,2], y=-50, label = "tilde(x)", col="red", parse=T) + #median OIB
  annotate("point", x=stat_IAB[1,2], y=0, shape=21, col="blue", fill="blue", size=3) + #mean IAB
  annotate("text",x=stat_IAB[1,2], y=-50, label = "bar(x)", col="blue", parse=T) + #mean IAB
  annotate("point", x=stat_IAB[4,2], y=0, shape=21, col="blue", fill="white", stroke=1, size=3) + #median IAB
  annotate("text",x=stat_IAB[4,2], y=-50, label = "tilde(x)", col="blue", parse=T) + #median IAB
  annotate("text", x=2.5, y = 2000, label = "IAB", hjust = 0, size = 4) +
  annotate("text", x=2.5, y = 1800, label = "mean = 0.457", hjust = 0, size = 3.5) +
  annotate("text", x=2.5, y = 1600, label = "median = 0.364", hjust = 0, size = 3.5) +
  annotate("text", x=2.5, y = 1400, label = "SD = 0.133 - 0.781", hjust = 0, size = 3.5) +
  annotate("text", x=2.5, y = 1000, label = "OIB", hjust = 0, size = 4) +
  annotate("text", x=2.5, y = 800, label = "mean = 1.338", hjust = 0, size = 3.5) +
  annotate("text", x=2.5, y = 600, label = "median = 1.266", hjust = 0, size = 3.5) +
  annotate("text", x=2.5, y = 400, label = "SD = 0.933 - 1.744", hjust = 0, size = 3.5) +
  scale_x_continuous(limits=c(0,2), breaks = c(0,.86,1,2)) +
  labs(x='Nb / La', y='Count') + theme_bw(base_size=12) +
  coord_cartesian(ylim = c(-20, 2400), clip = "off") +
  annotate("text", -Inf, Inf, label = "B", size = 7, hjust = -1, vjust = 2) +
  theme(line=element_blank(), axis.ticks=element_line(),
        legend.title=element_blank())
B

#save
pdf(here("analysis","figures","Figure_2.pdf"), width=10, height=10)
A / B
dev.off()

