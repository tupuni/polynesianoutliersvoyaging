require(here)
require(tidyverse)
require(RSQLite)
require(patchwork)

georoc <- dbConnect(RSQLite::SQLite(), path_to_georoc)
pofatu <- dbConnect(RSQLite::SQLite(), path_to_pofatu)

shapes <- c("Caroline islands"=21,"Samoan islands"=24,"Austral-Cook chain"=23,
            "Society islands"=22,"Hawai'i islands"=25,"Marquesas islands"=21,
            "Pitcairn-Gambier chain"=21,"North Fiji Basin"=25,
            "E-11-08"=5,"E-11-08dup"=9,"T-12-06"=2,"T-12-06dup"=14,
            "T-12-07"=7,"T-12-08"=6,"T-12-09"=10,"T-12-10"=11,"K-12-24"=12,
            "K-12-25"=13,"K-12-26"=14)
cols <- c("Caroline islands"="#320A5A","Samoan islands"="#781B6C",
          "Austral-Cook chain"="#BB3654","Society islands"="#EC6824",
          "Marquesas islands"="#FBB41A","Hawai'i islands"="#F4DD53",
          "Pitcairn-Gambier chain"="#C96FB6","North Fiji Basin"="#B4C630",
          "E-11-08"="red","E-11-08dup"="red","T-12-06"="red","T-12-06dup"="red",
          "T-12-07"="red","T-12-08"="red","T-12-09"="red",
          "T-12-10"="red","K-12-24"="red","K-12-25"="red","K-12-26"="red")
contour <- c("Caroline islands"="black","Samoan islands"="black",
             "Austral-Cook chain"="black","Society islands"="black",
             "Hawai'i islands"="black","Marquesas islands"="black",
             "Pitcairn-Gambier chain"="black","North Fiji Basin"="black",
             "E-11-08"="red","E-11-08dup"="red","T-12-06"="red","T-12-06dup"="red",
             "T-12-07"="red","T-12-08"="red","T-12-09"="red",
             "T-12-10"="red","K-12-24"="red","K-12-25"="red","K-12-26"="red")

dir.create(here("analysis","supplementary-materials","FigS15"))

#### Fig S15 A ####
OIB1 <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
TECTONIC_SETTING='OCEAN ISLAND'") %>%
  get_georoc_location() %>% filter(Location != "na") %>% rename_georoc() %>%
  dplyr::select(Sample,Location,lat,long,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

OIB2 <- dbGetQuery(pofatu,
"SELECT s.id AS sample_id, s.sample_category, s.location_region,
s.location_subregion, s.location_latitude, s.location_longitude,
max(CASE WHEN m.parameter='Nd143_Nd144' then m.value END) AS 'Nd143_Nd144',
max(CASE WHEN m.parameter='Sr87_Sr86' then m.value END) AS 'Sr87_Sr86',
max(CASE WHEN m.parameter='Pb206_Pb204' then m.value END) AS 'Pb206_Pb204',
max(CASE WHEN m.parameter='Pb207_Pb204' then m.value END) AS 'Pb207_Pb204',
max(CASE WHEN m.parameter='Pb208_Pb204' then m.value END) AS 'Pb208_Pb204'
FROM 'samples.csv' AS s JOIN 'measurements.csv' AS m ON s.id=m.sample_id
WHERE s.sample_category = 'SOURCE'
AND m.parameter IN ('Nd143_Nd144', 'Sr87_Sr86','Pb206_Pb204', 'Pb207_Pb204',
'Pb208_Pb204') GROUP BY sample_id") %>%
  dplyr::mutate(Location = case_when(
    location_region == "AUSTRAL" ~ "Austral-Cook chain",
    location_region == "CAROLINE" ~ "Caroline islands",
    location_region == "COOK ISLANDS" ~ "Austral-Cook chain",
    location_region == "GAMBIER" ~ "Pitcairn-Gambier chain",
    location_region == "HAWAI'I" ~ "Hawai'i islands",
    location_region == "HENDERSON" ~ "Pitcairn-Gambier chain",
    location_region == "MARQUESAS" ~ "Marquesas islands",
    location_region == "PITCAIRN" ~ "Pitcairn-Gambier chain",
    location_region == "SAMOA" ~ "Samoan islands",
    location_region == "SOCIETY" ~ "Society islands",
    TRUE ~ "na")) %>%
  dplyr::rename(Sample=sample_id,lat=location_latitude,long=location_longitude) %>%
  dplyr::filter(Location %in% c(
    "Caroline islands","Samoan islands","Austral-Cook chain","Society islands",
    "Hawai'i islands","Marquesas islands","Pitcairn-Gambier chain")) %>%
  dplyr::select(Sample,Location,lat,long,Sr87_Sr86,Nd143_Nd144,
                Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

OIB <- full_join(OIB1,OIB2)

Fiji <- dbGetQuery(georoc,
"SELECT * FROM 'sample'
WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
file_id = '2022-06-PVFZCE_TONGA_ARC.csv' AND
LATITUDE_MAX > -16") %>%
  dplyr::mutate(
    Location = case_when(
      file_id == '2022-06-PVFZCE_TONGA_ARC.csv' ~ "Fiji-Tonga")) %>%
  rename_georoc() %>% Ti_from_TiO2() %>% K_from_K2O() %>%
  dplyr::select(Sample,Location,LOCATION,lat,long,Sr87_Sr86,Nd143_Nd144,
                Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

price2014 <- read.csv(here("analysis", "data", "raw_data", "price2014G3.csv"),
                    header=TRUE, sep=",", stringsAsFactors=FALSE) %>%
  dplyr::mutate(Location = "North Fiji Basin")

s <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-08","T-12-06","T-12-07","T-12-08","T-12-09","T-12-10",
  "K-12-24","K-12-25","K-12-26")) %>%
  mutate(Location = case_when(
    grepl("E-11-08", Sample) ~ "E-11-08",
    grepl("T-12-06", Sample) ~ "T-12-06",
    grepl("T-12-07", Sample) ~ "T-12-07",
    grepl("T-12-08", Sample) ~ "T-12-08",
    grepl("T-12-09", Sample) ~ "T-12-09",
    grepl("T-12-10", Sample) ~ "T-12-10",
    grepl("K-12-24", Sample) ~ "K-12-24",
    grepl("K-12-25", Sample) ~ "K-12-25",
    grepl("K-12-26", Sample) ~ "K-12-26")) %>% dplyr::select(
      Sample,Location,Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

A <- OIB %>%
  ggplot(aes(x=Pb206_Pb204,y=Pb208_Pb204, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Caroline islands")), size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Samoan islands")), size=3, stroke=.25) +
  geom_point(data=price2014, size=3, stroke=.35) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(17,20)) +
  scale_y_continuous(position = "right", limits=c(37.7, 39.8)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 18), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^206*"Pb / "*{}^204*"Pb"),
       y=expression({}^208*"Pb / "*{}^204*"Pb"))
A
pdf(here("analysis","supplementary-materials","FigS15","FigS15-A.pdf"), width=8, height=8)
A
dev.off()

A_detail <- OIB %>%
  ggplot(aes(x=Pb206_Pb204,y=Pb208_Pb204, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Caroline islands")), size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Samoan islands")), size=3, stroke=.25) +
  geom_point(data=price2014, size=3, stroke=.35) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(position = "top", limits=c(18.1,19.25), expand=c(0,0)) +
  scale_y_continuous(limits=c(38, 39.25), expand=c(0,0)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_blank(), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1)
A_detail
pdf(here("analysis","supplementary-materials","FigS15","FigS15-A_detail.pdf"), width=6, height=6)
A_detail
dev.off()


B <- OIB %>%
  ggplot(aes(x=Sr87_Sr86,y=Nd143_Nd144, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Caroline islands")), size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Samoan islands")), size=3, stroke=.25) +
  geom_point(data=price2014, size=3, stroke=.35) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(.7027,.7065)) +
  scale_y_continuous(limits=c(.5126, .5131)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 18), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"),
       y=expression({}^143*"Nd / "*{}^144*"Nd"))
B
pdf(here("analysis","supplementary-materials","FigS15","FigS15-B.pdf"), width=8, height=8)
B
dev.off()

B_detail <- OIB %>%
  ggplot(aes(x=Sr87_Sr86,y=Nd143_Nd144, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Caroline islands")), size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Samoan islands")), size=3, stroke=.25) +
  geom_point(data=price2014, size=3, stroke=.35) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(position="top", limits=c(.703,.7056)) +
  scale_y_continuous(position="right", limits=c(.5128, .51305)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_blank(), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1)
B_detail
pdf(here("analysis","supplementary-materials","FigS15","FigS15-B_detail.pdf"), width=6, height=6)
B_detail
dev.off()


