require(here)
require(RSQLite)
require(tidyverse)
require(patchwork)
require(maps)


#### Fig 1 A ####
georoc <- dbConnect(RSQLite::SQLite(), path_to_georoc)
pofatu <- dbConnect(RSQLite::SQLite(), path_to_pofatu)

shapes <- c(
  "Luzon Arc"=21,"Sulawesi Arc"=22,"Sunda Arc"=23,"Banda Arc"=24,
  "Yap Arc"=25,"Mariana Arc"=21,"Bismarck Arc"=22,"Solomon Arc"=23,
  "Vanuatu Arc"=24,"Tonga-Fiji"=25,"New Zealand"=21,
  "Caroline islands"=21,"Samoan islands"=24,"Austral-Cook chain"=23,
  "Society islands"=22,"Hawai'i islands"=25,"Marquesas islands"=21,
  "Pitcairn-Gambier chain"=21,
  "E-11-03"=0,"E-11-06"=1,"E-11-07"=8,"E-11-08"=5,"E-11-08dup"=9,
  "E-11-10"=0,"E-11-11"=1,"E-11-13"=2,"E-11-16"=5,"E-11-18"=6,
  "E-11-19"=8,"T-12-06"=2,"T-12-06dup"=14,"T-12-07"=7,
  "T-12-08"=6,"T-12-09"=10,"T-12-10"=11,"K-12-24"=12,"K-12-25"=13,
  "K-12-26"=14,"K-12-28"=3,"K-12-29"=4)
cols <- c(
  "Luzon Arc"="#440154","Sulawesi Arc"="#345E8C","Sunda Arc"="#404386",
  "Banda Arc"="#472374","Yap Arc"="#29778E","Mariana Arc"="#218F8B",
  "Bismarck Arc"="#25A782","Solomon Arc"="#44BE6F","Vanuatu Arc"="#7AD04F",
  "Tonga-Fiji"="#BADD26","New Zealand"="#FDE725",
  "Caroline islands"="#320A5A","Samoan islands"="#781B6C",
  "Austral-Cook chain"="#BB3654","Society islands"="#EC6824",
  "Marquesas islands"="#FBB41A","Hawai'i islands"="#F4DD53",
  "Pitcairn-Gambier chain"="#C96FB6",
  "E-11-03"="red","E-11-06"="red","E-11-07"="red","E-11-08"="red",
  "E-11-08dup"="red","E-11-10"="blue","E-11-11"="blue","E-11-13"="blue",
  "E-11-16"="blue","E-11-18"="blue","E-11-19"="blue","T-12-06"="red",
  "T-12-06dup"="red","T-12-07"="red","T-12-08"="red","T-12-09"="red",
  "T-12-10"="red","K-12-24"="red","K-12-25"="red","K-12-26"="red",
  "K-12-28"="red","K-12-29"="red")
contour <- c(
  "Luzon Arc"="black","Sulawesi Arc"="black","Sunda Arc"="black",
  "Banda Arc"="black","Yap Arc"="black","Mariana Arc"="black",
  "Bismarck Arc"="black","Solomon Arc"="black","Vanuatu Arc"="black",
  "Tonga-Fiji"="black","New Zealand"="black",
  "Caroline islands"="black","Samoan islands"="black","Austral-Cook chain"="black",
  "Society islands"="black","Hawai'i islands"="black","Marquesas islands"="black",
  "Pitcairn-Gambier chain"="black",
  "E-11-03"="red","E-11-06"="red","E-11-07"="red","E-11-08"="red",
  "E-11-08dup"="red","E-11-10"="blue","E-11-11"="blue","E-11-13"="blue",
  "E-11-16"="blue","E-11-18"="blue","E-11-19"="blue","T-12-06"="red",
  "T-12-06dup"="red","T-12-07"="red","T-12-08"="red","T-12-09"="red",
  "T-12-10"="red","K-12-24"="red","K-12-25"="red","K-12-26"="red",
  "K-12-28"="red","K-12-29"="red")

d_map <- dbGetQuery(georoc,
                    "SELECT id, file_id, LOCATION, LATITUDE_MIN, LONGITUDE_MIN
                    FROM 'sample'
                    WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
                    PB206_PB204 > 0 AND PB207_PB204 > 0 AND PB208_PB204 > 0 AND
                    SR87_SR86 > 0 AND ND143_ND144 > 0") %>%
  dplyr::filter(!grepl("MURUROA|FANGATAUFA",LOCATION)) %>%
  dplyr::rename(lat=LATITUDE_MIN, long=LONGITUDE_MIN) %>% get_georoc_location()
d_map %>% group_by(file_id) %>% tally() %>% print(n=300)

d_map <- dbGetQuery(georoc,
                    "SELECT id, file_id, LOCATION, LATITUDE_MIN, LONGITUDE_MIN
                    FROM 'sample'
                    WHERE LAND_OR_SEA = 'SAE' AND ROCK_TYPE='VOL' AND
                    PB206_PB204 > 0 AND PB207_PB204 > 0 AND PB208_PB204 > 0") %>%
  dplyr::filter(!grepl("MURUROA|FANGATAUFA",LOCATION)) %>%
  dplyr::rename(lat=LATITUDE_MIN, long=LONGITUDE_MIN) %>% get_georoc_location()
d_map %>% group_by(file_id) %>% tally() %>% print(n=150)

d_map$long <- ifelse(d_map$long < -25, d_map$long + 360, d_map$long)
d_map <- d_map %>% dplyr::filter(long > 95 & long < 255 & lat > -45.5 & lat < 22.5)

Fig1_A <- d_map %>% ggplot() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
            fill = "#EDF5F7") +
  geom_polygon(data = fortify(maps::map("world2", plot=FALSE, fill=TRUE)),
               aes(x=long, y=lat, group=group),
               color="white", fill=NA, size=1) +
  geom_polygon(data = fortify(maps::map("world2", plot=FALSE, fill=TRUE)),
               aes(x=long, y=lat, group=group),
               color="#757575", fill="#DBD8C6", size=.2) +
  coord_equal(xlim = c(95,255), ylim = c(-45.5,22.5)) +
  geom_point(data=d_map, aes(x=long, y=lat, shape=factor(Location), fill=factor(Location),
                               color=factor(Location), group=id), size=2, stroke=.25) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=contour) +
  #scale_x_continuous(breaks=c(100, 130, 160, 190, 220, 250)) +
  theme(panel.border=element_rect(colour="black", fill=NA, size=.5),
        panel.background = element_rect(fill="white"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.title = element_blank(), axis.text = element_blank(),
        #legend.key=element_blank(), legend.background = element_blank(),
        #legend.text=element_text(size=8),
        axis.ticks = element_blank(), legend.position="none") +
  annotate("text", Inf, Inf, label = "A", size = 7,  hjust = 2, vjust = 2)
Fig1_A
#save
pdf(here("analysis","figures","Figure_1.pdf"), width=12, height=5)
Fig1_A
dev.off()


