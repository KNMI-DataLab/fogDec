
library(rgdal)

shp = "inst/extdata/Snelwegen/NDWApril2016WGS84/NDW_April_2016_WGS84.shp"
tmp <- readOGR(shp, layer = "NDW_April_2016_WGS84")

wegnummer <- c("001", "004", "006", "007", "009",
               "012", "015",
               "027", "028", "027",
               "032",
               "050", "059",
               "067",
               "073")

wegnummer <- c("002",
               "012")

plot(tmp[tmp$WEGNUMMER %in% wegnummer, ])

plot(tmp[tmp$DISTRNAAM == "MN District Zuid", ])
plot(tmp[tmp$WPSNAAMNEN == "GRONINGEN" & tmp$WEGNUMMER %in% c("007", "028"), ])

plot(tmp[tmp$WPSNAAMNEN == "GRONINGEN" & tmp$WEGNUMMER %in% c("007", "028") & tmp$HECTO_LTTR == "#", ])

tmp2 <- tmp[tmp$HECTO_LTTR == "#", ]

districts <- c("MN District Zuid",
               "WNN District Zuid", "WNN District Noord",
               "WNZ District Noord",
               "ON District Zuid")

rows <- (tmp2$DISTRNAAM %in% districts) | 
  (tmp2$WPSNAAMNEN == "GRONINGEN" & tmp2$WEGNUMMER %in% c("007", "028")) |
  (tmp2$WPSNAAMNEN == "ZWOLLE" & tmp2$WEGNUMMER == "028") |
  (tmp2$DISTRNAAM == "ZN District Midden" & tmp2$WEGNUMMER %in% c("058")) |
  (tmp2$WPSNAAMNEN == "EINDHOVEN" & tmp2$WEGNUMMER == "002") |
  (tmp2$WPSNAAMNEN == "'S-HERTOGENBOSCH"  & tmp2$WEGNUMMER == "002")

tmp3 <- tmp2[rows, ]
tmp3$foggy <- sample(c("red", "green"), nrow(tmp3), replace = TRUE, prob= c(0.1, 0.9))

plot(tmp3, col = tmp3$foggy)

library(ggplot2)
library(data.table)
library(ggmap)
tmp4 <- data.table(fortify(tmp3))

ggplot(tmp4, aes(x = long, y = lat, group = group)) + geom_path()

myMap <- get_map("Netherlands", zoom = 7)
ggmap(myMap) + 
  geom_path(aes(x = long, y = lat, group = group, col = foggy), data = tmp4, lwd = 2) +
  scale_color_manual(values = c("green", "red"))

ggmap(myMap) + 
  geom_path(aes(x = long, y = lat, group = group, col = foggy), data = tmp4, lwd = 2) +
  scale_color_manual(values = c("green", "red")) + xlim(5.5, 6.5) + ylim(51.75, 52.25)
