library(raster)
library(sf)

clim <- getData('worldclim', var='bio', res=10)

clim <- clim[[c(1,12)]]
names(clim) <- c("Temp","Prec")

big_coords <- dplyr::select(big, latitude, longitude)
coordinates(big_coords) = ~longitude+latitude

values <- extract(clim, big_coords)

big$temp <- values[,1]
big$prec <- values[,2]

ggplot(filter(big[big_with, ], search_str %in% top_5_spp$search_str[1:10]), aes(x = temp, y = doy)) +
  geom_point() + geom_smooth(method = "lm") +
  facet_wrap(~search_str)

big$doy_circ <- circular(big$doy, type = "angles", units = "degrees")

test <- lm.circular(y = big$doy_circ, x = big$temp, type = "c-l")
