library('ggmap')
library('ggplot2')
library('dplyr')

register_google(key='google_maps_api_keys')

# 지도형태별 불러오기
ggseoul <- get_googlemap("seoul", maptype = "terrain")
ggmap(ggseoul)

ggseoul <- get_googlemap("seoul", maptype = "roadmap")
ggmap(ggseoul)

# 특정 장소 불러오기
gc <- geocode(enc2utf8("국민대학교"))
cen <- as.numeric(gc)
map <- get_googlemap(center = cen,
                     zoom = 17,
                     size = c(940,640),
                     maptype = "hybrid")
ggmap(map)


# 여러 장소를 핀해서 지도보기
names <- c("용두암","성산일출봉","정방폭포",
           "중문관광단지","한라산1100고지","차귀도")
addr <- c("제주시 용두암길 15",
          "서귀포시 성산읍 성산리",
          "서귀포시 동홍동 299-3",
          "서귀포시 중문동 2624-1",
          "서귀포시 색달동 산1-2",
          "제주시 한경면 고산리 125")
gc <- geocode(enc2utf8(addr))
gc

df <- data.frame(name=names, lon=gc$lon, lat=gc$lat)
cen <- c(mean(df$lon),mean(df$lat))
map <- get_googlemap(center=cen, maptype="roadmap", 
                     zoom=10, size=c(640,640), marker=gc)
ggmap(map)
