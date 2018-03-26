library(leaflet)

leaflet() %>% 
  setView(174.764, -36.877, zoom = 16) %>% 
  addTiles() %>% 
  addMarkers(174.764, -36.87, popup = "Maungawhau")
  