```{r}

library(dplyr)
library(gstat)
library(sp)
library(maptools)
library(ggplot2)
```
pacman::p_load(sdalr, DBI, dplyr, data.table, dtplyr)


#vabc_arlington_restaurants on acpd db

read_vabc = function() {
  conn = con_db(dbname = 'acpd',
                pass = get_my_password())
  output = dbReadTable(conn = conn,
                       name = 'vabc_arlington_restaurants') %>%
    data.table()
  on.exit(expr = dbDisconnect(conn = conn))
  return(value = output)
}
vabc = read_vabc()


#abc_cleaned on acpd db

read_abc = function() {
  conn = con_db(dbname = 'acpd',
                pass = get_my_password())
  output = dbReadTable(conn = conn,
                       name = 'abc_cleaned') %>%
    data.table()
  on.exit(expr = dbDisconnect(conn = conn))
  return(value = output)
}
abc = read_abc()
```{r} 

tmp = vabc %>%
  select(license, longitude, latitude) 
tmp2 = abc %>%
  select(license, longitude, latitude, number_of_charges, date_violation)
tmp3 = merge(x = tmp, y = tmp2, all.x = TRUE) %>%
  mutate(number_of_charges = ifelse(test = is.na(x = number_of_charges),
                                                 yes = 0,
                                                 no = number_of_charges))


select(vabc, longitude, latitude) 
longitude = c(-77.10981, -77.05820),
latitude = c(38.88279, 38.86156))
abc = data.table(longitude = c(-77.08588, -77.08986, -77.07071),
                 latitude = c(38.86184, 38.86218, 38.89691),
                 setDT(abc)[, Month_Yr := format(as.Date(date_violation), "%Y-%m")]

chk = merge(vabc, abc, all.x = TRUE)

```{r}
aqi_monitors <- aqi_monitors[aqi_monitors$Year == 2017,]

aqi_monitors$x <- aqi_monitors$Longitude
aqi_monitors$y <- aqi_monitors$Latitude

coordinates(tmp3) = ~ longitude + latitude

x.range = as.numeric(c(min(tmp3$longitude), max(tmp3$longitude)))  # min/max longitude of the interpolation area
y.range = as.numeric(c(min(tmp3$latitude), max(tmp3$latitude)))  # min/max latitude of the interpolation area

grd = expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.05), y = seq(from = y.range[1], to = y.range[2], by = 0.05))  # expand points to grid
coordinates(grd) = ~x + y
gridded(grd) = TRUE

idw = idw(formula = number_of_charges ~ 1, locations = tmp3, newdata = grd)
```
idw.output = as.data.frame(idw)  # output is defined as a data table
names(idw.output)[1:3] <- c("long", "lat", "idw_aqi")  # give names to the modelled variables

class(idw)

gis = idw %>%
  as.data.table() %>%
  select(x, y, var1.pred) %>%
  setnames(old = 'var1.pred', new = 'value')

arlington_county = sf::st_read('https://opendata.arcgis.com/datasets/1ec04543da0546d38b63d8fd8e1019d5_23.geojson')

arlington = map_data('county', 'virginia') %>%
  filter(subregion %in% 'arlington')

readr::write_csv(gis, "~/git/acpd.git/data/values.csv")
#a = map_data('county', 'virginia') %>%
  #filter(subregion %in% 'arlington')

#ggplot(data = a, mapping = aes(map = fortify(a)))

#%>%
  #filter(subregion %in% 'arlington'))

ggplot() +
  geom_raster(data = gis,
              mapping = aes(x = x,
                            y = y,
                            alpha = value)) +
  geom_sf(data = arlington_county, color = "blue")

ggplot(data = arlington) +
  geom_sf(data = arlington_county, color = "blue") +
  geom_tile(data = gis,
            mapping = aes(x = x, y = y, fill = id))
class(gis)

  geom_map(map = gis,
           mapping = aes(x = x, y = y, id = id))
class(gis)
names(gis)


geom_tile(data = gis, alpha = 0.8, aes(x = x, y = y)) +
  scale_fill_gradient(low = "lightblue", high = "black") +
  # geom_path(data = bg) +
  geom_path(aes(x = x, y = y), color = "purple") +
 head(arlington)
ggplot(mapping = aes(map = arlington$group))
+
  geom_map(aes(data = arlington))

?geom_map
#+
  labs(fill = "AQI", title = "Air Quality Index for Virginia and its bordering states")
