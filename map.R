map_chart <- function(d) {
  traces <- lapply(split(d, d$continent), function(group) {
    region <- group$continent[1L]
    list(type = "scattergeo", mode = "markers", name = region,
         lat = group$latitude, lon = group$longitude,
         text = paste(htmltools::htmlEscape(group$brand_name),
                      htmltools::htmlEscape(group$city), sep = "<br>"),
         hovertemplate = "%{text}<extra>%{fullData.name}</extra>",
         marker = list(size = 8, color = unname(palette[region]), opacity = .65))
  })
  list(data = unname(traces), layout = list(geo = list(
    bgcolor = "#fff9f3", projection = list(type = "natural earth"),
    showland = TRUE, landcolor = "#e8d9cd", showocean = TRUE, oceancolor = "#fff9f3",
    showcountries = TRUE, countrycolor = "#a99688", showframe = FALSE
  )))
}
