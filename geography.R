map_chart <- function(d, detailed = FALSE) {
  locations <- aggregate(
    list(brands = d$brand_id),
    d[c("city", "country", "continent", "latitude", "longitude")], length
  )
  locations <- locations[order(-locations$brands), ]
  traces <- lapply(split(locations, locations$continent), function(group) {
    region <- group$continent[1L]
    list(type = "scattergeo", mode = "markers", name = region,
         lat = group$latitude, lon = group$longitude,
         text = paste(htmltools::htmlEscape(group$city), htmltools::htmlEscape(group$country), sep = ", "),
         customdata = group$brands,
         hovertemplate = "<b>%{text}</b><br>%{customdata} entreprise(s)<extra>%{fullData.name}</extra>",
         marker = list(size = I(6 + 20 * sqrt(group$brands / max(locations$brands))),
                       color = unname(palette[region]), opacity = .82,
                       line = list(color = "#ffffff", width = 1)))
  })
  if (detailed) {
    traces <- lapply(traces, function(trace) {
      trace$type <- "scattermap"
      trace
    })
    return(list(data = unname(traces), layout = list(
      margin = list(l = 0, r = 0, t = 4, b = 48),
      map = list(style = "https://tiles.openfreemap.org/styles/positron",
                 center = list(lat = 22, lon = 10), zoom = 0.9)
    )))
  }
  list(data = unname(traces), layout = list(
    margin = list(l = 0, r = 0, t = 12, b = 55),
    geo = list(bgcolor = "#ffffff", projection = list(type = "natural earth"),
      showland = TRUE, landcolor = "#e4ebf3", showocean = TRUE, oceancolor = "#ffffff",
      showcountries = TRUE, countrycolor = "#9daec4", countrywidth = .6,
      coastlinecolor = "#a3b3c7", showframe = FALSE, lataxis = list(range = c(-59, 85))
    )
  ))
}

heatmap_chart <- function(d, detailed = FALSE) {
  counts <- table(d$continent, d$primary_motorization)
  annotations <- lapply(seq_along(counts), function(i) {
    cell <- arrayInd(i, dim(counts))
    value <- counts[i]
    list(x = colnames(counts)[cell[2L]], y = rownames(counts)[cell[1L]],
         text = as.character(value), showarrow = FALSE,
         font = list(color = if (value >= max(counts) * .72) "#ffffff" else "#14243b", size = 15))
  })
  list(data = list(list(type = "heatmap", x = colnames(counts), y = rownames(counts),
    z = lapply(seq_len(nrow(counts)), function(i) unname(counts[i, ])), zmin = 0,
    colorscale = list(list(0, "#f0f5fa"), list(.55, "#6cbbc3"), list(1, "#14385f")),
    xgap = 4, ygap = 4, colorbar = list(title = "Marques", thickness = 12),
    hovertemplate = "%{y}<br>%{x}<br><b>%{z} marques</b><extra></extra>")),
    layout = list(annotations = annotations, margin = list(l = 145, r = 40, t = 25, b = 105),
                  xaxis = list(tickangle = -20, automargin = TRUE)))
}
