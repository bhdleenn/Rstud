violin_chart <- function(d) {
  traces <- lapply(split(d, d$continent), function(group) {
    region <- group$continent[1L]
    list(type = "violin", name = region, y = group$average_price_eur,
         line = list(color = unname(palette[region])), opacity = .85,
         box = list(visible = TRUE), meanline = list(visible = TRUE),
         points = if (nrow(group) > 1L) "outliers" else "all", spanmode = "hard",
         hovertemplate = "%{y:,.0f} €<extra>%{fullData.name}</extra>")
  })
  list(data = unname(traces), layout = list(showlegend = FALSE,
       yaxis = list(title = "Prix moyen par marque (€)", gridcolor = "#d4dfea")))
}

radar_chart <- function(d) {
  fields <- c("safety_rating_euroncap", "sustainability_index", "ev_model_count",
              "r_and_d_budget_million_eur", "dealer_network_size")
  traces <- lapply(split(d, d$continent), function(group) {
    region <- group$continent[1L]
    scores <- vapply(fields, function(field) {
      mean(scale01(d[[field]])[d$continent == region]) * 100
    }, numeric(1))
    axes <- unname(labels[fields])
    list(type = "scatterpolar", mode = "lines+markers", name = region,
         r = c(unname(scores), scores[[1L]]), theta = c(axes, axes[[1L]]),
         fill = "toself", opacity = .68, line = list(color = unname(palette[region])),
         hovertemplate = "%{theta} · %{r:.1f}/100<extra>%{fullData.name}</extra>")
  })
  list(data = unname(traces), layout = list(polar = list(
    bgcolor = "#ffffff", radialaxis = list(range = c(0, 100), gridcolor = "#d4dfea"),
    angularaxis = list(gridcolor = "#d4dfea")
  )))
}
