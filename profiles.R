violin_chart <- function(d) {
  groups <- split(d$average_price_eur, d$continent)
  violins <- Map(function(prices, region) {
    color <- unname(palette[region])
    list(type = "violin", name = region, x = rep(region, length(prices)), y = prices,
         width = .56, line = list(color = color, width = 1.8),
         fillcolor = paste0(color, "55"), box = list(visible = TRUE),
         meanline = list(visible = TRUE), points = if (length(prices) > 1L) FALSE else "all",
         spanmode = "hard", hoverinfo = "skip")
  }, groups, names(groups))
  summaries <- Map(function(prices, region) {
    quartiles <- stats::quantile(prices, c(.25, .75), names = FALSE)
    euros <- function(value) paste(format_number(value), "€")
    paste0(
      "<b>", htmltools::htmlEscape(region), "</b><br>",
      "Médiane : ", euros(stats::median(prices)), "<br>",
      "Moyenne : ", euros(mean(prices)), "<br>",
      "50 % des prix : ", euros(quartiles[1L]), " à ", euros(quartiles[2L]), "<br>",
      "Effectif : ", format_number(length(prices))
    )
  }, groups, names(groups))
  # Cible transparente sur la hauteur de chaque violon pour n’afficher qu’une infobulle.
  lows <- vapply(groups, min, numeric(1))
  widths <- vapply(groups, function(prices) diff(range(prices)), numeric(1))
  padding <- max(diff(range(d$average_price_eur)) * .025, 100)
  hover_zones <- list(
    type = "bar", x = names(groups), y = unname(widths + 2 * padding),
    base = I(unname(lows - padding)), width = .65,
    marker = list(color = "rgba(0,0,0,0)", line = list(width = 0)),
    text = unname(unlist(summaries)), textposition = "none", hovertemplate = "%{text}<extra></extra>", showlegend = FALSE
  )
  list(data = c(unname(violins), list(hover_zones)), layout = list(
    showlegend = FALSE, hovermode = "closest", barmode = "overlay", separators = ", ",
    xaxis = list(type = "category", categoryorder = "array", categoryarray = I(names(groups))),
    yaxis = list(title = "Prix moyen par marque", tickformat = ",.0f", ticksuffix = " €",
                 gridcolor = "#d4dfea", automargin = TRUE),
    margin = list(l = 100, r = 35, t = 24, b = 65)
  ))
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
