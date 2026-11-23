# Diagramme en violon et radar régional
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

# Graphique à bulles en 3D
bubble_chart <- function(d) {
  d <- head(d[order(-d$annual_production_units, d$brand_id), ], 50L)
  sizes <- 6 + 15 * sqrt(scale01(d$global_market_share_pct))
  traces <- lapply(split(seq_len(nrow(d)), d$continent), function(i) {
    group <- d[i, ]
    region <- group$continent[1L]
    list(type = "scatter3d", mode = "markers", name = region,
         x = group$r_and_d_budget_million_eur, y = group$ev_model_count,
         z = group$sustainability_index, text = htmltools::htmlEscape(group$brand_name),
         customdata = group$global_market_share_pct,
         marker = list(size = unname(sizes[i]), color = unname(palette[region]), opacity = .75),
         hovertemplate = paste0("%{text}<br>R&D : %{x:,.0f} M€<br>Modèles électriques : %{y}",
                                "<br>Durabilité : %{z:.1f}<br>Part de marché : %{customdata:.2f}%<extra></extra>"))
  })
  axis <- function(title) list(title = title, gridcolor = "#c5d2e1", backgroundcolor = "#eaf0f7",
                              showbackground = TRUE)
  list(data = unname(traces), layout = list(scene = list(
    bgcolor = "#ffffff", aspectmode = "cube",
    camera = list(eye = list(x = 1.55, y = 1.65, z = .95)),
    xaxis = axis("Budget R&D (M€)"),
    yaxis = axis("Modèles électriques"), zaxis = axis("Durabilité"), dragmode = "orbit"
  )))
}

# Densités de prix animées
ridge_chart <- function(d) {
  bands <- intersect(regions, unique(d$continent))
  prices <- d$average_price_eur
  window <- range(prices)
  if (diff(window) == 0) window <- window + c(-1, 1)
  years <- range(d$founding_year)
  cutoffs <- unique(as.integer(round(seq(years[1L], years[2L], length.out = 16L))))
  bandwidth <- max(diff(window) / 13, 1)
  grid <- seq(window[1L] - bandwidth, window[2L] + bandwidth, length.out = 120)
  curves <- lapply(cutoffs, function(year) {
    lapply(bands, function(region) {
      values <- prices[d$continent == region & d$founding_year <= year]
      if (!length(values)) return(rep(0, length(grid)))
      stats::density(values, bw = bandwidth, from = min(grid), to = max(grid), n = length(grid))$y
    })
  })
  peak <- max(unlist(curves, use.names = FALSE))
  trace_pair <- function(i, curve) {
    list(
      list(type = "scatter", mode = "lines", x = grid, y = rep(i, length(grid)),
           line = list(width = 0), showlegend = FALSE, hoverinfo = "skip"),
      list(type = "scatter", mode = "lines", x = grid, y = i + .95 * curve / peak,
           fill = "tonexty", line = list(color = unname(palette[bands[i]])), opacity = .75,
           hovertemplate = "%{x:,.0f} €<extra>%{fullData.name}</extra>", name = bands[i],
           showlegend = FALSE)
    )
  }
  traces_at <- function(curve) do.call(c, Map(trace_pair, seq_along(bands), curve))
  list(data = traces_at(tail(curves, 1L)[[1L]]),
       frames = Map(function(year, curve) list(name = as.character(year), data = traces_at(curve)),
                    cutoffs, curves),
       layout = list(xaxis = list(title = "Prix moyen actuel (€)", gridcolor = "#d4dfea"),
         yaxis = list(tickmode = "array", tickvals = seq_along(bands), ticktext = bands,
                      range = c(.75, length(bands) + 1.05), fixedrange = TRUE),
         margin = list(l = 140, r = 25, t = 25, b = 65)))
}
