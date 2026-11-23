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
  axis <- function(title) list(title = title, gridcolor = "#344660", backgroundcolor = "#172335",
                              showbackground = TRUE)
  list(data = unname(traces), layout = list(scene = list(
    bgcolor = "#101925", xaxis = axis("Budget R&D (M€)"),
    yaxis = axis("Modèles électriques"), zaxis = axis("Durabilité"), dragmode = "orbit"
  )))
}
