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
