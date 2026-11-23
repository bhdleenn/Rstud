heatmap_chart <- function(d) {
  counts <- table(d$continent, d$primary_motorization)
  list(data = list(list(type = "heatmap", x = colnames(counts), y = rownames(counts),
    z = lapply(seq_len(nrow(counts)), function(i) unname(counts[i, ])),
    colorscale = list(list(0, "#17253a"), list(.55, "#167e84"), list(1, "#67f4d0")),
    xgap = 3, ygap = 3, colorbar = list(title = "Marques"),
    hovertemplate = "%{y}<br>%{x}<br>%{z} marques<extra></extra>")),
    layout = list(margin = list(l = 145, r = 40, t = 25, b = 105),
                  xaxis = list(tickangle = -20, automargin = TRUE)))
}
