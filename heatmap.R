heatmap_chart <- function(d) {
  counts <- table(d$continent, d$primary_motorization)
  list(data = list(list(type = "heatmap", x = colnames(counts), y = rownames(counts),
    z = lapply(seq_len(nrow(counts)), function(i) unname(counts[i, ])),
    colorscale = list(list(0, "#f5ede3"), list(.55, "#74aaa2"), list(1, "#0c514d")),
    xgap = 3, ygap = 3, colorbar = list(title = "Marques"),
    hovertemplate = "%{y}<br>%{x}<br>%{z} marques<extra></extra>")),
    layout = list(margin = list(l = 145, r = 40, t = 25, b = 105),
                  xaxis = list(tickangle = -20, automargin = TRUE)))
}
