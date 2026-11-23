road_view <- function(partners) {
  d <- partners[partners$continent == "Asie", ]
  if (!nrow(d)) return(tags$p("Aucun partenaire asiatique admissible."))
  colors <- c("#54ebd0", "#fbad55", "#a9b8ff", "#ff84ae", "#e1f284",
              "#a2e4ff", "#bd8cff", "#ff8486", "#9aec9d", "#efe9de")
  cars <- lapply(seq_len(nrow(d)), function(i) {
    row <- d[i, ]
    # Le rang fixe la position horizontale. Chaque véhicule dispose de sa voie.
    position <- 79 - (i - 1L) * 5.6
    lane <- 6 + (i - 1L) * 9.65
    style <- sprintf("--lane: %.2f%%; --finish: %.2f%%; --paint: %s; --length: %dpx; --roof: %d%%; --radius: %dpx;",
                     lane, position, colors[i], 46 + (i %% 5L) * 5L,
                     31 + (i %% 4L) * 10L, 4 + (i %% 3L) * 4L)
    tags$div(class = "racer", style = style,
      title = paste(row$brand_name, "—", format_number(row$score, 2), "/ 100"),
      tags$div(class = "racer-label", paste0(i, ". ", row$brand_name)),
      tags$div(class = "car", `aria-hidden` = "true", tags$span(class = "car-roof"))
    )
  })
  tags$div(
    tags$p(class = "hint", "Les véhicules les plus à droite occupent les premières places."),
    tags$button(type = "button", id = "replay-road", class = "control", "Rejouer · 5 s"),
    tags$div(class = "track-scroll",
      tags$div(id = "track", class = "track", role = "img",
        `aria-label` = paste("Classement sur la route :", paste(d$brand_name, collapse = ", ")), cars))
  )
}
