road_view <- function(partners) {
  d <- partners[partners$continent == "Asie", ]
  if (!nrow(d)) return(tags$p("Aucun partenaire asiatique admissible."))
  colors <- c("#e7b697", "#dca75e", "#9fbaeb", "#cd8ba0", "#c7ca91",
              "#9acec3", "#b9a4d0", "#df8a76", "#a7c097", "#eee3d8")
  cars <- lapply(seq_len(nrow(d)), function(i) {
    row <- d[i, ]
    # Le rang fixe la position horizontale. Chaque véhicule dispose de sa voie.
    position <- 79 - (i - 1L) * 5.6
    lane <- 14.5 + (i - 1L) * 7.8
    style <- sprintf("--lane: %.2f%%; --finish: %.2f%%; --paint: %s; --length: %dpx; --roof: %d%%; --radius: %dpx;",
                     lane, position, colors[i], 46 + (i %% 5L) * 5L,
                     31 + (i %% 4L) * 10L, 4 + (i %% 3L) * 4L)
    tags$div(class = "racer", style = style,
      title = paste(row$brand_name, "—", format_number(row$score, 2), "/ 100"),
      tags$div(class = "racer-label",
        tags$span(class = "racer-rank", sprintf("%02d", i)),
        tags$span(class = "racer-name", row$brand_name),
        tags$span(class = "racer-score", format_number(row$score, 1))),
      tags$div(class = "car", `aria-hidden` = "true",
        tags$span(class = "car-roof"), tags$span(class = "car-lights"))
    )
  })
  tags$div(
    tags$div(class = "road-toolbar",
      tags$p(class = "hint", "Les véhicules les plus à droite occupent les premières places."),
      tags$button(type = "button", id = "replay-road", class = "control", "Rejouer · 5 s")),
    tags$div(class = "track-scroll",
      tags$div(id = "track", class = "track", role = "img",
        `aria-label` = paste("Classement sur la route :", paste(d$brand_name, collapse = ", ")), cars))
  )
}
