criteria <- c(r_and_d_budget_million_eur = .30, safety_rating_euroncap = .25,
              sustainability_index = .20, ev_model_count = .15, dealer_network_size = .10)

rank_partners <- function(d) {
  eligible <- d[d$has_autonomous_driving & d$continent %in% regions[1:2], ]
  if (!nrow(eligible)) return(transform(eligible, score = numeric(), rank = integer()))
  # La référence commune permet de comparer les scores des deux régions.
  scaled <- vapply(names(criteria), function(field) scale01(eligible[[field]]),
                   numeric(nrow(eligible)))
  scaled <- matrix(scaled, nrow = nrow(eligible), ncol = length(criteria))
  eligible$score <- drop(scaled %*% unname(criteria)) * 100
  eligible <- eligible[order(-eligible$score, eligible$brand_id), ]
  groups <- Map(function(region, limit) {
    group <- head(eligible[eligible$continent == region, ], limit)
    transform(group, rank = seq_len(nrow(group)))
  }, regions[1:2], c(10L, 5L))
  do.call(rbind, groups)
}

partner_table <- function(d) {
  render_row <- function(i) {
    row <- d[i, ]
    primary <- row$continent == "Asie"
    tags$tr(class = if (primary) "priority" else NULL, `data-brand` = row$brand_id,
      tags$td(row$rank),
      tags$td(tags$button(type = "button", class = "brand-link", row$brand_name)),
      tags$td(row$continent), tags$td(row$country),
      tags$td(format_number(row$score, 2)),
      lapply(names(criteria), function(field) tags$td(format_number(row[[field]], 1)))
    )
  }
  headings <- c("Rang", "Entreprise", "Région", "Pays", "Score /100",
                "R&D (M€)", "Sécurité /5", "Durabilité", "Modèles électriques", "Concessionnaires")
  tags$div(class = "table-wrap", tags$table(class = "leaderboard",
    tags$thead(tags$tr(lapply(headings, tags$th, scope = "col"))),
    tags$tbody(lapply(seq_len(nrow(d)), render_row))))
}

partner_contact <- function(row) {
  link <- function(value, pattern, prefix = "") {
    if (is.na(value) || !grepl(pattern, value, perl = TRUE)) return("Non disponible")
    tags$a(href = paste0(prefix, value), target = "_blank", rel = "noopener noreferrer", value)
  }
  tags$div(class = "contact",
    tags$strong(row$brand_name),
    tags$div("Contact corporate", link(row$corporate_email, "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", "mailto:")),
    tags$div("Site web", link(row$website_url, "^https?://[^[:space:]]+$"))
  )
}
