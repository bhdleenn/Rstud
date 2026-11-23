# Import et validation du schéma commun aux quatre versions de la base.
metrics <- c("annual_production_units", "average_price_eur", "ev_model_count",
             "global_market_share_pct", "safety_rating_euroncap", "sustainability_index",
             "dealer_network_size", "r_and_d_budget_million_eur")
labels <- setNames(c("Production annuelle", "Prix moyen", "Modèles électriques",
                     "Part de marché", "Sécurité", "Durabilité", "Concessionnaires",
                     "Budget R&D"), metrics)
regions <- c("Asie", "Amérique du Nord", "Europe", "Amérique du Sud", "Afrique", "Océanie")
palette <- setNames(c("#126762", "#a84730", "#365585", "#87354f", "#7a5b24", "#625184"), regions)

load_brands <- function(path, filename = path) {
  extension <- tolower(tools::file_ext(filename))
  brands <- switch(extension,
    rds = readRDS(path),
    xlsx = {
      if (!requireNamespace("readxl", quietly = TRUE)) stop("Installez readxl pour importer un fichier Excel.")
      readxl::read_xlsx(path)
    },
    csv = utils::read.csv(path, check.names = FALSE, fileEncoding = "UTF-8-BOM"),
    stop("Formats acceptés : RDS, XLSX et CSV.")
  )
  if (!is.data.frame(brands) || !nrow(brands)) stop("Le fichier doit contenir un tableau non vide.")
  texts <- c("brand_id", "brand_name", "country", "continent", "city", "primary_motorization",
             "corporate_email", "website_url")
  numbers <- c(metrics, "latitude", "longitude", "founding_year")
  required <- c(texts, numbers, "has_autonomous_driving")
  missing <- setdiff(required, names(brands))
  if (length(missing)) stop("Colonnes absentes : ", paste(missing, collapse = ", "))
  if (anyDuplicated(names(brands))) stop("Les en-têtes doivent être uniques.")
  brands <- as.data.frame(brands[required])
  for (column in texts) {
    brands[[column]] <- trimws(as.character(brands[[column]]))
  }
  for (column in numbers) {
    brands[[column]] <- suppressWarnings(as.numeric(as.character(brands[[column]])))
    if (any(!is.finite(brands[[column]]))) stop("Valeurs numériques manquantes ou invalides dans ", column, ".")
  }
  if (anyNA(brands$brand_id) || any(!nzchar(brands$brand_id)) || anyDuplicated(brands$brand_id)) {
    stop("Les identifiants doivent être renseignés et uniques.")
  }
  for (column in setdiff(texts, c("corporate_email", "website_url"))) {
    if (anyNA(brands[[column]]) || any(!nzchar(brands[[column]]))) {
      stop("Valeurs manquantes dans ", column, ".")
    }
  }
  autonomy <- tolower(trimws(as.character(brands$has_autonomous_driving)))
  yes <- c("true", "1", "oui", "vrai", "yes")
  no <- c("false", "0", "non", "faux", "no")
  if (any(!autonomy %in% c(yes, no))) stop("Valeurs de conduite autonome invalides.")
  brands$has_autonomous_driving <- autonomy %in% yes
  if (any(!brands$continent %in% regions)) stop("Régions inconnues dans continent.")
  if (any(abs(brands$latitude) > 90 | abs(brands$longitude) > 180)) stop("Coordonnées géographiques hors limites.")
  if (any(vapply(brands[metrics], function(x) any(x < 0), logical(1)))) stop("Les indicateurs doivent être positifs ou nuls.")
  brands
}

format_number <- function(x, digits = 0) {
  formatC(x, digits = digits, format = "f", big.mark = " ", decimal.mark = ",")
}

scale01 <- function(x) {
  bounds <- range(x)
  if (diff(bounds) == 0) return(rep(0.5, length(x)))
  (x - bounds[1L]) / diff(bounds)
}
