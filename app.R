needed <- c("shiny", "jsonlite", "readxl")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop('Installez les dépendances dans la console RStudio avec install.packages(c("shiny", "jsonlite", "readxl")).',
       call. = FALSE)
}
library(shiny)

for (file in c("data", "geography", "profiles", "bubble", "ridge", "partners", "road")) {
  source(paste0(file, ".R"), local = TRUE, encoding = "UTF-8")
}

chart_names <- c("Carte mondiale", "Prix par région", "Radar régional", "Bulles 3D",
                 "Motorisations", "Densités animées")
chart_ids <- c("map", "violin", "radar", "bubble", "heatmap", "ridge")
charts <- setNames(list(map_chart, violin_chart, radar_chart, bubble_chart, heatmap_chart, ridge_chart), chart_ids)
descriptions <- setNames(c(
  "Chaque cercle représente une ville. Sa taille indique le nombre de sièges et sa couleur la région.",
  "Distribution des prix moyens par marque, avec boîte centrale et indicateur de moyenne.",
  "Moyennes régionales sur cinq indicateurs, normalisés sur 100 dans la sélection.",
  "Les 50 plus gros producteurs de la sélection. La taille des bulles représente la part de marché.",
  "Nombre de marques par région et motorisation principale.",
  "Prix actuels des marques, ajoutées selon leur année de fondation. Lecture complète en cinq secondes."
), chart_ids)

style <- "
:root{color-scheme:light;--bg:#f7e7da;--panel:#fff9f3;--edge:#d6c4b4;--muted:#5b514b;--text:#252728;--accent:#0b5e59}
body{background:var(--bg);color:var(--text);font-family:system-ui,sans-serif}
.container-fluid{max-width:1440px;padding:24px 34px 55px}

.hero{display:grid;grid-template-columns:minmax(0,2fr) minmax(180px,1fr);min-height:192px;border-top:4px solid var(--accent);border-bottom:1px solid var(--edge);margin-bottom:24px}
.hero-copy{padding:32px 15px 30px 0;align-self:center}
.hero-image{background:linear-gradient(90deg,#f7e7da,transparent 42%),linear-gradient(#64482d33,#64482d33),url(road.jpg) center/cover}
.market-summary{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));border-bottom:1px solid var(--edge);margin-bottom:17px}
.market-metric{padding:4px 12px 13px;border-right:1px solid var(--edge);min-width:0}
.market-metric:first-child{padding-left:0}.market-metric:last-child{border-right:0}
.market-metric strong{display:block;font-family:Georgia,serif;font-size:clamp(20px,2.7vw,36px);font-weight:400;line-height:1.1;color:var(--accent);font-variant-numeric:tabular-nums}
.market-metric span{display:block;color:var(--muted);font-size:12px;margin-top:6px}
.chart-header{display:flex;justify-content:space-between;align-items:flex-start;column-gap:18px}
.chart-header h2{font-size:clamp(24px,3vw,36px)}
.chart-header .form-group{min-width:170px;width:190px;max-width:100%;margin-bottom:6px}
.chart-header .hint{max-width:55ch;margin-bottom:12px}

.hero h1{font-family:Georgia,serif;font-size:clamp(35px,6vw,68px);font-weight:700;letter-spacing:-.05em;margin:0 0 8px}
.hero p{margin:0;color:var(--muted);font-size:16px}
.well,.panel-card{background:var(--panel);border:1px solid var(--edge);border-radius:3px;box-shadow:none}
.panel-card{padding:20px;margin-top:14px}
.form-control,.selectize-input,.selectize-dropdown,.selectize-input.full,.selectize-input.input-active{background:#fffdfa!important;color:var(--text)!important;border-color:#b9a797!important}
.selectize-dropdown .active{background:#e1eee8;color:#133f3c}
.btn,.control{border:1px solid var(--accent);background:var(--accent);color:white;padding:7px 13px;border-radius:3px}
.btn:hover,.control:hover,.btn:focus,.control:focus{background:#074541;color:white}
.nav-tabs{border-color:var(--edge)}
.nav-tabs>li>a{color:var(--text);border-radius:3px 3px 0 0}
.nav-tabs>li.active>a,.nav-tabs>li.active>a:hover,.nav-tabs>li.active>a:focus,.nav-tabs>li>a:hover{color:var(--accent);background:var(--panel);border-color:var(--edge)}
h2{font-family:Georgia,serif;font-size:27px;margin:0 0 10px}
.hint{color:var(--muted);line-height:1.55}.error-note{color:#9a2f20}
#chart{height:510px;width:100%;min-width:0}
#ridge-state{padding-left:10px;color:var(--accent)}
.table-wrap,.track-scroll{overflow-x:auto}.leaderboard{width:100%;border-collapse:collapse;font-size:13px}
.leaderboard th,.leaderboard td{padding:9px;border-bottom:1px solid var(--edge);text-align:left}
.leaderboard th{color:#514439;background:#f2e5d8;white-space:nowrap}.leaderboard tr[data-brand]{cursor:pointer}
.leaderboard .priority{background:#e3eee6}.leaderboard tr[data-brand]:hover{background:#d8e7e2}
.brand-link{background:none;border:0;padding:0;color:var(--accent);text-align:left;text-decoration:underline}
.contact{margin:13px 0;padding:12px;border-left:3px solid var(--accent);background:#eaf2eb}
.score{display:grid;grid-template-columns:minmax(38px,1fr) auto;align-items:center;gap:8px;font-variant-numeric:tabular-nums;white-space:nowrap}
.score-track{display:block;height:5px;background:#c9c3b6;overflow:hidden;border-radius:2px}
.score-fill{display:block;height:100%;background:var(--accent)}
.road-toolbar{display:flex;justify-content:space-between;align-items:center;gap:16px;flex-wrap:wrap}
.road-toolbar .hint{margin:0}
.contact a{color:var(--accent);margin-left:8px;overflow-wrap:anywhere}
.track{position:relative;min-width:850px;height:680px;margin-top:14px;border:1px solid #70685f;border-radius:3px;overflow:hidden;background:linear-gradient(90deg,#3a281dcc,#342d2866),url(road.jpg) center/cover}
.racer{position:absolute;top:var(--lane);left:var(--finish);width:var(--length);transform:translateY(-30%);animation:drive 5s cubic-bezier(.15,.66,.23,1) both}
.racer-label{position:absolute;bottom:calc(100% + 2px);left:0;font-size:11px;font-weight:700;color:white;text-shadow:0 1px 5px #000;background:#06101bd9;padding:2px 5px;border-radius:4px;white-space:nowrap;max-width:230px;overflow:hidden;text-overflow:ellipsis}
.car{position:relative;height:18px;background:var(--paint);border-radius:var(--radius);box-shadow:0 1px 7px #000;border:1px solid #07101a}
.car::before,.car::after{content:'';position:absolute;left:17%;width:65%;height:3px;background:linear-gradient(90deg,#06090e 0 22%,transparent 22% 77%,#06090e 77%);border-radius:3px}
.car::before{top:-3px}.car::after{bottom:-3px}.car-roof{position:absolute;left:24%;top:3px;width:var(--roof);height:10px;background:#152234;border:1px solid #ffffff77;border-radius:3px 6px 6px 3px}
@keyframes drive{from{left:-7%}to{left:var(--finish)}}
@media(prefers-reduced-motion:reduce){.racer{animation:none}}
@media(max-width:700px){.container-fluid{padding:15px}.hero{min-height:0;grid-template-columns:minmax(0,1fr) 24%}.hero-copy{padding:18px 8px 18px 0}.hero p{font-size:14px}.chart-header{flex-wrap:wrap}.market-summary{grid-template-columns:repeat(2,minmax(0,1fr))}.market-metric:nth-child(2){border-right:0}.market-metric:nth-child(3){padding-left:0}.panel-card{padding:12px}#chart{height:450px}}
"

javascript <- "
(function () {
  function setup() {
    if (!window.Shiny) return;
    const target = document.getElementById('chart');
    const replay = document.getElementById('replay-ridge');
    const state = document.getElementById('ridge-state');
    let data = null, generation = 0, timer = null;
    const arrayKeys = ['x','y','z','lat','lon','text','r','theta','customdata'];
    function traces(items) {
      return items.map(item => {
        arrayKeys.forEach(key => {
          if (item[key] !== undefined && !Array.isArray(item[key])) item[key] = [item[key]];
        });
        if (item.type === 'heatmap') item.z = item.z.map(row => Array.isArray(row) ? row : [row]);
        if (item.marker && item.marker.size !== undefined && item.type === 'scatter3d' &&
            !Array.isArray(item.marker.size)) item.marker.size = [item.marker.size];
        return item;
      });
    }
    async function play() {
      if (!data || !data.frames || !window.Plotly) return;
      const version = ++generation;
      cancelAnimationFrame(timer);
      const start = performance.now();
      async function draw(now) {
        if (version !== generation) return;
        const ratio = Math.max(0, Math.min(1, (now - start) / 5000));
        const frame = data.frames[Math.floor(ratio * (data.frames.length - 1))];
        if (state.textContent !== 'Fondation jusqu’en ' + frame.name) {
          state.textContent = 'Fondation jusqu’en ' + frame.name;
          try { await Plotly.react(target, traces(frame.data), data.layout, data.config); }
          catch (error) { if (version === generation) state.textContent = 'Animation indisponible.'; return; }
        }
        if (version === generation && ratio < 1) timer = requestAnimationFrame(draw);
      }
      timer = requestAnimationFrame(draw);
    }
    replay.addEventListener('click', play);
    Shiny.addCustomMessageHandler('cockpit-chart', async (message) => {
      const version = ++generation;
      cancelAnimationFrame(timer);
      state.textContent = '';
      replay.hidden = !message.frames;
      if (!window.Plotly) {
        target.textContent = 'Le chargement de Plotly a échoué. Vérifiez la connexion Internet.';
        return;
      }
      message.layout = Object.assign({
        paper_bgcolor:'#fff9f3', plot_bgcolor:'#fff9f3', margin:{l:65,r:35,t:25,b:75},
        font:{color:'#252728',family:'system-ui, sans-serif'},
        legend:{orientation:'h',x:.5,xanchor:'center',y:-.12,font:{size:12}},
        hoverlabel:{bgcolor:'#fff9f3',bordercolor:'#b9a797',font:{color:'#252728'}},
        autosize:true
      }, message.layout || {});
      message.config = {responsive:true, displaylogo:false};
      data = message;
      try {
        await Plotly.react(target, traces(message.data), message.layout, message.config);
        if (version === generation && message.frames) {
          if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
            state.textContent = 'Vue finale';
          } else {
            play();
          }
        }
      } catch (error) {
        if (version === generation) {
          target.textContent = 'Cette visualisation est indisponible : ' + error.message;
        }
      }
    });
    document.addEventListener('click', event => {
      const row = event.target.closest('tr[data-brand]');
      if (row) Shiny.setInputValue('partner_id', row.dataset.brand, {priority:'event'});
      if (event.target.id === 'replay-road') {
        document.querySelectorAll('.racer').forEach(vehicle => {
          vehicle.style.animation = 'none';
          void vehicle.offsetWidth;
          vehicle.style.animation = '';
        });
      }
    });
  }
  document.addEventListener('DOMContentLoaded', setup);
})();
"

default_path <- file.path("data", "car_brands_600_3.rds")
options(shiny.maxRequestSize = 20 * 1024^2)

ui <- fluidPage(
  tags$head(tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
            tags$script(src = "https://cdn.plot.ly/plotly-2.35.2.min.js"),
            tags$style(HTML(style)), tags$script(HTML(javascript))),
  tags$header(class = "hero",
    tags$div(class = "hero-copy", tags$h1("Marché automobile"),
      tags$p("Analyse mondiale et sélection des partenaires stratégiques")),
    tags$div(class = "hero-image", `aria-hidden` = "true")
  ),
  sidebarLayout(
    sidebarPanel(width = 3,
      fileInput("upload", "Importer une base", accept = c(".rds", ".xlsx", ".csv"),
                buttonLabel = "Parcourir", placeholder = "RDS, XLSX ou CSV"),
      uiOutput("import_status"),
      selectInput("region", "Région des graphiques", choices = c("Toutes" = "", regions))
    ),
    mainPanel(width = 9, tabsetPanel(id = "section",
      tabPanel("Visualisations", value = "charts",
        tags$section(class = "panel-card",
          tags$div(class = "chart-header", uiOutput("chart_hint"),
            selectInput("chart_type", "Visualisation", setNames(chart_ids, chart_names))),
          uiOutput("market_summary"),
          tags$button(type = "button", id = "replay-ridge", class = "control", hidden = NA, "Rejouer · 5 s"),
          tags$span(id = "ridge-state", `aria-live` = "off"),
          tags$div(id = "chart", role = "region", `aria-label` = "Visualisation interactive"))),
      tabPanel("Partenaires", value = "partners",
        tags$section(class = "panel-card", tags$h2("Partenaires autonomes"),
          tags$p(class = "hint", "Les dix partenaires asiatiques sont surlignés en vert pâle. Les alternatives proviennent d’Amérique du Nord. Cliquez sur une entreprise pour afficher ses coordonnées."),
          tags$p(class = "hint", "Score sur 100, après normalisation commune aux deux régions. R&D 30 %, sécurité 25 %, durabilité 20 %, gamme électrique 15 %, concessionnaires 10 %."),
          uiOutput("contact"), uiOutput("ranked"))),
      tabPanel("Sur la route", value = "road",
        tags$section(class = "panel-card", tags$h2("Les partenaires asiatiques en piste"), uiOutput("road")))
    ))
  )
)

server <- function(input, output, session) {
  initial <- tryCatch(load_brands(default_path), error = function(e) e)
  brands <- reactiveVal(if (!inherits(initial, "error")) initial)
  import_error <- reactiveVal(if (inherits(initial, "error")) conditionMessage(initial))
  filename <- reactiveVal(basename(default_path))
  selected <- reactiveVal(NULL)

  observeEvent(input$upload, {
    upload <- input$upload
    loaded <- tryCatch(load_brands(upload$datapath, upload$name), error = function(e) e)
    if (inherits(loaded, "error")) {
      import_error(conditionMessage(loaded))
      return()
    }
    brands(loaded)
    filename(upload$name)
    selected(NULL)
    import_error(NULL)
  })
  observeEvent(brands(), {
    available <- intersect(regions, brands()$continent)
    current <- isolate(input$region)
    updateSelectInput(session, "region", choices = c("Toutes" = "", available),
                      selected = if (isTRUE(current %in% available)) current else "")
  })
  output$import_status <- renderUI({
    d <- brands()
    tagList(
      if (!is.null(d)) tags$p(class = "hint", paste0(nrow(d), " entreprises · ", filename())),
      if (!is.null(import_error())) tags$p(class = "error-note", import_error())
    )
  })
  filtered <- reactive({
    d <- req(brands())
    region <- input$region
    if (isTRUE(region %in% d$continent)) d <- d[d$continent == region, ]
    d
  })
  output$chart_hint <- renderUI({
    id <- req(input$chart_type)
    tags$div(tags$h2(chart_names[match(id, chart_ids)]), tags$p(class = "hint", descriptions[[id]]))
  })
  output$market_summary <- renderUI({
    d <- filtered()
    values <- c(format_number(nrow(d)), format_number(length(unique(d$country))),
                paste0(format_number(stats::median(d$average_price_eur)), " €"),
                paste0(format_number(100 * mean(d$has_autonomous_driving), 1), " %"))
    names(values) <- c("Entreprises", "Pays", "Prix médian", "Conduite autonome")
    tags$div(class = "market-summary", lapply(names(values), function(label) {
      tags$div(class = "market-metric", tags$strong(values[[label]]), tags$span(label))
    }))
  })
  observe({
    req(input$section == "charts")
    chart <- charts[[req(input$chart_type)]](filtered())
    session$sendCustomMessage("cockpit-chart", chart)
  })
  ranking <- reactive(rank_partners(req(brands())))
  observeEvent(input$partner_id, {
    if (input$partner_id %in% ranking()$brand_id) selected(input$partner_id)
  })
  output$ranked <- renderUI({
    d <- ranking()
    asia <- sum(d$continent == "Asie")
    north <- sum(d$continent == "Amérique du Nord")
    tagList(
      if (asia < 10 || north < 5) tags$p(class = "hint", sprintf(
        "Candidats admissibles dans cette base : %d sur 10 en Asie, %d sur 5 en Amérique du Nord.", asia, north)),
      if (nrow(d)) partner_table(d)
    )
  })
  output$contact <- renderUI({
    d <- ranking()
    id <- req(selected())
    row <- d[d$brand_id == id, ]
    if (nrow(row)) partner_contact(row)
  })
  output$road <- renderUI(road_view(ranking()))
}

shinyApp(ui, server)
