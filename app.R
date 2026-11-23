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
:root{color-scheme:light;--bg:#edf1f6;--panel:#ffffff;--edge:#ccd7e4;--muted:#506079;--text:#14243b;--accent:#15566d}
body{background:var(--bg);color:var(--text);font-family:system-ui,sans-serif}
.container-fluid{max-width:1440px;padding:24px 34px 55px}

.hero{display:grid;grid-template-columns:minmax(0,1.15fr) minmax(0,1fr);min-height:230px;border-top:4px solid var(--accent);border-bottom:1px solid var(--edge);margin-bottom:27px;background:#f8faff}
.hero-copy{padding:32px clamp(18px,4vw,60px);align-self:center}
.hero-image{background:#9fb2cc url(automobile.jpg) center 57%/cover;background-blend-mode:luminosity}
.hero h1,.chart-header h2{text-wrap:balance}

.market-summary{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:1px;border:1px solid var(--edge);border-radius:6px;background:var(--edge);overflow:hidden;margin-bottom:17px}
.market-metric{padding:13px 12px;background:#f1f5fa;min-width:0}
.market-metric strong{display:block;font-family:Georgia,serif;font-size:clamp(20px,2.7vw,36px);font-weight:400;line-height:1.1;color:var(--accent);font-variant-numeric:tabular-nums}
.market-metric span{display:block;color:var(--muted);font-size:12px;margin-top:6px}
.chart-header{display:flex;justify-content:space-between;align-items:flex-start;column-gap:18px}
.chart-header h2{font-size:clamp(24px,3vw,36px)}
.chart-nav{margin:0 0 18px}
.chart-nav .form-group{width:100%;margin:0}
.chart-nav .shiny-options-group{display:flex;gap:7px;flex-wrap:wrap}
.chart-nav .radio-inline{margin:0;padding:0;position:relative}
.chart-nav input{position:absolute;opacity:0}
.chart-nav .radio-inline span{display:block;padding:8px 12px;border:1px solid var(--edge);border-radius:6px;background:#fcfdff;color:#35465d;transition:background .15s}
.chart-nav .radio-inline:hover span{background:#edf2f8}
.chart-nav input:checked+span{background:var(--accent);color:white;border-color:var(--accent)}
.chart-nav input:focus-visible+span{outline:2px solid var(--accent);outline-offset:3px}
.chart-tools{display:flex;align-items:center;gap:10px;flex-wrap:wrap;margin-bottom:10px}
.chart-tools .form-group,.chart-tools .checkbox,.chart-tools label{margin:0}
.chart-tools .control-group{display:flex;align-items:center;gap:8px;flex-wrap:wrap}
.chart-tools .control-group[hidden],.chart-tools button[hidden]{display:none}
.chart-stage{border:1px solid var(--edge);border-radius:9px;overflow:hidden;background:var(--panel)}
.chart-stage:fullscreen{display:flex;align-items:center;justify-content:center;padding:16px;background:var(--panel)}
.chart-stage:fullscreen #chart{height:100%;width:100%}
.chart-tools .control.secondary{background:transparent;color:var(--accent)}
.chart-tools .control.secondary:hover{background:#e7eff6}
.chart-tools .control.secondary:disabled{opacity:.5;cursor:default}
.chart-tools .map-credit{font-size:11px;color:var(--muted)}
.map-credit a{color:var(--accent);text-decoration:underline}
.map-layers{display:flex;column-gap:10px;align-items:center;flex-wrap:wrap}
.map-layers .form-group{width:235px}
.map-layers .selectize-control{margin-bottom:0}
.map-profile{margin-top:15px;padding:16px;border:1px solid var(--edge);border-radius:8px;background:#f4f8fc}
.map-profile h3{font-family:Georgia,serif;color:var(--accent);margin:0 0 6px;font-size:23px}
.company-facts{display:grid;grid-template-columns:repeat(auto-fit,minmax(135px,1fr));gap:8px;margin:0}
.company-facts>div{padding:8px 10px;background:white;border-radius:4px}
.company-facts dt{font-size:12px;color:var(--muted)}
.company-facts dd{font-variant-numeric:tabular-nums;margin-top:3px}
.timeline-slider{width:clamp(110px,19vw,260px)!important;accent-color:var(--accent)}

.chart-header .hint{max-width:55ch;margin-bottom:12px}

.hero h1{font-family:Georgia,serif;font-size:clamp(35px,6vw,68px);font-weight:700;letter-spacing:-.05em;margin:0 0 8px}
.hero p{margin:0;color:var(--muted);font-size:16px}
.well,.panel-card{background:var(--panel);border:1px solid var(--edge);border-radius:8px;box-shadow:0 2px 4px #2337550e}
.well{padding:17px}.panel-card{padding:22px;margin-top:14px}
.form-control,.selectize-input,.selectize-dropdown,.selectize-input.full,.selectize-input.input-active{background:#fcfdff!important;color:var(--text)!important;border-color:#9babc0!important}
.selectize-dropdown .active{background:#dfedf3;color:#193b56}
.btn,.control{border:1px solid var(--accent);background:var(--accent);color:white;padding:7px 13px;border-radius:3px}
.btn:hover,.control:hover,.btn:focus,.control:focus{background:#114555;color:white}
.nav-tabs{border-color:var(--edge)}
.nav-tabs>li>a{color:var(--muted);border:0;border-radius:0;font-weight:650;padding:10px 14px 12px}
.nav-tabs>li>a:hover,.nav-tabs>li>a:focus{color:var(--accent);background:#f4f7fc}
.nav-tabs>li.active>a,.nav-tabs>li.active>a:hover,.nav-tabs>li.active>a:focus{color:var(--accent);background:transparent;border:0;box-shadow:inset 0 -3px var(--accent)}
h2{font-family:Georgia,serif;font-size:27px;margin:0 0 10px}
.hint{color:var(--muted);line-height:1.55}.error-note{color:#9e2e41}
#chart{height:510px;width:100%;min-width:0}
#ridge-state{padding-left:10px;color:var(--accent)}
.table-wrap,.track-scroll{overflow-x:auto}.leaderboard{width:100%;border-collapse:collapse;font-size:13px}
.leaderboard th,.leaderboard td{padding:9px;border-bottom:1px solid var(--edge);text-align:left}
.leaderboard th{color:#324861;background:#eaf0f7;white-space:nowrap}.leaderboard tr[data-brand]{cursor:pointer}
.leaderboard .priority{background:#e3f0f2}.leaderboard tr[data-brand]:hover{background:#d7e8ef}
.brand-link{background:none;border:0;padding:0;color:var(--accent);text-align:left;text-decoration:underline}
.contact{margin:13px 0;padding:12px;border-left:3px solid var(--accent);background:#e9f3f7}
.score{display:grid;grid-template-columns:minmax(38px,1fr) auto;align-items:center;gap:8px;font-variant-numeric:tabular-nums;white-space:nowrap}
.score-track{display:block;height:5px;background:#c4d1df;overflow:hidden;border-radius:2px}
.score-fill{display:block;height:100%;background:var(--accent)}
.road-toolbar{display:flex;justify-content:space-between;align-items:center;gap:16px;flex-wrap:wrap}
.road-toolbar .hint{margin:0}
.contact a{color:var(--accent);margin-left:8px;overflow-wrap:anywhere}
.track{position:relative;isolation:isolate;min-width:790px;height:690px;margin-top:14px;border:1px solid #94a4b9;border-radius:9px;overflow:hidden;background:#526071 url(road.jpg) center/100% 100%;background-blend-mode:luminosity}
.track::after{content:'';position:absolute;z-index:-1;inset:4% 9% 5% auto;width:3px;background:repeating-linear-gradient(#f6faffbb 0 7px,#17243aaa 7px 14px)}
.racer{position:absolute;top:var(--lane);left:var(--finish);width:var(--length);transform:translateY(-30%);animation:drive 5s cubic-bezier(.15,.66,.23,1) both}
.racer-label{position:absolute;bottom:calc(100% + 4px);left:50%;transform:translateX(-50%);display:flex;align-items:center;gap:6px;width:max-content;padding:3px 6px;border:1px solid #e8f4ff66;border-radius:4px;background:#f7fbff;color:#14243b;box-shadow:0 2px 6px #0b162f59;font-size:11px;line-height:1.25}
.racer-rank{font-weight:750;color:#15566d}
.racer-name{font-weight:650;max-width:160px;overflow-wrap:anywhere}
.racer-score{font-variant-numeric:tabular-nums;color:#506079;white-space:nowrap}
.car{position:relative;height:21px;background:linear-gradient(180deg,#ffffff80,transparent 35%,#0b142a50),var(--paint);border-radius:var(--radius) 43% 42% var(--radius);box-shadow:0 3px 6px #0a1327cc;border:1px solid #182437}
.car::before,.car::after{content:'';position:absolute;left:15%;width:67%;height:3px;background:linear-gradient(90deg,#172437 0 19%,transparent 19% 80%,#172437 80%);border-radius:3px}
.car::before{top:-2px}.car::after{bottom:-2px}
.car-roof{position:absolute;left:23%;top:3px;width:var(--roof);height:13px;background:linear-gradient(90deg,#283b53 0 21%,var(--paint) 21% 68%,#283b53 68%);border:1px solid #ffffff90;border-radius:28% 47% 46% 28%}
.car-lights{position:absolute;inset:4px 3px auto auto;width:4px;height:3px;border-radius:50%;background:#ecf8ff;box-shadow:0 8px #ecf8ff}

@keyframes drive{from{left:-7%}to{left:var(--finish)}}
@media(prefers-reduced-motion:reduce){.racer{animation:none}}
@media(max-width:700px){.container-fluid{padding:15px}.hero{min-height:0;grid-template-columns:minmax(0,1fr) 31%}.hero-copy{padding:15px}.hero p{font-size:14px}.chart-header{flex-wrap:wrap}.market-summary{grid-template-columns:repeat(2,minmax(0,1fr))} .panel-card{padding:12px}#chart{height:450px}}
"

javascript <- "
(function () {
  function setup() {
    if (!window.Shiny) return;
    const target = document.getElementById('chart');
    const replay = document.getElementById('replay-ridge');
    const state = document.getElementById('ridge-state');
    const slider = document.getElementById('ridge-slider');
    const animation = document.getElementById('ridge-controls');
    const zoom = document.getElementById('chart-fullscreen');
    const exportImage = document.getElementById('chart-export');
    const stage = document.getElementById('chart-stage');
    const toolStatus = document.getElementById('chart-tool-status');
    let data = null, generation = 0, timer = null, showCity = false;
    const cityOutput = document.getElementById('map_city_profile');
    new MutationObserver(() => {
      const profile = cityOutput.querySelector('.map-profile');
      if (showCity && profile) {
        showCity = false;
        profile.scrollIntoView({block:'nearest'});
      }
    }).observe(cityOutput, {childList:true});
    function selectCity(event) {
      const point = event?.points?.[0];
      if (!point || !['scattergeo','scattermap'].includes(point.data.type)) return;
      const representative = point.customdata ?? point.data.customdata?.[point.pointNumber];
      if (typeof representative === 'string') {
        const profile = cityOutput.querySelector('.map-profile');
        if (profile) profile.scrollIntoView({block:'nearest'});
        else showCity = true;
        Shiny.setInputValue('map_city', representative, {priority:'event'});
      }
    }
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
    async function drawFrame(index) {
      const frame = data.frames[index];
      slider.value = index;
      state.textContent = 'Fondation jusqu’en ' + frame.name;
      await Plotly.react(target, traces(frame.data), data.layout, data.config);
    }
    async function play() {
      if (!data || !data.frames || !window.Plotly) return;
      const version = ++generation;
      cancelAnimationFrame(timer);
      const start = performance.now();
      async function draw(now) {
        if (version !== generation) return;
        const ratio = Math.max(0, Math.min(1, (now - start) / 5000));
        const index = Math.floor(ratio * (data.frames.length - 1));
        if (String(index) !== slider.value || !state.textContent) {
          try { await drawFrame(index); }
          catch (error) { if (version === generation) state.textContent = 'Animation indisponible.'; return; }
        }
        if (version === generation && ratio < 1) timer = requestAnimationFrame(draw);
      }
      timer = requestAnimationFrame(draw);
    }
    replay.addEventListener('click', play);
    slider.addEventListener('input', async () => {
      if (!data?.frames) return;
      ++generation;
      cancelAnimationFrame(timer);
      try { await drawFrame(Number(slider.value)); }
      catch (error) { state.textContent = 'Vue indisponible.'; }
    });
    zoom.hidden = !document.fullscreenEnabled;
    zoom.addEventListener('click', () => {
      toolStatus.textContent = '';
      stage.requestFullscreen().catch(() => {
        toolStatus.textContent = 'Le plein écran est indisponible dans cette fenêtre.';
      });
    });
    document.addEventListener('fullscreenchange', () => {
      if (window.Plotly && target._fullLayout) Plotly.Plots.resize(target);
    });
    exportImage.addEventListener('click', async () => {
      exportImage.disabled = true;
      toolStatus.textContent = '';
      try { await Plotly.downloadImage(target, {format:'png', filename:'marche-automobile', scale:2}); }
      catch (error) { toolStatus.textContent = 'L’export PNG est indisponible pour cette vue.'; }
      finally { exportImage.disabled = false; }
    });
    Shiny.addCustomMessageHandler('cockpit-chart', async (message) => {
      const version = ++generation;
      cancelAnimationFrame(timer);
      state.textContent = '';
      toolStatus.textContent = '';
      animation.hidden = !message.frames;
      exportImage.disabled = true;
      if (message.frames) {
        slider.max = message.frames.length - 1;
        slider.value = slider.max;
      }
      if (!window.Plotly) {
        target.textContent = 'Le chargement de Plotly a échoué. Vérifiez la connexion Internet.';
        return;
      }
      message.layout = Object.assign({
        paper_bgcolor:'#ffffff', plot_bgcolor:'#ffffff', margin:{l:65,r:35,t:25,b:75},
        font:{color:'#14243b',family:'system-ui, sans-serif'},
        legend:{orientation:'h',x:.5,xanchor:'center',y:-.12,font:{size:12}},
        hoverlabel:{bgcolor:'#ffffff',bordercolor:'#9babc0',font:{color:'#14243b'}},
        autosize:true
      }, message.layout || {});
      message.config = {responsive:true, displaylogo:false, modeBarButtonsToRemove:['toImage']};
      data = message;
      try {
        await Plotly.react(target, traces(message.data), message.layout, message.config);
        if (version === generation) {
          target.removeListener('plotly_click', selectCity);
          target.on('plotly_click', selectCity);
          exportImage.disabled = false;
        }
        if (version === generation && message.frames) {
          if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
            state.textContent = 'Fondation jusqu’en ' + message.frames.at(-1).name;
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
            tags$script(src = "https://cdn.plot.ly/plotly-3.3.0.min.js"),
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
          tags$nav(class = "chart-nav", `aria-label` = "Choix de la visualisation",
            radioButtons("chart_type", NULL, setNames(chart_ids, chart_names), inline = TRUE)),
          tags$div(class = "chart-header", uiOutput("chart_hint")),
          uiOutput("market_summary"),
          tags$div(class = "chart-tools",
            tags$button(type = "button", id = "chart-export", class = "control secondary",
              disabled = NA, "Exporter en PNG"),
            tags$button(type = "button", id = "chart-fullscreen", class = "control secondary",
              "Plein écran"),
            conditionalPanel("input.chart_type === 'map'",
              checkboxInput("detail_map", "Fond de carte détaillé"),
              conditionalPanel("input.detail_map",
                tags$div(class = "map-layers",
                  selectInput("map_layer", "Fond détaillé",
                    c("OpenStreetMap · OpenFreeMap" = "street", "Satellite · Esri World Imagery" = "satellite")),
                  conditionalPanel("input.map_layer === 'street'", tags$small(class = "map-credit",
                    tags$a(href = "https://openfreemap.org/", "OpenFreeMap"), " · ",
                    tags$a(href = "https://www.openstreetmap.org/copyright", "© OpenStreetMap"))),
                  conditionalPanel("input.map_layer === 'satellite'", tags$small(class = "map-credit",
                    tags$a(href = "https://www.arcgis.com/home/item.html?id=10df2279f9684e4a9f6a7f08febac2a9",
                      "Esri World Imagery"), " · Vantor, Earthstar Geographics et GIS User Community"))))),
            tags$div(id = "ridge-controls", class = "control-group", hidden = NA,
              tags$button(type = "button", id = "replay-ridge", class = "control", "Rejouer · 5 s"),
              tags$input(id = "ridge-slider", class = "timeline-slider", type = "range",
                         min = 0, max = 0, value = 0, `aria-label` = "Année de fondation maximale"),
              tags$span(id = "ridge-state"))),
          tags$div(id = "chart-tool-status", class = "error-note", role = "status"),
          tags$div(id = "chart-stage", class = "chart-stage",
            tags$div(id = "chart", role = "region", `aria-label` = "Visualisation interactive")),
          conditionalPanel("input.chart_type === 'map'", uiOutput("map_city_profile")))),
      tabPanel("Partenaires", value = "partners",
        tags$section(class = "panel-card", tags$h2("Partenaires autonomes"),
          tags$p(class = "hint", "Les dix partenaires asiatiques sont surlignés en bleu pétrole clair. Les alternatives proviennent d’Amérique du Nord. Cliquez sur une entreprise pour afficher ses coordonnées."),
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
  map_city <- reactiveVal(NULL)

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
    map_city(NULL)
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
    description <- descriptions[[id]]
    if (id == "map") {
      description <- paste(description, "Cliquez sur un point pour consulter les entreprises de la ville.")
      if (!isTRUE(input$detail_map)) description <- paste(description, "Faites glisser le globe pour le faire pivoter.")
    }
    tags$div(tags$h2(chart_names[match(id, chart_ids)]), tags$p(class = "hint", description))
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
    id <- req(input$chart_type)
    d <- filtered()
    chart <- if (id == "map") {
      map_chart(d, detailed = isTRUE(input$detail_map), satellite = identical(input$map_layer, "satellite"))
    } else charts[[id]](d)
    session$sendCustomMessage("cockpit-chart", chart)
  })
  observeEvent(input$region, map_city(NULL), ignoreInit = TRUE)
  observeEvent(input$map_city, {
    id <- input$map_city
    if (is.character(id) && length(id) == 1L && id %in% filtered()$brand_id) map_city(id)
  })
  city_rows <- reactive(city_companies(filtered(), req(map_city())))
  output$map_city_profile <- renderUI({
    companies <- city_rows()
    req(nrow(companies) > 0L)
    sorted_names <- make.unique(companies$brand_name)
    tags$section(class = "map-profile",
      tags$h3(paste(companies$city[1L], companies$country[1L], sep = " · ")),
      tags$p(class = "hint", sprintf("%d entreprise(s) dans la sélection", nrow(companies))),
      selectInput("map_company", "Entreprise", setNames(companies$brand_id, sorted_names)),
      uiOutput("map_company_profile")
    )
  })
  output$map_company_profile <- renderUI({
    companies <- city_rows()
    req(nrow(companies) > 0L)
    index <- match(input$map_company, companies$brand_id)
    company_profile(companies[if (length(index) && !is.na(index)) index else 1L, , drop = FALSE])
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
