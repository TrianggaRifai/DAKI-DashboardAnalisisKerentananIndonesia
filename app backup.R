# install.packages(c("shiny", "shinydashboard", "tidyverse", "plotly", "DT", "sf", "leaflet", "janitor", "BAMMtools", "lmtest", "car", "nortest", "shinyBS", "ggplot2", "broom", "rmarkdown", "mapview", "tinytex", "terra", "EnvStats", "htmlwidgets", "markdown", "rsconnect", "scales"))
# Untuk fitur unduh PDF, diperlukan instalasi LaTeX. Jalankan sekali di konsol R: tinytex::install_tinytex()

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(sf)
library(leaflet)
library(janitor)
library(BAMMtools)
library(lmtest)
library(car)
library(nortest)
library(shinyBS)
library(ggplot2)
library(broom)
library(rmarkdown)
library(mapview)
library(htmlwidgets)
library(rsconnect)
library(markdown) # Diperlukan untuk merender teks interpretasi
library(EnvStats) # Diperlukan untuk Uji Varians
library(scales)   # Diperlukan untuk perbandingan radar

# ==== Load Data Asli ====
tryCatch({
  data_svi_raw <- read.csv('data.csv', fileEncoding = "UTF-8-BOM") %>%
    janitor::clean_names() %>%
    mutate(kodeprkab = as.character(kodeprkab))
  
  geo_data_raw <- st_read('indonesia511.geojson')
  
}, error = function(e) {
  showNotification("Data lokal tidak ditemukan. Memuat data dari sumber online...", type = "warning", duration = 10)
  data_svi_raw <<- read.csv(url("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv")) %>%
    slice(1:511) %>%
    select(track_popularity, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence) %>%
    rename(poverty=track_popularity, lowedu=danceability, noelectric=energy, nosewer=speechiness, rented=acousticness, fhead=instrumentalness, illiterate=liveness, growth=valence) %>%
    mutate(kodeprkab = as.character(1:511))
  
  geo_data_raw <<- st_read("https://raw.githubusercontent.com/ans-417/leaflet-geojson-mapid/main/data/indonesia-prov.geojson") %>%
    rename(kodeprkab = Kode)
})


geo_data_sf <- geo_data_raw %>%
  janitor::clean_names() %>%
  select(kodeprkab, any_of(c("nmkab", "nmprov", "provinsi")), geometry) %>%
  rename_with(~"nmkab", any_of(c("nmkab"))) %>%
  rename_with(~"nmprov", any_of(c("nmprov", "provinsi"))) %>%
  mutate(kodeprkab = as.character(kodeprkab)) %>%
  distinct(kodeprkab, .keep_all = TRUE)

map_data_full_raw <- geo_data_sf %>%
  left_join(data_svi_raw, by = "kodeprkab") %>%
  filter(!is.na(poverty))

# Menyiapkan data untuk analisis inferensia
map_data_full <- map_data_full_raw %>%
  mutate(
    poverty_level_2 = cut(poverty,
                          breaks = c(-Inf, median(poverty, na.rm = TRUE), Inf),
                          labels = c("Rendah", "Tinggi")),
    poverty_level_3 = cut(poverty,
                          breaks = quantile(poverty, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE, type = 7),
                          labels = c("Rendah", "Sedang", "Tinggi"),
                          include.lowest = TRUE,
                          duplicates.ok = TRUE),
    poverty_level_4 = cut(poverty,
                          breaks = quantile(poverty, probs = seq(0, 1, 0.25), na.rm = TRUE, type = 7),
                          labels = c("Sangat Rendah", "Rendah", "Tinggi", "Sangat Tinggi"),
                          include.lowest = TRUE,
                          duplicates.ok = TRUE),
    lowedu_level = cut(lowedu,
                       breaks = quantile(lowedu, probs = seq(0, 1, 0.5), na.rm = TRUE, type = 7),
                       labels = c("Rendah", "Tinggi"),
                       include.lowest = TRUE,
                       duplicates.ok = TRUE)
  )


# Daftar variabel kontinyu untuk dipilih
continuous_vars <- data_svi_raw %>%
  select_if(is.numeric) %>%
  names()

# Daftar variabel kategorik untuk dipilih
categorical_vars <- names(map_data_full)[sapply(map_data_full, function(x) is.factor(x) || is.character(x))]
categorical_vars <- setdiff(categorical_vars, c("kodeprkab", "nmkab", "nmprov", "geometry"))

with_tooltip <- function(ui_element, id, title, placement = "right") {
  tags$span(
    ui_element,
    bsButton(paste0("q_", id), label = "?", style = "info", size = "extra-small"),
    bsTooltip(id = paste0("q_", id), title = title, placement = placement, trigger = "click")
  )
}


# ==== UI ====
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Dashboard Analisis Kerentanan Indonesia (DAKI)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen", icon = icon("cogs")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-simple")),
      menuItem("Analisis Klaster", tabName = "klaster", icon = icon("cubes")),
      menuItem("Peringkat & Perbandingan", tabName = "perbandingan", icon = icon("balance-scale")),
      menuItem("Regresi Linear Berganda", tabName = "regresi", icon = icon("chart-line")),
      menuItem("Statistik Inferensia", tabName = "inferensia", icon = icon("calculator")),
      menuItem("Pusat Unduhan", tabName = "pusat_unduhan", icon = icon("download"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
        }
        pre, .shiny-text-output {
            white-space: pre-wrap !important;
            word-wrap: break-word !important;
        }
        .box-title { font-weight: bold; }
        .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #3c8dbc; }
        .test-result-box { padding: 10px; margin-bottom: 10px; border-radius: 5px; border: 1px solid transparent; }
        .pass { background-color: #d4edda; color: #155724; border-color: #c3e6cb; }
        .fail { background-color: #f8d7da; color: #721c24; border-color: #f5c6cb; }
        .btn-group-xs > .btn, .btn-xs { padding: 1px 5px; }

        /* --- KOTAK INTERPRETASI BARU --- */
        .interpretation-box {
          background-color: #f8f9fa;
          border: 1px solid #e9ecef;
          border-left: 5px solid #3c8dbc; /* Aksen warna biru */
          padding: 15px;
          margin-top: 15px;
          border-radius: 5px;
        }
        .interpretation-box h4 {
          margin-top: 0;
          font-weight: 600;
          color: #333;
          border-bottom: 1px solid #ddd;
          padding-bottom: 5px;
        }
        .interpretation-box p, .interpretation-box ul {
          font-size: 14px;
          line-height: 1.6;
        }
        .interpretation-box strong {
          color: #0056b3;
        }
      ")),
      tags$script(HTML("
        $(document).on('shiny:connected', function(event) {
          const adjustHeight = () => {
            const sidebarHeight = $('.main-sidebar').height();
            const contentHeight = $('.content').height();
            const newHeight = Math.max(sidebarHeight, contentHeight) + 50;
            $('.content-wrapper').css('min-height', newHeight + 'px');
          };

          setTimeout(adjustHeight, 200);

          $('a[data-toggle=\"tab\"]').on('shown.bs.tab', adjustHeight);
          
          const observer = new MutationObserver((mutations) => {
            setTimeout(adjustHeight, 200);
          });

          const targetNode = document.querySelector('.content-wrapper');
          if (targetNode) {
            observer.observe(targetNode, { childList: true, subtree: true });
          }
        });
      "))
    ),
    tabItems(
      # --- Halaman 1: Beranda ---
      tabItem(tabName = "beranda",
              fluidRow(
                tabBox(
                  id = "infoTab", width = 12,
                  tabPanel(
                    title = "Informasi Dashboard", icon = icon("info-circle"),
                    fluidRow(
                      column(width = 8,
                             box(title = "Tentang Dashboard", status = "primary", solidHeader = TRUE, width = NULL,
                                 p("Dashboard Analisis Kerentanan Indonesia (DAKI) adalah platform interaktif yang dirancang untuk memvisualisasikan dan menganalisis tingkat kerentanan sosial di berbagai kabupaten/kota di seluruh Indonesia."),
                                 p("Data utama yang digunakan dalam dashboard ini berasal dari publikasi ilmiah:",
                                   tags$br(),
                                   tags$a(href="https://www.sciencedirect.com/science/article/pii/S2352340921010180", target="_blank", "'Revisiting social vulnerability analysis in Indonesia'"),
                                   " oleh Robert Kurniawan, et al.", tags$br(),
                                   "Data geografis (peta) menggunakan file GeoJSON yang mencakup 511 kabupaten/kota di Indonesia."
                                 )
                             )
                      ),
                      column(width = 4,
                             infoBoxOutput("info_kabupaten", width = NULL),
                             infoBoxOutput("info_variabel", width = NULL)
                      )
                    )
                  ),
                  tabPanel(
                    title = "Metadata", icon = icon("book"),
                    DTOutput("tabel_variabel")
                  )
                )
              ),
              fluidRow(
                column(width = 8,
                       box(title = "Peta Sebaran Tingkat Kemiskinan (%)", width = NULL, solidHeader = TRUE, status = "primary", leafletOutput("peta_svi", height = "520px")),
                       box(title = "Rata-Rata Komponen Kerentanan Sosial", width = NULL, solidHeader = TRUE, status = "primary", plotlyOutput("grafik_komponen", height = "300px"))
                ),
                column(width = 4,
                       box(title = "Filter Data", width = NULL, solidHeader = TRUE, status = "info",
                           selectInput("provinsi_filter", "Pilih Provinsi:", choices = c("Seluruh Indonesia" = "Semua", sort(unique(map_data_full_raw$nmprov))))),
                       valueBoxOutput("total_kabupaten", width = NULL),
                       valueBoxOutput("rata_kemiskinan", width = NULL),
                       valueBoxOutput("kemiskinan_tertinggi", width = NULL)
                )
              ),
              fluidRow(
                column(width = 12,
                       box(title = "📋 Tabel Data Interaktif", width = NULL, solidHeader = TRUE, status = "info",
                           DTOutput("tabel_data"), br(),
                           downloadButton("download_data", "📥 Unduh Data (.csv)", class = "btn-success"),
                           actionButton("save_beranda", "Simpan Hasil ke Pusat Unduhan", icon = icon("save"), class = "btn-primary"))
                )
              )
      ),
      # --- Halaman 2: Eksplorasi Data ---
      tabItem(tabName = "eksplorasi",
              h2("Eksplorasi Data Antar Variabel"),
              p("Gunakan panel di bawah untuk memilih variabel dan memfilter data untuk analisis eksplorasi."),
              fluidRow(
                column(width = 12,
                       box(title = "Pengaturan Eksplorasi", status = "primary", solidHeader = TRUE, width = "100%", collapsible = TRUE,
                           fluidRow(
                             column(4, selectInput("explore_var_x", "Pilih Variabel X (Utama):", choices = str_to_title(continuous_vars), selected = "Poverty")),
                             column(4, selectInput("explore_var_y", "Pilih Variabel Y (Pembanding):", choices = str_to_title(continuous_vars), selected = "Lowedu")),
                             column(4, selectInput("explore_provinsi", "Filter Provinsi:", choices = c("Seluruh Indonesia" = "Semua", sort(unique(map_data_full_raw$nmprov)))))
                           ),
                           actionButton("save_eksplorasi", "Simpan Hasil ke Pusat Unduhan", icon = icon("save"), class = "btn-primary")
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(title = "Hasil Eksplorasi", status = "success", solidHeader = TRUE, width = "100%",
                           tabBox(
                             width = 12, id = "explore_tabs",
                             tabPanel("Statistik Deskriptif", icon = icon("calculator"),
                                      br(),
                                      tableOutput("explore_stats_table"),
                                      hr(),
                                      uiOutput("explore_stats_interpretasi")),
                             tabPanel("Grafik Sebaran (Variabel X)", icon = icon("chart-area"),
                                      plotlyOutput("explore_boxplot"),
                                      hr(),
                                      uiOutput("explore_dist_interpretasi")),
                             tabPanel("Grafik Hubungan (X & Y)", icon = icon("chart-line"),
                                      plotlyOutput("explore_scatterplot"),
                                      hr(),
                                      uiOutput("explore_scatter_interpretasi"))
                           )
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(title = "Profil Kerentanan Tematik", status = "info", solidHeader = TRUE, width = "100%",
                           tabBox(
                             id = "tema_tabs_eksplorasi", width = NULL,
                             tabPanel("Demografi", icon = icon("users"),
                                      plotlyOutput("plot_demografi"),
                                      hr(),
                                      uiOutput("plot_demografi_interp")),
                             tabPanel("Perumahan & Infrastruktur", icon = icon("home"),
                                      plotlyOutput("plot_perumahan"),
                                      hr(),
                                      uiOutput("plot_perumahan_interp")),
                             tabPanel("Pendidikan & Ketenagakerjaan", icon = icon("briefcase"),
                                      plotlyOutput("plot_pendidikan"),
                                      hr(),
                                      uiOutput("plot_pendidikan_interp"))
                           )
                       )
                )
              )
      ),
      # --- Halaman 3: Manajemen Data ---
      tabItem(tabName = "manajemen",
              h2("Manajemen Data: Kategorisasi Variabel"),
              p("Halaman ini berfungsi untuk mengubah variabel kontinyu menjadi data kategorik menggunakan berbagai metode statistik."),
              fluidRow(
                column(width = 4,
                       box(title = "Pengaturan Kategorisasi", status = "primary", solidHeader = TRUE, width = NULL,
                           selectInput("var_kategorisasi", "1. Pilih Variabel:", choices = str_to_title(continuous_vars)),
                           selectInput("metode_kategorisasi", "2. Pilih Metode:",
                                       choices = c("Equal Interval", "Kuantil (Equal Count)", "Jenks Natural Breaks")),
                           sliderInput("jumlah_kategori", "3. Tentukan Jumlah Kategori:", min = 2, max = 5, value = 3, step = 1),
                           actionButton("proses_btn", "Proses Kategorisasi", icon = icon("play-circle"), class = "btn-success", width = "100%"),
                           br(), br(),
                           actionButton("save_manajemen", "Simpan Hasil ke Pusat Unduhan", icon = icon("save"), class = "btn-primary", width="100%")
                       ),
                       box(title = "Ringkasan Variabel Asli", status = "info", solidHeader = TRUE, width = NULL,
                           uiOutput("summary_stats_ui")
                       )
                ),
                column(width = 8,
                       uiOutput("hasil_analisis_tabs")
                )
              ),
              fluidRow(
                uiOutput("summary_results_ui")
              )
      ),
      # --- Halaman 4: Analisis Klaster ---
      tabItem(tabName = "klaster",
              h2("Analisis Klaster (K-Means)"),
              p("Gunakan halaman ini untuk mengelompokkan kabupaten/kota ke dalam beberapa segmen berdasarkan profil kerentanan mereka."),
              fluidRow(
                column(width = 4,
                       box(title = "Pengaturan Analisis", status = "primary", solidHeader = TRUE, width = NULL,
                           selectizeInput("cluster_vars", "1. Pilih Variabel untuk Klaster:",
                                          choices = str_to_title(continuous_vars),
                                          selected = c("Poverty", "Lowedu", "Noelectric", "Nosewer"),
                                          multiple = TRUE, options = list(plugins = list('remove_button'))),
                           hr(),
                           box(title = with_tooltip(tags$span("Bantuan Penentuan Klaster (Elbow Method)"), "elbow_tip", "Metode Siku (Elbow Method) adalah teknik visual untuk menemukan jumlah klaster (K) yang optimal. Cara kerjanya: 1. Menghitung 'Within-Cluster Sum of Squares' (WSS) untuk setiap nilai K (misal, 1 sampai 10). WSS adalah total jarak kuadrat antara setiap titik data dengan pusat klasternya. Semakin kecil WSS, semakin padat klasternya. 2. Plot WSS vs Jumlah Klaster (K). Grafik akan menurun seiring penambahan K. 3. Cari 'siku' pada grafik, yaitu titik di mana penurunan WSS melambat secara drastis. Titik ini adalah kandidat K optimal karena penambahan klaster setelah titik ini tidak memberikan banyak perbaikan."),
                               status = "info", solidHeader = TRUE, width = NULL, collapsible = TRUE, collapsed = FALSE,
                               conditionalPanel(
                                 condition = "input.cluster_vars.length > 1",
                                 plotOutput("elbow_plot", height = "250px")
                               )
                           ),
                           hr(),
                           sliderInput("cluster_k", "2. Tentukan Jumlah Klaster (K):", min = 2, max = 10, value = 3, step = 1),
                           actionButton("cluster_btn", "Jalankan Analisis Klaster", icon = icon("play"), class = "btn-success", width = "100%"),
                           br(),br(),
                           actionButton("save_klaster", "Simpan Hasil ke Pusat Unduhan", icon = icon("save"), class = "btn-primary", width = "100%")
                       )
                ),
                column(width = 8,
                       uiOutput("cluster_results_ui"),
                       uiOutput("cluster_interpretation_ui")
                )
              )
      ),
      # --- Halaman 5: Peringkat & Perbandingan ---
      tabItem(tabName = "perbandingan",
              h2("Peringkat dan Perbandingan Antar Wilayah"),
              p("Pilih dua atau lebih kabupaten/kota untuk membandingkan profil kerentanan mereka secara langsung."),
              fluidRow(
                box(title = "Pengaturan Perbandingan", status = "primary", solidHeader = TRUE, width = 12,
                    selectizeInput("compare_kab", "Pilih Kabupaten/Kota:",
                                   choices = sort(unique(map_data_full$nmkab)),
                                   selected = c("KABUPATEN BANDUNG", "KOTA SURABAYA"),
                                   multiple = TRUE, options = list(plugins = list('remove_button'))),
                    actionButton("save_perbandingan", "Simpan Hasil ke Pusat Unduhan", icon = icon("save"), class = "btn-primary")
                )
              ),
              fluidRow(
                column(width = 7, uiOutput("compare_results_ui")),
                column(width = 5, uiOutput("compare_interpretation_ui"))
              )
      ),
      # --- Halaman 6: Regresi Linear Berganda ---
      tabItem(tabName = "regresi",
              h2("Analisis Regresi Linear Berganda"),
              p("Gunakan halaman ini untuk membangun model regresi, menguji asumsi klasik, dan menginterpretasikan hasilnya."),
              fluidRow(
                column(width = 4,
                       box(title = "1. Pengaturan Model", status = "primary", solidHeader = TRUE, width = NULL,
                           selectInput("reg_var_y", "Pilih Variabel Dependen (Y):", choices = str_to_title(continuous_vars), selected = "Poverty"),
                           selectizeInput("reg_vars_x", "Pilih Variabel Independen (X):", choices = str_to_title(continuous_vars), selected = c("Lowedu", "Noelectric", "Nosewer"), multiple = TRUE, options = list(plugins = list('remove_button'))),
                           hr(),
                           actionButton("reg_proses_btn", "Jalankan Analisis Regresi", icon = icon("play-circle"), class = "btn-success", width = "100%"),
                           br(),br(),
                           actionButton("save_regresi", "Simpan Hasil ke Pusat Unduhan", icon = icon("save"), class = "btn-primary", width = "100%")
                       ),
                       uiOutput("reg_summary_boxes_ui")
                ),
                column(width = 8,
                       uiOutput("reg_hasil_tabs_ui")
                )
              )
      ),
      # --- Halaman 7: Statistik Inferensia ---
      tabItem(tabName = "inferensia",
              h2("Statistik Inferensia"),
              p("Halaman ini menyediakan berbagai alat uji statistik untuk analisis inferensial."),
              fluidRow(
                column(12, actionButton("save_inferensia", "Simpan Semua Hasil Uji ke Pusat Unduhan", icon = icon("save"), class = "btn-primary", width="100%"), br(), br())
              ),
              tabBox(
                id = "inferensia_main_tabs", width = 12,
                tabPanel(title = "Uji Beda Rata-Rata", icon = icon("t"),
                         fluidRow(
                           column(12,
                                  box(title = with_tooltip(tags$span("Uji T Satu Sampel"), "ttest1_tip", "Uji T Satu Sampel digunakan untuk menguji apakah rata-rata dari satu kelompok data berbeda secara signifikan dari suatu nilai hipotesis yang telah ditentukan (μ₀)."), status = "primary", solidHeader = TRUE, width = NULL, collapsible = TRUE,
                                      fluidRow(
                                        column(4, selectInput("ttest1_var", "Pilih Variabel:", choices = str_to_title(continuous_vars))),
                                        column(4, numericInput("ttest1_mu", "Nilai Hipotesis Rata-rata (μ₀):", value = 0)),
                                        column(4, actionButton("ttest1_btn", "Jalankan Uji", class = "btn-info", style="margin-top: 25px;"))
                                      ),
                                      hr(),
                                      verbatimTextOutput("ttest1_out"),
                                      uiOutput("ttest1_interp")
                                  ),
                                  box(title = with_tooltip(tags$span("Uji T Dua Sampel Independen"), "ttest2_tip", "Uji T Dua Sampel Independen membandingkan rata-rata dari dua kelompok yang tidak berhubungan (independen) untuk melihat apakah ada perbedaan yang signifikan secara statistik di antara keduanya."), status = "success", solidHeader = TRUE, width = NULL, collapsible = TRUE,
                                      fluidRow(
                                        column(4, selectInput("ttest2_var", "Pilih Variabel Numerik:", choices = str_to_title(continuous_vars))),
                                        column(4, selectInput("ttest2_group", "Pilih Variabel Grup (2 Level):", choices = categorical_vars[sapply(map_data_full[categorical_vars], function(c) n_distinct(c) == 2)], selected = "lowedu_level")),
                                        column(4, actionButton("ttest2_btn", "Jalankan Uji", class = "btn-info", style="margin-top: 25px;"))
                                      ),
                                      hr(),
                                      verbatimTextOutput("ttest2_out"),
                                      uiOutput("ttest2_interp")
                                  )
                           )
                         )
                ),
                tabPanel(title = "Uji Proporsi & Varians", icon = icon("percentage"),
                         fluidRow(
                           column(12,
                                  box(title = with_tooltip(tags$span("Uji Proporsi Satu Sampel"), "proptest1_tip", "Uji Proporsi Satu Sampel membandingkan proporsi dari satu kategori dalam sampel dengan nilai hipotesis yang ditentukan."), status = "primary", solidHeader = TRUE, width = NULL, collapsible = TRUE,
                                      fluidRow(
                                        column(3, selectInput("proptest1_var", "Pilih Variabel (2 Level):", choices = categorical_vars[sapply(map_data_full[categorical_vars], function(c) n_distinct(c) == 2)], selected = "lowedu_level")),
                                        column(3, uiOutput("proptest1_level_ui")),
                                        column(3, numericInput("proptest1_p", "Nilai Hipotesis Proporsi (p₀):", value = 0.5, min = 0, max = 1, step = 0.01)),
                                        column(3, actionButton("proptest1_btn", "Jalankan Uji", class = "btn-info", style="margin-top: 25px;"))
                                      ),
                                      hr(),
                                      verbatimTextOutput("proptest1_out"),
                                      uiOutput("proptest1_interp")
                                  ),
                                  box(title = with_tooltip(tags$span("Uji Proporsi Dua Sampel"), "proptest2_tip", "Uji Proporsi Dua Sampel membandingkan proporsi suatu kategori antara dua kelompok yang berbeda."), status = "success", solidHeader = TRUE, width = NULL, collapsible = TRUE,
                                      fluidRow(
                                        column(3, selectInput("proptest2_var", "Pilih Variabel Outcome (2 Level):", choices = categorical_vars[sapply(map_data_full[categorical_vars], function(c) n_distinct(c) == 2)], selected = "lowedu_level")),
                                        column(3, uiOutput("proptest2_level_ui")),
                                        column(3, selectInput("proptest2_group", "Pilih Variabel Grup (2 Level):", choices = categorical_vars[sapply(map_data_full[categorical_vars], function(c) n_distinct(c) == 2)])),
                                        column(3, actionButton("proptest2_btn", "Jalankan Uji", class = "btn-info", style="margin-top: 25px;"))
                                      ),
                                      hr(),
                                      verbatimTextOutput("proptest2_out"),
                                      uiOutput("proptest2_interp")
                                  ),
                                  box(title = with_tooltip(tags$span("Uji Varians Satu Sampel"), "vartest1_tip", "Uji Chi-Square untuk varians membandingkan varians dari satu sampel dengan nilai hipotesis varians populasi."), status = "primary", solidHeader = TRUE, width = NULL, collapsible = TRUE,
                                      fluidRow(
                                        column(4, selectInput("vartest1_var", "Pilih Variabel:", choices = str_to_title(continuous_vars))),
                                        column(4, numericInput("vartest1_sigma2", "Nilai Hipotesis Varians (σ²₀):", value = 1, min = 0.01)),
                                        column(4, actionButton("vartest1_btn", "Jalankan Uji", class = "btn-info", style="margin-top: 25px;"))
                                      ),
                                      hr(),
                                      verbatimTextOutput("vartest1_out"),
                                      uiOutput("vartest1_interp")
                                  ),
                                  box(title = with_tooltip(tags$span("Uji Varians Dua Sampel"), "vartest2_tip", "Uji F untuk varians membandingkan varians dari dua kelompok independen untuk melihat apakah mereka sama atau berbeda secara signifikan."), status = "success", solidHeader = TRUE, width = NULL, collapsible = TRUE,
                                      fluidRow(
                                        column(4, selectInput("vartest2_var", "Pilih Variabel Numerik:", choices = str_to_title(continuous_vars))),
                                        column(4, selectInput("vartest2_group", "Pilih Variabel Grup (2 Level):", choices = categorical_vars[sapply(map_data_full[categorical_vars], function(c) n_distinct(c) == 2)], selected = "lowedu_level")),
                                        column(4, actionButton("vartest2_btn", "Jalankan Uji", class = "btn-info", style="margin-top: 25px;"))
                                      ),
                                      hr(),
                                      verbatimTextOutput("vartest2_out"),
                                      uiOutput("vartest2_interp")
                                  )
                           )
                         )
                ),
                tabPanel(title = "ANOVA (Beda Rata-Rata >2 Kelompok)", icon = icon("sort-amount-down"),
                         fluidRow(
                           column(12,
                                  box(title = with_tooltip(tags$span("ANOVA Satu Arah"), "anova1_tip", "Analysis of Variance (ANOVA) Satu Arah digunakan untuk membandingkan rata-rata dari tiga atau lebih kelompok independen untuk menentukan apakah setidaknya ada satu kelompok yang berbeda secara signifikan."), status = "warning", solidHeader = TRUE, width = NULL, collapsible = TRUE,
                                      fluidRow(
                                        column(4, selectInput("anova1_var", "Pilih Variabel Dependen (Numerik):", choices = str_to_title(continuous_vars))),
                                        column(4, selectInput("anova1_group", "Pilih Faktor Independen:", choices = categorical_vars[sapply(map_data_full[categorical_vars], function(c) n_distinct(c) > 1 & n_distinct(c) < 10)], selected = "poverty_level_3")),
                                        column(4, actionButton("anova1_btn", "Jalankan Uji", class = "btn-info", style="margin-top: 25px;"))
                                      ),
                                      hr(),
                                      verbatimTextOutput("anova1_out"),
                                      uiOutput("anova1_interp"),
                                      uiOutput("anova1_posthoc_ui")
                                  ),
                                  box(title = with_tooltip(tags$span("ANOVA Dua Arah"), "anova2_tip", "ANOVA Dua Arah digunakan untuk mengevaluasi efek dari dua variabel faktor independen terhadap satu variabel dependen, termasuk kemungkinan adanya interaksi antara kedua faktor tersebut."), status = "danger", solidHeader = TRUE, width = NULL, collapsible = TRUE,
                                      fluidRow(
                                        column(3, selectInput("anova2_var", "Pilih Variabel Dependen (Numerik):", choices = str_to_title(continuous_vars))),
                                        column(3, selectInput("anova2_group1", "Pilih Faktor Independen 1:", choices = categorical_vars, selected = "poverty_level_3")),
                                        column(3, selectInput("anova2_group2", "Pilih Faktor Independen 2:", choices = categorical_vars, selected = "lowedu_level")),
                                        column(3, actionButton("anova2_btn", "Jalankan Uji", class = "btn-info", style="margin-top: 25px;"))
                                      ),
                                      hr(),
                                      verbatimTextOutput("anova2_out"),
                                      uiOutput("anova2_interp")
                                  )
                           )
                         )
                )
              )
      ),
      # --- Halaman 8: Pusat Unduhan ---
      tabItem(tabName = "pusat_unduhan",
              h2("Pusat Unduhan Laporan"),
              p("Kumpulkan hasil analisis dari berbagai halaman di sini, lalu unduh sebagai satu laporan PDF gabungan."),
              fluidRow(
                box(
                  title = "Pengaturan Laporan", status = "primary", solidHeader = TRUE, width = 12,
                  textInput("report_title", "Judul Laporan:", "Laporan Analisis Kerentanan Sosial"),
                  textInput("report_author", "Nama Penyusun:", "Triangga Hafid Rifai"),
                  hr(),
                  downloadButton("download_combined_pdf", "Unduh Laporan Gabungan (PDF)", class = "btn-danger"),
                  actionButton("clear_saved_reports", "Bersihkan Daftar", icon = icon("trash"), class = "btn-warning")
                )
              ),
              fluidRow(
                box(
                  title = "Hasil Analisis yang Disimpan", status = "info", solidHeader = TRUE, width = 12,
                  uiOutput("saved_items_ui")
                )
              )
      )
    )
  )
)

# ==== SERVER ====
server <- function(input, output, session) {
  
  saved_reports <- reactiveVal(list())
  add_to_report <- function(item) {
    current_reports <- saved_reports()
    item$id <- paste0(item$type, "_", round(as.numeric(Sys.time())*1000))
    new_reports <- append(current_reports, list(item))
    saved_reports(new_reports)
    showNotification(paste("Hasil untuk '", item$title, "' berhasil disimpan!"), type = "message", duration = 3)
  }
  
  # Helper to sanitize text for LaTeX to prevent rendering errors in PDF
  sanitize_latex <- function(text) {
    if (is.null(text) || !is.character(text)) return(text)
    text <- gsub("\\", "\\textbackslash{}", text, fixed = TRUE)
    text <- gsub("_", "\\_", text, fixed = TRUE)
    text <- gsub("^", "\\textasciicircum{}", text, fixed = TRUE)
    text <- gsub("~", "\\textasciitilde{}", text, fixed = TRUE)
    text <- gsub("&", "\\&", text, fixed = TRUE)
    text <- gsub("%", "\\%", text, fixed = TRUE)
    text <- gsub("$", "\\$", text, fixed = TRUE)
    text <- gsub("#", "\\#", text, fixed = TRUE)
    text <- gsub("{", "\\{", text, fixed = TRUE)
    text <- gsub("}", "\\}", text, fixed = TRUE)
    return(text)
  }
  
  html_to_text <- function(html_code) {
    if (is.null(html_code)) return("")
    text <- gsub("<br/?>", "\n", as.character(html_code))
    text <- gsub("</p>", "\n\n", text)
    text <- gsub("<li>", "\n* ", text)
    text <- gsub("<ul>", "\n", text)
    text <- gsub("<.*?>", "", text)
    text <- paste(text, collapse = "\n")
    return(text)
  }
  
  # --- SERVER LOGIC UNTUK BERANDA ---
  
  output$info_kabupaten <- renderInfoBox({
    infoBox("Kabupaten/Kota", n_distinct(map_data_full_raw$kodeprkab), icon = icon("map-marked-alt"), color = "aqua")
  })
  
  output$info_variabel <- renderInfoBox({
    infoBox("Variabel Analisis", ncol(data_svi_raw) - 1, icon = icon("list"), color = "purple")
  })
  
  output$tabel_variabel <- renderDT({
    deskripsi_vars <- tibble::tribble(
      ~Variabel, ~Deskripsi,
      "poverty", "Persentase penduduk miskin",
      "lowedu", "Persentase penduduk usia 15+ dengan pendidikan rendah (maksimal SMP)",
      "noelectric", "Persentase rumah tangga tanpa akses listrik",
      "nosewer", "Persentase rumah tangga tanpa fasilitas sanitasi yang layak",
      "rented", "Persentase rumah tangga yang menyewa/kontrak rumah",
      "fhead", "Persentase rumah tangga dengan kepala rumah tangga perempuan",
      "illiterate", "Persentase penduduk buta huruf",
      "growth", "Laju pertumbuhan penduduk per tahun",
      "children", "Persentase penduduk anak-anak (usia 0-14 tahun)",
      "elderly", "Persentase penduduk lansia (usia 65+ tahun)",
      "female", "Persentase penduduk perempuan",
      "tapwater", "Persentase rumah tangga dengan akses air ledeng",
      "notraining", "Persentase tenaga kerja yang tidak terlatih"
    )
    datatable(deskripsi_vars, options = list(pageLength = 5, scrollY = "300px", searching = TRUE), rownames = FALSE, colnames = c("Nama Variabel", "Deskripsi"))
  })
  
  data_terfilter <- reactive({
    df <- map_data_full_raw
    if (!is.null(input$provinsi_filter) && input$provinsi_filter != "Semua") {
      df <- df %>% filter(nmprov == input$provinsi_filter)
    }
    df
  })
  
  output$total_kabupaten <- renderValueBox({
    total <- n_distinct(data_terfilter()$kodeprkab)
    subtitle_text <- if (is.null(input$provinsi_filter) || input$provinsi_filter == "Semua") "Total Kab/Kota" else paste("Kab/Kota di", input$provinsi_filter)
    valueBox(value = total, subtitle = subtitle_text, icon = icon("map-marked-alt"), color = "blue")
  })
  
  output$rata_kemiskinan <- renderValueBox({
    rata <- if(nrow(data_terfilter()) == 0) 0 else round(mean(data_terfilter()$poverty, na.rm = TRUE), 2)
    valueBox(value = paste0(rata, "%"), subtitle = "Rata-rata Tingkat Kemiskinan", icon = icon("users"), color = "green")
  })
  
  output$kemiskinan_tertinggi <- renderValueBox({
    df <- data_terfilter()
    if(nrow(df) > 0 && "poverty" %in% names(df)) {
      top_data <- df %>% arrange(desc(poverty)) %>% na.omit() %>% slice(1)
      value <- paste0(round(top_data$poverty, 2), "%")
      subtitle <- top_data$nmkab
    } else {
      value <- "N/A"
      subtitle <- "Tidak ada data"
    }
    valueBox(value = value, subtitle = paste("Kemiskinan Tertinggi:", subtitle), icon = icon("arrow-up"), color = "red")
  })
  
  peta_svi_obj <- reactive({
    df <- data_terfilter()
    if(nrow(df) == 0 || !"poverty" %in% names(df)) return(NULL)
    
    pal <- colorNumeric(palette = "YlOrRd", domain = df$poverty)
    popup_content <- paste0("<strong>", df$nmkab, "</strong><br/>", "Provinsi: ", df$nmprov, "<br/>", "Tingkat Kemiskinan: ", round(df$poverty, 2), "%")
    
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(poverty), weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
                  highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
                  label = ~nmkab, popup = popup_content,
                  layerId = ~kodeprkab) %>%
      addLegend(pal = pal, values = ~poverty, opacity = 0.7, title = "Kemiskinan (%)", position = "bottomright")
  })
  
  output$peta_svi <- renderLeaflet({
    peta_svi_obj()
  })
  
  observe({
    proxy <- leafletProxy("peta_svi", data = map_data_full_raw)
    if (!is.null(input$provinsi_filter) && input$provinsi_filter != "Semua") {
      df <- data_terfilter()
      if (nrow(df) > 0) {
        selected_geom <- df %>% st_union()
        bounds <- st_bbox(selected_geom)
        proxy %>% fitBounds(bounds$xmin, bounds$ymin, bounds$xmax, bounds$ymax)
      }
    } else {
      proxy %>% setView(lng = 118, lat = -2, zoom = 5)
    }
  })
  
  observeEvent(input$peta_svi_shape_click, {
    click <- input$peta_svi_shape_click
    if (!is.null(click$id)) {
      selected_prov <- map_data_full_raw %>%
        filter(kodeprkab == click$id) %>%
        pull(nmprov)
      
      if (length(selected_prov) > 0) {
        updateSelectInput(session, "provinsi_filter", selected = selected_prov[1])
      }
    }
  })
  
  observeEvent(input$tabel_data_rows_selected, {
    selected_row <- input$tabel_data_rows_selected
    if (length(selected_row)) {
      selected_province <- data_terfilter()$nmprov[selected_row]
      if (input$provinsi_filter != selected_province) {
        updateSelectInput(session, "provinsi_filter", selected = selected_province)
      }
    }
  })
  
  grafik_komponen_obj <- reactive({
    req(nrow(data_terfilter()) > 0)
    vars_to_summarise <- intersect(c('poverty', 'lowedu', 'noelectric', 'nosewer', 'rented'), names(data_terfilter()))
    req(length(vars_to_summarise) > 0)
    
    df_plot <- data_terfilter() %>% st_drop_geometry() %>% summarise(across(all_of(vars_to_summarise), ~mean(.x, na.rm = TRUE))) %>% pivot_longer(cols = everything(), names_to = "Komponen", values_to = "Nilai")
    
    ggplot(df_plot, aes(x = Komponen, y = Nilai, fill = Komponen)) +
      geom_col() +
      labs(
        title = paste("Profil Komponen di", ifelse(input$provinsi_filter != "Semua", input$provinsi_filter, "Seluruh Indonesia")),
        x = NULL,
        y = "Rata-rata Persentase (%)"
      ) +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle=45, hjust=1))
  })
  
  output$grafik_komponen <- renderPlotly({
    ggplotly(grafik_komponen_obj())
  })
  
  tabel_data_obj <- reactive({
    vars_to_select <- intersect(c("nmprov", "nmkab", "poverty", "lowedu", "noelectric", "elderly", "growth"), names(data_terfilter()))
    data_terfilter() %>% st_drop_geometry() %>% select(all_of(vars_to_select))
  })
  
  output$tabel_data <- renderDT({
    tabel_data <- tabel_data_obj()
    datatable(tabel_data, options = list(pageLength = 5, scrollX = TRUE, responsive = TRUE), rownames = FALSE, selection = 'single') %>% formatRound(columns = which(sapply(tabel_data, is.numeric)), digits = 2)
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste("data-svi-", tolower(gsub(" ", "-", input$provinsi_filter)), ".csv", sep = "") },
    content = function(file) { write.csv(data_terfilter() %>% st_drop_geometry(), file, row.names = FALSE) }
  )
  
  observeEvent(input$save_beranda, {
    add_to_report(list(
      title = paste("Peta Sebaran Kemiskinan -", ifelse(input$provinsi_filter == "Semua", "Seluruh Indonesia", input$provinsi_filter)),
      type = "leaflet",
      content = peta_svi_obj()
    ))
    add_to_report(list(
      title = paste("Grafik Komponen Kerentanan -", ifelse(input$provinsi_filter == "Semua", "Seluruh Indonesia", input$provinsi_filter)),
      type = "ggplot",
      content = grafik_komponen_obj()
    ))
    add_to_report(list(
      title = paste("Tabel Data -", ifelse(input$provinsi_filter == "Semua", "Seluruh Indonesia", input$provinsi_filter)),
      type = "table",
      content = tabel_data_obj()
    ))
  })
  
  # --- SERVER LOGIC UNTUK EKSPLORASI DATA ---
  
  explore_data_filtered <- reactive({
    df <- map_data_full_raw
    if (input$explore_provinsi != "Semua") {
      df <- df %>% filter(nmprov == input$explore_provinsi)
    }
    df
  })
  
  create_thematic_plot <- function(data, vars, title, color) {
    req(nrow(data) > 0)
    vars_exist <- intersect(vars, names(data))
    req(length(vars_exist) > 0)
    
    df_plot <- data %>%
      st_drop_geometry() %>%
      summarise(across(all_of(vars_exist), ~mean(.x, na.rm = TRUE))) %>%
      pivot_longer(cols = everything(), names_to = "Komponen", values_to = "Nilai") %>%
      mutate(Komponen = str_to_title(Komponen))
    
    ggplot(df_plot, aes(x = Komponen, y = Nilai)) +
      geom_col(fill = color) +
      labs(title = title, x = NULL, y = "Rata-rata (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=45, hjust=1))
  }
  
  plot_demografi_obj <- reactive({
    vars <- c("children", "elderly", "female", "fhead", "growth")
    title <- paste("Profil Demografi di", ifelse(input$explore_provinsi != "Semua", input$explore_provinsi, "Seluruh Indonesia"))
    create_thematic_plot(explore_data_filtered(), vars, title, 'rgba(0, 123, 255, 0.8)')
  })
  output$plot_demografi <- renderPlotly({ ggplotly(plot_demografi_obj()) })
  
  plot_perumahan_obj <- reactive({
    vars <- c("noelectric", "nosewer", "tapwater", "rented")
    title <- paste("Profil Perumahan & Infrastruktur di", ifelse(input$explore_provinsi != "Semua", input$explore_provinsi, "Seluruh Indonesia"))
    create_thematic_plot(explore_data_filtered(), vars, title, 'rgba(40, 167, 69, 0.8)')
  })
  output$plot_perumahan <- renderPlotly({ ggplotly(plot_perumahan_obj()) })
  
  plot_pendidikan_obj <- reactive({
    vars <- c("lowedu", "illiterate", "notraining")
    title <- paste("Profil Pendidikan & Ketenagakerjaan di", ifelse(input$explore_provinsi != "Semua", input$explore_provinsi, "Seluruh Indonesia"))
    create_thematic_plot(explore_data_filtered(), vars, title, 'rgba(255, 193, 7, 0.8)')
  })
  output$plot_pendidikan <- renderPlotly({ ggplotly(plot_pendidikan_obj()) })
  
  create_thematic_interp <- function(data, vars, theme_name) {
    vars_exist <- intersect(vars, names(data))
    if(nrow(data) == 0 || length(vars_exist) == 0) return(p("Data tidak cukup untuk interpretasi."))
    
    df_summary <- data %>%
      st_drop_geometry() %>%
      summarise(across(all_of(vars_exist), ~mean(.x, na.rm = TRUE))) %>%
      pivot_longer(cols = everything(), names_to = "Komponen", values_to = "Nilai") %>%
      mutate(Komponen = str_to_title(Komponen))
    
    if (nrow(df_summary) == 0) return(p("Data tidak cukup untuk interpretasi."))
    
    highest <- df_summary %>% filter(Nilai == max(Nilai, na.rm=TRUE))
    lowest <- df_summary %>% filter(Nilai == min(Nilai, na.rm=TRUE))
    
    p(
      "Dalam tema ", strong(theme_name), ", komponen '", strong(highest$Komponen[1]),
      "' memiliki rata-rata tertinggi (", round(highest$Nilai[1], 2), "%). ",
      "Sementara itu, komponen '", strong(lowest$Komponen[1]),
      "' memiliki rata-rata terendah (", round(lowest$Nilai[1], 2), "%)."
    )
  }
  
  output$plot_demografi_interp <- renderUI({
    tags$div(class = "interpretation-box", create_thematic_interp(explore_data_filtered(), c("children", "elderly", "female", "fhead", "growth"), "Demografi"))
  })
  
  output$plot_perumahan_interp <- renderUI({
    tags$div(class = "interpretation-box", create_thematic_interp(explore_data_filtered(), c("noelectric", "nosewer", "tapwater", "rented"), "Perumahan & Infrastruktur"))
  })
  
  output$plot_pendidikan_interp <- renderUI({
    tags$div(class = "interpretation-box", create_thematic_interp(explore_data_filtered(), c("lowedu", "illiterate", "notraining"), "Pendidikan & Ketenagakerjaan"))
  })
  
  explore_stats_table_obj <- reactive({
    req(input$explore_var_x, input$explore_var_y)
    df <- explore_data_filtered()
    var_x <- tolower(input$explore_var_x)
    var_y <- tolower(input$explore_var_y)
    
    df %>%
      st_drop_geometry() %>%
      summarise(
        across(c(all_of(var_x), all_of(var_y)),
               list(
                 Rata_Rata = ~mean(.x, na.rm = TRUE),
                 Median = ~median(.x, na.rm = TRUE),
                 Standar_Deviasi = ~sd(.x, na.rm = TRUE),
                 Minimum = ~min(.x, na.rm = TRUE),
                 Maksimum = ~max(.x, na.rm = TRUE)
               ),
               .names = "{.col}_{.fn}")
      ) %>%
      pivot_longer(everything(), names_to = c("Variabel", ".value"), names_sep = "_") %>%
      mutate(Variabel = str_to_title(Variabel))
  })
  
  output$explore_stats_interpretasi <- renderUI({
    tags$div(class = "interpretation-box", p("Tabel di atas menyajikan ringkasan statistik untuk variabel yang dipilih, termasuk ukuran pemusatan (rata-rata, median) dan ukuran penyebaran (standar deviasi, min, maks)."))
  })
  
  output$explore_stats_table <- renderTable({ explore_stats_table_obj() }, align = "c", spacing = "m")
  
  explore_boxplot_obj <- reactive({
    req(input$explore_var_x)
    df <- explore_data_filtered()
    var_x <- tolower(input$explore_var_x)
    
    ggplot(df, aes(y = .data[[var_x]])) +
      geom_boxplot(fill = "lightblue") +
      labs(title = paste("Boxplot Sebaran", str_to_title(var_x)), y = "Nilai", x = "") +
      theme_minimal()
  })
  
  output$explore_boxplot <- renderPlotly({ ggplotly(explore_boxplot_obj()) })
  
  explore_scatterplot_obj <- reactive({
    req(input$explore_var_x, input$explore_var_y)
    df <- explore_data_filtered()
    var_x <- tolower(input$explore_var_x)
    var_y <- tolower(input$explore_var_y)
    
    ggplot(df, aes(x = .data[[var_x]], y = .data[[var_y]], text = nmkab)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        title = paste("Hubungan antara", str_to_title(var_x), "dan", str_to_title(var_y)),
        x = str_to_title(var_x),
        y = str_to_title(var_y)
      ) +
      theme_minimal()
  })
  
  output$explore_scatterplot <- renderPlotly({ ggplotly(explore_scatterplot_obj(), tooltip = "text") })
  
  observeEvent(input$save_eksplorasi, {
    add_to_report(list(
      title = paste("Eksplorasi - Statistik Deskriptif untuk", input$explore_var_x, "dan", input$explore_var_y),
      type = "table",
      content = explore_stats_table_obj()
    ))
    add_to_report(list(
      title = paste("Eksplorasi - Grafik Sebaran", input$explore_var_x),
      type = "ggplot",
      content = explore_boxplot_obj()
    ))
    add_to_report(list(
      title = paste("Eksplorasi - Grafik Hubungan", input$explore_var_x, "vs", input$explore_var_y),
      type = "ggplot",
      content = explore_scatterplot_obj()
    ))
  })
  
  # --- SERVER LOGIC UNTUK MANAJEMEN DATA ---
  
  hasil_kategorisasi <- reactiveVal(NULL)
  
  output$summary_stats_ui <- renderUI({
    var_dipilih <- tolower(input$var_kategorisasi)
    data_vektor <- map_data_full[[var_dipilih]]
    
    tagList(
      p(tags$strong("Nilai Minimum: "), round(min(data_vektor, na.rm = TRUE), 2)),
      p(tags$strong("Rata-rata: "), round(mean(data_vektor, na.rm = TRUE), 2)),
      p(tags$strong("Nilai Maksimum: "), round(max(data_vektor, na.rm = TRUE), 2))
    )
  })
  
  observeEvent(input$proses_btn, {
    
    var_dipilih <- tolower(input$var_kategorisasi)
    metode_dipilih <- input$metode_kategorisasi
    n_kategori <- input$jumlah_kategori
    data_vektor <- map_data_full[[var_dipilih]]
    
    labels <- switch(as.character(n_kategori),
                     "2" = c("Rendah", "Tinggi"),
                     "3" = c("Rendah", "Sedang", "Tinggi"),
                     "4" = c("Sangat Rendah", "Rendah", "Tinggi", "Sangat Tinggi"),
                     "5" = c("Sangat Rendah", "Rendah", "Sedang", "Tinggi", "Sangat Tinggi"),
                     paste("Kategori", 1:n_kategori))
    
    if (metode_dipilih == "Equal Interval") {
      breaks <- seq(min(data_vektor, na.rm = TRUE), max(data_vektor, na.rm = TRUE), length.out = n_kategori + 1)
    } else if (metode_dipilih == "Kuantil (Equal Count)") {
      breaks <- quantile(data_vektor, probs = seq(0, 1, length.out = n_kategori + 1), na.rm = TRUE)
    } else { # Jenks Natural Breaks
      breaks <- getJenksBreaks(data_vektor, n_kategori + 1)
    }
    
    breaks <- unique(breaks)
    
    if (length(breaks) < 2) {
      hasil_kategorisasi(list(error = TRUE))
      return()
    }
    
    actual_n_categories <- length(breaks) - 1
    if (length(labels) > actual_n_categories) {
      labels <- labels[1:actual_n_categories]
    }
    
    data_kategorik <- cut(data_vektor, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)
    
    hasil_kategorisasi(list(
      error = FALSE,
      data_lengkap = map_data_full %>% mutate(kategori = data_kategorik),
      tabel_frekuensi = as.data.frame(table(data_kategorik)) %>% rename(Kategori = data_kategorik, Jumlah_Kab_Kota = Freq),
      interpretasi_data = list(
        var = str_to_title(var_dipilih),
        metode = metode_dipilih,
        n_kat = n_kategori,
        breaks = breaks
      )
    ))
  })
  
  output$hasil_analisis_tabs <- renderUI({
    req(hasil_kategorisasi())
    if(hasil_kategorisasi()$error) {
      return(box(title = "Error", status = "danger", solidHeader = TRUE, width = NULL,
                 p("Variabel yang dipilih tidak memiliki variasi yang cukup untuk dikategorikan.")))
    }
    
    tabBox(
      width = 12, id = "output_tabs",
      tabPanel("Grafik Distribusi", icon = icon("chart-bar"), plotlyOutput("output_grafik_distribusi")),
      tabPanel("Data Hasil Kategorisasi", icon = icon("table"),
               DTOutput("tabel_kategori"),
               br(),
               downloadButton("download_kategori", "📥 Unduh Data Hasil Kategorisasi (.csv)", class = "btn-info")
      )
    )
  })
  
  output$summary_results_ui <- renderUI({
    req(hasil_kategorisasi())
    if(hasil_kategorisasi()$error) return()
    
    tagList(
      column(width = 6,
             box(title = "Tabel Frekuensi", status = "success", solidHeader = TRUE, width = NULL,
                 tableOutput("output_tabel_frekuensi"))
      ),
      column(width = 6,
             box(title = "Interpretasi Hasil", status = "success", solidHeader = TRUE, width = NULL,
                 uiOutput("output_interpretasi"))
      )
    )
  })
  
  output$output_tabel_frekuensi <- renderTable({
    req(hasil_kategorisasi())
    hasil_kategorisasi()$tabel_frekuensi
  })
  
  manajemen_dist_plot_obj <- reactive({
    req(hasil_kategorisasi())
    df <- hasil_kategorisasi()$tabel_frekuensi
    ggplot(df, aes(x = Kategori, y = Jumlah_Kab_Kota, fill = Kategori)) +
      geom_col() +
      labs(title = paste("Distribusi Variabel", hasil_kategorisasi()$interpretasi_data$var),
           x = "Kategori", y = "Jumlah Kabupaten/Kota") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  output$output_grafik_distribusi <- renderPlotly({ ggplotly(manajemen_dist_plot_obj()) })
  
  manajemen_interp_obj <- reactive({
    req(hasil_kategorisasi())
    data <- hasil_kategorisasi()
    
    rentang_teks <- sapply(1:(length(data$interpretasi_data$breaks)-1), function(i) {
      if (i <= length(levels(data$data_lengkap$kategori))) {
        paste0("<li>Kategori '", levels(data$data_lengkap$kategori)[i], "' mencakup nilai dari ",
               round(data$interpretasi_data$breaks[i], 2), " hingga ", round(data$interpretasi_data$breaks[i+1], 2), ".</li>")
      }
    })
    
    paste0(
      "Variabel '", data$interpretasi_data$var, "' telah dikategorikan menggunakan metode '", data$interpretasi_data$metode, "'.\n\n",
      "Rentang Nilai untuk Setiap Kategori:\n",
      paste(rentang_teks, collapse = "\n")
    )
  })
  output$output_interpretasi <- renderUI({
    tags$div(class="interpretation-box", HTML(manajemen_interp_obj()))
  })
  
  output$tabel_kategori <- renderDT({
    req(hasil_kategorisasi())
    df <- hasil_kategorisasi()$data_lengkap %>% st_drop_geometry()
    datatable(df, options = list(pageLength = 5, scrollX = TRUE, responsive = TRUE), rownames = FALSE)
  })
  
  output$download_kategori <- downloadHandler(
    filename = function() { paste0("data_kategorisasi_", tolower(input$var_kategorisasi), ".csv") },
    content = function(file) {
      write.csv(hasil_kategorisasi()$data_lengkap %>% st_drop_geometry(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$save_manajemen, {
    req(hasil_kategorisasi())
    add_to_report(list(
      title = paste("Manajemen Data - Distribusi Kategori untuk", input$var_kategorisasi),
      type = "ggplot",
      content = manajemen_dist_plot_obj()
    ))
    add_to_report(list(
      title = paste("Manajemen Data - Tabel Frekuensi untuk", input$var_kategorisasi),
      type = "table",
      content = hasil_kategorisasi()$tabel_frekuensi
    ))
    add_to_report(list(
      title = paste("Manajemen Data - Interpretasi untuk", input$var_kategorisasi),
      type = "text",
      content = sanitize_latex(manajemen_interp_obj())
    ))
  })
  
  # --- SERVER LOGIC UNTUK ANALISIS KLASTER (PETA DIHAPUS) ---
  
  elbow_data <- reactive({
    req(input$cluster_vars, length(input$cluster_vars) > 1)
    
    data_for_cluster <- map_data_full %>%
      st_drop_geometry() %>%
      select(all_of(tolower(input$cluster_vars))) %>%
      na.omit()
    
    scaled_data <- scale(data_for_cluster)
    
    wss <- sapply(1:10, function(k){
      kmeans(scaled_data, k, nstart=25, iter.max=15)$tot.withinss
    })
    
    tibble(k = 1:10, wss = wss)
  })
  
  output$elbow_plot <- renderPlot({
    df <- elbow_data()
    ggplot(df, aes(x = k, y = wss)) +
      geom_line(color = "blue") +
      geom_point(color = "red", size = 3) +
      labs(title = "Elbow Method untuk Penentuan K",
           x = "Jumlah Klaster (K)",
           y = "Total Within-Cluster Sum of Squares (WSS)") +
      theme_minimal()
  })
  
  cluster_results <- eventReactive(input$cluster_btn, {
    req(input$cluster_vars, length(input$cluster_vars) > 1, input$cluster_k)
    
    data_for_cluster <- map_data_full %>%
      st_drop_geometry() %>%
      select(kodeprkab, nmkab, nmprov, all_of(tolower(input$cluster_vars))) %>%
      na.omit()
    
    scaled_data <- scale(select(data_for_cluster, -kodeprkab, -nmkab, -nmprov))
    
    set.seed(123)
    kmeans_result <- kmeans(scaled_data, centers = input$cluster_k, nstart = 25)
    
    data_with_clusters <- data_for_cluster %>%
      mutate(Klaster = as.factor(kmeans_result$cluster))
    
    cluster_profiles <- data_with_clusters %>%
      group_by(Klaster) %>%
      summarise(across(all_of(tolower(input$cluster_vars)), ~mean(.x, na.rm = TRUE))) %>%
      pivot_longer(-Klaster, names_to = "Variabel", values_to = "Rata_Rata")
    
    list(
      table_data = data_with_clusters,
      profiles = cluster_profiles
    )
  })
  
  output$cluster_results_ui <- renderUI({
    req(cluster_results())
    box(title = "Hasil Analisis Klaster", status = "success", solidHeader = TRUE, width = NULL,
        tabBox(
          width = 12,
          # Peta dihapus dari sini
          tabPanel("Profil Klaster", icon = icon("chart-bar"), plotlyOutput("cluster_profile_plot", height = "600px")),
          tabPanel("Tabel Data Klaster", icon = icon("table"), DTOutput("cluster_table"))
        )
    )
  })
  
  cluster_profile_plot_obj <- reactive({
    req(cluster_results())
    res <- cluster_results()
    
    ggplot(res$profiles, aes(x = Variabel, y = Rata_Rata, fill = Klaster)) +
      geom_col(position = "dodge") +
      labs(
        title = "Profil Rata-Rata Variabel per Klaster",
        x = NULL,
        y = "Rata-Rata (Nilai Asli)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$cluster_profile_plot <- renderPlotly({ ggplotly(cluster_profile_plot_obj()) })
  
  output$cluster_table <- renderDT({
    req(cluster_results())
    res <- cluster_results()
    datatable(res$table_data, options = list(pageLength = 10, scrollX = TRUE, responsive = TRUE), rownames = FALSE)
  })
  
  cluster_interpretation_text_obj <- reactive({
    req(cluster_results())
    res <- cluster_results()
    profiles <- res$profiles
    
    scaled_profiles <- profiles %>%
      group_by(Variabel) %>%
      mutate(scaled_mean = scale(Rata_Rata)) %>%
      ungroup()
    
    interp_list <- lapply(sort(unique(profiles$Klaster)), function(k) {
      
      cluster_data <- scaled_profiles %>% filter(Klaster == k)
      
      strongest_char <- cluster_data %>% filter(scaled_mean == max(scaled_mean))
      weakest_char <- cluster_data %>% filter(scaled_mean == min(scaled_mean))
      
      n_members <- nrow(res$table_data %>% filter(Klaster == k))
      
      paste0(
        "* **Klaster ", k, "** (", n_members, " wilayah): Klaster ini memiliki karakteristik kerentanan paling menonjol pada **",
        str_to_title(strongest_char$Variabel[1]),
        "**. Sebaliknya, klaster ini relatif lebih baik dalam hal **",
        str_to_title(weakest_char$Variabel[1]),
        "**."
      )
    })
    
    paste(
      "Analisis ini mengelompokkan wilayah berdasarkan kesamaan profil kerentanan mereka. Berikut adalah ringkasan karakteristik utama dari setiap klaster:\n",
      paste(interp_list, collapse = "\n")
    )
  })
  
  output$cluster_interpretation_ui <- renderUI({
    req(cluster_results())
    box(title = "Interpretasi Klaster", status = "info", solidHeader = TRUE, width = NULL,
        uiOutput("cluster_interpretation_text")
    )
  })
  output$cluster_interpretation_text <- renderUI({
    tags$div(class="interpretation-box", HTML(markdownToHTML(text = cluster_interpretation_text_obj(), fragment.only=TRUE)))
  })
  
  observeEvent(input$save_klaster, {
    req(cluster_results())
    # Peta dihapus dari laporan
    add_to_report(list(
      title = paste("Analisis Klaster - Profil", input$cluster_k, "Klaster"),
      type = "ggplot",
      content = cluster_profile_plot_obj()
    ))
    add_to_report(list(
      title = paste("Analisis Klaster - Interpretasi", input$cluster_k, "Klaster"),
      type = "text",
      content = sanitize_latex(cluster_interpretation_text_obj())
    ))
    add_to_report(list(
      title = paste("Analisis Klaster - Tabel Data", input$cluster_k, "Klaster"),
      type = "table",
      content = cluster_results()$table_data
    ))
  })
  
  # --- SERVER LOGIC UNTUK PERINGKAT & PERBANDINGAN ---
  
  output$compare_results_ui <- renderUI({
    req(input$compare_kab, length(input$compare_kab) >= 2)
    box(title = "Hasil Perbandingan", status = "success", solidHeader = TRUE, width = 12,
        tabBox(
          width = 12,
          tabPanel("Grafik Radar", icon = icon("dot-circle"), plotlyOutput("compare_radar", height = "500px")),
          tabPanel("Tabel Data", icon = icon("table"), DTOutput("compare_table"))
        )
    )
  })
  
  output$compare_interpretation_ui <- renderUI({
    req(input$compare_kab, length(input$compare_kab) >= 2)
    box(title = "Interpretasi Perbandingan", status = "info", solidHeader = TRUE, width = NULL,
        uiOutput("compare_interpretation_text")
    )
  })
  
  compare_data <- reactive({
    req(input$compare_kab)
    map_data_full %>%
      st_drop_geometry() %>%
      filter(nmkab %in% input$compare_kab)
  })
  
  compare_table_obj <- reactive({
    req(compare_data())
    compare_data() %>%
      select(nmkab, all_of(continuous_vars)) %>%
      pivot_longer(-nmkab, names_to = "Variabel", values_to = "Nilai") %>%
      pivot_wider(names_from = nmkab, values_from = Nilai) %>%
      mutate(Variabel = str_to_title(Variabel))
  })
  output$compare_table <- renderDT({
    datatable(compare_table_obj(), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  radar_data <- reactive({
    req(input$compare_kab)
    radar_vars <- c("poverty", "lowedu", "noelectric", "nosewer", "rented", "fhead", "illiterate")
    radar_vars <- intersect(radar_vars, names(map_data_full))
    
    df_norm <- map_data_full %>%
      st_drop_geometry() %>%
      select(nmkab, all_of(radar_vars)) %>%
      mutate(across(-nmkab, ~ scales::rescale(.x, to = c(0, 100)))) # Skala 0-100
    
    df_plot <- df_norm %>%
      filter(nmkab %in% input$compare_kab)
    
    return(list(data = df_plot, vars = radar_vars))
  })
  
  compare_radar_obj <- reactive({
    req(radar_data())
    radar_info <- radar_data()
    df_plot <- radar_info$data
    radar_vars <- radar_info$vars
    
    p <- plot_ly(type = 'scatterpolar', fill = 'toself')
    
    for (i in 1:nrow(df_plot)) {
      p <- p %>% add_trace(
        r = as.numeric(df_plot[i, radar_vars]),
        theta = str_to_title(radar_vars),
        name = df_plot$nmkab[i]
      )
    }
    
    p %>% layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,100)
        )
      ),
      title = "Perbandingan Profil Kerentanan (Nilai Ternormalisasi 0-100)"
    )
  })
  output$compare_radar <- renderPlotly({ compare_radar_obj() })
  
  compare_barchart_obj <- reactive({
    req(radar_data())
    df_plot <- radar_data()$data %>%
      pivot_longer(-nmkab, names_to = "variabel", values_to = "nilai")
    
    ggplot(df_plot, aes(x = variabel, y = nilai, fill = nmkab)) +
      geom_col(position = "dodge") +
      coord_flip() +
      labs(
        title = "Perbandingan Profil Kerentanan",
        x = NULL,
        y = "Nilai Ternormalisasi (0-100)",
        fill = "Wilayah"
      ) +
      theme_minimal()
  })
  
  compare_interpretation_text_obj <- reactive({
    req(radar_data())
    df_plot <- radar_data()$data
    
    scores <- df_plot %>%
      mutate(Total_Score = rowSums(select(., -nmkab))) %>%
      select(nmkab, Total_Score) %>%
      arrange(desc(Total_Score))
    
    most_vulnerable <- scores$nmkab[1]
    
    interp_text <- ""
    if (nrow(df_plot) == 2) {
      diffs <- abs(as.numeric(df_plot[1, -1]) - as.numeric(df_plot[2, -1]))
      max_diff_var <- names(df_plot[, -1])[which.max(diffs)]
      
      interp_text <- paste0(
        "Perbedaan kerentanan terbesar antara kedua wilayah ini terdapat pada variabel ",
        str_to_title(max_diff_var), "."
      )
    }
    
    paste(
      "Berdasarkan total skor dari variabel yang divisualisasikan, ",
      most_vulnerable,
      " menunjukkan profil kerentanan yang secara umum lebih tinggi dibandingkan wilayah lainnya.",
      interp_text,
      sep = "\n"
    )
  })
  output$compare_interpretation_text <- renderUI({
    tags$div(class="interpretation-box", HTML(markdownToHTML(text=compare_interpretation_text_obj(), fragment.only=TRUE)))
  })
  
  observeEvent(input$save_perbandingan, {
    req(input$compare_kab, length(input$compare_kab) >= 2)
    add_to_report(list(
      title = paste("Perbandingan - Grafik Batang"),
      type = "ggplot",
      content = compare_barchart_obj()
    ))
    add_to_report(list(
      title = paste("Perbandingan - Tabel Data"),
      type = "table",
      content = compare_table_obj()
    ))
    add_to_report(list(
      title = paste("Perbandingan - Interpretasi"),
      type = "text",
      content = sanitize_latex(compare_interpretation_text_obj())
    ))
  })
  
  # --- SERVER LOGIC UNTUK REGRESI LINEAR BERGANDA ---
  
  reg_results <- eventReactive(input$reg_proses_btn, {
    req(input$reg_var_y, length(input$reg_vars_x) > 0)
    
    y_var <- tolower(input$reg_var_y)
    x_vars <- tolower(input$reg_vars_x)
    
    formula_text <- paste(y_var, "~", paste(x_vars, collapse = " + "))
    model_formula <- as.formula(formula_text)
    
    model_data <- na.omit(map_data_full)
    model <- lm(model_formula, data = model_data)
    model_summary <- summary(model)
    
    list(
      model = model,
      summary = model_summary,
      formula = formula_text
    )
  })
  
  reg_tests <- reactive({
    req(reg_results())
    model <- reg_results()$model
    resids <- residuals(model)
    x_vars <- tolower(input$reg_vars_x)
    
    # Uji Asumsi
    norm_test <- lillie.test(resids)
    homo_test <- bptest(model)
    auto_test <- dwtest(model)
    vif_values <- if (length(x_vars) > 1) tryCatch(vif(model), error = function(e) NULL) else NULL
    
    list(
      normality = norm_test,
      homoscedasticity = homo_test,
      autocorrelation = auto_test,
      vif = vif_values
    )
  })
  
  output$reg_summary_boxes_ui <- renderUI({
    req(reg_results())
    res <- reg_results()
    
    box(title = "2. Ringkasan Kebaikan Model", status = "info", solidHeader = TRUE, width = NULL,
        infoBox(
          "R-Squared",
          value = round(res$summary$r.squared, 3),
          icon = icon("chart-pie"),
          color = "aqua",
          width = 12
        ),
        infoBox(
          "Adj. R-Squared",
          value = round(res$summary$adj.r.squared, 3),
          icon = icon("chart-area"),
          color = "green",
          width = 12
        ),
        infoBox(
          "P-value (F-statistic)",
          value = format.pval(pf(res$summary$fstatistic[1], res$summary$fstatistic[2], res$summary$fstatistic[3], lower.tail = FALSE), digits=3),
          icon = icon("check-circle"),
          color = "yellow",
          width = 12
        )
    )
  })
  
  output$reg_hasil_tabs_ui <- renderUI({
    req(reg_results())
    
    box(title = "3. Hasil Analisis", status = "success", solidHeader = TRUE, width = NULL,
        tabBox(
          width = 12,
          id = "reg_tabs",
          tabPanel("Ringkasan Model", icon = icon("list-alt"),
                   verbatimTextOutput("reg_model_summary_print")),
          tabPanel("Uji Asumsi Klasik", icon = icon("flask-vial"),
                   uiOutput("reg_asumsi_summary_ui")),
          tabPanel("Interpretasi Koefisien", icon = icon("comment-dots"),
                   uiOutput("reg_coef_interp_ui")),
          tabPanel("Plot Diagnostik", icon = icon("chart-bar"),
                   plotOutput("reg_diagnostic_plots", height = "600px"))
        )
    )
  })
  
  output$reg_model_summary_print <- renderPrint({
    req(reg_results())
    reg_results()$summary
  })
  
  output$reg_asumsi_summary_ui <- renderUI({
    req(reg_tests())
    res <- reg_tests()
    
    norm_pass <- res$normality$p.value > 0.05
    homo_pass <- res$homoscedasticity$p.value > 0.05
    auto_pass <- res$autocorrelation$p.value > 0.05 && res$autocorrelation$p.value < (4 - res$autocorrelation$statistic)
    
    vif_pass <- TRUE
    vif_text <- "Tidak diuji (hanya 1 prediktor)."
    if (!is.null(res$vif)) {
      vif_pass <- all(res$vif < 10)
      vif_text <- paste0("VIF maks = ", round(max(res$vif), 2))
    }
    
    tagList(
      h4("Ringkasan Hasil Uji Asumsi"),
      p(strong("Model Formula: "), code(reg_results()$formula)),
      hr(),
      div(class = paste("test-result-box", ifelse(norm_pass, "pass", "fail")),
          strong("Normalitas Residual (Lilliefors):"), ifelse(norm_pass, " Terpenuhi", " Tidak Terpenuhi"), " (p = ", round(res$normality$p.value, 3), ")"),
      div(class = paste("test-result-box", ifelse(homo_pass, "pass", "fail")),
          strong("Homoskedastisitas (Breusch-Pagan):"), ifelse(homo_pass, " Terpenuhi", " Tidak Terpenuhi"), " (p = ", round(res$homoscedasticity$p.value, 3), ")"),
      div(class = paste("test-result-box", ifelse(auto_pass, "pass", "fail")),
          strong("Non-Autokorelasi (Durbin-Watson):"), ifelse(auto_pass, " Terpenuhi", " Tidak Terpenuhi"), " (DW = ", round(res$autocorrelation$statistic, 2), ")"),
      div(class = paste("test-result-box", ifelse(vif_pass, "pass", "fail")),
          strong("Non-Multikolinearitas (VIF):"), ifelse(vif_pass, " Terpenuhi", " Tidak Terpenuhi"), " (", vif_text, ")"),
      hr(),
      h4("Visualisasi Diagnostik Asumsi"),
      fluidRow(
        column(width = 6,
               box(title = "Normalitas Residual (Q-Q Plot)", status = "primary", solidHeader = TRUE, width = NULL,
                   plotOutput("reg_norm_plot", height = "300px")
               )
        ),
        column(width = 6,
               box(title = "Homoskedastisitas (Residuals vs Fitted)", status = "primary", solidHeader = TRUE, width = NULL,
                   plotOutput("reg_homo_plot", height = "300px")
               )
        )
      )
    )
  })
  
  reg_norm_plot_obj <- reactive({
    req(reg_results())
    model <- reg_results()$model
    model_data <- augment(model)
    ggplot(model_data, aes(sample = .resid)) +
      stat_qq() + stat_qq_line(color = "blue") +
      labs(title = "Normal Q-Q Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal(base_size = 14)
  })
  output$reg_norm_plot <- renderPlot({ reg_norm_plot_obj() })
  
  reg_homo_plot_obj <- reactive({
    req(reg_results())
    model <- reg_results()$model
    model_data <- augment(model)
    ggplot(model_data, aes(x = .fitted, y = .resid)) +
      geom_point(alpha = 0.6, color = "black") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
      geom_smooth(method = "loess", se = FALSE, color = "blue", size = 1) +
      labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
      theme_minimal(base_size = 14)
  })
  output$reg_homo_plot <- renderPlot({ reg_homo_plot_obj() })
  
  reg_coef_interp_text_obj <- reactive({
    req(reg_results())
    res <- reg_results()
    coefs <- res$summary$coefficients
    y_var <- input$reg_var_y
    
    f_stat <- res$summary$fstatistic
    p_val_f <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
    
    p_val_f_text <- if (p_val_f < 0.001) "< 0.001" else as.character(round(p_val_f, 3))
    
    overall_interp <- paste0(
      "Secara keseluruhan, model regresi ini ",
      if(p_val_f < 0.05) "**signifikan secara statistik**" else "**tidak signifikan secara statistik**",
      " (p-value Uji F = ", p_val_f_text, "). ",
      "Model ini mampu menjelaskan sekitar **", round(res$summary$r.squared * 100, 2), "%** variasi pada variabel '", y_var,
      "' (R-Squared = ", round(res$summary$r.squared, 3), ")."
    )
    
    interps <- lapply(2:nrow(coefs), function(i) {
      var_name <- rownames(coefs)[i]
      est <- coefs[i, "Estimate"]
      p_val <- coefs[i, "Pr(>|t|)"]
      
      if (p_val < 0.05) {
        significance <- "signifikan"
        direction <- if(est > 0) "meningkatkan" else "menurunkan"
        paste0(
          "* **", str_to_title(var_name), "**: Variabel ini berpengaruh secara **", significance, "**. Setiap kenaikan satu satuan pada '", var_name, "' diprediksi akan **",
          direction, "** nilai '", y_var, "' sebesar **",
          abs(round(est, 4)), "**, dengan asumsi variabel lain konstan."
        )
      } else {
        NULL
      }
    })
    
    significant_interps <- compact(interps)
    
    if (length(significant_interps) > 0) {
      coef_interp <- paste(significant_interps, collapse="\n")
    } else {
      coef_interp <- "Tidak ada variabel independen yang berpengaruh signifikan secara statistik terhadap variabel dependen pada tingkat signifikansi 5%."
    }
    
    paste(
      "#### Evaluasi Model\n\n",
      overall_interp,
      "\n\n#### Interpretasi Variabel Signifikan\n\n",
      coef_interp
    )
  })
  output$reg_coef_interp_ui <- renderUI({
    req(reg_results())
    tags$div(class = "interpretation-box", HTML(markdownToHTML(text = reg_coef_interp_text_obj(), fragment.only=TRUE)))
  })
  
  reg_diagnostic_plots_obj <- reactive({
    req(reg_results())
    reg_results()$model
  })
  output$reg_diagnostic_plots <- renderPlot({
    par(mfrow = c(2, 2))
    plot(reg_diagnostic_plots_obj())
    par(mfrow = c(1, 1))
  })
  
  observeEvent(input$save_regresi, {
    req(reg_results())
    add_to_report(list(
      title = paste("Regresi - Ringkasan Model untuk", input$reg_var_y),
      type = "verbatim",
      content = sanitize_latex(capture.output(print(reg_results()$summary)) %>% paste(collapse = "\n"))
    ))
    add_to_report(list(
      title = paste("Regresi - Plot Diagnostik untuk", input$reg_var_y),
      type = "plot_base",
      content = reg_diagnostic_plots_obj()
    ))
    add_to_report(list(
      title = paste("Regresi - Interpretasi Koefisien untuk", input$reg_var_y),
      type = "text",
      content = sanitize_latex(reg_coef_interp_text_obj())
    ))
    add_to_report(list(
      title = paste("Regresi - Plot Normalitas untuk", input$reg_var_y),
      type = "ggplot",
      content = reg_norm_plot_obj()
    ))
    add_to_report(list(
      title = paste("Regresi - Plot Homoskedastisitas untuk", input$reg_var_y),
      type = "ggplot",
      content = reg_homo_plot_obj()
    ))
  })
  
  # --- SERVER LOGIC UNTUK STATISTIK INFERENSIA (REFACTORED) ---
  
  # Helper untuk menangkap output print
  capture_print <- function(x) {
    paste(capture.output(print(x)), collapse = "\n")
  }
  
  # Reactive Values to store test results and reports
  rv_inferensia <- reactiveValues(
    ttest1_raw = NULL, ttest1_report = NULL,
    ttest2_raw = NULL, ttest2_report = NULL,
    proptest1_raw = NULL, proptest1_report = NULL,
    proptest2_raw = NULL, proptest2_report = NULL,
    vartest1_raw = NULL, vartest1_report = NULL,
    vartest2_raw = NULL, vartest2_report = NULL,
    anova1_raw = NULL, anova1_report = NULL, anova1_posthoc_raw = NULL,
    anova2_raw = NULL, anova2_report = NULL
  )
  
  # Uji T 1 Sampel
  observeEvent(input$ttest1_btn, {
    req(input$ttest1_var)
    var <- tolower(input$ttest1_var)
    mu <- input$ttest1_mu
    data_for_test <- na.omit(map_data_full[[var]])
    res <- t.test(data_for_test, mu = mu)
    rv_inferensia$ttest1_raw <- res
    
    p_val <- res$p.value
    kesimpulan <- if (p_val < 0.05) "berbeda secara signifikan" else "tidak berbeda secara signifikan"
    kesimpulan_h0 <- if (p_val < 0.05) "Tolak H0" else "Gagal Tolak H0"
    
    rv_inferensia$ttest1_report <- paste0(
      "**Hipotesis:**\n",
      "* H0: Rata-rata populasi untuk '", sanitize_latex(input$ttest1_var), "' sama dengan ", mu, ".\n",
      "* H1: Rata-rata populasi untuk '", sanitize_latex(input$ttest1_var), "' tidak sama dengan ", mu, ".\n\n",
      "**Hasil Uji Statistik:**\n",
      "```\n",
      sanitize_latex(capture_print(res)), "\n",
      "```\n\n",
      "**Kesimpulan:**\n",
      "Dengan p-value **", round(p_val, 4), "**, yang ", if(p_val<0.05) "lebih kecil dari" else "lebih besar dari", " alpha (0.05), maka **", kesimpulan_h0, "**. ",
      "Terdapat cukup bukti statistik untuk menyatakan bahwa rata-rata sampel '", sanitize_latex(input$ttest1_var), "' (", round(res$estimate, 2), ") **", kesimpulan, "** dari nilai hipotesis (", mu, ")."
    )
  })
  output$ttest1_out <- renderPrint({ rv_inferensia$ttest1_raw })
  output$ttest1_interp <- renderUI({
    req(rv_inferensia$ttest1_report)
    tags$div(class="interpretation-box", HTML(markdownToHTML(text = rv_inferensia$ttest1_report, fragment.only=TRUE)))
  })
  
  # Uji T 2 Sampel
  observeEvent(input$ttest2_btn, {
    req(input$ttest2_var, input$ttest2_group)
    var_num <- tolower(input$ttest2_var)
    var_group <- input$ttest2_group
    formula <- as.formula(paste(var_num, "~", var_group))
    res <- t.test(formula, data = map_data_full)
    rv_inferensia$ttest2_raw <- res
    
    p_val <- res$p.value
    kesimpulan <- if (p_val < 0.05) "terdapat perbedaan rata-rata yang signifikan" else "tidak terdapat perbedaan rata-rata yang signifikan"
    kesimpulan_h0 <- if (p_val < 0.05) "Tolak H0" else "Gagal Tolak H0"
    
    rv_inferensia$ttest2_report <- paste0(
      "**Hipotesis:**\n",
      "H0: Tidak ada perbedaan rata-rata '", sanitize_latex(input$ttest2_var), "' antara kelompok '", sanitize_latex(input$ttest2_group), "'.\n",
      "H1: Terdapat perbedaan rata-rata '", sanitize_latex(input$ttest2_var), "' antara kelompok '", sanitize_latex(input$ttest2_group), "'.\n\n",
      "**Hasil Uji Statistik:**\n",
      "```\n",
      sanitize_latex(capture_print(res)), "\n",
      "```\n\n",
      "**Kesimpulan:**\n",
      "Dengan p-value **", round(p_val, 4), "**, yang ", if(p_val<0.05) "lebih kecil dari" else "lebih besar dari", " alpha (0.05), maka **", kesimpulan_h0, "**. ",
      "Terdapat cukup bukti statistik untuk menyatakan bahwa ", kesimpulan, " antara dua kelompok pada variabel '", sanitize_latex(input$ttest2_var), "'."
    )
  })
  output$ttest2_out <- renderPrint({ rv_inferensia$ttest2_raw })
  output$ttest2_interp <- renderUI({
    req(rv_inferensia$ttest2_report)
    tags$div(class="interpretation-box", HTML(markdownToHTML(text = rv_inferensia$ttest2_report, fragment.only=TRUE)))
  })
  
  # Uji Proporsi 1 Sampel
  output$proptest1_level_ui <- renderUI({
    req(input$proptest1_var)
    choices <- unique(na.omit(map_data_full[[input$proptest1_var]]))
    selectInput("proptest1_level", "Pilih Level 'Success':", choices = choices)
  })
  observeEvent(input$proptest1_btn, {
    req(input$proptest1_var, input$proptest1_level)
    var_data <- na.omit(map_data_full[[input$proptest1_var]])
    success_count <- sum(var_data == input$proptest1_level, na.rm = TRUE)
    total_count <- length(var_data)
    res <- prop.test(x = success_count, n = total_count, p = input$proptest1_p)
    rv_inferensia$proptest1_raw <- res
    
    p_val <- res$p.value
    kesimpulan <- if (p_val < 0.05) "berbeda secara signifikan" else "tidak berbeda secara signifikan"
    kesimpulan_h0 <- if (p_val < 0.05) "Tolak H0" else "Gagal Tolak H0"
    
    rv_inferensia$proptest1_report <- paste0(
      "**Hipotesis:**\n",
      "H0: Proporsi populasi untuk '", sanitize_latex(input$proptest1_level), "' sama dengan ", input$proptest1_p, ".\n",
      "H1: Proporsi populasi untuk '", sanitize_latex(input$proptest1_level), "' tidak sama dengan ", input$proptest1_p, ".\n\n",
      "**Hasil Uji Statistik:**\n",
      "```\n",
      sanitize_latex(capture_print(res)), "\n",
      "```\n\n",
      "**Kesimpulan:**\n",
      "Dengan p-value **", round(p_val, 4), "**, yang ", if(p_val<0.05) "lebih kecil dari" else "lebih besar dari", " alpha (0.05), maka **", kesimpulan_h0, "**. ",
      "Terdapat cukup bukti statistik untuk menyatakan bahwa proporsi sampel (", round(res$estimate, 3), ") ", kesimpulan, " dari nilai hipotesis (", input$proptest1_p, ")."
    )
  })
  output$proptest1_out <- renderPrint({ rv_inferensia$proptest1_raw })
  output$proptest1_interp <- renderUI({
    req(rv_inferensia$proptest1_report)
    tags$div(class="interpretation-box", HTML(markdownToHTML(text = rv_inferensia$proptest1_report, fragment.only=TRUE)))
  })
  
  # Uji Proporsi 2 Sampel
  output$proptest2_level_ui <- renderUI({
    req(input$proptest2_var)
    choices <- unique(na.omit(map_data_full[[input$proptest2_var]]))
    selectInput("proptest2_level", "Pilih Level 'Success':", choices = choices)
  })
  observeEvent(input$proptest2_btn, {
    req(input$proptest2_var, input$proptest2_group, input$proptest2_level)
    tbl <- table(map_data_full[[input$proptest2_var]], map_data_full[[input$proptest2_group]])
    success_level <- input$proptest2_level
    
    if (success_level %in% rownames(tbl)) {
      x <- tbl[success_level, ]
      n <- colSums(tbl)
      res <- prop.test(x = x, n = n)
      rv_inferensia$proptest2_raw <- res
      
      p_val <- res$p.value
      kesimpulan <- if (p_val < 0.05) "terdapat perbedaan proporsi yang signifikan" else "tidak ada perbedaan proporsi yang signifikan"
      kesimpulan_h0 <- if (p_val < 0.05) "Tolak H0" else "Gagal Tolak H0"
      
      rv_inferensia$proptest2_report <- paste0(
        "**Hipotesis:**\n",
        "H0: Tidak ada perbedaan proporsi '", sanitize_latex(input$proptest2_level), "' antara kelompok '", sanitize_latex(input$proptest2_group), "'.\n",
        "H1: Terdapat perbedaan proporsi '", sanitize_latex(input$proptest2_level), "' antara kelompok '", sanitize_latex(input$proptest2_group), "'.\n\n",
        "**Hasil Uji Statistik:**\n",
        "```\n",
        sanitize_latex(capture_print(res)), "\n",
        "```\n\n",
        "**Kesimpulan:**\n",
        "Dengan p-value **", round(p_val, 4), "**, yang ", if(p_val<0.05) "lebih kecil dari" else "lebih besar dari", " alpha (0.05), maka **", kesimpulan_h0, "**. ",
        "Terdapat cukup bukti statistik untuk menyatakan bahwa ", kesimpulan, " antara dua kelompok."
      )
    } else {
      rv_inferensia$proptest2_raw <- "Error: Level 'Success' tidak ditemukan dalam data."
      rv_inferensia$proptest2_report <- NULL
    }
  })
  output$proptest2_out <- renderPrint({ rv_inferensia$proptest2_raw })
  output$proptest2_interp <- renderUI({
    req(rv_inferensia$proptest2_report)
    tags$div(class="interpretation-box", HTML(markdownToHTML(text = rv_inferensia$proptest2_report, fragment.only=TRUE)))
  })
  
  # Uji Varians 1 Sampel
  observeEvent(input$vartest1_btn, {
    req(input$vartest1_var, input$vartest1_sigma2 > 0)
    var_data <- na.omit(map_data_full[[tolower(input$vartest1_var)]])
    sigma2 <- input$vartest1_sigma2
    
    if(requireNamespace("EnvStats", quietly = TRUE)) {
      res <- EnvStats::varTest(var_data, sigma.squared = sigma2)
      rv_inferensia$vartest1_raw <- res
      
      p_val <- res$p.value
      kesimpulan <- if (p_val < 0.05) "berbeda secara signifikan" else "tidak berbeda secara signifikan"
      kesimpulan_h0 <- if (p_val < 0.05) "Tolak H0" else "Gagal Tolak H0"
      
      rv_inferensia$vartest1_report <- paste0(
        "**Hipotesis:**\n",
        "H0: Varians populasi untuk '", sanitize_latex(input$vartest1_var), "' sama dengan ", sigma2, ".\n",
        "H1: Varians populasi untuk '", sanitize_latex(input$vartest1_var), "' tidak sama dengan ", sigma2, ".\n\n",
        "**Hasil Uji Statistik:**\n",
        "```\n",
        sanitize_latex(capture_print(res)), "\n",
        "```\n\n",
        "**Kesimpulan:**\n",
        "Dengan p-value **", round(p_val, 4), "**, yang ", if(p_val<0.05) "lebih kecil dari" else "lebih besar dari", " alpha (0.05), maka **", kesimpulan_h0, "**. ",
        "Terdapat cukup bukti statistik untuk menyatakan bahwa varians sampel (", round(res$estimate, 2), ") ", kesimpulan, " dari nilai hipotesis (", sigma2, ")."
      )
    } else {
      rv_inferensia$vartest1_raw <- "Paket 'EnvStats' diperlukan untuk uji ini. Silakan install."
      rv_inferensia$vartest1_report <- NULL
    }
  })
  output$vartest1_out <- renderPrint({ rv_inferensia$vartest1_raw })
  output$vartest1_interp <- renderUI({
    req(rv_inferensia$vartest1_report)
    tags$div(class="interpretation-box", HTML(markdownToHTML(text = rv_inferensia$vartest1_report, fragment.only=TRUE)))
  })
  
  # Uji Varians 2 Sampel
  observeEvent(input$vartest2_btn, {
    req(input$vartest2_var, input$vartest2_group)
    var_num <- tolower(input$vartest2_var)
    var_group <- input$vartest2_group
    formula <- as.formula(paste(var_num, "~", var_group))
    res <- var.test(formula, data = map_data_full)
    rv_inferensia$vartest2_raw <- res
    
    p_val <- res$p.value
    kesimpulan <- if (p_val < 0.05) "terdapat perbedaan varians yang signifikan (varians tidak homogen)" else "tidak terdapat perbedaan varians yang signifikan (varians homogen)"
    kesimpulan_h0 <- if (p_val < 0.05) "Tolak H0" else "Gagal Tolak H0"
    
    rv_inferensia$vartest2_report <- paste0(
      "**Hipotesis:**\n",
      "H0: Rasio varians antara dua kelompok adalah 1 (varians sama).\n",
      "H1: Rasio varians antara dua kelompok tidak sama dengan 1 (varians berbeda).\n\n",
      "**Hasil Uji Statistik:**\n",
      "```\n",
      sanitize_latex(capture_print(res)), "\n",
      "```\n\n",
      "**Kesimpulan:**\n",
      "Dengan p-value **", round(p_val, 4), "**, yang ", if(p_val<0.05) "lebih kecil dari" else "lebih besar dari", " alpha (0.05), maka **", kesimpulan_h0, "**. ",
      "Terdapat cukup bukti statistik untuk menyatakan bahwa ", kesimpulan, " antara dua kelompok."
    )
  })
  output$vartest2_out <- renderPrint({ rv_inferensia$vartest2_raw })
  output$vartest2_interp <- renderUI({
    req(rv_inferensia$vartest2_report)
    tags$div(class="interpretation-box", HTML(markdownToHTML(text = rv_inferensia$vartest2_report, fragment.only=TRUE)))
  })
  
  # ANOVA 1 Arah
  observeEvent(input$anova1_btn, {
    req(input$anova1_var, input$anova1_group)
    var_num <- tolower(input$anova1_var)
    var_group <- input$anova1_group
    num_levels <- n_distinct(na.omit(map_data_full[[var_group]]))
    formula <- as.formula(paste(var_num, "~", var_group))
    aov_model <- aov(formula, data = map_data_full)
    aov_summary <- summary(aov_model)
    rv_inferensia$anova1_raw <- aov_summary
    
    p_val <- aov_summary[[1]][["Pr(>F)"]][1]
    p_val_text <- if (is.na(p_val)) "NA" else if (p_val < 0.001) "< 0.001" else as.character(round(p_val, 4))
    kesimpulan <- if (!is.na(p_val) && p_val < 0.05) "terdapat setidaknya satu perbedaan rata-rata yang signifikan" else "tidak ada perbedaan rata-rata yang signifikan"
    kesimpulan_h0 <- if (!is.na(p_val) && p_val < 0.05) "Tolak H0" else "Gagal Tolak H0"
    
    main_report <- paste0(
      "**Hipotesis:**\n",
      "H0: Rata-rata '", sanitize_latex(input$anova1_var), "' adalah sama di semua kelompok '", sanitize_latex(input$anova1_group), "'.\n",
      "H1: Setidaknya ada satu kelompok yang rata-ratanya berbeda.\n\n",
      "**Hasil Uji Statistik:**\n",
      "```\n",
      sanitize_latex(capture_print(aov_summary)), "\n",
      "```\n\n",
      "**Kesimpulan:**\n",
      "Dengan p-value **", p_val_text, "**, yang ", if(!is.na(p_val) && p_val<0.05) "lebih kecil dari" else "lebih besar dari", " alpha (0.05), maka **", kesimpulan_h0, "**. ",
      "Terdapat cukup bukti statistik untuk menyatakan bahwa ", kesimpulan, " di antara kelompok."
    )
    
    rv_inferensia$anova1_posthoc_raw <- NULL # Reset post-hoc
    if (num_levels > 2 && !is.na(p_val) && p_val < 0.05) {
      tukey_results <- TukeyHSD(aov_model)
      rv_inferensia$anova1_posthoc_raw <- tukey_results
      
      tukey_df <- as.data.frame(tukey_results[[1]])
      significant_pairs <- tukey_df %>% filter(`p adj` < 0.05) %>% rownames()
      
      posthoc_interp <- if (length(significant_pairs) > 0) {
        paste("Perbedaan rata-rata yang signifikan (p < 0.05) ditemukan antara pasangan kelompok berikut:", paste(sanitize_latex(significant_pairs), collapse = ", "))
      } else {
        "Tidak ada pasangan kelompok spesifik yang menunjukkan perbedaan rata-rata yang signifikan setelah penyesuaian."
      }
      main_report <- paste0(main_report, "\n\n**Interpretasi Post-Hoc (Tukey HSD):**\n", posthoc_interp)
    }
    rv_inferensia$anova1_report <- main_report
  })
  output$anova1_out <- renderPrint({ rv_inferensia$anova1_raw })
  output$anova1_interp <- renderUI({
    req(rv_inferensia$anova1_report)
    tags$div(class="interpretation-box", HTML(markdownToHTML(text = rv_inferensia$anova1_report, fragment.only=TRUE)))
  })
  output$anova1_posthoc_ui <- renderUI({
    req(rv_inferensia$anova1_posthoc_raw)
    box(title = with_tooltip(tags$span("Uji Lanjutan Tukey HSD"), "tukey_tip", "Membandingkan setiap pasangan kelompok untuk melihat mana yang berbeda secara signifikan."), status = "info", solidHeader = TRUE, width = NULL,
        verbatimTextOutput("anova1_posthoc_out")
    )
  })
  output$anova1_posthoc_out <- renderPrint({ rv_inferensia$anova1_posthoc_raw })
  
  # ANOVA 2 Arah
  observeEvent(input$anova2_btn, {
    req(input$anova2_var, input$anova2_group1, input$anova2_group2)
    var_num <- tolower(input$anova2_var)
    var_g1 <- input$anova2_group1
    var_g2 <- input$anova2_group2
    
    if (var_g1 == var_g2) {
      res <- "Error: Faktor independen tidak boleh sama."
      rv_inferensia$anova2_raw <- res
      rv_inferensia$anova2_report <- res
      return()
    }
    
    formula <- as.formula(paste(var_num, "~", var_g1, "*", var_g2))
    res_list <- summary(aov(formula, data = map_data_full))
    rv_inferensia$anova2_raw <- res_list
    
    res <- res_list[[1]]
    p_g1 <- res[["Pr(>F)"]][1]
    p_g2 <- res[["Pr(>F)"]][2]
    p_int <- res[["Pr(>F)"]][3]
    p_g1_text <- if (is.na(p_g1)) "NA" else if (p_g1 < 0.001) "< 0.001" else as.character(round(p_g1, 4))
    p_g2_text <- if (is.na(p_g2)) "NA" else if (p_g2 < 0.001) "< 0.001" else as.character(round(p_g2, 4))
    p_int_text <- if (is.na(p_int)) "NA" else if (p_int < 0.001) "< 0.001" else as.character(round(p_int, 4))
    kesimpulan_g1 <- if (!is.na(p_g1) && p_g1 < 0.05) "signifikan" else "tidak signifikan"
    kesimpulan_g2 <- if (!is.na(p_g2) && p_g2 < 0.05) "signifikan" else "tidak signifikan"
    kesimpulan_int <- if (!is.na(p_int) && p_int < 0.05) "terdapat efek interaksi yang signifikan" else "tidak ada efek interaksi yang signifikan"
    
    rv_inferensia$anova2_report <- paste0(
      "**Hipotesis:**\n",
      "1. H0: Tidak ada efek utama dari faktor '", sanitize_latex(input$anova2_group1), "'.\n",
      "2. H0: Tidak ada efek utama dari faktor '", sanitize_latex(input$anova2_group2), "'.\n",
      "3. H0: Tidak ada efek interaksi antara '", sanitize_latex(input$anova2_group1), "' dan '", sanitize_latex(input$anova2_group2), "'.\n\n",
      "**Hasil Uji Statistik:**\n",
      "```\n",
      sanitize_latex(capture_print(res_list)), "\n",
      "```\n\n",
      "**Kesimpulan:**\n",
      "- Efek utama dari faktor '", sanitize_latex(input$anova2_group1), "' adalah **", kesimpulan_g1, "** (p = ", p_g1_text, ").\n",
      "- Efek utama dari faktor '", sanitize_latex(input$anova2_group2), "' adalah **", kesimpulan_g2, "** (p = ", p_g2_text, ").\n",
      "- Hasil uji menunjukkan bahwa **", kesimpulan_int, "** antara kedua faktor (p = ", p_int_text, ")."
    )
  })
  output$anova2_out <- renderPrint({ rv_inferensia$anova2_raw })
  output$anova2_interp <- renderUI({
    req(rv_inferensia$anova2_report)
    tags$div(class="interpretation-box", HTML(markdownToHTML(text = rv_inferensia$anova2_report, fragment.only=TRUE)))
  })
  
  # Tombol Simpan untuk halaman Inferensia
  observeEvent(input$save_inferensia, {
    reports_added <- 0
    add_report_if_exists <- function(report_text, title){
      if(!is.null(report_text)){
        add_to_report(list(title = title, type = "text", content = report_text))
        reports_added <<- reports_added + 1
      }
    }
    add_report_if_exists(rv_inferensia$ttest1_report, "Inferensia - Laporan Uji T Satu Sampel")
    add_report_if_exists(rv_inferensia$ttest2_report, "Inferensia - Laporan Uji T Dua Sampel")
    add_report_if_exists(rv_inferensia$proptest1_report, "Inferensia - Laporan Uji Proporsi Satu Sampel")
    add_report_if_exists(rv_inferensia$proptest2_report, "Inferensia - Laporan Uji Proporsi Dua Sampel")
    add_report_if_exists(rv_inferensia$vartest1_report, "Inferensia - Laporan Uji Varians Satu Sampel")
    add_report_if_exists(rv_inferensia$vartest2_report, "Inferensia - Laporan Uji Varians Dua Sampel")
    add_report_if_exists(rv_inferensia$anova1_report, "Inferensia - Laporan ANOVA Satu Arah & Post-Hoc")
    add_report_if_exists(rv_inferensia$anova2_report, "Inferensia - Laporan ANOVA Dua Arah")
    
    if (reports_added > 0) {
      showNotification(paste(reports_added, "hasil uji berhasil disimpan!"), type = "message")
    } else {
      showNotification("Tidak ada hasil uji yang telah dijalankan untuk disimpan.", type = "warning")
    }
  })
  
  # --- SERVER LOGIC UNTUK PUSAT UNDUHAN ---
  
  output$saved_items_ui <- renderUI({
    reports <- saved_reports()
    if (length(reports) == 0) {
      return(p("Belum ada hasil analisis yang disimpan. Klik tombol 'Simpan Hasil...' di halaman analisis untuk menambahkannya ke sini."))
    }
    
    lapply(reports, function(item) {
      box(
        title = item$title,
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        p(paste("Tipe:", item$type)),
        p(paste("ID Unik:", item$id))
      )
    })
  })
  
  observeEvent(input$clear_saved_reports, {
    showModal(modalDialog(
      title = "Konfirmasi",
      "Apakah Anda yakin ingin membersihkan semua hasil yang tersimpan?",
      footer = tagList(
        modalButton("Batal"),
        actionButton("confirm_clear", "Ya, Bersihkan")
      )
    ))
  })
  
  observeEvent(input$confirm_clear, {
    saved_reports(list())
    removeModal()
    showNotification("Daftar hasil telah dibersihkan.", type = "warning")
  })
  
  output$download_combined_pdf <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", tolower(input$report_title)), "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      withProgress(message = 'Menyiapkan laporan PDF...', value = 0, {
        
        temp_report_path <- file.path(tempdir(), "report.Rmd")
        reports_to_process <- saved_reports()
        
        # Saring item yang tidak didukung (leaflet)
        reports_to_render <- Filter(function(item) item$type != "leaflet", reports_to_process)
        
        if (any(sapply(reports_to_process, function(item) item$type == "leaflet"))) {
          showNotification("Peta interaktif tidak disertakan dalam laporan PDF.", type = "warning", duration = 8)
        }
        
        if (length(reports_to_render) == 0) {
          showNotification("Tidak ada item yang dapat di-render ke PDF.", type="error", duration=8)
          return(NULL)
        }
        
        incProgress(0.4, detail = "Menyusun konten laporan...")
        
        rmd_content <- c(
          "---",
          paste0("title: '", input$report_title, "'"),
          paste0("author: '", input$report_author, "'"),
          paste0("date: '", Sys.Date(), "'"),
          "output: ",
          "  pdf_document:",
          "    toc: true",
          "    number_sections: true",
          "geometry: 'left=2.5cm, right=2.5cm, top=2.5cm, bottom=2.5cm'",
          "header-includes:",
          "  - \\usepackage{xcolor}",
          "  - \\usepackage{framed}",
          "  - \\definecolor{shadecolor}{RGB}{240,240,240}",
          "  - \\newenvironment{interpretation}{\\begin{snugshade}\\small\\itshape}{\\end{snugshade}}",
          "---",
          "",
          "```{r setup, include=FALSE}",
          "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.pos = 'H', fig.align = 'center')",
          "library(ggplot2)",
          "library(knitr)",
          "```",
          ""
        )
        
        reports_env <- new.env()
        reports_env$reports_for_rmd <- reports_to_render
        
        for (i in seq_along(reports_to_render)) {
          item <- reports_to_render[[i]]
          incProgress(0.4 + (0.5 / length(reports_to_render)), detail = paste("Menambahkan:", item$title))
          
          chunk_code <- ""
          if (item$type == 'table') {
            chunk_code <- "knitr::kable(item$content, caption = item$title)"
          } else if (item$type == 'ggplot') {
            chunk_code <- "print(item$content)"
          } else if (item$type == 'verbatim') {
            chunk_code <- paste0("cat('```\\n', item$content, '\\n```')")
          } else if (item$type == 'text') {
            # Wrap text content in the custom interpretation environment
            chunk_code <- paste0("cat('\\n\\n', item$content, '\\n\\n')")
          } else if (item$type == 'plot_base') {
            chunk_code <- "par(mfrow = c(2, 2)); plot(item$content); par(mfrow = c(1, 1))"
          }
          
          rmd_content <- c(rmd_content,
                           paste0("\n\n## ", sanitize_latex(item$title), "\n"),
                           "```{r, results='asis'}",
                           paste0("item <- reports_for_rmd[[", i, "]]"),
                           chunk_code,
                           "```",
                           "\n"
          )
        }
        
        writeLines(rmd_content, temp_report_path, useBytes = TRUE)
        
        incProgress(0.9, detail = "Merender PDF...")
        tryCatch({
          rmarkdown::render(
            input = temp_report_path,
            output_file = file,
            envir = reports_env,
            quiet = TRUE
          )
        }, error = function(e){
          showNotification(paste("Gagal merender PDF:", e$message), type="error", duration=15)
          # Copy log file for debugging
          log_file <- sub("\\.tex$", ".log", rmarkdown::render(input = temp_report_path, quiet = TRUE))
          if(file.exists(log_file)) file.copy(log_file, file.path(dirname(file), "error_log.log"))
        })
      })
    }
  )
}

# ==== Run App ====
shinyApp(ui, server)
