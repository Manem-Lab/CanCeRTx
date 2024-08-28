
library(shiny)
library(htmlwidgets)
library(shinycssloaders)
library(plotly)

library(DBI)

#CSS

css <- "
body {
overflow-x: hidden;
}
.navbar {
display: none;
} .tab-content {
display: block;
} #title {
font-size: 50px;
color: black;
} #card1 {
aspect-ratio: 2377 / 1119;
width: 100%;
height: auto !important;
} .shiny-image-output img {
width: 100%;
height: 100%;
} .shiny-split-layout, .shiny-split-layout>div {
width: 100%;
height: 100%;
overflow: visible !important;
} .shiny-spinner-output-container, .plotly, .shiny-plot-output, .shiny-html-output {
width: 100% !important;
height: 100% !important;
} .selectize-input>div {
background: white !important;
margin: 0;
} .row {
overflow: visible;
}
"

oldcss <- "
.nav>li>a, .nav>li>a:hover {
font-weight: bold;
background-color: white;
border-color: #004e8f;
color: black;
} .nav>li.active>a, .nav>li.active>a:focus, .nav>li.active>a:hover {
font-weight: bold;
background-color: #d1e5ff;
border-color: #004e8f;
color: black;
} .optgroup-header {
display: none !important;
} .shiny-input-container {
width: 100% !important;
} .selectize-input {
width: 100%;
border-width: 2px;
border-color: #004e8f;
padding: 15px !important;
} .well {
background-color: #fff4db;
} .btn
"
# margin-left: 200px;
# margin-top: -100px;
idspecific <- "
#tissue_chart {

} .dataTables_filter, .dataTables_length {
display: none;
} #tissue_table {
margin-top: -25px;
} #explore_options>div {
display: inline-flex;
justify-content: center;
align-items: center;
} #card3 {
height: 1000px !important;
} #logos img {
height: auto;
} #vskm, #location, #ak, #vsk, #dhawan, #lambie {
border-left: 5px solid blue;
padding-left: 10px;
} #ak, #vsk, #dhawan, #lambie {
padding-bottom: 10px;
}
"

fluidPage(
  tags$head(
    tags$title("CanCeRTx"),
    tags$style(
      HTML(css)
    ),
    tags$style(
      HTML(oldcss)
    ),
    tags$style(
      HTML(idspecific)
    )
  ),
  
  tags$div(
    style = "background-image: url('topbar_v3.svg');
    background-size: cover; background-position: center;
    color: white; display: flex; align-items: center;
    justify-content: center; margin-top: 15px;
    padding: 25px; background-color: #fff4db; font-size:
    50px; font-weight: bold; border-radius: 10px;",
    splitLayout(
      cellWidths = c("60%", "40%"),
      tags$div(
        style = "padding: 0px;",
        h1(id = "title", "CanCeRTx")
      ),
      tags$div(
        style = "padding: 10px;",
        tags$style(HTML(
          ".home, .home:focus, .home:hover {
            background-color: rgba(181, 198, 245, 0.2);
            color: white;
            border: 0px;
            border-radius: 10px;
            padding: 20px;
            width: 20%;
            font-size: 50%;
          }"
        )),
        actionButton("home", "Home", class = "home"),
        actionButton("explore", "Explore", class = "home"),
        actionButton("about", "About", class = "home"),
        actionButton("contact", "Contact", class = "home")
      )
    )
  ),
  
  navbarPage(
    id = "navbar",
    tabPanel(
      "empty",
      value = "empty"
    ),
    
    tabPanel(
      "home",
      value = "hometab",
      tags$style(HTML("
        .carousel-container {
          position: relative;
          width: 100%; /* Full viewport width */
          height: 50vw; /* Full viewport height, adjust if needed */
          overflow: hidden;
          display: flex;
          justify-content: center;
          align-items: center;
        } .carousel-wrapper {
          width: 100%;
          height: 100%;
          display: flex;
          justify-content: center;
          align-items: center;
        } .carousel {
          display: flex;
          transition: transform 0.5s ease;
          width: 100%; /* Take full width of the container */
          height: 100%; /* Take full height of the container */
        } .carousel-item {
          flex: 0 0 100%; /* Each card takes full viewport width */
          height: 90%; /* Full height of the container */
          display: flex;
          justify-content: center;
          align-items: center;
          box-sizing: border-box;
        } .carousel-item .card {
          width: 90%; /* Width of the card */
          height: 90%; /* Height of the card */
          display: flex;
          justify-content: center;
          align-items: center;
          background-color: white;
          border: 1px solid white;
          border-radius: 0.25rem;
          padding: 20px;
          box-shadow: none;
          box-sizing: border-box;
        } .nav-buttons {
          position: absolute;
          top: 50%;
          width: 100%;
          display: flex;
          justify-content: space-between;
          transform: translateY(-50%);
          z-index: 10;
        } .nav-button, .nav-button:hover, .nav-button:focus {
          background-color: rgba(0,0,0,0.5);
          width: 40px;
          height: 40px;
          color: white;
          border: none;
          border-radius: 50%;
          padding: 10px;
          cursor: pointer;
          transition: opacity 0.3s ease;
        } .nav-buttons button.disabled {
          opacity: 0.3;
          cursor: not-allowed;
        }
      ")),
      
      tags$div(class = "carousel-container",
          tags$div(class = "carousel-wrapper",
              tags$div(id = "carousel", class = "carousel",
                  tags$div(class = "carousel-item",
                      tags$div(class = "card",
                          imageOutput("card1")
                          )
                  ),
                  tags$div(class = "carousel-item",
                      tags$div(class = "card",
                          tags$style(HTML("
                            #dataset {
                            padding: 10px;
                            background-image: url('dataset.png');
                            background-size: cover;
                            background-position: center;
                            border: none;
                            color: transparent;
                            width: 70%;
                            height: 100%;
                            } #gene {
                            padding: 10px;
                            background-image: url('explorebiology.svg');
                            background-size: cover;
                            background-position: center;
                            border: none;
                            color: transparent;
                            width: 66%;
                            height: 100%;
                            }
                          ")),
                          splitLayout(
                            id = "explore_options",
                            actionButton("dataset", "d"),
                            actionButton("gene", "g"),
                          )
                      )
                  ),
                  tags$div(class = "carousel-item",
                      tags$div(class = "card",
                               imageOutput("card3")
                               )
                  ),
                  tags$div(class = "carousel-item",
                      tags$div(class = "card",
                               style = "display: flex; flex-direction: column;",
                               tags$div(class = "top-section",
                                   style = "height: 25%; display: flex;",
                                   tags$div(
                                     style = "padding: 10px; width: 30%; font-size: 50px;",
                                     textOutput("contactus")
                                   ),
                                   tags$div(
                                     style = "padding: 10px; width: 40%; font-size: 20px;",
                                     htmlOutput("vskm")
                                   ),
                                   tags$div(
                                     style = "padding: 10px; width: 30%; font-size: 20px;",
                                     htmlOutput("location")
                                   )
                               ),
                               tags$div(class = "middle-section",
                                   style = "height: 40%; display: flex;",
                                   tags$div(
                                       style = "width: 30%; padding: 10px; font-size: 50px;",
                                       textOutput("citeus")
                                   ),
                                   tags$div(
                                       style = "width: 70%; padding: 10px; font-size: 20px;",
                                       textOutput("ak"),
                                       textOutput("vsk"),
                                       textOutput("dhawan"),
                                       textOutput("lambie")
                                   )
                               ),
                               tags$div(class = "bottom-section",
                                   style = "height: 35%; width: 40%;",
                                   tags$div(class = "image",
                                       imageOutput("logos")
                                   )
                               )
                               )
                  )
              )
          ),
          tags$div(class = "nav-buttons",
              actionButton("prev", "<", class = "nav-button"),
              actionButton("next", ">", class = "nav-button")
          )
      ),
          
      tags$script(HTML("
        document.addEventListener('DOMContentLoaded', function() {
          const carousel = document.getElementById('carousel');
          const items = carousel.children;
          const totalItems = items.length;
          let currentIndex = 0;
          
          function updatePosition() {
            const itemWidth = document.querySelector('.carousel-item').offsetWidth;
            carousel.style.transform = 'translateX(' + (-currentIndex * itemWidth) + 'px)';
            updateButtonVisibility();
          }
          
          function updateButtonVisibility() {
            document.getElementById('prev').classList.toggle('disabled', currentIndex === 0);
            document.getElementById('next').classList.toggle('disabled', currentIndex === totalItems - 1);
          }
          
          document.getElementById('prev').addEventListener('click', function() {
            if (currentIndex > 0) {
              currentIndex--;
              updatePosition();
            }
          });
          
          document.getElementById('next').addEventListener('click', function() {
            if (currentIndex < totalItems - 1) {
              currentIndex++;
              updatePosition();
            }
          });
          
          updatePosition();
          
          window.addEventListener('resize', function() {
            updatePosition();
          });
          

        });
        
        function goToCard(index) {
          const carousel = document.getElementById('carousel');
          const itemWidth = document.querySelector('.carousel-item').offsetWidth;
          carousel.style.transform = 'translateX(' + (-index * itemWidth) + 'px)';
        }
        
        Shiny.addCustomMessageHandler('goToCard', function(index) {
          setTimeout(function() {
            goToCard(index);
          }, 100);
        });
      "))
    ),
    
    tabPanel(
      "dataset",
      value = "exploredataset",
      tags$div(
        style = "padding: 5px;",
        tabsetPanel(
          id = "tabs1",
          title = NULL,
          tabPanel(
            "Data Description",
            verticalLayout(
              tags$div(
                style = "padding: 25px; width: 25%;",
                uiOutput("dataset1")
                )
              ),
              
            splitLayout(
              cellWidths = c("60%", "40%"),
              tags$div(
                style = "padding: 25px; width: 100%; height: 100%;",
                plotlyOutput("tissue_chart") %>% withSpinner(color = "#fff4db")
              ),
              
              tags$div(
                style = "padding: 25px;",
                tags$style(HTML("
                  .dataTable {
                    border-collapse: separate;
                    border-spacing: 10px;
                    width: 100%;
                  }
                  .dataTable.display>tbody>tr.odd>td, .dataTable.display>tbody>tr.even>td, .dataTable>thead>tr>td, .dataTable>thead>tr>th {
                    border: none !important;
                    box-shadow:
                      inset 0 0 0 2px white,
                      inset 0 0 0 5px #004e8f;
                    padding: 15px;
                    text-align: center;
                    vertical-align: middle;
                    box-sizing: border-box;
                  }
                  .dataTable>thead>tr>th {
                    padding: 20px !important;
                    background-color: #fff4db;
                  }
                  .dataTable th {
                    background-color: white;
                    font-weight: bold;
                  }
                  .dataTable tbody tr:nth-child(even) {
                    background-color: white;
                  }
                  .dataTable tbody tr:hover {
                    background-color: #e2e2e2;
                  }
                  .dataTables_wrapper .dataTables_paginate .paginate_button {
                    padding: 6px 12px;
                    margin: 2px;
                    border-radius: 4px;
                    border: 1px solid #004e8f;
                  }
                  .dataTables_wrapper .dataTables_paginate .paginate_button.current {
                    border: 1px solid #004e8f;
                    background-color: #e2e2e2;
                    color: white;
                  }
                ")),
                DT::DTOutput("tissue_table") %>% withSpinner(color = "#fff4db")
              )
            )
          ),
          
          tabPanel(
            "Radiation Response",
            verticalLayout(
              tags$div(
                style = "padding: 25px; width: 25%;",
                uiOutput("dataset2")
              )
            ),
            
            splitLayout(
              cellWidths = c("30%", "70%"),
              tags$div(
                style = "padding: 25px;",
                conditionalPanel(
                  condition = "input.dataset2 !== ''",
                  wellPanel(
                    verticalLayout(
                      uiOutput("dist_tissue"),
                      uiOutput("dist_res"),
                      selectInput("dist_type", "Distribution plot:", choices = c("Histogram", "Density")),
                      conditionalPanel(
                        condition = "input.dist_type == 'Histogram'",
                        sliderInput("n_bins", "Number of bins:", min = 1, max = 50, value = 10)
                      )
                    )
                  ),
                  actionButton("submit2", "Submit"),
                  downloadButton("export2", "Export")
                )
              ),

              tags$div(
                style = "padding: 25px; width: 1000px; aspect-ratio: 4 / 3;",
                plotlyOutput("dist_chart") %>% withSpinner(color = "#fff4db")
              )
            )
          ),
          
          tabPanel(
            "LQ Model Fitting",
            verticalLayout(
              tags$div(
                style = "padding: 25px; width: 50%; display: flex; flex-direction: row; gap: 50px;",
                selectInput("drc_comparison", "Comparison of:", c("Cell Lines", "Datasets")),
                uiOutput("dataset3")
              ),
              
              splitLayout(
                cellWidths = c("30%", "70%"),
                tags$div(
                  style = "padding: 25px;",
                  conditionalPanel(
                    condition = "input.dataset3a !== '' & input.drc_comparison == 'Cell Lines'",
                    wellPanel(
                      verticalLayout(
                        uiOutput("drc_tissue_a"),
                        uiOutput("drc_cells_a")
                      )
                    ),
                    actionButton("submit3a", "Submit"),
                    downloadButton("export3a", "Export")
                  ),
                  
                  conditionalPanel(
                    condition = "input.dataset3b != null && input.dataset3b !== undefined & input.drc_comparison == 'Datasets'",
                    wellPanel(
                      verticalLayout(
                        uiOutput("drc_tissue_b"),
                        uiOutput("drc_cell_b")
                      )
                    ),
                    actionButton("submit3b", "Submit"),
                    downloadButton("export3b", "Export")
                  )
                ),
              
                tags$div(
                  style = "padding: 25px; width: 1000px; aspect-ratio: 4 / 3;",
                  uiOutput("drc_plot") %>% withSpinner(color = "#fff4db")
                )
              )
            )
          ),
          
          tabPanel(
            "Correlation Analysis",
            splitLayout(
              cellWidths = c("30%", "70%"),
              verticalLayout(
                tags$div(
                  style = "padding: 25px; width: 83%; padding-bottom: 0px;",
                  uiOutput("dataset4a")
                ),
                
                tags$div(
                  style = "padding: 25px; width : 83%; padding-bottom: 0px; padding-top: 0px;",
                  conditionalPanel(
                    condition = "input.dataset4a !== ''",
                    wellPanel(
                      h1("X Variable"),
                      verticalLayout(
                        uiOutput("corr_tissue1"),
                        uiOutput("corr_res1")
                      )
                    )
                  )
                ),
                
                tags$div(
                  style = "padding: 25px; width: 83%; padding-bottom: 0px; padding-top: 0px;",
                  uiOutput("dataset4b")
                ),
                
                tags$div(
                  style = "padding: 25px; width : 83%; padding-bottom: 0px; padding-top: 0px;",
                  conditionalPanel(
                    condition = "input.dataset4b !== ''",
                    wellPanel(
                      h1("Y Variable"),
                      verticalLayout(
                        uiOutput("corr_tissue2"),
                        uiOutput("corr_res2")
                      )
                    ),
                    actionButton("submit4", "Submit"),
                    downloadButton("export4", "Export")
                  )
                )
              ),
              
              tags$div(
                style = "padding: 25px; width: 1000px; aspect-ratio: 4 / 3;",
                uiOutput("cor_analysis_plot") %>% withSpinner(color = "#fff4db")
              )
            )
          ),
          
          tabPanel(
            "Radiation Quality",
            splitLayout(
              cellWidths = c("30%", "70%"),
              verticalLayout(
                tags$div(
                  style = "padding: 25px; width: 83%; padding-bottom: 0px;",
                  uiOutput("dataset5")
                ),
                
                tags$div(
                  style = "padding: 25px; width : 83%; padding-bottom: 0px; padding-top: 0px;",
                  conditionalPanel(
                    condition = "input.dataset5 !== ''",
                    wellPanel(
                      h1("X Variable (Gamma)"),
                      verticalLayout(
                        uiOutput("rad_tissue1"),
                        uiOutput("rad_res1")
                      )
                    )
                  )
                ),
                
                tags$div(
                  style = "padding: 25px; width : 83%; padding-bottom: 0px; padding-top: 0px;",
                  conditionalPanel(
                    condition = "input.dataset5 !== ''",
                    wellPanel(
                      h1("Y Variable (Alpha)"),
                      verticalLayout(
                        uiOutput("rad_tissue2"),
                        uiOutput("rad_res2")
                      )
                    ),
                    actionButton("submit5", "Submit"),
                    downloadButton("export5", "Export")
                  )
                )
              ),
              
              tags$div(
                style = "padding: 25px; width: 1000px; aspect-ratio: 4 / 3;",
                uiOutput("rad_quality_plot") %>% withSpinner(color = "#fff4db")
              )
            )
          ),
          
          tabPanel(
            "Hypoxia Analysis",
            verticalLayout(
              tags$div(
                style = "padding: 25px; width: 25%;",
                uiOutput("dataset6")
              ),
              
              splitLayout(
                cellWidths = c("30%", "70%"),
                tags$div(
                  style = "padding: 25px;",
                  conditionalPanel(
                    condition = "input.dataset6 !== ''",
                    wellPanel(
                      verticalLayout(
                        uiOutput("hypoxia_tissue"),
                        uiOutput("hypoxia_cell"),
                        sliderInput("oxygen", "Oxygen concentration (mmHg):", min = 1, max = 160, value = 10)
                      )
                    ),
                    actionButton("submit6", "Submit"),
                    downloadButton("export6", "Export")
                  )
                ),
                
                tags$div(
                  style = "padding: 25px; width: 1000px; aspect-ratio: 4 / 3;",
                  plotOutput("hypoxia_chart") %>% withSpinner(color = "#fff4db")
                )
              )
            )
          )
        )
      )
    ),
    
    tabPanel(
      "gene",
      value = "exploregene",
      tags$div(
        style = "padding: 5px;",
        tabsetPanel(
          id = "tabs2",
          title = NULL,
          tabPanel(
            "Gene Expression Analysis",
            splitLayout(
              cellWidths = c("30%", "70%"),
              verticalLayout(
                tags$div(
                  style = "padding: 25px; width: 83%; padding-bottom: 0px;",
                  uiOutput("gdataset1")
                ),
                
                tags$div(
                  style = "padding: 25px; width: 83%; padding-bottom: 0px;",
                  conditionalPanel(
                    condition = "input.gdataset1 !== ''",
                    wellPanel(
                      selectizeInput(
                        "gea_gene",
                        "Gene:",
                        multiple = FALSE,
                        choices = NULL,
                        options = list(
                          create = FALSE,
                          placeholder = "Search gene",
                          maxItems = "1",
                          maxOptions = "5",
                          onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                          onType = I("function (str) {if (str === \"\") {this.close();}}")
                        )
                      ),
                      uiOutput("gea_tissue"),
                      uiOutput("gea_res")
                    ),
                    actionButton("gsubmit1", "Submit"),
                    downloadButton("gexport1", "Export")
                  )
                )
              ),
              
              tags$div(
                style = "padding: 25px;",
                tags$div(
                  style = "padding: 25px; width: 1000px; aspect-ratio: 4 / 3;",
                  uiOutput("gea_plot") %>% withSpinner(color = "#fff4db"),
                )
              )
            )
          ),
          
          tabPanel(
            "Gene Correlation Analysis",
            splitLayout(
              cellWidths = c("30%", "70%"),
              verticalLayout(
                tags$div(
                  style = "padding: 25px; width: 83%; padding-bottom: 0px;",
                  uiOutput("gdataset2")
                ),
                
                tags$div(
                  style = "padding: 25px; width: 83%; padding-bottom: 0px;",
                  conditionalPanel(
                    condition = "input.gdataset2 !== ''",
                    wellPanel(
                      selectizeInput(
                        "gca_gene",
                        "Gene:",
                        multiple = FALSE,
                        choices = NULL,
                        options = list(
                          create = FALSE,
                          placeholder = "Search gene",
                          maxItems = "1",
                          maxOptions = "5",
                          onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                          onType = I("function (str) {if (str === \"\") {this.close();}}")
                        )
                      ),
                      uiOutput("gca_tissue"),
                      uiOutput("gca_res")
                    ),
                    actionButton("gsubmit2", "Submit")
                  )
                )
              ),
              
              tags$div(
                style = "padding: 25px;",
                tags$div(
                  style = "padding: 25px; width: 1000px; aspect-ratio: 4 / 3;",
                  uiOutput("gca_plot") %>% withSpinner(color = "#fff4db"),
                )
              )
            )
          ),
          
          tabPanel(
            "Pathway Analysis",
            splitLayout(
              cellWidths = c("30%", "70%"),
              verticalLayout(
                tags$div(
                  style = "padding: 25px; width: 83%; padding-bottom: 0px;",
                  uiOutput("gdataset3")
                ),
                
                tags$div(
                  style = "padding: 25px; width: 83%; padding-bottom: 0px;",
                  conditionalPanel(
                    condition = "input.gdataset3 !== ''",
                    wellPanel(
                      selectInput("pa_db", "Pathway Database:", c("kegg", "reactome")),
                      selectInput("pa_rad", "Radiation Quality:", c("Gamma", "Alpha")),
                      uiOutput("pa_res"),
                      conditionalPanel(
                        condition = "input.pa_rad !== 'Alpha'",
                        uiOutput("pa_tissue")
                      )
                    )
                  )
                ),
                
                tags$div(
                  style = "padding: 25px;",
                  conditionalPanel(
                    condition = "input.gdataset3 !== ''",
                    selectInput("pa_significance", "Significance Threshold (%)", c(1, 5, 10)),
                    actionButton("gsubmit3", "Submit")
                  )
                )
              ),
              
              tags$div(
                style = "padding: 25px;",
                tags$div(
                  style = "padding: 25px; width: 1000px; aspect-ratio: 4 / 3;",
                  uiOutput("pa_plot") %>% withSpinner(color = "#fff4db")
                )
              )
            )
          ),
          
          
        )
      )
    )
  )
)
