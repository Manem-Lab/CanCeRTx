
library(shiny)
library(DBI)
library(RadioGx)
library(dplyr)
library(tidyr)

#DATABASE

function(input, output, session) {
  #TOP BAR
   
  observeEvent(input$home, {
    updateNavbarPage(session, "navbar", selected = "hometab")
    session$sendCustomMessage(type = "goToCard", message = 0)
  })
  
  observeEvent(input$explore, {
    updateNavbarPage(session, "navbar", selected = "hometab")
    session$sendCustomMessage(type = "goToCard", message = 1)
  })
  
  observeEvent(input$about, {
    updateNavbarPage(session, "navbar", selected = "hometab")
    session$sendCustomMessage(type = "goToCard", message = 2)
  })
  
  observeEvent(input$contact, {
    updateNavbarPage(session, "navbar", selected = "hometab")
    session$sendCustomMessage(type = "goToCard", message = 3)
  })
  
  #CARDS
  output$card1 <- renderImage({
    list(src = "www/card1_v3.svg", contentType = "image/svg+xml")
  }, deleteFile = FALSE)
  
  output$card3 <- renderImage({
    list(src = "www/homepage_v3.svg", contentType = "image/svg+xml")
  }, deleteFile = FALSE)
  
  #EXPLORE OPTIONS
  
  observeEvent(input$dataset, {
    updateNavbarPage(session, "navbar", selected = "exploredataset")
  })
  
  observeEvent(input$gene, {
    updateNavbarPage(session, "navbar", selected = "exploregene")
  })
  
  #CONTACT
  
  output$contactus <- renderText({
    "Contact us"
  })
  
  output$vskm <- renderUI({
    HTML(
    "Venkata Manem, PhD
<br>Assistant Professor
<br>Department of Molecular Biology, Medical Biochemistry and Pathology, Faculty of Medicine, Université Laval
<br>Email: venkata.manem@crchudequebec.ulaval.ca  "
    )
  })
  
  output$location <- renderUI({
    HTML(
      "Location:
<br>Centre de recherche du CHU de Québec - <br>Université Laval, Oncology Axis
<br>9 McMahon, Quebec, QC, Canada G1R 3S3"
    )
  })
  
  output$citeus <- renderText({
    "Cite us"
  })
  
  output$ak <- renderText({
    "Kolnohuz, A., Ebrahimpour, L., Yolchuyeva, S. et al. Gene expression signature predicts radiation sensitivity in cell lines using the integral of dose–response curve. BMC Cancer 24, 2 (2024).  "
  })
  
  output$vsk <- renderText({
    "Manem, V.S.K. Development and validation of genomic predictors of radiation sensitivity using preclinical data. BMC Cancer 21, 937 (2021)."
  })
  
  output$dhawan <- renderText({
    "Manem, VS., Dhawan, A. RadiationGeneSigDB: a database of oxic and hypoxic radiation response gene signatures and their utility in pre-clinical research. Br J Radiol 92, 1103 (2019)."
  })
  
  output$lambie <- renderText({
    "Manem, VS., Lambie, M., Smith, I., et al. Modeling Cellular Response in Large-Scale Radiogenomic Databases to Advance Precision Radiotherapy. Cancer Res 79, 24 (2019)."
  })
  
  output$logos <- renderImage({
    list(src = "www/contactus_v3.svg", contentType = "image/svg+xml")
  }, deleteFile = FALSE)
  
  #DATASET
  
  db <- dbConnect(RSQLite::SQLite(), "CANCERTX.db")
  
  datalist <- dbGetQuery(db, "SELECT name FROM datasets")
  
  datasets <- list()
  for (name in datalist$name) {
    query <- "SELECT data FROM datasets WHERE name = ?"
    object <- dbGetQuery(db, query, params = list(name))
    
    if (nrow(object) > 0) {
      datasets[[name]] <- unserialize(object$data[[1]])
    } else {
      warning(paste("No data found for:", name))
    }
  }
  
  library(piano)
  library(GSA)
  
  kegg <- loadGSC(dbReadTable(db, "kegg"))
  reactome <- loadGSC(dbReadTable(db, "reactome"))
  
  dbDisconnect(db)
  
  fetch_dataset <- function(name) {
    if (name %in% names(datasets)) {
      return(datasets[[name]])
    } else {
      stop(paste("Dataset not found:", name))
    }
  }
  
  #DATA DESCRIPTION
  
  output$dataset1 <- renderUI({
    selectizeInput(inputId = "dataset1",
                   label = NULL,
                   choices = c("Search Bar" = "", datalist),
                   multiple = FALSE,
                   selected = NULL,
                   options = list(
                     create = FALSE,
                     placeholder = "Search dataset (for ex. Cleveland)",
                     maxItems = '1',
                     onDropdownOpen = I("function($dropdown) { this.settings.openOnFocus = true; }"),
                     onType = I("function (str) { if (str === \"\") { this.close(); } else { this.open(); } }")
                   )
    )
  })
  
  output$tissue_chart <- renderPlotly({
    validate(
      need(input$dataset1, "")
    )
    object <- fetch_dataset(input$dataset1)

    df <- as.data.frame(table(object@SampleData$gamma$tissueid))
    colnames(df) <- c("var", "freq")
    
    col_pals <- RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == "qual",]
    cols <- unlist(mapply(RColorBrewer::brewer.pal, col_pals$maxcolors, rownames(col_pals)))
    shades <- sample(cols, nrow(df))
    
    p <- plot_ly(
      df, labels = ~var, values = ~freq, type = "pie",
      textinfo = "label",
      hoverinfo = "text",
      text = ~paste("n:", freq),
      showlegend = FALSE,
      insidetextorientation = "radial",
      marker = list(
        line = list(
          color = "white", width = 1
        )
      )
    ) %>% layout(
      title = list(
        test = "Cell lines per tissue type",
        font = list(
          size = 20,
          weight = "bold"
        )
      ),
      font = list(
        family = "Arial, sans-serif",
        size = 15,
        color = "#333333"
      ),
      xaxis = list(
        showgrid = FALSE,
        zeroline= FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline= FALSE,
        showticklabels = FALSE
      ),
      margin = list(
        l = 0,
        r = 0,
        b = 0,
        t = 0,
        pad = 0
      )
    )
    
    p
  })
  
  output$tissue_table <- DT::renderDT({
    validate(
      need(input$dataset1, "")
    )
    object <- fetch_dataset(input$dataset1)
    
    df <- (object@SampleData$gamma)
    colnames(df) <- c("Cell Line", "Tissue Type")
    DT::datatable(
      data = df,
      filter = "top",
      rownames = FALSE
    )
  })
  
  #RADIATION RESPONSE
  
  output$dataset2 <- renderUI({
    selectizeInput(inputId = "dataset2",
                   label = NULL,
                   choices = c("Search Bar" = "", datalist),
                   multiple = FALSE,
                   selected = NULL,
                   options = list(
                     create = FALSE,
                     placeholder = "Search dataset (for ex. Cleveland)",
                     maxItems = '1',
                     onDropdownOpen = I("function($dropdown) { this.settings.openOnFocus = true; }"),
                     onType = I("function (str) { if (str === \"\") { this.close(); } else { this.open(); } }")
                   )
    )
  })
  
  output$dist_tissue <- renderUI({
    validate(
      need(input$dataset2, "")
    )
    object <- fetch_dataset(input$dataset2)
    
    d <- as.character(as.data.frame(table(object@SampleData$gamma$tissueid))[,1])
    selectInput("dist_tissue", "Tissue Type:", c("All Tissue Types", d))
  })
  
  output$dist_res <- renderUI({
    validate(
      need(input$dataset2, "")
    )
    object <- fetch_dataset(input$dataset2)
    
    d <- colnames(object@DoseResponseData$gamma)[-1]
    selectInput("dist_res", "Dose Response Type:", d)
  })
  
  dist_df <- reactive({
    input$submit2
    
    isolate({
      validate(
        need(input$dist_tissue, ""),
        need(input$dist_res, "")
      )
      object <- fetch_dataset(input$dataset2)
      
      if (input$dist_tissue == "All Tissue Types"){
        d <- object@DoseResponseData$gamma[[input$dist_res]]
      } else {
        t <- na.omit((object@SampleData$gamma[object@SampleData$gamma$tissueid == input$dist_tissue])[,1])
        d <- (object@DoseResponseData$gamma[object@DoseResponseData$gamma$Cell_line %in% t,])[, input$dist_res]
      }
      
      return(data.frame(value = d))
    })
  })
  
  output$dist_chart <- renderPlotly({
    validate(
      need(dist_df(), "")
    )
    df <- dist_df()
    
    isolate({
      if (input$dist_type == "Histogram"){
        bin_width = (max(df$value) - min(df$value)) / input$n_bins
        breaks <- seq(min(df$value), max(df$value), by = bin_width)
        
        gg <- ggplot(df, aes(x = value)) +
          geom_histogram(breaks = breaks, fill = "#2c90e8", color ="black") +
          theme_bw(base_size = 13) +
          theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.text.y = element_text(size = 11),
            axis.text.x = element_text(size = 11),
            plot.title = element_text(hjust = 0.5, size = 13, face = "bold")
          ) +
          xlim(0, max(df$value)) +
          labs(title = paste("Distribution of", input$dist_res), x = input$dist_res, y = "Count")
        
        p <- ggplotly(gg, tooltip = NULL)
        
        p
      } else if (input$dist_type == "Density"){
        gg <- ggplot(df, aes(x = value)) +
          geom_density(fill = "#2c90e8", color = "black", alpha = 1) +
          theme_bw(base_size = 13) +
          theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.text.y = element_text(size = 13),
            axis.text.x = element_text(size = 13),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
          ) +
          xlim(0, max(df$value)) +
          labs(title = paste("Distribution of", input$dist_res), x = input$dist_res, y = "Count")
        
        p <- ggplotly(gg, tooltip = NULL)
        
        p
      }
    })
  })
  
  output$export2 <- downloadHandler(
    filename = function() {
      paste("Radiation-Response-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      validate(
        need(dist_df(), "")
      )
      df <- dist_df()
      
      if (input$dist_type == "Histogram"){
        bin_width = (max(df$value) - min(df$value)) / input$n_bins
        breaks <- seq(min(df$value), max(df$value), by = bin_width)
        
        gg <- ggplot(df, aes(x = value)) +
          geom_histogram(breaks = breaks, fill = "#2c90e8", color ="black") +
          theme_bw(base_size = 13) +
          theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.text.y = element_text(size = 11),
            axis.text.x = element_text(size = 11),
            plot.title = element_text(hjust = 0.5, size = 13, face = "bold")
          ) +
          xlim(0, max(df$value)) +
          labs(title = paste("Distribution of", input$dist_res), x = input$dist_res, y = "Count")
      } else if (input$dist_type == "Density"){
        gg <- ggplot(df, aes(x = value)) +
          geom_density(fill = "#2c90e8", color = "black", alpha = 1) +
          theme_bw(base_size = 13) +
          theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.text.y = element_text(size = 13),
            axis.text.x = element_text(size = 13),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
          ) +
          xlim(0, max(df$value)) +
          labs(title = paste("Distribution of", input$dist_res), x = input$dist_res, y = "Count")
      }
      
      ggsave(file, plot = gg, device = "png", width = 8, height = 6)
    }
  )
  
  #LQ MODEL FITTING
  
  output$dataset3 <- renderUI({
    if (input$drc_comparison == "Cell Lines"){
      selectizeInput(inputId = "dataset3a",
                     label = NULL,
                     choices = c("Search Bar" = "", datalist),
                     multiple = FALSE,
                     selected = NULL,
                     options = list(
                       create = FALSE,
                       placeholder = "Search dataset (for ex. Cleveland)",
                       maxItems = '1',
                       onDropdownOpen = I("function($dropdown) { this.settings.openOnFocus = true; }"),
                       onType = I("function (str) { if (str === \"\") { this.close(); } else { this.open(); } }")
                     )
      )
    } else if (input$drc_comparison == "Datasets"){
      selectizeInput(inputId = "dataset3b",
                     label = NULL,
                     choices = c("Search Bar" = "", datalist),
                     multiple = TRUE,
                     selected = NULL,
                     options = list(
                       create = FALSE,
                       placeholder = "Search dataset (for ex. Cleveland)",
                       onDropdownOpen = I("function($dropdown) { this.settings.openOnFocus = true; }"),
                       onType = I("function (str) { if (str === \"\") { this.close(); } else { this.open(); } }")
                     )
      )
    }
  })
  
  output$drc_tissue_a <- renderUI({
    validate(
      need(input$dataset3a, "")
    )
    object <- fetch_dataset(input$dataset3a)
    
    d <- as.character(as.data.frame(table(object@SampleData$gamma$tissueid))[,1])
    selectInput("drc_tissue_a", "Tissue Type:", c("All Tissue Types", d))
  })
  
  output$drc_cells_a <- renderUI({
    validate(
      need(input$drc_tissue_a, "")
    )
    object <- fetch_dataset(input$dataset3a)
    
    if (input$drc_tissue_a != "All Tissue Types"){
      d <- as.vector(na.omit((object@SampleData$gamma[object@SampleData$gamma$tissueid == input$drc_tissue_a,])[,1]))
      d <- intersect(d, object@SensitivityData$gamma$Cell_line)
    } else {
      d <- as.vector(na.omit(object@SensitivityData$gamma$Cell_line))
    }
    
    selectizeInput("drc_cells_a", "Cell Line:", d, multiple = TRUE, selected = d[1])
  })
  
  output$drc_tissue_b <- renderUI({
    validate(
      need(input$dataset3b, "")
    )
    
    l <- list()
    for (i in input$dataset3b){
      object <- fetch_dataset(i)
      d <- as.character(as.data.frame(table(object@SampleData$gamma$tissueid))[, 1])
      l[[i]] <- d
    }
    
    d <- Reduce(intersect, l)
    selectInput("drc_tissue_b", "Tissue Type:", c("All Tissue Types", d))
  })
  
  output$drc_cell_b <- renderUI({
    validate(
      need(input$drc_tissue_b, "")
    )
    
    if (input$drc_tissue_b != "All Tissue Types"){
      l <- list()
      for (i in input$dataset3b){
        object <- fetch_dataset(i)
        lines <- na.omit((object@SampleData$gamma[object@SampleData$gamma$tissueid == input$drc_tissue_b,])$sampleid)
        lines <- intersect(lines, object@SensitivityData$gamma$Cell_line)
        l[[i]] <- lines
      }
      
      d <- Reduce(intersect, l)
    } else {
      l <- list()
      for (i in input$dataset3b){
        object <- fetch_dataset(i)
        lines <- object@SensitivityData$gamma$Cell_line
        l[[i]] <- lines
      }
      
      d <- Reduce(intersect, l)
    }
    
    selectizeInput("drc_cell_b", "Cell Line:", d, multiple = FALSE, selected = d[1])
  })
  
  output$drc_plot <- renderUI({
    if (input$drc_comparison == "Cell Lines"){
      plotOutput("drc_chart_a")
    } else if (input$drc_comparison == "Datasets"){
      plotOutput("drc_chart_b")
    }
  })
  
  output$drc_chart_a <- renderPlot({
    input$submit3a
    
    isolate({
      validate(
        need(input$drc_cells_a, "")
      )
      object <- fetch_dataset(input$dataset3a)
      numbers <- gsub("\\D", "", colnames(object@SensitivityData$gamma[,-1]))
      
      if (length(input$drc_cells_a) == 0){
        return()
      }
      
      sfs <- list()
      for (cell in input$drc_cells_a){
        sfs[[cell]] <- as.numeric((object@SensitivityData$gamma[object@SensitivityData$gamma$Cell_line == cell,])[,-1])
      }
      
      doses <- list()
      for (cell in input$drc_cells_a){
        doses[[cell]] <- as.numeric(numbers)
      }
      
      fnzd <- function(x){
        x_str <- as.character(x)
        dpos <- regexpr("\\.", x_str)
        
        if (dpos == -1){
          return(NA)
        }
        
        dpart <- substr(x_str, dpos +1, nchar(x_str))
        fnz <- regexpr("[1-9]", dpart)
        
        if (fnz == -1){
          return(NA)
        }
        
        return(fnz[1])
      }
      
      y <- c(10^-fnzd(min(unlist(sfs))), 1.1)
      
      par(cex.lab = 1.5)
      doseResponseCurve(
        Ds = doses,
        SFs = sfs,
        plot.type = "Both",
        cex = 1.6,
        cex.main = 1.6,
        ylim = y
      )
    })
  })
  
  output$drc_chart_b <- renderPlot({
    input$submit3b
    
    isolate({
      validate(
        need(input$dataset3b, ""),
        need(input$drc_cell_b, "")
      )
      
      sfs <- list()
      doses <- list()
      
      for (i in input$dataset3b){
        object <- fetch_dataset(i)
        numbers <- gsub("\\D", "", colnames(object@SensitivityData$gamma[, -1]))
        
        name = paste0(input$drc_cell_b, " (", i, ")")
        
        sfs[[name]] <- as.numeric(object@SensitivityData$gamma[object@SensitivityData$gamma$Cell_line == input$drc_cell_b,])[-1]
        doses[[name]] <- as.numeric(numbers)
      }
      
      fnzd <- function(x){
        x_str <- as.character(x)
        dpos <- regexpr("\\.", x_str)
        
        if (dpos == -1){
          return(NA)
        }
        
        dpart <- substr(x_str, dpos +1, nchar(x_str))
        fnz <- regexpr("[1-9]", dpart)
        
        if (fnz == -1){
          return(NA)
        }
        
        return(fnz[1])
      }
      
      y <- c(10^-fnzd(min(unlist(sfs))), 1.1)
      
      par(cex.lab = 1.5)
      doseResponseCurve(
        Ds = doses,
        SFs = sfs,
        plot.type = "Both",
        cex = 1.6,
        cex.main = 1.6,
        ylim = y
      )
    })
  })
  
  output$export3a <- downloadHandler(
    filename = function() {
      paste("LQ-Model-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      
      png(file, width = 800, height = 600)
      
      validate(
        need(input$drc_cells_a, "")
      )
      object <- fetch_dataset(input$dataset3a)
      numbers <- gsub("\\D", "", colnames(object@SensitivityData$gamma[,-1]))
      
      if (length(input$drc_cells_a) == 0){
        return()
      }
      
      sfs <- list()
      for (cell in input$drc_cells_a){
        sfs[[cell]] <- as.numeric((object@SensitivityData$gamma[object@SensitivityData$gamma$Cell_line == cell,])[,-1])
      }
      
      doses <- list()
      for (cell in input$drc_cells_a){
        doses[[cell]] <- as.numeric(numbers)
      }
      
      fnzd <- function(x){
        x_str <- as.character(x)
        dpos <- regexpr("\\.", x_str)
        
        if (dpos == -1){
          return(NA)
        }
        
        dpart <- substr(x_str, dpos +1, nchar(x_str))
        fnz <- regexpr("[1-9]", dpart)
        
        if (fnz == -1){
          return(NA)
        }
        
        return(fnz[1])
      }
      
      y <- c(10^-fnzd(min(unlist(sfs))), 1.1)
      
      par(cex.lab = 1.5)
      doseResponseCurve(
        Ds = doses,
        SFs = sfs,
        plot.type = "Both",
        cex = 1.6,
        cex.main = 1.6,
        ylim = y
      )
      
      dev.off()
    }
  )
  
  output$export3b <- downloadHandler(
    filename = function() {
      paste("LQ-Model-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      
      validate(
        need(input$dataset3b, ""),
        need(input$drc_cell_b, "")
      )
      
      sfs <- list()
      doses <- list()
      
      for (i in input$dataset3b){
        object <- fetch_dataset(i)
        numbers <- gsub("\\D", "", colnames(object@SensitivityData$gamma[, -1]))
        
        name = paste0(input$drc_cell_b, " (", i, ")")
        
        sfs[[name]] <- as.numeric(object@SensitivityData$gamma[object@SensitivityData$gamma$Cell_line == input$drc_cell_b,])[-1]
        doses[[name]] <- as.numeric(numbers)
      }
      
      fnzd <- function(x){
        x_str <- as.character(x)
        dpos <- regexpr("\\.", x_str)
        
        if (dpos == -1){
          return(NA)
        }
        
        dpart <- substr(x_str, dpos +1, nchar(x_str))
        fnz <- regexpr("[1-9]", dpart)
        
        if (fnz == -1){
          return(NA)
        }
        
        return(fnz[1])
      }
      
      y <- c(10^-fnzd(min(unlist(sfs))), 1.1)
      
      par(cex.lab = 1.5)
      doseResponseCurve(
        Ds = doses,
        SFs = sfs,
        plot.type = "Both",
        cex = 1.6,
        cex.main = 1.6,
        ylim = y
      )
      
      dev.off()
    }
  )
  
  observe({print(input$dataset3b)})
  
  #CORRELATION ANALYSIS
  
  output$dataset4a <- renderUI({
    selectizeInput(inputId = "dataset4a",
                   label = NULL,
                   choices = c("Search Bar" = "", datalist),
                   multiple = FALSE,
                   selected = NULL,
                   options = list(
                     create = FALSE,
                     placeholder = "Search dataset (for ex. Cleveland)",
                     maxItems = '1',
                     onDropdownOpen = I("function($dropdown) { this.settings.openOnFocus = true; }"),
                     onType = I("function (str) { if (str === \"\") { this.close(); } else { this.open(); } }")
                   )
    )
  })
  
  output$dataset4b <- renderUI({
    selectizeInput(inputId = "dataset4b",
                   label = NULL,
                   choices = c("", datalist),
                   multiple = FALSE,
                   selected = NULL,
                   options = list(
                     create = FALSE,
                     placeholder = "Search dataset (for ex. Cleveland)",
                     maxItems = '1',
                     onDropdownOpen = I("function($dropdown) { this.settings.openOnFocus = true; }"),
                     onType = I("function (str) { if (str === \"\") { this.close(); } else { this.open(); } }")
                   )
    )
  })
  
  output$corr_tissue1 <- renderUI({
    validate(
      need(input$dataset4a, "")
    )
    object <- fetch_dataset(input$dataset4a)
    
    d <- as.character(as.data.frame(table(object@SampleData$gamma$tissueid))[,1])
    selectInput("corr_tissue1", "Tissue Type:", c("All Tissue Types", d))
  })
  
  output$corr_tissue2 <- renderUI({
    validate(
      need(input$dataset4b, "")
    )
    object <- fetch_dataset(input$dataset4b)
    
    d <- as.character(as.data.frame(table(object@SampleData$gamma$tissueid))[,1])
    selectInput("corr_tissue2", "Tissue Type:", c("All Tissue Types", d))
  })
  
  output$corr_res1 <- renderUI({
    validate(
      need(input$dataset4a, "")
    )
    object <- fetch_dataset(input$dataset4a)
    
    d <- colnames(object@DoseResponseData$gamma)[-1]
    selectInput("corr_res1", "Dose Response Type:", d)
  })
  
  output$corr_res2 <- renderUI({
    validate(
      need(input$dataset4b, "")
    )
    object <- fetch_dataset(input$dataset4b)
    
    d <- colnames(object@DoseResponseData$gamma)[-1]
    selectInput("corr_res2", "Dose Response Type:", d, selected = d[2])
  })
  
  observeEvent(input$corr_tissue2, {
    validate(
      need(input$dataset4a, "")
    )
    object <- fetch_dataset(input$dataset4a)
    
    d <- as.character(as.data.frame(table(object@SampleData$gamma$tissueid))[,1])
    if (!input$corr_tissue2 %in% d){
      updateSelectInput(session, "corr_tissue1", selected = "All Tissue Types")
    } else {
      updateSelectInput(session, "corr_tissue1", selected = input$corr_tissue2)
    }
  })
  
  observeEvent(input$corr_tissue1, {
    validate(
      need(input$dataset4b, "")
    )
    object <- fetch_dataset(input$dataset4b)
    
    d <- as.character(as.data.frame(table(object@SampleData$gamma$tissueid))[,1])
    if (!input$corr_tissue1 %in% d){
      updateSelectInput(session, "corr_tissue2", selected = "All Tissue Types")
    } else {
      updateSelectInput(session, "corr_tissue2", selected = input$corr_tissue1)
    }
  })
  
  corr_df <- reactive({
    input$submit4
    
    isolate({
      validate(
        need(input$corr_tissue1, ""),
        need(input$corr_tissue2, ""),
        need(input$corr_res1, ""),
        need(input$corr_res2, ""),
      )
      object <- fetch_dataset(input$dataset4a)
      
      df1 <- data.frame(Cell_line = object@DoseResponseData$gamma$Cell_line, value = object@DoseResponseData$gamma[,input$corr_res1])
      df1 <- setNames(df1, c("Cell_line", input$corr_res1))
      if (input$corr_tissue1 != "All Tissue Types"){
        lines <- (object@SampleData$gamma[object@SampleData$gamma$tissueid == input$corr_tissue1,])[,1]
        df1 <- df1[df1$Cell_line %in% lines,]
      }
      
      object <- fetch_dataset(input$dataset4b)
      
      df2 <- data.frame(Cell_line = object@DoseResponseData$gamma$Cell_line, value = object@DoseResponseData$gamma[,input$corr_res2])
      df2 <- setNames(df2, c("Cell_line", input$corr_res2))
      if (input$corr_tissue2 != "All Tissue Types"){
        lines <- (object@SampleData$gamma[object@SampleData$gamma$tissueid == input$corr_tissue2,])[,1]
        df2 <- df2[df2$Cell_line %in% lines,]
      }
      
      df <- merge(df1, df2, by = "Cell_line")
      return(df)
    })
  })
  
  output$cor_analysis_plot <- renderUI({
    validate(
      need(corr_df(), "")
    )
    
    isolate({
      if (input$corr_tissue1 == input$corr_tissue2 & nrow(corr_df()) > 4){
        plotlyOutput("cor_chart")
      } else if (nrow(corr_df()) != 0){
        textOutput("cor_low_samples")
      } else {
        textOutput("cor_warning")
      }
    })
  })
  
  output$cor_warning <- renderText({
    if (input$corr_tissue1 == "All Tissue Types"){
      paste("WARNING:", input$dataset4a, "does not have data for this tissue type")
    } else if (input$corr_tissue2 == "All Tissue Types"){
      paste("WARNING:", input$dataset4b, "does not have data for this tissue type")
    }
  })
  
  output$cor_low_samples <- renderText({
    "WARNING: Less than 5 common cell lines"
  })
  
  output$cor_chart <- renderPlotly({
    validate(
      need(corr_df(), "")
    )
    df <- corr_df()
    i <- colnames(df)
    colnames(df) <- c("Cell Line", "X", "Y")
    
    cor_coef <- cor(df[,2], df[,3])
    cor_coef <- trunc(cor_coef*10^2)/10^2
    cor_pval <- cor.test(df[,2],df[,3])$p.value
    cor_pval <- formatC(cor_pval, format = "e", digits = 2)
    
    gg <- ggplot(df, aes(x = X, y = Y, label = `Cell Line`)) +
      geom_point(color = "orange") +
      geom_smooth(method = "lm", se = FALSE, alpha = 0.2) +
      geom_text(aes(label = paste("Pearson correlation =", cor_coef, ", Sample size =", nrow(df), ", p-value = ", cor_pval)),
                x = min(df[,2]) + (0.5 * max(df[,2])), y = max(df[,3] + (0.01 * max(df[,3]))), hjust = 1, vjust = 1, size = 5) +
      labs(title = paste("Correlation between", input$dataset4a, i[2], "and", input$dataset4b, i[3]),
           x = i[2],
           y = i[3]) +
      theme_bw(base_size = 20)
    
    p <- ggplotly(gg)
    
    p
  })
  
  output$export4 <- downloadHandler(
    filename = function() {
      paste("Correlation-Analysis-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      validate(
        need(corr_df(), "")
      )
      df <- corr_df()
      i <- colnames(df)
      colnames(df) <- c("Cell Line", "X", "Y")
      
      cor_coef <- cor(df[,2], df[,3])
      cor_coef <- trunc(cor_coef*10^2)/10^2
      cor_pval <- cor.test(df[,2],df[,3])$p.value
      cor_pval <- formatC(cor_pval, format = "e", digits = 2)
      
      gg <- ggplot(df, aes(x = X, y = Y, label = `Cell Line`)) +
        geom_point(color = "orange") +
        geom_smooth(method = "lm", se = FALSE, alpha = 0.2) +
        geom_text(aes(label = paste("Pearson correlation =", cor_coef, ", Sample size =", nrow(df), ", p-value = ", cor_pval)),
                  x = min(df[,2]) + (0.9 * max(df[,2])), y = max(df[,3] + (0.01 * max(df[,3]))), hjust = 1, vjust = 1.5, size = 5) +
        labs(title = paste("Correlation between", input$dataset4a, i[2], "and", input$dataset4b, i[3]),
             x = i[2],
             y = i[3]) +
        theme_bw(base_size = 20)
      
      ggsave(file, plot = gg, device = "png", width = 8, height = 6)
    }
  )
  
  #RADIATION QUALITY
  
  output$dataset5 <- renderUI({
    selectizeInput(inputId = "dataset5",
                   label = NULL,
                   choices = c("Search Bar" = "", datalist),
                   multiple = FALSE,
                   selected = NULL,
                   options = list(
                     create = FALSE,
                     placeholder = "Search dataset (for ex. Cleveland)",
                     maxItems = '1',
                     onDropdownOpen = I("function($dropdown) { this.settings.openOnFocus = true; }"),
                     onType = I("function (str) { if (str === \"\") { this.close(); } else { this.open(); } }")
                   )
    )
  })
  
  output$rad_tissue1 <- renderUI({
    validate(
      need(input$dataset5, "")
    )
    object <- fetch_dataset(input$dataset5)
    
    d <- as.character(as.data.frame(table(object@SampleData$gamma$tissueid))[,1])
    selectInput("rad_tissue1", "Tissue Type:", c("All Tissue Types", d))
  })
  
  output$rad_tissue2 <- renderUI({
    validate(
      need(input$dataset5, "")
    )
    object <- fetch_dataset(input$dataset5)
    
    d <- as.character(as.data.frame(table(object@SampleData$alpha$tissueid))[,1])
    selectInput("rad_tissue2", "Tissue Type:", c("All Tissue Types", d))
  })
  
  output$rad_res1 <- renderUI({
    validate(
      need(input$dataset5, "")
    )
    object <- fetch_dataset(input$dataset5)
    
    d <- colnames(object@DoseResponseData$gamma)[-1]
    selectInput("rad_res1", "Dose Response Type:", d)
  })
  
  output$rad_res2 <- renderUI({
    validate(
      need(input$dataset5, "")
    )
    object <- fetch_dataset(input$dataset5)
    
    d <- colnames(object@DoseResponseData$alpha)[-1]
    selectInput("rad_res2", "Dose Response Type:", d, selected = d[2])
  })
  
  rad_df <- reactive({
    input$submit5
    
    isolate({
      validate(
        need(input$rad_tissue1, ""),
        need(input$rad_tissue2, ""),
        need(input$rad_res1, ""),
        need(input$rad_res2, ""),
      )
      object <- fetch_dataset(input$dataset5)
      
      df1 <- data.frame(Cell_line = object@DoseResponseData$gamma$Cell_line, value = object@DoseResponseData$gamma[,input$rad_res1])
      df1 <- setNames(df1, c("Cell_line", input$rad_res1))
      if (input$rad_tissue1 != "All Tissue Types"){
        lines <- (object@SampleData$gamma[object@SampleData$gamma$tissueid == input$rad_tissue1,])[,1]
        df1 <- df1[df1$Cell_line %in% lines,]
      }
      
      df2 <- data.frame(Cell_line = object@DoseResponseData$alpha$Cell_line, value = object@DoseResponseData$alpha[,input$rad_res2])
      df2 <- setNames(df2, c("Cell_line", input$rad_res2))
      if (input$rad_tissue2 != "All Tissue Types"){
        lines <- (object@SampleData$alpha[object@SampleData$alpha$tissueid == input$rad_tissue2,])[,1]
        df2 <- df2[df2$Cell_line %in% lines,]
      }
      
      df <- merge(df1, df2, by = "Cell_line")
      df[,3] <- as.numeric(df[,3])
      
      return(df)
    })
  })
  
  observeEvent(input$rad_tissue2, {
    validate(
      need(input$dataset5, "")
    )
    object <- fetch_dataset(input$dataset5)
    
    d <- as.character(as.data.frame(table(object@SampleData$gamma$tissueid))[,1])
    if (!input$rad_tissue2 %in% d){
      updateSelectInput(session, "rad_tissue1", selected = "All Tissue Types")
    } else {
      updateSelectInput(session, "rad_tissue1", selected = input$rad_tissue2)
    }
  })
  
  observeEvent(input$rad_tissue1, {
    validate(
      need(input$dataset5, "")
    )
    object <- fetch_dataset(input$dataset5)
    
    d <- as.character(as.data.frame(table(object@SampleData$alpha$tissueid))[,1])
    if (!input$rad_tissue1 %in% d){
      updateSelectInput(session, "rad_tissue2", selected = "All Tissue Types")
    } else {
      updateSelectInput(session, "rad_tissue2", selected = input$rad_tissue1)
    }
  })
  
  output$rad_quality_plot <- renderUI({
    validate(
      need(rad_df(), "")
    )
    isolate({
      if (input$rad_tissue1 == input$rad_tissue2 & nrow(rad_df()) > 4){
        plotlyOutput("rad_chart")
      } else if (nrow(rad_df()) != 0){
        textOutput("rad_low_samples")
      } else {
        textOutput("rad_warning")
      }
    })
  })
  
  output$rad_warning <- renderText({
    if (input$rad_tissue1 == "All Tissue Types"){
      paste("WARNING:", input$dataset5, "does not have data for this radiation type and tissue type")
    } else if (input$rad_tissue2 == "All Tissue Types"){
      paste("WARNING:", input$dataset5, "does not have data for this radiation type and tissue type")
    }
  })
  
  output$rad_low_samples <- renderText({
    "WARNING: Less than 5 common cell lines"
  })
  
  output$rad_chart <- renderPlotly({
    validate(
      need(rad_df(), "")
    )
    df <- rad_df()
    i <- colnames(df)
    colnames(df) <- c("Cell Line", "X", "Y")
    
    cor_coef <- cor(df[,2], df[,3])
    cor_coef <- trunc(cor_coef*10^2)/10^2
    cor_pval <- cor.test(df[,2],df[,3])$p.value
    cor_pval <- formatC(cor_pval, format = "e", digits = 2)
    
    gg <- ggplot(df, aes(x = X, y = Y, label = `Cell Line`)) +
      geom_point(color = "orange") +
      geom_smooth(method = "lm", se = FALSE, alpha = 0.2) +
      geom_text(aes(label = paste("Pearson correlation =", cor_coef, ", Sample size =", nrow(df), ", p-value = ", cor_pval)),
                x = min(df[,2]) + (0.5 * max(df[,2])), y = max(df[,3] + (0.01 * max(df[,3]))), hjust = 1, vjust = 1, size = 5) +
      labs(title = paste("Correlation between Gamma", i[2], "and Alpha", i[3]),
           x = i[2],
           y = i[3]) +
      theme_bw(base_size = 20)
    
    p <- ggplotly(gg)
    
    p
  })
  
  output$export5 <- downloadHandler(
    filename = function() {
      paste("Radiation-Quality-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      validate(
        need(rad_df(), "")
      )
      df <- rad_df()
      i <- colnames(df)
      colnames(df) <- c("Cell Line", "X", "Y")
      
      cor_coef <- cor(df[,2], df[,3])
      cor_coef <- trunc(cor_coef*10^2)/10^2
      cor_pval <- cor.test(df[,2],df[,3])$p.value
      cor_pval <- formatC(cor_pval, format = "e", digits = 2)
      
      gg <- ggplot(df, aes(x = X, y = Y, label = `Cell Line`)) +
        geom_point(color = "orange") +
        geom_smooth(method = "lm", se = FALSE, alpha = 0.2) +
        geom_text(aes(label = paste("Pearson correlation =", cor_coef, ", Sample size =", nrow(df), ", p-value = ", cor_pval)),
                  x = min(df[,2]) + (0.9 * max(df[,2])), y = max(df[,3] + (0.01 * max(df[,3]))), hjust = 1, vjust = 1.5, size = 5) +
        labs(title = paste("Correlation between Gamma", i[2], "and Alpha", i[3]),
             x = i[2],
             y = i[3]) +
        theme_bw(base_size = 20)
      
      ggsave(file, plot = gg, device = "png", width = 8, height = 6)
    }
  )
  
  #HYPOXIA ANALYSIS
  
  output$dataset6 <- renderUI({
    selectizeInput(inputId = "dataset6",
                   label = NULL,
                   choices = c("Search Bar" = "", datalist),
                   multiple = FALSE,
                   selected = NULL,
                   options = list(
                     create = FALSE,
                     placeholder = "Search dataset (for ex. Cleveland)",
                     maxItems = '1',
                     onDropdownOpen = I("function($dropdown) { this.settings.openOnFocus = true; }"),
                     onType = I("function (str) { if (str === \"\") { this.close(); } else { this.open(); } }")
                   )
    )
  })
  
  output$hypoxia_tissue <- renderUI({
    validate(
      need(input$dataset6, "")
    )
    object <- fetch_dataset(input$dataset6)
    
    d <- as.data.frame(table(object@SampleData$gamma$tissueid))[,1]
    selectInput("hypoxia_tissue", "Tissue Type:", c("All Tissue Types", as.character(d)))
  })
  
  output$hypoxia_cell <- renderUI({
    validate(
      need(input$hypoxia_tissue, "")
    )
    object <- fetch_dataset(input$dataset6)
    
    if (input$hypoxia_tissue != "All Tissue Types"){
      d <- as.vector(na.omit((object@SampleData$gamma[object@SampleData$gamma$tissueid == input$hypoxia_tissue,])[,1]))
      d <- intersect(d, object@SensitivityData$gamma$Cell_line)
    } else {
      d <- object@SensitivityData$gamma$Cell_line
    }
    
    selectizeInput("hypoxia_cell", "Cell Line:", d, selected = d[1])
  })
  
  output$hypoxia_chart <- renderPlot({
    input$submit6
    
    isolate({
      validate(
        need(input$hypoxia_cell, "")
      )
      object <- fetch_dataset(input$dataset6)
      
      ds <- as.numeric(gsub("\\D", "", colnames(object@SensitivityData$gamma)[-1]))
      fractions <- as.numeric((object@SensitivityData$gamma[object@SensitivityData$gamma$Cell_line == input$hypoxia_cell,])[-1])
      
      alpha <- linearQuadraticModel(D = ds, SF = fractions)[1]
      beta <- linearQuadraticModel(D = ds, SF = fractions)[2]
      OER_m = 3
      K_m = 3
      
      po2 = 160
      a = ((OER_m * po2) + K_m) / (po2 + K_m)
      OMF = (1 / OER_m) * a
      sf160 <- exp(-alpha * ds * OMF-(beta * ds * ds * OMF))
      
      po2 = input$oxygen
      a = ((OER_m * po2) + K_m) / (po2 + K_m)
      OMF = (1 / OER_m) * a
      sfoxy <- exp(-alpha * ds * OMF - (beta * ds * ds * OMF))
      
      percentage <- round((input$oxygen / 760) * 100)
      hyp <- paste0("Hypoxia (", percentage, "% = ", input$oxygen, "mmHg)")
      
      sfs <- list("Hypoxia" = sf160, "Hypoxia" = sfoxy)
      doses <- list("Normoxia (21% = 160 mmHg)" = ds)
      doses[[hyp]] = ds
      
      fnzd <- function(x){
        x_str <- as.character(x)
        dpos <- regexpr("\\.", x_str)
        
        if (dpos == -1){
          return(NA)
        }
        
        dpart <- substr(x_str, dpos +1, nchar(x_str))
        fnz <- regexpr("[1-9]", dpart)
        
        if (fnz == -1){
          return(NA)
        }
        
        return(fnz[1])
      }
      
      y <- c(10^-fnzd(min(unlist(sfs))), 1.1)
      
      doseResponseCurve(
        Ds = doses,
        SFs = sfs,
        plot.type = "Both",
        cex = 1.2,
        cex.main = 1.2,
        title = paste("Hypoxic response for", input$hypoxia_cell),
        legends.label = NULL,
        ylim = y
      )
    })
  })
  
  output$export6 <- downloadHandler(
    filename = function() {
      paste("Hypoxia-Analysis-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      
      validate(
        need(input$hypoxia_cell, "")
      )
      object <- fetch_dataset(input$dataset6)
      
      ds <- as.numeric(gsub("\\D", "", colnames(object@SensitivityData$gamma)[-1]))
      fractions <- as.numeric((object@SensitivityData$gamma[object@SensitivityData$gamma$Cell_line == input$hypoxia_cell,])[-1])
      
      alpha <- linearQuadraticModel(D = ds, SF = fractions)[1]
      beta <- linearQuadraticModel(D = ds, SF = fractions)[2]
      OER_m = 3
      K_m = 3
      
      po2 = 160
      a = ((OER_m * po2) + K_m) / (po2 + K_m)
      OMF = (1 / OER_m) * a
      sf160 <- exp(-alpha * ds * OMF-(beta * ds * ds * OMF))
      
      po2 = input$oxygen
      a = ((OER_m * po2) + K_m) / (po2 + K_m)
      OMF = (1 / OER_m) * a
      sfoxy <- exp(-alpha * ds * OMF - (beta * ds * ds * OMF))
      
      percentage <- round((input$oxygen / 760) * 100)
      hyp <- paste0("Hypoxia (", percentage, "% = ", input$oxygen, "mmHg)")
      
      sfs <- list("Hypoxia" = sf160, "Hypoxia" = sfoxy)
      doses <- list("Normoxia (21% = 160 mmHg)" = ds)
      doses[[hyp]] = ds
      
      fnzd <- function(x){
        x_str <- as.character(x)
        dpos <- regexpr("\\.", x_str)
        
        if (dpos == -1){
          return(NA)
        }
        
        dpart <- substr(x_str, dpos +1, nchar(x_str))
        fnz <- regexpr("[1-9]", dpart)
        
        if (fnz == -1){
          return(NA)
        }
        
        return(fnz[1])
      }
      
      y <- c(10^-fnzd(min(unlist(sfs))), 1.1)
      
      doseResponseCurve(
        Ds = doses,
        SFs = sfs,
        plot.type = "Both",
        cex = 1.2,
        cex.main = 1.2,
        title = paste("Hypoxic response for", input$hypoxia_cell),
        legends.label = NULL,
        ylim = y
      )
      
      dev.off()
    }
  )
  
  #DEG
  
  output$gdataset1 <- renderUI({
    selectizeInput(inputId = "gdataset1",
                   label = NULL,
                   choices = c("Search Bar" = "", datalist),
                   multiple = FALSE,
                   selected = NULL,
                   options = list(
                     create = FALSE,
                     placeholder = "Search dataset (for ex. Cleveland)",
                     maxItems = '1',
                     onDropdownOpen = I("function($dropdown) { this.settings.openOnFocus = true; }"),
                     onType = I("function (str) { if (str === \"\") { this.close(); } else { this.open(); } }")
                   )
    )
  })
  
  observe({
    validate(
      need(input$gdataset1, "")
    )
    object <- fetch_dataset(input$gdataset1)
    
    d <- c(na.omit(object@GeneExpressionData$symbol), object@GeneExpressionData$gene)
    updateSelectizeInput(
      session, 
      "gea_gene",
      choices = d,
      server = TRUE
    )
  })
  
  output$gea_tissue <- renderUI({
    validate(
      need(input$gdataset1, "")
    )
    object <- fetch_dataset(input$gdataset1)
    
    d <- as.character(as.data.frame(table(object@SampleData$gamma$tissueid))[,1])
    selectInput("gea_tissue", "Tissue Type:", d)
  })
  
  output$gea_res <- renderUI({
    validate(
      need(input$gdataset1, "")
    )
    object <- fetch_dataset(input$gdataset1)
    
    d <- colnames(object@DoseResponseData$gamma)[-1]
    selectInput("gea_res", "Dose Response Type:", d)
  })
  
  gea_samples_status <- reactive({
    input$gsubmit1
    
    isolate({
      validate(
        need(input$gea_tissue, "")
      )
      object <- fetch_dataset(input$gdataset1)
      
      samples <- na.omit((object@SampleData$gamma[object@SampleData$gamma$tissueid == input$gea_tissue,])$sampleid)
      samples <- intersect(samples, colnames(object@GeneExpressionData))
      samples <- length(intersect(samples, object@DoseResponseData$gamma$Cell_line))
      
      if (samples < 20){
        return(1)
      } else {
        return(0)
      }
    })
  })
  
  gea_vector <- reactive({
    input$gsubmit1
    
    
      validate(
        need(input$gea_gene, ""),
        need(input$gea_tissue, ""),
        need(input$gea_res, "")
      )
      object <- fetch_dataset(input$gdataset1)
      if (grepl("[a-zA-Z]", input$gea_gene)){
        gene <- na.omit((object@GeneExpressionData[object@GeneExpressionData$symbol == input$gea_gene,])$gene)
      } else {
        gene <- input$gea_gene
      }
      
      lines <- na.omit((object@SampleData$gamma[object@SampleData$gamma$tissueid == input$gea_tissue,])$sampleid)
      lines <- intersect(lines, colnames(object@GeneExpressionData)[-c(1,2)])
      lines <- intersect(lines, object@DoseResponseData$gamma$Cell_line)
      
      res <- as.data.frame(object@DoseResponseData$gamma[, input$gea_res])
      rownames(res) <- object@DoseResponseData$gamma$Cell_line
      colnames(res) <- "R"
      res <- subset(res, rownames(res) %in% lines)
      
      median <- median(res$R)
      lines_low <- rownames(subset(res, res$R < median))
      lines_high <- rownames(subset(res, res$R >= median))
      
      big_vector <- (object@GeneExpressionData[object@GeneExpressionData$gene == gene,])[-c(1,2)]
      
      low <- big_vector[, lines_low]
      high <- big_vector[, lines_high]
      df <- cbind(low, high)
      rownames(df) <- (ncol(low))
      return(df)
    
  })
  
  output$gea_plot <- renderUI({
    validate(
      need(gea_samples_status(), "")
    )
    
    isolate({
      if (gea_samples_status() != 0) {
        textOutput("gea_low_samples")
      } else {
        plotlyOutput("gea_chart")
      }
    })
  })
  
  output$gea_low_samples <- renderText({
    paste("WARNING: Less than 10 samples in tissue group")
  })
  
  output$gea_chart <- renderPlotly({
    validate(
      need(gea_vector(), "")
    )
    object <- fetch_dataset(input$gdataset1)
    if (grepl("[a-zA-Z]", input$gea_gene)){
      gene <- na.omit((object@GeneExpressionData[object@GeneExpressionData$symbol == input$gea_gene,])$gene)
    } else {
      gene <- input$gea_gene
    }
    
    df <- gea_vector()
    n <- as.numeric(rownames(df)[1])
    
    df1 <- df[, 1:n]
    df2 <- df[, (n+1):ncol(df)]
    
    df1 <- df1 %>%
      pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
      mutate(Source = "Radiation Resistant")
    
    df2 <- df2 %>%
      pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
      mutate(Source = "Radiation Sensitive")
    
    combined <- rbind(df1, df2)
    
    statistic <- t.test(df1$Value, df2$Value)$statistic
    statistic <- trunc(statistic * 10 ^ 2) / 10 ^ 2
    p.value <- t.test(df1$Value, df2$Value)$p.value
    p.value <- formatC(p.value, digits = 2, format = "e")
    
    stars <- ifelse(p.value < 0.001, "***", ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "ns")))
    y_position <- max(combined$Value) * 1.1
    
    p <- plot_ly(
      data = combined, y = ~Value, color = ~Source, facet_col = ~Column,
      text = paste0(colnames(df), "<br>Gene Expression: ", combined$Value),
      type = "box", hoverinfo = "text", source = "gea_chart",
      boxpoints = "all", jitter = 2.0
    ) %>%
      # add_trace(
      #   y = ~Value,
      #   color = ~Source,
      #   type = "scatter",
      #   mode = "markers",
      #   marker = list(
      #     opacity = 0.5,
      #     line = list(
      #       width = 0.5,
      #       color = "black"
      #     )
      #   )
      # ) %>%
      layout(
        title = paste0("Gene Expression of ", input$gea_gene, " Grouped by Radiation Resistance"),
        xaxis = list(
          title = ""
        ),
        yaxis = list(
          title = list(
            text = "Gene Expression",
            font = list(size = 16)
          )
        ),
        shapes = list(
          list(
            type = "line",
            x0 = 0.2, x1 = 0.8,
            y0 = y_position, y1 = y_position,
            line = list(color = "black", width = 1.5)
          )
        ),
        annotations = list(
          list(
            x = 0.5,
            y = y_position * 1.05,
            text = stars,
            showarrow = FALSE,
            font = list(size = 16, color = "black"),
            align = "center"
          )
        )
      )
  })
  
  output$gexport1 <- downloadHandler(
    filename = function() {
      paste("Gene-Expression-Analysis-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      validate(
        need(gea_vector(), "")
      )
      object <- fetch_dataset(input$gdataset1)
      if (grepl("[a-zA-Z]", input$gea_gene)){
        gene <- na.omit((object@GeneExpressionData[object@GeneExpressionData$symbol == input$gea_gene,])$gene)
      } else {
        gene <- input$gea_gene
      }
      
      df <- gea_vector()
      n <- as.numeric(rownames(df)[1])
      
      df1 <- df[, 1:n]
      df2 <- df[, (n+1):ncol(df)]
      
      df1 <- df1 %>%
        pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
        mutate(Source = "Radiation Resistant")
      
      df2 <- df2 %>%
        pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
        mutate(Source = "Radiation Sensitive")
      
      combined <- rbind(df1, df2)
      
      statistic <- t.test(df1$Value, df2$Value)$statistic
      statistic <- trunc(statistic * 10 ^ 2) / 10 ^ 2
      p.value <- t.test(df1$Value, df2$Value)$p.value
      p.value <- formatC(p.value, digits = 2, format = "e")
      
      p <- plot_ly(
        data = combined, y = ~Value, color = ~Source, facet_col = ~Column,
        text = paste0(colnames(df), "<br>Gene Expression: ", combined$Value),
        type = "box", hoverinfo = "text", source = "gea_chart",
        boxpoints = "all", jitter = 2.0
      ) %>%
        # add_trace(
        #   y = ~Value,
        #   color = ~Source,
        #   type = "scatter",
        #   mode = "markers",
        #   marker = list(
        #     opacity = 0.5,
        #     line = list(
        #       width = 0.5,
        #       color = "black"
        #     )
        #   )
        # ) %>%
        layout(
          title = paste0("Gene Expression of ", gene, " Grouped by Radiation Resistance"),
          xaxis = list(
            title = ""
          ),
          yaxis = list(
            title = list(
              text = "Gene Expression",
              font = list(size = 16)
            )
          ),
          annotations = list(
            list(
              x = 0,
              y = 1,
              xref = "paper",
              yref = "paper",
              text = paste0("T-test statistic: ", statistic, ", p-value: ", p.value),
              showarrow = FALSE,
              font = list(size = 14, color = "black"),
              align = "left"
            )
          )
        )
      
      plotly::export(p, file = file)
    }
  )
  
  #GENE CORRELATION ANALYSIS
  
  output$gdataset2 <- renderUI({
    selectizeInput(inputId = "gdataset2",
                   label = NULL,
                   choices = c("Search Bar" = "", datalist),
                   multiple = FALSE,
                   selected = NULL,
                   options = list(
                     create = FALSE,
                     placeholder = "Search dataset (for ex. Cleveland)",
                     maxItems = '1',
                     onDropdownOpen = I("function($dropdown) { this.settings.openOnFocus = true; }"),
                     onType = I("function (str) { if (str === \"\") { this.close(); } else { this.open(); } }")
                   )
    )
  })
  
  output$gca_tissue <- renderUI({
    validate(
      need(input$gdataset2, "")
    )
    object <- fetch_dataset(input$gdataset2)
    
    d <- as.character(as.data.frame(table(object@SampleData$gamma$tissueid))[,1])
    selectInput("gca_tissue", "Tissue Type:", d)
  })
  
  observe({
    validate(
      need(input$gdataset2, ""),
      need(input$gca_tissue, "")
    )
    object <- fetch_dataset(input$gdataset2)
    
    lines <- unlist(na.omit((object@SampleData$gamma[object@SampleData$gamma$tissueid == input$gca_tissue,])$sampleid))
    lines <- intersect(lines, colnames(object@GeneExpressionData)[-c(1,2)])
    gene_df <- object@GeneExpressionData[, lines]
    x <- data.frame(
      gene = object@GeneExpressionData$gene,
      symbol = object@GeneExpressionData$symbol
    )
    min <- ceiling(ncol(gene_df) * 0.5)
    validity <- rowSums(gene_df > 0) >= min
    
    gene_df <- cbind(x, gene_df)
    new_df <- gene_df[validity, , drop = FALSE]
    
    d <- c(na.omit(new_df$symbol), new_df$gene)
    updateSelectizeInput(
      session, 
      "gca_gene",
      choices = d,
      server = TRUE
    )
  })
  
  output$gca_res <- renderUI({
    validate(
      need(input$gdataset2, "")
    )
    object <- fetch_dataset(input$gdataset2)
    
    d <- colnames(object@DoseResponseData$gamma)[-1]
    selectInput("gca_res", "Dose Response Type:", d)
  })
  
  gca_samples_status <- reactive({
    input$gsubmit2
    
    isolate({
      validate(
        need(input$gca_tissue, "")
      )
      object <- fetch_dataset(input$gdataset2)
      
      samples <- na.omit((object@SampleData$gamma[object@SampleData$gamma$tissueid == input$gca_tissue,])$sampleid)
      samples <- intersect(samples, colnames(object@GeneExpressionData))
      samples <- length(intersect(samples, object@DoseResponseData$gamma$Cell_line))
      
      if (samples < 10){
        return(1)
      } else {
        return(0)
      }
    })
  })
  
  gca_df <- reactive({
    input$gsubmit2
    
    isolate({
      validate(
        need(input$gca_gene, ""),
        need(input$gca_tissue, ""),
        need(input$gca_res, "")
      )
      object <- fetch_dataset(input$gdataset2)
      if (grepl("[a-zA-Z]", input$gca_gene)){
        gene <- na.omit((object@GeneExpressionData[object@GeneExpressionData$symbol == input$gca_gene,])$gene)
      } else {
        gene <- input$gca_gene
      }
      
      lines <- na.omit((object@SampleData$gamma[object@SampleData$gamma$tissueid == input$gca_tissue,])$sampleid)
      lines <- intersect(lines, colnames(object@GeneExpressionData)[-c(1,2)])
      lines <- intersect(lines, object@DoseResponseData$gamma$Cell_line)
      
      big_df <- object@GeneExpressionData[object@GeneExpressionData$gene == gene,]
      gene_exp <- t(big_df[, lines])
      
      res <- as.data.frame(object@DoseResponseData$gamma[, input$gca_res])
      rownames(res) <- object@DoseResponseData$gamma$Cell_line
      res <- subset(res, rownames(res) %in% lines)
      
      merged <- merge(gene_exp, res, by = "row.names", all.x = TRUE)
      rownames(merged) <- merged$Row.names
      merged <- merged[, -1]
      colnames(merged) <- c(input$gca_gene, input$gca_res)
      
      return(merged)
    })
  })
  
  output$gca_plot <- renderUI({
    validate(
      need(gca_samples_status(), "")
    )
    
    isolate({
      if (gca_samples_status() != 0) {
        textOutput("gca_low_samples")
      } else {
        plotlyOutput("gca_chart")
      }
    })
  })
  
  output$gca_low_samples <- renderText({
    paste("WARNING: Less than 10 samples in tissue group")
  })
  
  output$gca_chart <- renderPlotly({
    validate(
      need(gca_df(), "")
    )
    df <- gca_df()
    i <- colnames(df)
    colnames(df) <- c("X", "Y")
    df <- cbind(df, `Cell Line` = rownames(df))
    
    cor_coef <- cor(df[, 1], df[, 2])
    cor_coef <- trunc(cor_coef * 10 ^ 2) / 10 ^ 2
    cor_pval <- cor.test(df[, 1], df[, 2])$p.value
    cor_pval <- formatC(cor_pval, format = "e", digits = 2)
    
    gg <- ggplot(df, aes(label = `Cell Line`, x = X, y = Y)) +
      geom_point(color = "orange") +
      geom_smooth(method = "lm", se = FALSE, alpha = 0.2) +
      geom_text(aes(label = paste("Pearson correlation =", cor_coef, ", Sample size =", nrow(df), ", p-value = ", cor_pval)),
                x = min(df[, 1]) + (0.5 * (max(df[, 1]) - min(df[, 1]))), y = max(df[, 2] + (0.01 * max(df[, 2]))), hjust = 1, vjust = 1, size = 5) +
      labs(title = paste("Correlation between", i[1], "and", input$gca_tissue, i[2]),
           x = i[1],
           y = i[2]) +
      theme_bw(base_size = 20)
    
    p <- ggplotly(gg)
    
    # p <- p %>%
    #   style(hoverinfo = "text", text = paste0(rownames(df), "<br>Gene Expression: ", df[, 1], "<br>", input$gca_res, ": ", df[, 2]))

    # lm_fit <- lm(df[, 2] ~ df[, 1])
    # reg_line <- data.frame(x = seq(min(df[, 1]), max(df[, 1]), length.out = nrow(df)))
    # reg_line$y <- predict(lm_fit, newdata = reg_line)
    # 
    # p <- plot_ly(df, x = df[, 1], y = df[, 2], type = 'scatter', mode = 'markers', marker = list(color = 'orange'),
    #              text = paste("Gene Expression: ", df[, 1], "<br>Label: ", df[, 2])) %>%
    #   add_trace(x = ~reg_line$x, y = ~reg_line$y, type = 'scatter', mode = 'lines', line = list(color = 'blue')) %>%
    #   layout(title = paste("Correlation between", i[1], "and", i[2]),
    #          xaxis = list(title = i[1]),
    #          yaxis = list(title = i[2]))
    
    p
  })
  
  #PA
  
  output$gdataset3 <- renderUI({
    selectizeInput(inputId = "gdataset3",
                   label = NULL,
                   choices = c("Search Bar" = "", datalist),
                   multiple = FALSE,
                   selected = NULL,
                   options = list(
                     create = FALSE,
                     placeholder = "Search dataset (for ex. Cleveland)",
                     maxItems = '1',
                     onDropdownOpen = I("function($dropdown) { this.settings.openOnFocus = true; }"),
                     onType = I("function (str) { if (str === \"\") { this.close(); } else { this.open(); } }")
                   )
    )
  })
  
  output$pa_res <- renderUI({
    validate(
      need(input$pa_rad, ""),
      need(input$gdataset3, "")
    )
    object <- fetch_dataset(input$gdataset3)
    rad <- tolower(input$pa_rad)
    
    d <- colnames(object@DoseResponseData[[rad]])[-1]
    selectInput("pa_res", "Dose Response Type:", d)
  })
  
  output$pa_tissue <- renderUI({
    validate(
      need(input$gdataset3, "")
    )
    object <- fetch_dataset(input$gdataset3)
    
    d <- as.character(as.data.frame(table(object@SampleData$gamma$tissueid))[,1])
    selectInput("pa_tissue", "Tissue Type:", d)
  })
  
  pa_samples_status <- reactive({
    input$gsubmit3
    
    isolate({
      validate(
        need(input$pa_rad, ""),
        need(input$pa_tissue, "")
      )
      object <- fetch_dataset(input$gdataset3)
      
      if (input$pa_rad == "Gamma"){
        lines <- na.omit((object@SampleData$gamma[object@SampleData$gamma$tissueid == input$pa_tissue,])$sampleid)
        lines <- length(intersect(lines, colnames(object@GeneExpressionData)))
      
        if (lines < 20){
          return(1)
        } else {
          return(0)
        }
      } else if (input$pa_rad == "Alpha"){
        return(0)
      }
    })
  })
  
  pa_df <- reactive({
    input$gsubmit3
    
    isolate({
      validate(
        need(input$pa_rad, ""),
        need(input$pa_res, "")
      )
      object <- fetch_dataset(input$gdataset3)
      
      if (pa_samples_status() == 1){
        return()
      }
      
      if (input$pa_db == "kegg"){
        pd <- kegg
      } else if (input$pa_db == "reactome"){
        pd <- reactome
      }
      
      progress <- Progress$new()
      on.exit(progress$close())
      
      progress$set(message = "Pathway analysis", value = 0)
      
      if (input$pa_rad == "Gamma"){
        validate(
          need(input$pa_tissue, "")
        )
        
        lines <- na.omit((object@SampleData$gamma[object@SampleData$gamma$tissueid == input$pa_tissue,])$sampleid)
        lines <- intersect(lines, colnames(object@GeneExpressionData))
        lines <- intersect(lines, object@DoseResponseData$gamma$Cell_line)
        
        df1 <- object@GeneExpressionData[, -c(1,2)]
        df1 <- df1[, lines]
        
        min <- ceiling(ncol(df1) * 0.5)
        validity <- rowSums(df1 > 0) >= min
        
        rownames(df1) <- object@GeneExpressionData$gene
        df1 <- df1[validity, , drop = FALSE]
        
        
        df2 <- object@DoseResponseData$gamma[object@DoseResponseData$gamma$Cell_line %in% lines,]
        df1 <- df1[, df2$Cell_line]
        
        cor.val <- as.numeric()
        for (i in 1:nrow(object@GeneExpressionData)){
          cor.val[i] <- as.numeric(cor(unlist(df1[i,]), df2[, input$pa_res], method = "spearman"))
          progress$inc(1 / (3 * nrow(object@GeneExpressionData)), )
        }
        metric <- data.frame(object@GeneExpressionData$gene, cor.val)
        colnames(metric) <- c("gene", "cor")
        metric <- na.omit(metric)
        
        stats <- as.vector(metric$cor)
        names(stats) <- as.vector(metric$gene)
        gsa.kegg <- runGSA(geneLevelStats = stats, gsc = pd, nPerm = 1000, geneSetStat = "gsea", adjMethod = "none")
        gsasummary <- GSAsummaryTable(gsa.kegg)
        gsea <- gsasummary[order(gsasummary[, 4]),]
        
        gsea <- gsea[, c("Name", "Genes (tot)", "Stat (dist.dir)", "p (dist.dir.up)", "p (dist.dir.dn)", "Genes (up)", "Genes (down)")]
        gsea[, "pval"] <- rowSums(cbind(gsea[, 4], gsea[, 5]), na.rm = TRUE)
        gsea[which(as.numeric(gsea[, "pval"]) == "0"), "pval"] <- 1 / (1000 + 1)
        gsea[, "FDR"] <- p.adjust(gsea[, "pval"], method = "fdr", length(gsea[, "pval"]))
        gsea <- gsea[, -c(4,5)]
        
        gsea[, "group"] <- "Not Significant"
        
        progress$inc(1 / 3, detail = "generating plot...")
        return(gsea)
        
      } else if (input$pa_rad == "Alpha"){
        lines <- intersect(object@DoseResponseData$alpha$Cell_line, colnames(object@GeneExpressionData))
        
        df1 <- object@GeneExpressionData[, -c(1,2)]
        df1 <- df1[, lines]
        
        df2 <- object@DoseResponseData$alpha[object@DoseResponseData$alpha$Cell_line %in% lines,]
        df1 <- df1[, df2$Cell_line]
        
        cor_res <- as.numeric(unlist(df2[, input$pa_res]))
        
        cor.val <- as.numeric()
        for (i in 1:nrow(object@GeneExpressionData)){
          cor.val[i] <- as.numeric(cor(unlist(df1[i,]), cor_res, method = "spearman"))
          progress$inc(1 / (3 * nrow(object@GeneExpressionData)), )
        }
        metric <- data.frame(object@GeneExpressionData$gene, cor.val)
        colnames(metric) <- c("gene", "cor")
        metric <- na.omit(metric)
        
        stats <- as.vector(metric$cor)
        names(stats) <- as.vector(metric$gene)
        gsa.kegg <- runGSA(geneLevelStats = stats, gsc = pd, nPerm = 1000, geneSetStat = "gsea", adjMethod = "none")
        gsasummary <- GSAsummaryTable(gsa.kegg)
        gsea <- gsasummary[order(gsasummary[, 4]),]
        
        gsea <- gsea[, c("Name", "Genes (tot)", "Stat (dist.dir)", "p (dist.dir.up)", "p (dist.dir.dn)", "Genes (up)", "Genes (down)")]
        gsea[, "pval"] <- rowSums(cbind(gsea[, 4], gsea[, 5]), na.rm = TRUE)
        gsea[which(as.numeric(gsea[, "pval"]) == "0"), "pval"] <- 1 / (1000 + 1)
        gsea[, "FDR"] <- p.adjust(gsea[, "pval"], method = "fdr", length(gsea[, "pval"]))
        gsea <- gsea[, -c(4,5)]
        
        gsea[, "group"] <- "Not Significant"
        
        progress$inc(1 / 3, detail = "generating plot...")
        return(gsea)
      }
    })
  })
  
  output$pa_plot <- renderUI({
    validate(
      need(pa_samples_status(), "")
    )
    
    isolate({
      if (pa_samples_status() != 0) {
        textOutput("pa_low_samples")
      } else {
        plotlyOutput("pa_chart")
      }
    })
  })
  
  output$pa_low_samples <- renderText({
    paste("WARNING: Less than 20 samples in tissue group")
  })
  
  output$pa_chart <- renderPlotly({
    validate(
      need(pa_df(), "")
    )
    df <- pa_df()
    
    decimal <- as.numeric(input$pa_significance) / 100
    df[which(df$FDR < decimal & df[, 3] > 0), "group"] <- "Significant and Positively-Correlated"
    df[which(df$FDR < decimal & df[, 3] < 0), "group"] <- "Significant and Negatively-Correlated"
    shades = c("Significant and Positively-Correlated" = "maroon", "Significant and Negatively-Correlated" = "blue", "Not Significant" = "gray")
    
    df$negLogP <- -log10(df$FDR)
    xlimit <- max(abs(min(df[, 3])), abs(max(df[, 3])))
    
    p <- plot_ly(
      data = df, x = df[, 3], y = ~negLogP, color = ~group, colors = shades,
      text = paste(df$Name, "<br>Enrichment Score:", trunc(df[, 3] * 10 ^ 2) / 10 ^ 2, "<br>FDR:", formatC(df$FDR, format = "e", digits = 2)),
      type = "scatter",
      mode = "markers",
      marker = list(
        opacity = 0.5,
        line = list(
          width = 0.5,
          color = "black"
        )
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste0("Enrichment Score of Pathways (", input$pa_res, ")"),
        xaxis = list(
          title = "Enrichment score",
          range = c(-xlimit, xlimit)
        ),
        yaxis = list(
          title = "-Log 10 FDR"
        )
      )
  })
  
  #PA
  
  # output$gdataset4 <- renderUI({
  #   selectizeInput(inputId = "gdataset4",
  #                  label = NULL,
  #                  choices = c("Search Bar" = "", datalist),
  #                  multiple = FALSE,
  #                  selected = NULL,
  #                  options = list(
  #                    create = FALSE,
  #                    placeholder = "Search dataset (for ex. Cleveland)",
  #                    maxItems = '1',
  #                    onDropdownOpen = I("function($dropdown) { this.settings.openOnFocus = true; }"),
  #                    onType = I("function (str) { if (str === \"\") { this.close(); } else { this.open(); } }")
  #                  )
  #   )
  # })
  # 
  # output$pa1_tissue <- renderUI({
  #   validate(
  #     need(input$gdataset4, "")
  #   )
  #   object <- fetch_dataset(input$gdataset4)
  #   
  #   d <- as.character(as.data.frame(table(object@SampleData$gamma$tissueid))[,1])
  #   selectInput("pa_tissue", "Tissue Group 1:", d)
  # })
  # 
  # output$pa1_res <- renderUI({
  #   validate(
  #     need(input$gdataset4, "")
  #   )
  #   object <- fetch_dataset(input$gdataset4)
  #   
  #   d <- colnames(object@DoseResponseData$gamma)[-1]
  #   selectInput("pa_res", "Dose Response Type:", d)
  # })
  # 
  # pa1_samples_status <- reactive({
  #   input$gsubmit4
  #   
  #   isolate({
  #     validate(
  #       need(input$pa_tissue, ""),
  #       need(input$pa_res, "")
  #     )
  #     object <- fetch_dataset(input$gdataset4)
  #     
  #     lines <- na.omit((object@SampleData$gamma[object@SampleData$gamma$tissueid == input$pa_tissue,])$sampleid)
  #     lines <- intersect(lines, colnames(object@GeneExpressionData))
  #     lines <- length(intersect(lines, object@DoseResponseData$gamma$Cell_line))
  #     
  #     if (lines < 10){
  #       return(1)
  #     } else {
  #       return(0)
  #     }
  #   })
  # })
  # 
  # pa1_df <- reactive({
  #   input$gsubmit4
  #   
  #   validate(
  #     need(input$pa_tissue, ""),
  #     need(input$pa_res, ""),
  #     need(input$pa_db, "")
  #   )
  #   if (pa_samples_status() == 1){
  #     return()
  #   }
  #     
  #   object <- fetch_dataset(input$gdataset4)
  #   
  #   progress <- Progress$new()
  #   on.exit(progress$close())
  #   
  #   progress$set(message = "Pathway analysis", value = 0)
  #   
  #   if (input$pa_db == "kegg"){
  #     pd <- loadGSC(dbReadTable(db, "kegg"))
  #   } else if (input$pa_db == "reactome"){
  #     pd <- loadGSC(dbReadTable(db, "reactome"))
  #   }
  #   
  #   lines <- na.omit((object@SampleData$gamma[object@SampleData$gamma$tissueid == input$pa_tissue,])$sampleid)
  #   lines <- intersect(lines, colnames(object@GeneExpressionData))
  #   lines <- intersect(lines, object@DoseResponseData$gamma$Cell_line)
  #   
  #   df1 <- object@GeneExpressionData[, -c(1,2)]
  #   df1 <- df1[, lines]
  #   
  #   df2 <- object@DoseResponseData$gamma[object@DoseResponseData$gamma$Cell_line %in% lines,]
  #   df1 <- df1[, df2$Cell_line]
  #   
  #   cor.val <- as.numeric()
  #   for (i in 1:nrow(object@GeneExpressionData)){
  #     cor.val[i] <- as.numeric(cor(unlist(df1[i,]), df2[, input$pa_res], method = "spearman"))
  #     progress$inc(1 / (3 * nrow(object@GeneExpressionData)), )
  #   }
  #   metric <- data.frame(object@GeneExpressionData$gene, cor.val)
  #   colnames(metric) <- c("gene", "cor")
  #   metric <- na.omit(metric)
  #   
  #   stats <- as.vector(metric$cor)
  #   names(stats) <- as.vector(metric$gene)
  #   gsa.kegg <- runGSA(geneLevelStats = stats, gsc = pd, nPerm = 1000, geneSetStat = "gsea", adjMethod = "none")
  #   gsasummary <- GSAsummaryTable(gsa.kegg)
  #   gsea <- gsasummary[order(gsasummary[, 4]),]
  #   
  #   gsea <- gsea[, c("Name", "Genes (tot)", "Stat (dist.dir)", "p (dist.dir.up)", "p (dist.dir.dn)", "Genes (up)", "Genes (down)")]
  #   gsea[, "pval"] <- rowSums(cbind(gsea[, 4], gsea[, 5]), na.rm = TRUE)
  #   gsea[which(as.numeric(gsea[, "pval"]) == "0"), "pval"] <- 1 / (1000 + 1)
  #   gsea[, "FDR"] <- p.adjust(gsea[, "pval"], method = "fdr", length(gsea[, "pval"]))
  #   gsea <- gsea[, -c(4,5)]
  #   
  #   gsea[, "group"] <- "Not Significant"
  #   
  #   progress$inc(1 / 3, detail = "generating plot...")
  #   return(gsea)
  # })
  # 
  # output$pa1_plot <- renderUI({
  #   validate(
  #     need(pa_samples_status(), "")
  #   )
  #   
  #   isolate({
  #     if (pa1_samples_status() != 0){
  #       textOutput("pa_low_samples")
  #     } else {
  #       plotlyOutput("pa_chart")
  #     }
  #   })
  # })
  # 
  # output$pa1_low_samples <- renderText({
  #   "WARNING: Less than 10 samples in tissue group"
  # })
  # 
  # output$pa1_chart <- renderPlotly({
  #   validate(
  #     need(pa_df(), "")
  #   )
  #   df <- pa_df()
  #   
  #   decimal <- as.numeric(input$pa_significance) / 100
  #   df[which(df$FDR < decimal & df[, 3] > 0), "group"] <- "Significant and Up-Regulated"
  #   df[which(df$FDR < decimal & df[, 3] < 0), "group"] <- "Significant and Down-Regulated"
  #   shades = c("Significant and Up-Regulated" = "maroon", "Significant and Down-Regulated" = "blue", "Not Significant" = "gray")
  #   
  #   df$negLogP <- -log10(df$FDR)
  #   xlimit <- max(abs(min(df[, 3])), abs(max(df[, 3])))
  #   
  #   p <- plot_ly(
  #     data = df, x = df[, 3], y = ~negLogP, color = ~group, colors = shades,
  #     text = paste(df$Name, "<br>Enrichment Score:", trunc(df[, 3] * 10 ^ 2) / 10 ^ 2, "<br>FDR:", formatC(df$FDR, format = "e", digits = 2)),
  #     type = "scatter",
  #     mode = "markers",
  #     marker = list(
  #       opacity = 0.5,
  #       line = list(
  #         width = 0.5,
  #         color = "black"
  #       )
  #     ),
  #     hoverinfo = "text"
  #     ) %>%
  #       layout(
  #         title = paste0("Enrichment Score of Pathways (", input$pa_res, ")"),
  #         xaxis = list(
  #           title = "Enrichment score",
  #           range = c(-xlimit, xlimit)
  #         ),
  #         yaxis = list(
  #           title = "-Log 10 FDR"
  #         )
  #       )
  # })
}
