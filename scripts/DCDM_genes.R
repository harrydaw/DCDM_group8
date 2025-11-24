
library(DT)
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
library(heatmaply)


#connect csv files to the rshiny

data <- read.csv("../data/merged/all_clean_data_mutated.csv", stringsAsFactors = FALSE)

phenotype_groups <- read.csv("../data/parameter_groups_2.csv", stringsAsFactors = FALSE)


phenotype_groups <- phenotype_groups %>% rename(parameter_name = Parameter)
#clean parameter_name
data <- data %>% mutate(parameter_name = trimws(parameter_name),
                        parameter_name = tolower(parameter_name))

phenotype_groups<- phenotype_groups %>%
  mutate(parameter_name = trimws(parameter_name), parameter_name = tolower(parameter_name))


#assign columns from csv to specific variables

data <- left_join(data, phenotype_groups, by = "parameter_name")

get_pvalue <- function(data, gene_symbol){
  #take the data and filter it and then select the appropriate columns from csv
  return(data %>% filter(gene_symbol == !!gene_symbol) %>% select(parameter_name, pvalue, Group))
  
  
}

get_phen <- function(data, parameter_name){
  return(data %>% filter(parameter_name == !!parameter_name) %>% select(gene_symbol,pvalue))
  
  
}


# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Mouse Phenotype Explorer"),

    # Sidebar with an input selection 
    sidebarLayout(
      #give user a selection panel for the type of viewer to run
        sidebarPanel(
            selectInput("view",
                        "Select Phenotype Viewer:",
                        choices = c("View by Gene", "View Statistical Score", "Visualise Gene Clusters"))
        ,
        conditionalPanel(
          #scatter plot or heatmap option for view by gene and only significant scores option
          condition = "input.view == 'View by Gene'",
          selectInput("gene", "Select Knockout Mouse:", choices = NULL),
          selectInput("phenotype_group", "Select Phenotype:", choices = NULL),
          radioButtons("plot_choice", "Select the Plot You Wish to View:",
                       choices = c("Scatterplot", "Heatmap"),
                       selected = "Scatterplot"),
          checkboxInput("only_significant", "Show only Phenotypes with Significant Scores (p < 0.05)", value = FALSE)
        ),
        conditionalPanel(
          condition = "input.view == 'View Statistical Score'",
          selectInput("phenotype_groups_scores", "Select Phenotype Group:", choices = NULL),
          radioButtons("pvalue_display", "Choose Your Display Type:",
                       choices = c("Scatterplot of All P Values in Group", "Single Parameter P Value"), selected = "Scatterplot of All P Values in Group"),
          conditionalPanel(
            condition = "input.pvalue_display == 'Single Parameter P Value'",
            uiOutput("phenotype_parameter_pvalue") 
            )
        ),
          
          
          
        ),
        
        
     

        # Show a plot of the generated distribution
        mainPanel(
          conditionalPanel(
            condition = "input.view == 'View Statistical Score' && input.pvalue_display == 'Single Parameter P Value'",
            textOutput("singlePvaluecontent")
          ),
           
          conditionalPanel(
            condition = "input.view == 'Visualise Gene Clusters'",
            plotlyOutput("clusterPlot")
          ),
          conditionalPanel(
            condition = "input.view != 'Visualise Gene Clusters'",
            plotlyOutput("mainPlot")
            
          ),
           DTOutput("dataTable")
        )
    )
)



server <- function(input, output, session) {

    observe({
      updateSelectInput(session, "gene", choices = unique(data$gene_symbol))
      updateSelectInput(session, "phenotype", choices = unique(data$parameter_name))
      updateSelectInput(session, "phenotype_group", choices = c("All", sort(unique(data$Group))))
      updateSelectInput(session, "phenotype_groups_scores", choices = sort(unique(data$Group)))
    })
  
  #VIEW BY GENE OPTION
  gene_info <- reactive({
    req(input$gene)
    df <- get_pvalue(data, input$gene)
    
    if (!is.null(input$phenotype_group) && input$phenotype_group != "All") {
      df <- df %>% filter(Group == input$phenotype_group)
    }
    
    return(df)
    
  })
  
  #VIEW BY PHENOTYPE
 
  output$phenotype_parameter_pvalue <- renderUI({
    req(input$phenotype_groups_scores)
    group_parameters <- data %>% filter(Group == input$phenotype_groups_scores) %>%
      pull(parameter_name) %>% unique()
    selectInput("phenotype_parameter_pvalue", "Select Parameter In Group:", 
                choices = c("None", sort(group_parameters)))
  })
  
  scores_data <- reactive({
    #execution only works if requirement met
    req(input$phenotype_groups_scores)
    df <- data %>% filter(Group == input$phenotype_groups_scores)
   #the if statement ensures that if you switch between Scatter plot of all phenotypes and the 
    #single parameter, then the scatterplot is of the Phenotype Group and not the single parameter
    if (input$pvalue_display == "Single Parameter P Value" &&
        !is.null(input$phenotype_parameter_pvalue) && input$phenotype_parameter_pvalue!= "None") 
    {df <- df %>% filter(parameter_name == input$phenotype_parameter_pvalue)}
    df %>% select(gene_symbol, parameter_name, pvalue)
  })
  
  #CLUSTER GENES
  #make a gene matrix using parameters for the clustering
  cluster <- reactive({
    df <- data %>% filter(!is.na(pvalue))
    
    
    df <- df %>% 
      select(gene_symbol, parameter_name, pvalue) %>%
      group_by(gene_symbol, parameter_name) %>%
      summarise(pvalue = min(pvalue), .groups = "drop") %>%
      pivot_wider(names_from = parameter_name, values_from = pvalue)
    
    #na values turned into '1' and then turns it into -log10
    df_matrix <- df %>% select(-gene_symbol) %>% replace_na(as.list(rep(1, ncol(df) -1))) %>%
      mutate(across(everything(),~ -log10(.)))
    
    #remove infinite value rows
    adjusted_rows <- apply(df_matrix, 1, function(x) all(is.finite(x)))
    df_matrix <- df_matrix[adjusted_rows, ]
    gene_names <- df$gene_symbol[adjusted_rows]
    df_matrix <- as.matrix(df_matrix)
    rownames(df_matrix) <- gene_names
    
    validate(need(nrow(df_matrix) > 1, "No Data Available"))
    return(df_matrix)
  })
  
  
  
  output$mainPlot <- renderPlotly({
    #Make a plot for VIEW BY GENE option- scatter plot or heat map
    if (input$view == "View by Gene") {
      df <- gene_info()
      df <- df %>% filter(!is.na(pvalue) & pvalue >0)
      
      if (input$only_significant) {
        df <- df %>% filter(pvalue < 0.05)
      }
      
      if (input$plot_choice == "Scatterplot") {
        plotlyplot <- ggplot(df, aes(x = reorder(parameter_name, -pvalue), y = -log10(pvalue), colour = Group)) +
        geom_point(shape = 4, size = 1) +
        geom_hline(yintercept = -log10(0.05), linetype = "dashed", colour = "indianred3") +
        annotate("text", x = Inf, y = -log10(0.05), label = "p = 0.05", hjust = 1, vjust = -0.05,
                   colour = "slateblue3", size = 3)
        labs(title = paste("Phenotype Statistical Scores for", input$gene), colour = "Phenotype Group")
        ggplotly(plotlyplot) %>% layout(
          xaxis = list(title = "Phenotype"),
          yaxis = list(title = "Significance Score (-log10)")
        )
      } else if (input$plot_choice == "Heatmap") {
        df$parameter_name <- factor(df$parameter_name, levels = unique(df$parameter_name))
        ggplot(df, aes(x =1, y = parameter_name, fill = -log10(pvalue))) +
          geom_tile()+
          scale_fill_gradient(low = "plum1", high = "red2") +
          labs(title = paste("Heatmap of Phenotype Statistical Scores for", input$gene),
               x = "", y = "Phenotype", fill = "-log10(pvalue)") 
          }
    }
    #want to see the phenotypes significantly affected by gene knockout, so the dashed line with further show which pvalues are significant
    else if (input$view == "View Statistical Score") {
      df <- scores_data()
      if (input$pvalue_display == "Scatterplot of All P Values in Group") {
      req(nrow(df) > 0)
        plot <- ggplot(df, aes(x = reorder(parameter_name, -pvalue), y = -log10(pvalue), colour = gene_symbol)) +
        geom_point(shape = 4, size = 2) +
        geom_hline(yintercept = -log10(0.05), linetype = "dashed", colour = "plum3") +
        labs(title = paste("Statistical Scores for Phenotype Group:", input$phenotype_groups_scores), x = "Phenotype", y = "-log10(p value)", colour = "Gene")
      ggplotly(plot)
      } else if (input$pvalue_display == "Single Parameter P Value")
      {return(NULL)}
     } 
      
    }) 
    
  output$clusterPlot <- renderPlotly({
    if (input$view == "Visualise Gene Clusters") {
      
      df_matrix <- cluster()
      req(nrow(df_matrix) > 1, ncol(df_matrix) > 1)
      
      #make heat map of gene clusters
      heatmaply(df_matrix,
                dendrogram = "row",
                colours = colourRampPalette(c("moccasin", "blue"))(100),
                scale_fill_gradient_fun = ggplot2::scale_fill_gradientn(
                  colours = c("moccasin", "blue"),
                  limits = range(df_matrix, na.rm = TRUE)
                ),
                limits = c(min(df_matrix), max(df_matrix)),
                main = "Gene Clustering Heatmap",
                xlab = "Phenotypes",
                ylab = "Genes",
                fontsize_row = 11,
                fontsize_col = 11,
                labCol = stringr::str_wrap(colnames(df_matrix), width = 20), 
                labRow = rownames(df_matrix),
                showticklabels = c(TRUE, TRUE),
                margins = c(80, 120, 40, 40)
                
                )
                
                
      
          
     } 
    
    })
  
  output$dataTable <- renderDataTable({
    if (input$view == "View by Gene") {gene_info()}
    else if (input$view == "View Statistical Score") {scores_data()}
    else if (input$view == "Visualise Gene Clusters") {
      df_matrix <- cluster()
      data.frame(Gene = rownames(df_matrix), df_matrix)}
  })
  
  output$singlePvaluecontent <- renderText({
    if (input$view == "View Statistical Score" && 
        input$pvalue_display == "Single Parameter P Value" &&
        !is.null(input$phenotype_parameter_pvalue) &&
        input$phenotype_parameter_pvalue != "None") 
      {df <- scores_data()
      p <- df$pvalue[1]
      paste("P value for", input$phenotype_parameter_pvalue, ":", signif(p, 4))
      #only gets the p value to 4 significant figures
      
    } else {
      ""
    }
  })

  }



# Run the application 
  shinyApp(ui = ui, server = server)
