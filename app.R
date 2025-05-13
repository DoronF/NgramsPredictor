library(shiny)
library(ggplot2)
library(RColorBrewer)
library(bslib)

# source for predict_next_word and calculate_perplexity functions
source("predict.R")

# load prefetched top ngrams dataframes
load("top_ngrams.RData")

my_theme <- bs_theme(
    version = 5,
    bootswatch = "cosmo", 
    font_scale = 1
)

ngram_levels_all <- c("bigram",
                      "trigram",
                      "fourgram",
                      "fivegram",
                      "fallback (bigram)")

ngram_colors <- setNames(brewer.pal(5, "Set2"), ngram_levels_all)

# format probabilities to include all digits
format_probabilities <- function(df) {
    df$probability <- format(df$probability, scientific = FALSE)
    return(df)
}

# UI
ui <- fluidPage(
    theme = my_theme,
    titlePanel("Stochastic N-gram Word Predictor"),
    
    # ENTER key triggers submit
    tags$script(
        HTML(
            "$(document).on('keypress', function(e) {
                     if(e.which == 13) {
                        $('#submit_btn').click();
                     }
                  });
                 "
        )
    ),
    sidebarLayout(
        sidebarPanel(
            numericInput("num_predictions",
                        "Number of predictions:",
                        min = 1, max = 15, value = 5
            ),
            h4("How to Use:"),
            p("Type a phrase in the box at the top of the main panel and press Enter or click
                Predict to see suggested next words. Adjust the number of
                predictions above to a maximum of 15 prediction."
              ),
            p("Review other tabs to see the statstics behind the prediction, 
              overview of the dataset, prediction model and Markov Chain"),
            hr(),
            HTML("<p style = 'font-size: 75%;')>Developed by Doron Fingold
          in May 2025</p>")
        ),
        mainPanel(
            # phrase input and button
            fluidRow(
                div(style = "display: flex; gap: 10px; align-items: flex-end;margin-right:10px;margin-top:5px",
                    div(style = "flex-grow: 2;",
                        #HTML('<label for="input_text">Enter phrase:</label>'),
                        textInput("input_text",
                            label = NULL,
                            placeholder = "Type a phrase...",
                            width = "100%"
                        )
                    ),
                    div(style = "padding-bottom: 16px;", 
                        actionButton("submit_btn", "Predict"))
                )
            ),
            tabsetPanel(
                tabPanel("Prediction",
                    br(),
                    htmlOutput("phrase"),
                    uiOutput("prediction_warning"),
                    tableOutput("prediction_table"),
                    htmlOutput("prediction_time")
                ),
                tabPanel("Stats",
                    br(),
                    uiOutput("stats_ui")
                ),
                tabPanel("Dataset",
                    br(),
                    h4("Overview of N-gram Dataset"),
                    p("The dataset consists of English text from three sources:
                    Twitter (informal), Blogs(natural), and News(formal).
                    It was generated from 100,000 sampled sentences and stored in 
                    SQLite database which takes about 90MB of storage. 
                    Stop-words were excluded from being predicted but
                    included in the context words to allow for more
                    meaningful prediction. Words are not filtered out unless 
                    they include digits or symbols. This helps to include 
                    informal words however it also results in some noise."),
                    tableOutput("ngram_counts_table"),
                    p("Below are the most probable n-grams from each level 
                      (bigrams to fivegrams)."),
                    h4("Top 5 Bigrams"),
                    tableOutput("top_bigrams"),
                    h4("Top 5 Trigrams"),
                    tableOutput("top_trigrams"),
                    h4("Top 5 Fourgrams"),
                    tableOutput("top_fourgrams"),
                    h4("Top 5 Fivegrams"),
                    tableOutput("top_fivegrams")
                ),
                tabPanel( "Model",
                    HTML("</br><h4>Probability Calculation Method</h4>
                         <p>This model uses interpolated Kneser-Ney smoothing to 
                         calculate word probabilities.
                         </p><ul><li>Each n-gram level (from bigrams to 
                         fivegrams) is assigned a weight (λ).
                         </li><li>If a higher-order n-gram match is found, 
                         its discounted probability is used.
                         Otherwise, the model backs off recursively to 
                         lower-order n-grams.
                         </li><li>The final probability of a word is a weighted 
                         sum of these discounted estimates. This approach 
                         balances high-order specificity with low-order 
                         generalization, producing robust and realistic 
                         next-word predictions.
                         </li></ul>")
                ),
                tabPanel("Markov Chain",
                    br(),
                    h4("What is a Markov Chain?"),
                    div(style = "display: flex; align-items: flex-start; gap: 20px;",
                       div(style = "flex: 1;",
                            p("A Markov chain is a stochastic model describing a
                            sequence of possible events where the probability of
                            each event depends only on the state attained in the
                            previous event. In language modeling, Markov chains
                            assume the next word depends only on a fixed number of
                            previous words (e.g., bigrams, trigrams)."),
                            p("Markov chains were first introduced by Russian
                            mathematician Andrey Markov in the early 1900s. They
                            have been widely applied in linguistics, physics,
                            finance, and more. In NLP, they form the basis for
                            n-gram language models.")
                        ),
                        div(style = "flex: 0 0 auto;",
                            tags$img(src = "Andrei_Markov.jpg",
                                height = "200px",
                                style = "border-radius: 5px;")
                        )
                    )
                )
            )
            ,
        )
    )
)

server <- function(input, output) {
    predictions <- eventReactive(input$submit_btn, {
        req(input$input_text)
        default_view <- FALSE
        # measure prediction time 
        start_time <- Sys.time()
        result <- predict_next_word(input$input_text, input$num_predictions)
        end_time <- Sys.time()
        prediction_time(
            paste(
                "Prediction time:<b>",
                round(difftime(end_time, start_time, units = "secs"), 3),
                "seconds </b>"
            ))
        result
    })
    phrase <- eventReactive(input$submit_btn,{
        req(input$input_text)
        input$input_text
    })
    phrase <- reactiveVal()
    
    prediction_time <- reactiveVal()
    
    perplexity <- eventReactive(input$submit_btn, {
        req(input$input_text)
        default_view <- FALSE
        result <- calculate_perplexity(input$input_text)
        result
    })
    
    prediction_entropy <- reactive({
        req(predictions())
        probs <- predictions()$probability
        probs <- probs[probs > 0]
        - sum(probs * log2(probs))
    })
    
    prediction_warning <- reactive({
        ent <- prediction_entropy()
        perp <- perplexity()
        
        if (is.null(ent) || is.null(perp))
            return(NULL)
        # Classify entropy and perplexity
        ent_label <- if (ent < 1)
            "low"
        else
            "high"
        perp_label <- if (perp < 1000)
            "low"
        else
            "high"
        
        # Determine combined state
        message <- switch(
            paste(ent_label, perp_label),
            "low low" = '<div style="color: #006600;">
            ✅ Confident and likely accurate prediction (Low entropy, Low 
            perplexity)</div>',
            "high high" = '<div style="color: #cc0000;">
            ⚠️ Uncertain and likely inaccurate prediction (High entropy,
            High perplexity)</div>',
            "low high" = '<div style="color: #e69500;">
            ⚠️ Confident but possibly wrong prediction (Low entropy,
            High perplexity)</div>',
            "high low" = '<div style="color: #1a75ff;">
            ℹ️ Cautious but possibly accurate prediction (High entropy, 
            Low perplexity)</div>'
        )
        HTML(paste0("<div style='margin: 10px;'>", message, "</div>"))
    })
    
    top_ngrams <- reactiveValues(
        bigrams = top_bigrams,
        trigrams = top_trigrams,
        fourgrams = top_fourgrams,
        fivegrams = top_fivegrams
    )
    ## prediction
   output$phrase <-renderUI({
       req(phrase())
       HTML(paste0("<h4>", phrase() ,"</h4>"))
   })
    output$prediction_warning <- renderUI({
        prediction_warning()
    })
    
    output$predictions_ui <- renderUI({
        preds <- predictions()
        
        tableOutput("prediction_table")
    })
    
    output$prediction_table <- renderTable({
        req(predictions())
        format_probabilities(predictions()) %>%
            mutate(rank = row_number()) %>%
            select(rank, everything()) 
    })
    
    output$prediction_time <- renderText({
        req(predictions())
        prediction_time()
    })
    
    ## Stats
    output$perplexity_output <- renderText({
        req(perplexity())
        perplexity_value <- round(perplexity(), 2)
        perplexity_label <- if (perplexity_value < 100) {
            " - Low (The model is highly confident)"
        } else if (perplexity_value <= 500) {
            " - Medium (Reasonable predictions but room for improvement)"
        } else {
            " - High (predictions may not be reliable)"
        }
        
        paste0("<h5><b>Perplexity: </b>",
               perplexity_value,
               perplexity_label,
               "</h5>")
        
    })
    
    output$entropy_text <- renderText({
        ent <- prediction_entropy()
        interpretation <- if (ent < 1.5) {
            "Low (confident prediction)"
        } else if (ent < 2.5) {
            "Medium (moderate uncertainty)"
        } else {
            "High (uncertain prediction)"
        }
        paste0("<h5><b>Entropy:</b> ", round(ent, 2), " — ", interpretation, " </h5>")
    })
    
    output$stats_ui <- renderUI({
        preds <- predictions()
        tagList(
            br(),
            htmlOutput("perplexity_output"),
            htmlOutput("entropy_text"),
            plotOutput("prob_plot"),
            plotOutput("backoff_plot"),
            
        )
    })
    
    output$prob_plot <- renderPlot({
        req(predictions())
        
        preds <- predictions()
        
        prob_range <- range(preds$probability, na.rm = TRUE)
        if (diff(prob_range) == 0) {
            scaled_probs <- rep(1, length(preds$probability))
        } else {
            scaled_probs <- (preds$probability - prob_range[1]) / diff(prob_range)
            scaled_probs <- scaled_probs * 0.9 + 0.1
        }
        
        
        colors_vector <- ngram_colors[preds$ngram_source]
        
        layout(matrix(c(1, 2), nrow = 2), heights = c(5, 1))  
        
        # Barplot 
        par(mar = c(7, 4, 4, 2))  
        barplot(
            scaled_probs,
            names.arg = preds$word,
            col = colors_vector,
            main = "Predicted Word Probabilities",
            ylab = "Scaled Probability",
            las = 2,
            border = "white"
        )
        
        # Legend 
        par(mar = c(0, 0, 0, 0))
        plot.new()
        legend(
            "center",
            legend = names(ngram_colors),
            fill = ngram_colors,
            title = "N-gram Source",
            horiz = TRUE,
            cex = 0.9,
            bty = "n"
        )
    })
    
    output$backoff_plot <- renderPlot({
        req(predictions())
        df <- predictions()
        
        backoff_counts <- table(df$ngram_source)
        backoff_df <- as.data.frame(backoff_counts)
        names(backoff_df) <- c("ngram", "count")
        
        ggplot(backoff_df, aes(x = reorder(ngram, -count), y = count, fill = ngram)) +
            geom_bar(stat = "identity") +
            labs(
                title = "N-gram Source Contribution",
                x = "N-gram Level",
                y = "Number of Predictions"
            ) +
            theme_minimal() +
            scale_fill_manual(values = ngram_colors) +  # Match ngram to defined colors
            theme(legend.position = "none")
    })
    
    ## Dataset
    output$ngram_counts_table <- renderTable({
        data.frame(
            `N-gram` = c("Bigrams", "Trigrams", "Fourgrams", "Fivegrams"),
            `Count` = format(
                c(count_bigrams, count_trigrams, count_fourgrams, count_fivegrams),
                big.mark = ",",
                scientific = FALSE
            ),
            check.names = FALSE
        )
    })
    
    output$top_bigrams <- renderTable({
        head(format_probabilities(top_bigrams), 5)
        
    })
    
    output$top_trigrams <- renderTable({
        head(format_probabilities(top_trigrams), 5)
    })
    
    output$top_fourgrams <- renderTable({
        head(format_probabilities(top_fourgrams), 5)
    })
    
    output$top_fivegrams <- renderTable({
        head(format_probabilities(top_fivegrams), 5)
    })
}

shinyApp(ui = ui, server = server)
