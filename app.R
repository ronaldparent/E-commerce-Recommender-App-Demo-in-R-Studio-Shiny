# Load necessary libraries
library(shiny)
library(shinythemes) # Added for theming
library(recommenderlab)
library(ggplot2)
library(dplyr)
library(DT)
library(reshape2)
library(tools)
library(htmltools)

# --- Configuration for Local Files ---
# Define the names of the local CSV files the app should look for
local_transaction_file <- "transactions_consistent.csv"
local_product_file <- "products_1000_consistent.csv"

# --- 1. Data Generation Functions (Keep as before) ---
# Define some realistic components
product_categories <- c("Electronics", "Clothing", "Home Goods", "Books", "Sports & Outdoors", "Beauty", "Toys", "Grocery", "Automotive", "Garden")
product_templates <- list(
  Electronics = c("Wireless", "Bluetooth", "Smart", "Portable", "HD", "4K", "Gaming", "Noise-Cancelling", "Fast Charging", "Ultra-Thin", "LED", "OLED"),
  Clothing = c("Cotton", "Wool", "Silk", "Organic", "Casual", "Formal", "Slim Fit", "Comfort", "Vintage", "Breathable", "Quick-Dry"),
  `Home Goods` = c("Stainless Steel", "Wooden", "Ceramic", "Non-Stick", "Compact", "Ergonomic", "Modern", "Rustic", "Bamboo", "Glass"),
  Books = c("Hardcover", "Paperback", "Illustrated", "Bestselling", "Classic", "Contemporary", "Sci-Fi", "Mystery", "Thriller", "Fantasy"),
  `Sports & Outdoors` = c("Lightweight", "Durable", "Waterproof", "Adjustable", "Professional", "Camping", "Hiking", "Training", "Insulated", "Collapsible"),
  Beauty = c("Organic", "Vegan", "Anti-Aging", "Hydrating", "Sensitive Skin", "Natural", "Fragrance-Free", "Cruelty-Free", "Mineral-Based"),
  Toys = c("Educational", "Wooden", "Plush", "Interactive", "Building Blocks", "Remote Control", "Outdoor Play", "STEM", "Art"),
  Grocery = c("Organic", "Gluten-Free", "Non-GMO", "Farm-Fresh", "Artisanal", "Imported", "Low-Sodium", "Sugar-Free"),
  Automotive = c("Heavy-Duty", "All-Weather", "Universal Fit", "Easy Install", "Premium", "Synthetic Blend", "LED"),
  Garden = c("Ergonomic", "Long-Handle", "Stainless Steel", "Weather-Resistant", "Organic", "Non-Toxic", "Slow-Release")
)
product_nouns <- list(
  Electronics = c("Headphones", "Speaker", "Watch", "Monitor", "Keyboard", "Mouse", "Charger", "Webcam", "Drone", "Tablet", "Projector"),
  Clothing = c("T-Shirt", "Shirt", "Jeans", "Sweater", "Jacket", "Dress", "Socks", "Hat", "Scarf", "Gloves", "Belt"),
  `Home Goods` = c("Pan Set", "Knife Block", "Blender", "Coffee Maker", "Storage Box", "Lamp", "Towel Set", "Cutlery", "Mug", "Air Fryer"),
  Books = c("Novel", "Anthology", "Cookbook", "Biography", "Textbook", "Guide", "Memoir", "Atlas"),
  `Sports & Outdoors` = c("Backpack", "Tent", "Water Bottle", "Yoga Mat", "Dumbbell Set", "Running Shoes", "Cooler", "Sleeping Bag", "Bike Helmet"),
  Beauty = c("Moisturizer", "Serum", "Cleanser", "Lipstick", "Shampoo", "Perfume", "Sunscreen", "Face Mask"),
  Toys = c("Action Figure", "Doll", "Puzzle", "Board Game", "Car Set", "Stuffed Animal", "Drone", "Craft Kit"),
  Grocery = c("Coffee Beans", "Olive Oil", "Pasta", "Chocolate Bar", "Granola", "Tea Bags", "Spice Mix", "Cereal"),
  Automotive = c("Floor Mats", "Seat Cover", "Phone Mount", "Air Freshener", "Wiper Blades", "Motor Oil", "Jump Starter"),
  Garden = c("Trowel", "Pruning Shears", "Hose Nozzle", "Gloves", "Watering Can", "Fertilizer", "Potting Soil")
)
description_templates <- list(
  Electronics = "Experience {adj} performance and seamless connectivity with the new {noun}. Perfect for enthusiasts and professionals.",
  Clothing = "Stay comfortable and stylish with this {adj} {noun}, crafted from high-quality materials. A versatile addition to any wardrobe.",
  `Home Goods` = "Upgrade your home with this durable and {adj} {noun}. Designed for convenience and lasting quality.",
  Books = "A compelling {adj} {noun}. This {category} title is a must-read for fans of the genre.",
  `Sports & Outdoors` = "This {adj} {noun} is your perfect companion for any adventure. Built to be {adj} and reliable.",
  Beauty = "Nourish yourself with our {adj} {noun}. Formulated with care using {adj} ingredients for the best results.",
  Toys = "Spark imagination and fun with this {adj} {noun}. Provides hours of entertainment and learning.",
  Grocery = "Enjoy the authentic taste of this {adj} {noun}. Sourced responsibly and packed with flavor.",
  Automotive = "Keep your vehicle in top shape with this {adj} {noun}. Engineered for performance and reliability.",
  Garden = "Maintain a beautiful garden with this {adj} {noun}. Makes gardening tasks easier and more enjoyable."
)

generate_synthetic_data <- function(n_users = 200, n_items = 150, n_ratings = 6000) {
  set.seed(1234)
  user_ids <- paste0("User_", 1:n_users)
  item_ids <- paste0("Item_", sprintf("%04d", 1:n_items)) # Using Item_xxxx format
  item_data_list <- lapply(1:n_items, function(i) {
    category <- sample(product_categories, 1)
    adj <- sample(product_templates[[category]], 1)
    noun <- sample(product_nouns[[category]], 1)
    name <- paste(adj, noun)
    desc_template <- description_templates[[category]]
    description <- gsub("\\{adj\\}", adj, desc_template)
    description <- gsub("\\{noun\\}", noun, description)
    description <- gsub("\\{category\\}", category, description)
    data.frame( itemID = item_ids[i], productName = name, category = category, description = description, stringsAsFactors = FALSE)
  })
  item_metadata <- bind_rows(item_data_list)
  user_indices <- sample(1:n_users, n_ratings, replace = TRUE)
  item_indices <- sample(1:n_items, n_ratings, replace = TRUE)
  item_popularity_bias <- rnorm(n_items, mean = 3.5, sd = 0.8)
  ratings <- round(pmin(5, pmax(1, rnorm(n_ratings, mean = item_popularity_bias[item_indices], sd = 1.2))))
  transactions <- data.frame( userID = user_ids[user_indices], itemID = item_ids[item_indices], rating = ratings, stringsAsFactors = FALSE )
  transactions <- transactions %>% distinct(userID, itemID, .keep_all = TRUE)
  return(list(transactions = transactions, item_metadata = item_metadata))
}

# --- Generate Backup Synthetic Data ---
# This is generated regardless, to be used as a fallback or template
synthetic_data_list <- generate_synthetic_data()

# --- Attempt to Load Local Consistent Files ---
initial_data_source_type <- "synthetic" # Default to synthetic
initial_transactions <- synthetic_data_list$transactions
initial_metadata <- synthetic_data_list$item_metadata
load_message <- "Defaulting to internal synthetic data."

if (file.exists(local_transaction_file) && file.exists(local_product_file)) {
  message(paste("Found local files:", local_transaction_file, "and", local_product_file))
  tryCatch({
    # Attempt to read the local files
    local_trans <- read.csv(local_transaction_file, stringsAsFactors = FALSE, header = TRUE)
    local_prod <- read.csv(local_product_file, stringsAsFactors = FALSE, header = TRUE)

    # Basic validation (can be expanded)
    req_trans_cols <- c("userID", "itemID", "rating")
    req_prod_cols <- c("itemID", "productName")
    if (all(req_trans_cols %in% names(local_trans)) && all(req_prod_cols %in% names(local_prod))) {
        # If read and validation successful, use these as initial data
        initial_transactions <- local_trans %>%
                                  mutate(userID = as.character(userID),
                                         itemID = as.character(itemID),
                                         rating = as.numeric(rating)) %>%
                                  filter(!is.na(rating)) %>% # Ensure ratings are numeric
                                  distinct(userID, itemID, .keep_all = TRUE)

        initial_metadata <- local_prod %>%
                              mutate(itemID = as.character(itemID),
                                     productName = as.character(productName)) %>%
                              distinct(itemID, .keep_all = TRUE) # Ensure unique items

        # Add optional columns if missing
        if (!"category" %in% names(initial_metadata)) initial_metadata$category <- "Unknown"
        if (!"description" %in% names(initial_metadata)) initial_metadata$description <- "No description."


        initial_data_source_type <- "local_csv" # Set flag indicating local data was loaded
        load_message <- paste("Successfully loaded data from", local_transaction_file, "and", local_product_file)
        message(load_message)
    } else {
      load_message <- paste("Local CSV files found, but column headers are incorrect. Required:",
                            paste(req_trans_cols, collapse=", "), "(transactions) and",
                            paste(req_prod_cols, collapse=", "), "(products). Defaulting to synthetic data.")
      message(load_message)
    }
  }, error = function(e) {
    # If reading fails, default to synthetic
    load_message <- paste("Error reading local CSV files:", e$message, ". Defaulting to synthetic data.")
    message(load_message)
    initial_data_source_type <- "synthetic"
    initial_transactions <- synthetic_data_list$transactions
    initial_metadata <- synthetic_data_list$item_metadata
  })
} else {
  load_message <- paste("Local files (",local_transaction_file, ",", local_product_file, ") not found. Defaulting to internal synthetic data.", sep="")
  message(load_message)
}

# --- Save Synthetic Data to CSV (Optional: Good for template) ---
tryCatch({
  write.csv(synthetic_data_list$transactions, "synthetic_transactions.csv", row.names = FALSE, quote = TRUE)
  write.csv(synthetic_data_list$item_metadata, "synthetic_products.csv", row.names = FALSE, quote = TRUE)
  message("Backup synthetic data saved to synthetic_transactions.csv and synthetic_products.csv (for template use).")
}, error = function(e) {
  message("Could not write backup synthetic data to CSV files: ", e$message)
})


# --- Define Footer UI ---
app_footer <- div(class = "footer",
    p("This application is for demonstration purposes only."),
    p("Interested in implementing a recommendation system for your business?"),
    p(tags$a(href="https://precisiondatastrategies.com", target="_blank", "Book a consultation at precisiondatastrategies.com")),
    hr(style="border-top: 1px solid #ccc; width: 50%; margin-left: auto; margin-right: auto;"),
    p(HTML("&copy;"), format(Sys.Date(), "%Y"), "Precision Data Strategies. All rights reserved.")
)


# --- UI Definition ---
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),

    navbarPage(
        title = "Precision Data Strategies - Recommendation System Demo",

        # --- Data Source Tab ---
        tabPanel("Data Source & Summary",
            titlePanel("Data Source Configuration"),
            sidebarLayout(
                sidebarPanel(
                    h4("Select Data Source"),
                    # Updated radio buttons: Add "local_csv" option
                    radioButtons("dataSource", "Choose data:",
                                 choices = c("Use Local CSV Files (if found)" = "local_csv",
                                             "Use Generated Synthetic Data" = "synthetic",
                                             "Upload New CSV Files" = "upload"),
                                 selected = initial_data_source_type), # Set default based on initial load
                    hr(),
                    # Conditional Panel for Uploading NEW files
                    conditionalPanel(
                        condition = "input.dataSource == 'upload'",
                        h4("Upload Your Data"),
                        fileInput("transactionFile", "Upload Transactions CSV (.csv)", accept = c("text/csv", ".csv")),
                        tags$small("Required columns: 'userID', 'itemID', 'rating'"),
                        br(),br(),
                        fileInput("productFile", "Upload Product Metadata CSV (.csv)", accept = c("text/csv", ".csv")),
                        tags$small("Required columns: 'itemID', 'productName'. Optional: 'category', 'description'"),
                         hr(),
                         actionButton("loadCsvData", "Load Uploaded Data", icon = icon("upload"), class = "btn-primary")
                    ),
                     hr(),
                     p(strong("Status:")),
                     textOutput("dataLoadStatus"), # Show status message
                     hr(),
                     p("Synthetic data template files saved to:",
                       br(), strong("synthetic_transactions.csv"),
                       br(), strong("synthetic_products.csv"))
                ),
                mainPanel(
                    h4("Current Data Summary"),
                    verbatimTextOutput("dataSummary")
                )
            )
        ), # End TabPanel

        # --- EDA Tab ---
        tabPanel("Exploratory Data Analysis",
            titlePanel("Exploratory Data Analysis"),
            sidebarLayout(
                sidebarPanel(
                    h4("EDA Options"),
                    p("Visualizations based on the currently loaded data. Update data source on the 'Data Source' tab."),
                    actionButton("refreshEDA", "Refresh Plots", icon=icon("sync"))
                ),
                mainPanel(
                    h4("Distribution of Ratings"), plotOutput("ratingDistributionPlot"), hr(),
                    h4("Most Rated Items (Top 10)"), plotOutput("mostRatedItemsPlot"), hr(),
                    h4("Users with Most Ratings (Top 10)"), plotOutput("mostActiveUsersPlot"), hr(),
                    h4("Heatmap of User-Item Ratings (Sampled Subset)"), plotOutput("ratingMatrixHeatmap")
                )
            )
        ), # End TabPanel

        # --- Model Training & Evaluation Tab ---
        tabPanel("Model Training & Evaluation",
            titlePanel("Train and Evaluate Recommendation Models"),
             p("Evaluation is performed on the currently loaded data."),
            sidebarLayout(
                sidebarPanel(
                    h4("Evaluation Parameters"),
                    sliderInput("trainSplit", "Proportion of data for training:", min = 0.5, max = 0.9, value = 0.8, step = 0.05),
                    numericInput("evalK", "Evaluate Precision/Recall @K:", value = 5, min = 1, max = 20),
                    numericInput("knnNeighbors", "Neighbors (K) for UBCF:", value = 25, min = 5, max = 100),
                    numericInput("svdFactors", "Factors (k) for SVD:", value = 10, min = 2, max = 50),
                    actionButton("runEvaluation", "Run Evaluation", icon = icon("cogs"), class = "btn-primary"),
                    hr(),
                    h4("Models Evaluated"), tags$ul(tags$li("UBCF"), tags$li("SVD"), tags$li("Random"))
                ),
                mainPanel(
                    h4("Evaluation Scheme Setup"), verbatimTextOutput("evalSchemeOutput"), hr(),
                    h4("Evaluation Results"), uiOutput("evaluationStatus"), verbatimTextOutput("evaluationResults")
                )
            )
        ), # End TabPanel

        # --- Recommendations Dashboard Tab ---
        tabPanel("Personalized Recommendations",
            titlePanel("Recommendations Dashboard"),
             p("Recommendations based on the currently loaded data, trained on the full dataset."),
            sidebarLayout(
                sidebarPanel(
                    h4("Recommendation Parameters"),
                    selectInput("selectedUser", "Select User:", choices = NULL),
                    numericInput("numRecs", "Number of Recommendations (N):", value = 5, min = 1, max = 20),
                    actionButton("getRecommendations", "Get Recommendations", icon = icon("gift"), class = "btn-primary"),
                    hr(), h5("Note:"), p("Default model parameters used (KNN=25, SVD k=10).")
                ),
                mainPanel(
                    h4(textOutput("ubcfRecTitle")), DTOutput("ubcfRecommendationsTable"), hr(),
                    h4(textOutput("svdRecTitle")), DTOutput("svdRecommendationsTable")
                )
            )
        ) # End TabPanel
    ), # End navbarPage

    # --- Add Footer ---
    hr(),
    app_footer

) # End fluidPage


# --- Server Logic ---
server <- function(input, output, session) {

    # --- Reactive Values Store ---
    # Initialize with data determined during startup (local CSV or synthetic)
    rv <- reactiveValues(
      transactions = initial_transactions,
      item_metadata = initial_metadata,
      rating_matrix = NULL,
      data_summary = "",
      all_users = NULL,
      status_message = load_message # Store initial load message
      )

    # --- Output for Data Load Status ---
    output$dataLoadStatus <- renderText({
        rv$status_message # Display the current status
    })

    # --- Data Loading Logic ---

    # Observer for changes in the data source selection
    observeEvent(input$dataSource, {
        current_selection <- input$dataSource
        message(paste("Data Source selection changed to:", current_selection))

        if (current_selection == "synthetic") {
            message("Loading synthetic data...")
            showNotification("Loading synthetic data...", type="message", duration=2)
            rv$transactions <- synthetic_data_list$transactions
            rv$item_metadata <- synthetic_data_list$item_metadata
            rv$rating_matrix <- NULL # Invalidate
            rv$all_users <- NULL # Invalidate
            rv$status_message <- "Using internal synthetic data."
        } else if (current_selection == "local_csv") {
            message("Attempting to reload local CSV data...")
            # Try to reload from the consistent local files
            if (file.exists(local_transaction_file) && file.exists(local_product_file)) {
                tryCatch({
                    local_trans <- read.csv(local_transaction_file, stringsAsFactors = FALSE, header = TRUE)
                    local_prod <- read.csv(local_product_file, stringsAsFactors = FALSE, header = TRUE)
                    req_trans_cols <- c("userID", "itemID", "rating"); req_prod_cols <- c("itemID", "productName")
                    if (all(req_trans_cols %in% names(local_trans)) && all(req_prod_cols %in% names(local_prod))) {
                        rv$transactions <- local_trans %>% mutate(userID = as.character(userID), itemID = as.character(itemID), rating = as.numeric(rating)) %>% filter(!is.na(rating)) %>% distinct(userID, itemID, .keep_all = TRUE)
                        rv$item_metadata <- local_prod %>% mutate(itemID = as.character(itemID), productName = as.character(productName)) %>% distinct(itemID, .keep_all = TRUE)
                        if (!"category" %in% names(rv$item_metadata)) rv$item_metadata$category <- "Unknown"
                        if (!"description" %in% names(rv$item_metadata)) rv$item_metadata$description <- "No description."
                        rv$rating_matrix <- NULL; rv$all_users <- NULL
                        rv$status_message <- paste("Reloaded data from", local_transaction_file, "and", local_product_file)
                        showNotification(rv$status_message, type="message", duration=3)
                    } else {
                        rv$status_message <- "Local CSV files found, but headers incorrect. Cannot reload."
                        showNotification(rv$status_message, type="warning", duration=5)
                    }
                }, error = function(e) {
                    rv$status_message <- paste("Error reloading local CSV files:", e$message)
                    showNotification(rv$status_message, type="error", duration=10)
                })
            } else {
                rv$status_message <- "Local CSV files not found. Cannot switch to this source."
                showNotification(rv$status_message, type="warning", duration=5)
                # Optional: switch back to synthetic if local files disappear?
                # updateRadioButtons(session, "dataSource", selected = "synthetic")
            }
        } else if (current_selection == "upload") {
            # Clear current data when switching to upload mode, before files are chosen
            # Or just wait for the button press
            rv$status_message <- "Ready to upload new CSV files."
            message("Switched to Upload mode. Awaiting file selection and button press.")
        }
    }, ignoreInit = TRUE) # ignoreInit = TRUE prevents this from running on startup


   # Observer for the "Load Uploaded Data" button
   observeEvent(input$loadCsvData, {
        message("--- Load Uploaded Data button pressed ---")
        req(input$dataSource == "upload", input$transactionFile, input$productFile)
        showNotification("Attempting to load uploaded CSV files...", type="message", duration=3, id="load-notify")
        message("DEBUG: Uploaded Transaction file path: ", input$transactionFile$datapath)
        message("DEBUG: Uploaded Product file path: ", input$productFile$datapath)
        trans_ext <- file_ext(input$transactionFile$name); prod_ext <- file_ext(input$productFile$name)
        if (trans_ext != "csv" || prod_ext != "csv") { msg <- "Error: Please upload files with the .csv extension."; message(msg); showNotification(msg, type="error", duration=10); removeNotification("load-notify"); return() }

        tryCatch({
            message("DEBUG: Reading uploaded transaction file...") ; trans_df <- read.csv(input$transactionFile$datapath, stringsAsFactors = FALSE, header = TRUE); message("DEBUG: Uploaded Trans read. Dims: ", paste(dim(trans_df), collapse = "x")); message("DEBUG: Uploaded Trans colnames: ", paste(names(trans_df), collapse=", "))
            req_trans_cols <- c("userID", "itemID", "rating"); if (!all(req_trans_cols %in% names(trans_df))) { stop(paste("Transaction CSV must contain columns:", paste(req_trans_cols, collapse=", "))) }
            trans_df <- trans_df %>% mutate(userID = as.character(userID), itemID = as.character(itemID), rating = as.numeric(rating)) %>% filter(!is.na(rating)) %>% distinct(userID, itemID, .keep_all = TRUE) ; message("DEBUG: Uploaded Trans data processed. Dims: ", paste(dim(trans_df), collapse = "x"))

            message("DEBUG: Reading uploaded product file...") ; prod_df <- read.csv(input$productFile$datapath, stringsAsFactors = FALSE, header = TRUE); message("DEBUG: Uploaded Prod read. Dims: ", paste(dim(prod_df), collapse = "x")); message("DEBUG: Uploaded Prod colnames: ", paste(names(prod_df), collapse=", "))
            req_prod_cols <- c("itemID", "productName"); if (!all(req_prod_cols %in% names(prod_df))) { stop(paste("Product CSV must contain columns:", paste(req_prod_cols, collapse=", "))) }
            prod_df <- prod_df %>% mutate(itemID = as.character(itemID), productName = as.character(productName)) %>% distinct(itemID, .keep_all = TRUE)
            if (!"category" %in% names(prod_df)) prod_df$category <- "Unknown"; if (!"description" %in% names(prod_df)) prod_df$description <- "No description."; message("DEBUG: Uploaded Prod data processed. Dims: ", paste(dim(prod_df), collapse = "x"))

            items_in_trans <- unique(trans_df$itemID); items_in_prod <- unique(prod_df$itemID); missing_items <- setdiff(items_in_trans, items_in_prod)
            if (length(missing_items) > 0) { warn_msg <- paste("WARN:", length(missing_items), "itemIDs in uploaded trans missing from prod metadata. First few:", paste(head(missing_items), collapse=", ")); message(warn_msg); showNotification(warn_msg, type="warning", duration=8) }
            if (any(duplicated(prod_df$itemID))) { warn_msg <- "WARN: Duplicate itemIDs found in uploaded product metadata. Keeping first."; message(warn_msg); showNotification(warn_msg, type="warning", duration=5); prod_df <- prod_df %>% distinct(itemID, .keep_all = TRUE) }

            message("DEBUG: Updating rv with uploaded data...") ; rv$transactions <- trans_df; rv$item_metadata <- prod_df; message("DEBUG: rv updated. Invalidating matrix and user list."); rv$rating_matrix <- NULL; rv$all_users <- NULL
            rv$status_message <- paste("Successfully loaded data from uploaded files:", input$transactionFile$name, "and", input$productFile$name)
            showNotification("Uploaded CSV data loaded successfully!", type="message", duration=5); message("--- Uploaded CSV Load successful ---")

        }, error = function(e) { error_msg <- paste("Error loading uploaded CSV files:", e$message); message(error_msg); showNotification(error_msg, type="error", duration = 15); rv$status_message <- error_msg; message("--- Uploaded CSV Load failed ---") })
        removeNotification("load-notify")
    })


    # --- Reactive Expression for Rating Matrix ---
    reactive_rating_matrix <- reactive({
        current_transactions <- rv$transactions; message("DEBUG: reactive_rating_matrix() triggered.")
        req(current_transactions); if (nrow(current_transactions) == 0) { showNotification("Transaction data is empty.", type="warning"); message("DEBUG: Transaction data empty in reactive_rating_matrix()."); return(NULL) }
        showNotification("Preparing rating matrix...", type="message", duration = 2, id="matrix-prep"); message("DEBUG: Attempting to create realRatingMatrix...")
        matrix_obj <- tryCatch({ as(current_transactions[, c("userID", "itemID", "rating")], "realRatingMatrix") }, error = function(e) { error_msg <- paste("Error creating rating matrix:", e$message); message(error_msg); showNotification(error_msg, type = "error", duration=10); NULL })
        removeNotification("matrix-prep"); if (!is.null(matrix_obj)) { message("DEBUG: realRatingMatrix created successfully. Dims:", paste(dim(matrix_obj), collapse="x")) } else { message("DEBUG: realRatingMatrix creation failed.") }
        return(matrix_obj)
    })

   # --- Reactive Expression for Current User List ---
    reactive_user_list <- reactive({
        trans_data <- rv$transactions; req(trans_data); users <- sort(unique(trans_data$userID)); message("DEBUG: reactive_user_list() updated. Found", length(users), "users."); return(users)
    })

    # --- Update User Selector Input ---
    observe({
        users <- reactive_user_list(); selected_user <- isolate(input$selectedUser); updateSelectInput(session, "selectedUser", choices = users, selected = if (!is.null(selected_user) && selected_user %in% users) selected_user else if(length(users) > 0) users[1] else NULL); message("DEBUG: User selector updated.")
    })

    # --- Data Summary Output ---
    output$dataSummary <- renderPrint({
        mat <- reactive_rating_matrix(); meta <- rv$item_metadata; trans <- rv$transactions; req(trans); message("DEBUG: Updating Data Summary output.")
        # Determine current source description based on rv data, not just input$dataSource
        current_source_desc <- rv$status_message # Use the status message for more context
        cat("Current Data Status:", current_source_desc, "\n"); cat("--------------------------------------------\n")
        if (!is.null(mat)) { n_users <- nrow(mat); n_items_matrix <- ncol(mat); n_ratings_actual <- length(getData.frame(mat)$rating); sparsity <- 1 - (n_ratings_actual / (n_users * n_items_matrix)); cat("Users in Matrix:", n_users, "\n"); cat("Items in Matrix:", n_items_matrix, "\n"); cat("Number of Ratings:", n_ratings_actual, "\n"); cat("Matrix Sparsity:", round(sparsity * 100, 2), "%\n"); cat("Rating Range:", paste(range(getData.frame(mat)$rating), collapse = " - "), "\n") } else { cat("Rating matrix not yet generated or failed.\n"); cat("Number of Transactions Records:", nrow(trans), "\n"); cat("Unique Users in Transactions:", length(unique(trans$userID)), "\n"); cat("Unique Items in Transactions:", length(unique(trans$itemID)), "\n") }
        if (!is.null(meta)) { cat("Total Items in Metadata:", nrow(meta), "\n") } else { cat("Product metadata not available.\n") }
        cat("\nSample Transactions (first 6 rows):\n"); print(head(trans))
        if (!is.null(meta)) { cat("\nSample Product Metadata (first 6 rows):\n"); cols_to_show <- intersect(c("itemID", "productName", "category"), names(meta)); print(head(meta[, cols_to_show, drop = FALSE])) }
    })

    # --- EDA Tab Server Logic ---
    eda_trigger <- reactive({ input$refreshEDA; reactive_rating_matrix() })
    output$ratingDistributionPlot <- renderPlot({ mat <- eda_trigger(); req(mat); ratings_df <- getData.frame(mat); ggplot(ratings_df, aes(x = rating)) + geom_histogram(binwidth = 1, fill = "steelblue", color = "black") + labs(title = "Distribution of User Ratings", x = "Rating", y = "Frequency") + theme_minimal(base_size = 14) + theme(plot.title = element_text(hjust = 0.5)) })
    output$mostRatedItemsPlot <- renderPlot({ mat <- eda_trigger(); req(mat); meta <- rv$item_metadata; req(meta); item_counts <- colCounts(mat); item_counts_df <- data.frame(itemID = names(item_counts), count = item_counts) %>% arrange(desc(count)) %>% slice_head(n = 10) %>% left_join(meta[, intersect(c("itemID", "productName"), names(meta)), drop = FALSE], by = "itemID"); item_counts_df$display_name <- ifelse(!is.na(item_counts_df$productName) & item_counts_df$productName != "", item_counts_df$productName, item_counts_df$itemID); req(nrow(item_counts_df) > 0); ggplot(item_counts_df, aes(x = reorder(display_name, count), y = count)) + geom_bar(stat = "identity", fill = "coral") + coord_flip() + labs(title = "Top 10 Most Rated Items", x = "Item", y = "Number of Ratings") + theme_minimal(base_size = 14) + theme(plot.title = element_text(hjust = 0.5)) })
    output$mostActiveUsersPlot <- renderPlot({ mat <- eda_trigger(); req(mat); user_counts <- rowCounts(mat); user_counts_df <- data.frame(userID = names(user_counts), count = user_counts) %>% arrange(desc(count)) %>% slice_head(n = 10); req(nrow(user_counts_df) > 0); ggplot(user_counts_df, aes(x = reorder(userID, count), y = count)) + geom_bar(stat = "identity", fill = "lightblue") + coord_flip() + labs(title = "Top 10 Users by Number of Ratings", x = "User ID", y = "Number of Ratings") + theme_minimal(base_size = 14) + theme(plot.title = element_text(hjust = 0.5)) })
    output$ratingMatrixHeatmap <- renderPlot({ mat_orig <- eda_trigger(); req(mat_orig); max_dim_plot <- 50; user_indices <- sample(1:nrow(mat_orig), min(nrow(mat_orig), max_dim_plot)); item_indices <- sample(1:ncol(mat_orig), min(ncol(mat_orig), max_dim_plot)); mat_sample <- mat_orig[user_indices, item_indices]; mat_dense <- tryCatch({ as(mat_sample, "matrix") }, error = function(e){ plot.new(); title(main="Error converting matrix for heatmap", col.main="red"); text(0.5, 0.5, paste("Error:", e$message), cex = 0.8); return(NULL) }); req(mat_dense); mat_dense[mat_dense == 0] <- NA; if(all(is.na(mat_dense))) { plot.new(); title(main="Heatmap (Sampled Subset)", col.main="blue"); text(0.5, 0.5, "Sampled matrix contains no ratings.", cex = 0.9); return() }; image(x = 1:ncol(mat_dense), y = 1:nrow(mat_dense), z = t(mat_dense), main = "Heatmap of User-Item Ratings (Sampled Subset)", xlab = "Items", ylab = "Users", col = rev(heat.colors(20)), xaxt = "n", yaxt = "n"); axis(1, at = 1:ncol(mat_dense), labels = colnames(mat_dense), las = 2, cex.axis = 0.7); axis(2, at = 1:nrow(mat_dense), labels = rownames(mat_dense), las = 1, cex.axis = 0.7) })

    # --- Model Training & Evaluation Tab Server Logic ---
    eval_status <- reactiveVal(""); output$evaluationStatus <- renderUI({ if(eval_status() != "") { p(strong(eval_status())) } })
    evaluation_scheme <- eventReactive(input$runEvaluation, { mat <- reactive_rating_matrix(); req(mat); eval_status("Setting up..."); message("Creating evaluation scheme..."); es <- tryCatch({ evaluationScheme(mat, method = "split", train = input$trainSplit, given = -1, goodRating = 4, k = 1) }, error = function(e) { err_msg <- paste("Scheme Error:", e$message); showNotification(err_msg, type = "error", duration = 10); message(err_msg); eval_status(err_msg); return(NULL) }); if (!is.null(es)) eval_status("Scheme created."); return(es) })
    output$evalSchemeOutput <- renderPrint({ scheme <- evaluation_scheme(); req(scheme); print(scheme) })
    evaluation_results <- eventReactive(input$runEvaluation, { req(evaluation_scheme()); scheme <- evaluation_scheme(); if(is.null(scheme)) { return(list(topN = NULL, rmse = NULL, error = TRUE)) }; k_eval <- input$evalK; nn_param <- input$knnNeighbors; svd_k_param <- input$svdFactors; eval_status("Running evaluation..."); showNotification("Running evaluation...", duration = 10, type = "message", id="eval-notify"); algorithms_to_evaluate <- list("UBCF_KNN" = list(name = "UBCF", param = list(method = "cosine", nn = nn_param)), "SVD" = list(name = "SVD", param = list(k = svd_k_param)), "Random" = list(name = "RANDOM", param = NULL)); results_topN <- tryCatch({ evaluate(scheme, method = algorithms_to_evaluate, type = "topNList", n = c(1, 3, k_eval, 10, 20)) }, error = function(e) { err_msg <- paste("Top-N Eval Error:", e$message); showNotification(err_msg, type = "error", duration = 10); message(err_msg); eval_status(err_msg); return(NULL) }); algorithms_for_rmse <- algorithms_to_evaluate[names(algorithms_to_evaluate) != "Random"]; results_rmse <- NULL; if (length(algorithms_for_rmse) > 0) { results_rmse <- tryCatch({ evaluate(scheme, method = algorithms_for_rmse, type = "ratings") }, error = function(e) { err_msg <- paste("RMSE Eval Error:", e$message); showNotification(err_msg, type = "error", duration = 10); message(err_msg); return(NULL) }) } else { showNotification("No algos for RMSE.", type="warning") }; removeNotification("eval-notify"); if(!is.null(results_topN) || !is.null(results_rmse)) { eval_status("Evaluation complete."); return(list(topN = results_topN, rmse = results_rmse, error = FALSE)) } else { if (eval_status() == "Running evaluation...") eval_status("Evaluation failed."); return(list(topN = NULL, rmse = NULL, error = TRUE)) } })
    output$evaluationResults <- renderPrint({ results_list <- evaluation_results(); req(results_list); if (results_list$error) { cat("Eval incomplete.\n"); return() }; if (is.null(results_list$topN) && is.null(results_list$rmse)) { cat("No results.\n"); return() }; cat("--- Evaluation Results ---\n\n"); if (!is.null(results_list$rmse)) { cat("RMSE (lower=better):\n"); tryCatch({ rmse_values <- getConfusionMatrix(results_list$rmse); rmse_df <- data.frame(Model = names(rmse_values), RMSE = sapply(rmse_values, function(x) x$RMSE)); print(rmse_df, row.names = FALSE) }, error = function(e){ cat("RMSE extract error.\n")}) } else { cat("RMSE N/A.\n") }; cat("\n--- Top-N Recommendation Quality ---\n"); if (!is.null(results_list$topN)) { cat(paste("P/R/TPR/FPR @ K up to", input$evalK, "(higher P/R/TPR, lower FPR)\n\n")); tryCatch({ print(avg(results_list$topN)) }, error = function(e){ cat("Top-N extract error.\n") }) } else { cat("Top-N N/A.\n") }; cat("\n--- Metric Definitions ---\nRMSE; Precision=TP/(Recs); Recall(TPR)=TP/(Relevant); FPR=FP/(NonRelevant)\n") })

    # --- Recommendations Dashboard Tab Server Logic ---
    trained_ubcf_model <- reactive({ mat <- reactive_rating_matrix(); req(mat); message("Training UBCF model for recs..."); showNotification("Training UBCF...", duration = 2); Recommender(mat, method = "UBCF", param = list(method = "cosine", nn = 25)) })
    trained_svd_model <- reactive({ mat <- reactive_rating_matrix(); req(mat); message("Training SVD model for recs..."); showNotification("Training SVD...", duration = 2); Recommender(mat, method = "SVD", param=list(k = 10)) })
    recs_ubcf <- reactiveVal(data.frame()); recs_svd <- reactiveVal(data.frame())
    observeEvent(input$getRecommendations, { recs_ubcf(data.frame()); recs_svd(data.frame()); model_ubcf <- trained_ubcf_model(); model_svd <- trained_svd_model(); mat <- reactive_rating_matrix(); meta <- rv$item_metadata; req(model_ubcf, model_svd, mat, meta, input$selectedUser, input$numRecs); user_id <- input$selectedUser; n_recs <- input$numRecs; user_index <- which(rownames(mat) == user_id); if(length(user_index) == 0) { showNotification(paste("User", user_id, "not found."), type="warning"); return() }; pred_ubcf <- tryCatch({ predict(model_ubcf, mat[user_index,], n = n_recs, type="topNList") }, error = function(e) { msg<-paste("UBCF Rec Error:", e$message); showNotification(msg, type="error"); message(msg); NULL }); if (!is.null(pred_ubcf) && length(getList(pred_ubcf)[[1]]) > 0) { rec_items <- getList(pred_ubcf)[[1]]; rec_df <- data.frame(Rank = 1:length(rec_items), ProductID = rec_items) %>% left_join(meta[, intersect(c("itemID", "productName", "category"), names(meta)), drop=FALSE], by = c("ProductID" = "itemID")) %>% select(any_of(c("Rank", "ProductID", "productName", "category"))) %>% head(n_recs); names(rec_df)[names(rec_df) == 'productName'] <- 'ProductName'; names(rec_df)[names(rec_df) == 'category'] <- 'Category'; recs_ubcf(rec_df) } else { recs_ubcf(data.frame(Message="No UBCF recs.")) }; pred_svd <- tryCatch({ predict(model_svd, mat[user_index,], n = n_recs, type="topNList") }, error = function(e) { msg<-paste("SVD Rec Error:", e$message); showNotification(msg, type="error"); message(msg); NULL }); if (!is.null(pred_svd) && length(getList(pred_svd)[[1]]) > 0) { rec_items <- getList(pred_svd)[[1]]; rec_df <- data.frame(Rank = 1:length(rec_items), ProductID = rec_items) %>% left_join(meta[, intersect(c("itemID", "productName", "category"), names(meta)), drop=FALSE], by = c("ProductID" = "itemID")) %>% select(any_of(c("Rank", "ProductID", "productName", "category"))) %>% head(n_recs); names(rec_df)[names(rec_df) == 'productName'] <- 'ProductName'; names(rec_df)[names(rec_df) == 'category'] <- 'Category'; recs_svd(rec_df) } else { recs_svd(data.frame(Message="No SVD recs.")) } })
    output$ubcfRecTitle <- renderText({ paste("Top", input$numRecs, "UBCF Recs for:", input$selectedUser) }); output$svdRecTitle <- renderText({ paste("Top", input$numRecs, "SVD Recs for:", input$selectedUser) })
    output$ubcfRecommendationsTable <- renderDT({ df <- recs_ubcf(); req(nrow(df) > 0); datatable(df, options = list(pageLength = 5, lengthChange=F, info=F, searching=F), rownames = FALSE, caption = tags$caption(style = 'caption-side: bottom; text-align: left;', 'User-Based Collaborative Filtering results.')) })
    output$svdRecommendationsTable <- renderDT({ df <- recs_svd(); req(nrow(df) > 0); datatable(df, options = list(pageLength = 5, lengthChange=F, info=F, searching=F), rownames = FALSE, caption = tags$caption(style = 'caption-side: bottom; text-align: left;', 'SVD results.')) })

} # End Server function

# --- Run the Application ---
shinyApp(ui = ui, server = server)