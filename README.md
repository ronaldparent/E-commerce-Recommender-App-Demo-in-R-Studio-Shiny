# E-commerce-Recommender-App-Demo-in-R-Studio-Shiny
Using R  Framework, Shiny, shinythemes, recommenderlab, dplyr, ggplot2, DT, tools, htmltools

Precision Data Strategies - E-commerce Recommendation System Demo (R Shiny App)
  Explore the power of personalized product recommendations! This interactive R Shiny application, developed by Precision Data Strategies, demonstrates key concepts in building and evaluating recommendation systems for e-commerce platforms.

Overview
In today's competitive e-commerce landscape, presenting customers with relevant product suggestions is crucial for increasing engagement, cross-selling, and up-selling. This application provides a hands-on tool to:

Visualize user rating patterns and data characteristics.

Experiment with common recommendation algorithms (User-Based Collaborative Filtering & SVD).

Evaluate model performance using standard metrics.

See personalized recommendations generated for individual users.

It serves as an educational tool and a demonstrator for the types of recommendation solutions Precision Data Strategies can build.

✨ Features
The application is divided into several intuitive tabs:

📊 Data Source & Summary:

Choose between using built-in synthetic data or uploading your own transaction and product CSV files.

View a concise summary of the loaded data (users, items, ratings, sparsity).

Validate data format requirements for uploads.

📈 Exploratory Data Analysis (EDA):

Visualize the distribution of user ratings.

Identify the most frequently rated items (popular products).

Discover the most active users (highest number of ratings).

Get a visual sense of the user-item interaction matrix sparsity via a heatmap (on a sample).

⚙️ Model Training & Evaluation:

Configure parameters for model evaluation (train/test split, K for Top-N lists, algorithm-specific settings like neighbors/factors).

Train and evaluate two common recommendation models:

User-Based Collaborative Filtering (UBCF) with KNN: Recommends items based on what similar users liked.

Matrix Factorization (SVD): Uncovers latent features connecting users and items.

Compare model performance against a random baseline using:

RMSE: How accurate are rating predictions? (Lower is better)

Precision & Recall @ K: How good are the Top-K recommended lists? (Higher is better)

🎁 Personalized Recommendations:

Select any user from the loaded dataset.

Specify the number of recommendations (N) desired.

View the Top-N product recommendations generated specifically for that user by both the UBCF and SVD models.

See product names and categories alongside the recommendations.

🚀 Technology Stack
Language: R

Framework: Shiny & shinythemes

Core Recommendation Logic: recommenderlab

Data Manipulation: dplyr

Plotting: ggplot2

Interactive Tables: DT

Utilities: tools, htmltools

🎯 Use Cases & Benefits
This demonstration highlights capabilities relevant to:

E-commerce Businesses: Understand how recommendation engines work and visualize potential insights from your own customer data.

Data Analysts/Scientists: Experiment with recommenderlab parameters and evaluation metrics in an interactive environment.

Marketing Teams: See how personalized recommendations can be tailored to individual users to potentially increase conversion rates and average order value.

Product Managers: Evaluate the feasibility and potential impact of adding recommendation features to a platform.

Benefits of Implementing Recommendations:

Improved Customer Experience: Show users products they are genuinely likely to be interested in.

Increased Sales: Drive cross-sells and up-sells by suggesting relevant items.

Enhanced Engagement: Keep users browsing longer by surfacing interesting products.

Better Inventory Management: Gain insights into product relationships and popularity.

🛠️ Setup & Installation

Install R & RStudio: Ensure you have a recent version of R and RStudio installed.

Install R Packages: Open RStudio and run the following command in the console to install required packages:

install.packages(c("shiny", "shinythemes", "recommenderlab", "ggplot2", "dplyr", "DT", "reshape2", "tools", "htmltools"))

(Note: recommenderlab might require additional system libraries depending on your OS. Consult its documentation if installation fails.)

Create www Folder: In the main project directory (where app.R is located), create a new folder named www.

Create styles.css: Inside the www folder, create a file named styles.css and paste the CSS code provided here (or use the code from the previous conversation).

▶️ Running the App
Open the app.R file in RStudio.

Click the "Run App" button located at the top of the RStudio editor pane.

The application should launch in a new window or in the RStudio Viewer pane.

💾 Data Format for Upload
To use your own data, ensure your CSV files adhere to the following (case-sensitive headers):

Transactions File (.csv):

userID: Unique identifier for each user (character/string).

itemID: Unique identifier for each item (character/string). Must match itemIDs in the Product File.

rating: Numerical rating given by the user to the item (e.g., 1-5).

Product Metadata File (.csv):

itemID: Unique identifier for each item (character/string). Must match itemIDs in the Transactions File.

productName: Name of the product (character/string).

(Optional) category: Product category (character/string).

(Optional) description: Product description (character/string).

The application automatically generates consistent synthetic_transactions.csv and synthetic_products.csv files on startup, which can serve as formatting templates.

⚠️ Disclaimer & Consultation
This application is provided by Precision Data Strategies for demonstration purposes only. The models and data are illustrative and may not reflect the complexities of a production-level recommendation system.

Interested in implementing a tailored recommendation system for your business?

👉 Book a consultation at https://precisiondatastrategies.com

📄 License - This project is licensed under the MIT License - see the LICENSE.md file for details.

© 2025 Precision Data Strategies. All rights reserved.
