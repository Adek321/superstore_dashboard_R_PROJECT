# ---- Install Required Packages (Run Once) ----
install.packages(c("readxl", "dplyr", "ggplot2", "lubridate", "forcats",
                   "tidyr", "plotly", "crosstalk", "shiny", "ggthemes", "patchwork", "htmlwidgets"))

# ---- Load Libraries ----
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(tidyr)
library(plotly)
library(crosstalk)
library(shiny)
library(ggthemes)
library(patchwork)
library(htmlwidgets)

# ---- Load and Clean Dataset ----
data <- read_excel("C:/Users/hp/Desktop/sample_-_superstore.xls")

data_clean <- data %>%
  rename_with(~ gsub(" ", ".", .x)) %>%
  mutate(Order.Date = as.Date(Order.Date),
         Month = floor_date(Order.Date, "month")) %>%
  filter(!is.na(Sales), !is.na(Profit))

# ---- Shared Data for Crosstalk ----
shared_data <- SharedData$new(data_clean)
# ---- Plot 1: Monthly Sales Trend ----
p1 <- ggplot(shared_data$data(), aes(x = Month, y = Sales)) +
  geom_line(color = "steelblue", linewidth = 1.2)+
  geom_point(color = "red", size = 2, alpha = 0.6) +
  labs(title = "📈 Monthly Sales Trend", x = "Month", y = "Sales") +
  theme_minimal() +
  annotate("text", x = as.Date("2017-11-01"), y = max(data_clean$Sales)*0.8, 
           label = "Peak Sales Season", color = "darkred")

# ---- Plot 2: Profit by Category ----
p2 <- ggplot(shared_data$data(), aes(x = Category, y = Profit, fill = Category)) +
  geom_col(width = 0.6) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "💰 Profit by Category", y = "Profit") +
  theme_minimal()

# ---- Plot 3: Sales vs Profit by Region ----
p3 <- ggplot(shared_data$data(), aes(x = Sales, y = Profit, color = Region)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Sales vs Profit by Region", x = "Sales", y = "Profit") +
  theme_light()

# ---- Plot 4: Top 10 Sub-Categories by Sales ----
top_subs <- shared_data$data() %>%
  group_by(`Sub-Category`) %>%
  summarise(TotalSales = sum(Sales)) %>%
  arrange(desc(TotalSales)) %>%
  slice_head(n = 10)

p4 <- ggplot(top_subs, aes(x = reorder(`Sub-Category`, TotalSales), y = TotalSales)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "🏆 Top 10 Sub-Categories by Sales", x = "Sub-Category", y = "Sales") +
  theme_minimal()

# ---- Plot 5: Profit Distribution by Region ----
p5 <- ggplot(shared_data$data(), aes(x = Region, y = Profit, fill = Region)) +
  geom_boxplot() +
  labs(title = "📊 Profit Distribution by Region", y = "Profit") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic()

# ---- Convert Plots to Interactive Plotly ----
ip1 <- ggplotly(p1, tooltip = c("Month", "Sales"))
ip2 <- ggplotly(p2, tooltip = c("Category", "Profit"))
ip3 <- ggplotly(p3, tooltip = c("Sales", "Profit", "Region"))
ip4 <- ggplotly(p4, tooltip = c("Sub-Category", "TotalSales"))
ip5 <- ggplotly(p5, tooltip = c("Region", "Profit"))

# ---- SHINY APP: Interactive Dashboard ----
ui <- fluidPage(
  titlePanel("📊 Advanced Sales Dashboard"),
  sidebarLayout(
    sidebarPanel(
      filter_slider("SalesFilter", "🔍 Filter by Sales", shared_data, ~Sales)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Monthly Sales", ip1),
        tabPanel("Category Profit", ip2),
        tabPanel("Sales vs Profit", ip3),
        tabPanel("Top Sub-Categories", ip4),
        tabPanel("Region Profit", ip5)
      )
    )
  )
)

server <- function(input, output, session) {}

# ---- Run the App ----
shinyApp(ui = ui, server = server)

# ---- Save Plot as HTML (optional) ----
saveWidget(ip1, "C:/Users/hp/Desktop/monthly_sales.html", selfcontained = TRUE)

