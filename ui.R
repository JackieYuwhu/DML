# ui.R - 定义双重机器学习 (DML) 结果分析的 UI 界面

library(shiny)
library(DT)
library(shinyWidgets)
library(ggplot2)

# 定义 UI 
ui <- fluidPage(
  
  # ----------------------------------------------------
  # I. 引入外部资源 (CSS & JS)
  # ----------------------------------------------------
  tags$head(
    tags$style(HTML("
            /* 自定义样式以适应 DML 应用 */
            .navbar-brand {
                font-weight: bold;
                font-size: 24px;
                padding-top: 5px; /* 调整垂直对齐 */
                display: flex;
                align-items: center;
            }
            .logo-img {
                height: 35px; /* 徽标高度 */
                margin-right: 10px;
            }
        "))
  ),
  
  # ----------------------------------------------------
  # II. 导航栏布局 (使用 logo.png)
  # ----------------------------------------------------
  navbarPage(
    # 导航栏标题中包含 logo.png (只显示 Logo，不显示文字标题)
    title = div(
      img(src = "logo.png", class = "logo-img") 
    ),
    
    # ----------------------------------------------------
    # Tab 1: 数据上传与配置
    # ----------------------------------------------------
    tabPanel("数据与配置",
             sidebarLayout(
               # A. 侧边栏: 控制面板
               sidebarPanel(
                 width = 3,
                 h3("数据上传"),
                 fileInput("file_upload", 
                           "选择 CSV 或 RDS 文件",
                           accept = c(".csv", ".rds")),
                 
                 hr(),
                 h3("DML 配置"),
                 
                 # 结果变量 Y
                 selectInput("outcome_variable", "1. 结果变量 (Y):", choices = NULL),
                 
                 # 潜在 D 和 C 变量 (多选)
                 # 用户在此选择所有潜在的协变量和处理变量
                 pickerInput("potential_D_C_variables", 
                             "2. 潜在的处理/协变量 (D, C):", 
                             choices = NULL, 
                             options = list(`actions-box` = TRUE),
                             multiple = TRUE),
                 
                 # 始终作为协变量 C 的变量 (可选, 多选)
                 # 确保这些变量不被选为处理变量 D
                 pickerInput("always_C_variables", 
                             "3. 始终作为协变量 (C): (可选)", 
                             choices = NULL, 
                             options = list(`actions-box` = TRUE),
                             multiple = TRUE),
                 
                 # 权重变量 (可选)
                 selectInput("weight_variable", "4. 权重变量 (W): (可选)", choices = NULL),
                 
                 hr(),
                 h3("机器学习模型选择"),
                 
                 # 模型 Q (用于拟合 Y ~ X)
                 selectInput(
                   inputId = "model_Q_selection",
                   label = "5. 模型 Q (Y ~ X) 选择:",
                   choices = c("随机森林" = "rf", "LASSO/Ridge" = "glmnet", "XGBoost" = "xgb", "神经网络" = "nn"),
                   selected = NA,
                   multiple = TRUE
                 ),
                 
                 # 模型 G (用于拟合 D ~ X)
                 selectInput(
                   inputId = "model_G_selection",
                   label = "6. 模型 G (D ~ X) 选择:",
                   choices = c("随机森林" = "rf", "LASSO/Ridge" = "glmnet", "XGBoost" = "xgb", "神经网络" = "nn"),
                   selected = NA, 
                   multiple = TRUE
                 ),
                 
                 # 运行按钮
                 actionButton("run_analysis", "运行 DML 分析", 
                              icon = icon("play-circle"), 
                              class = "btn-success btn-lg btn-block")
               ),
               
               # B. 主面板: 数据预览
               mainPanel(
                 width = 9,
                 h3("原始数据预览"),
                 DT::dataTableOutput("data_preview")
               )
             )
    ),
    
    # ----------------------------------------------------
    # Tab 2: 结果分析与可视化
    # ----------------------------------------------------
    tabPanel("结果与可视化",
             fluidRow(
               column(12, 
                      h3("分析状态"),
                      textOutput("analysis_status"),
                      hr()
               )
             ),
             fluidRow(
               column(12, 
                      h3("DML 估计结果对比图"),
                      plotOutput("main_bar_chart", height = "500px")
               )
             ),
             fluidRow(
               column(12, 
                      hr(),
                      h3("详细结果数据表"),
                      DT::dataTableOutput("results_table")
               )
             )
    )
  )
)