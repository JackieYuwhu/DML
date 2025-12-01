# server.R - 定义双重机器学习 (DML) 结果分析的服务器逻辑

library(shiny)
library(data.table)
library(DT)
library(shinyWidgets)
library(ggplot2)

tryCatch({
  source("R/helpers.R", local = TRUE)
}, error = function(e) {
  stop(paste("致命错误：无法加载 R/helpers.R 文件。请确保 'helpers.R' 文件位于名为 'R' 的子目录中。原始错误:", e$message))
})

server <- function(input, output, session) {
  
  # ----------------------------------------------------
  # I. 数据上传与预处理
  # ----------------------------------------------------
  uploaded_data <- reactive({
    req(input$file_upload)
    
    file_path <- input$file_upload$datapath
    ext <- tools::file_ext(file_path)
    
    data <- NULL
    
    # 使用更健壮的 tryCatch 结构并明确指定 shiny::showNotification
    if (tolower(ext) == "csv") {
      data <- tryCatch({
        data.table::fread(file_path)
      }, error = function(e) {
        shiny::showNotification(paste("CSV 文件读取失败：", e$message), type = "error")
        return(NULL)
      })
    } else if (tolower(ext) == "rds") {
      data <- tryCatch({
        readRDS(file_path)
      }, error = function(e) {
        shiny::showNotification(paste("RDS 文件读取失败：", e$message), type = "error")
        return(NULL)
      })
    } else {
      shiny::showNotification("不支持的文件格式！请上传 .csv 或 .rds 文件。", type = "error")
      return(NULL)
    }
    
    # 在处理数据前检查是否成功读取
    validate(need(!is.null(data), "文件读取失败或数据为空"))
    
    data <- as.data.table(data)
    validate(need(ncol(data) > 0, "上传的数据为空"))
    return(data)
  })
  
  # ----------------------------------------------------
  # II. 动态 UI 更新
  # ----------------------------------------------------
  observeEvent(uploaded_data(), {
    data <- uploaded_data()
    req(data)
    
    cols <- names(data)
    
    updateSelectInput(session, "outcome_variable",
                      choices = cols,
                      selected = if ("Y" %in% cols) "Y" else cols[1])
    
    updatePickerInput(session, "potential_D_C_variables", choices = cols)
    updatePickerInput(session, "always_C_variables", choices = cols)
    
    updateSelectInput(session, "weight_variable",
                      choices = c("无权重" = "", cols),
                      selected = if ("weight" %in% cols) "weight" else "")
  })
  
  # ----------------------------------------------------
  # III. 运行 DML 主逻辑 (使用 helpers.R 中的函数)
  # ----------------------------------------------------
  analysis_results <- eventReactive(input$run_analysis, {
    req(uploaded_data())
    
    # 必要的输入检查
    validate(
      need(input$outcome_variable != "", "请选择结果变量"),
      need(length(input$potential_D_C_variables) > 0, "需选择至少一个潜在 D 或 C 变量"),
      need(length(input$model_Q_selection) > 0, "需至少选择一个模型 Q"),
      need(length(input$model_G_selection) > 0, "需至少选择一个模型 G")
    )
    
    data_in <- uploaded_data()
    
    # 1. 配置生成 (调用 helpers.R 中的函数)
    config_list <- tryCatch({
      prepare_dml_configurations(
        data = data_in,
        potential_D_C_vars = input$potential_D_C_variables,
        always_C_vars = input$always_C_variables,
        outcome_var = input$outcome_variable,
        weight_var = input$weight_variable
      )
    }, error = function(e) {
      shiny::showNotification(paste("配置生成失败：", e$message), type = "error")
      return(NULL)
    })
    
    validate(need(length(config_list) > 0, "DML 配置为空，无法运行"))
    
    total_runs <- length(config_list) *
      length(input$model_Q_selection) *
      length(input$model_G_selection)
    
    if (total_runs == 0) {
      shiny::showNotification("无可运行的模型组合", type = "warning")
      return(NULL)
    }
    
    # 2. 正式运行 DML
    results <- list()
    
    withProgress(message = "DML 分析中", value = 0, {
      counter <- 0
      
      for (treat_name in names(config_list)) {
        config <- config_list[[treat_name]]
        
        for (Q_type in input$model_Q_selection) {
          for (G_type in input$model_G_selection) {
            
            counter <- counter + 1
            setProgress(counter / total_runs,
                        detail = sprintf("T:%s | Q:%s | G:%s",
                                         treat_name, Q_type, G_type))
            
            key <- paste(treat_name, Q_type, G_type, sep = "_")
            
            # 运行单模型 (调用 helpers.R 中的函数)
            out <- tryCatch({
              run_dml_stages(config, Q_type, G_type)
            }, error = function(e) {
              shiny::showNotification(
                sprintf("模型失败 (%s / %s / %s): %s",
                        treat_name, Q_type, G_type, e$message),
                type = "error"
              )
              return(NULL)
            })
            
            if (!is.null(out) && !is.null(out$metrics)) {
              results[[key]] <- list(
                treatment = treat_name,
                model_Q_type = Q_type,
                model_G_type = G_type,
                metrics = out$metrics
              )
            }
          }
        }
      }
    })
    
    if (length(results) == 0) {
      output$analysis_status <- renderText({"分析失败，无有效结果。"})
      shiny::showNotification("所有模型均失败，无结果。", type = "warning")
      return(NULL)
    }
    
    output$analysis_status <- renderText({
      sprintf("分析完成，共成功运行 %d 个模型组合。", length(results))
    })
    
    return(results)
  })
  
  # ----------------------------------------------------
  # IV. 输出
  # ----------------------------------------------------
  output$data_preview <- DT::renderDataTable({
    req(uploaded_data())
    head(uploaded_data(), 100)
  }, options = list(pageLength = 10))
  
  output$main_bar_chart <- renderPlot({
    req(analysis_results())
    # 调用 helpers.R 中的函数
    plot_comparison_bar(analysis_results())
  })
  
  output$results_table <- DT::renderDataTable({
    req(analysis_results())
    # 调用 helpers.R 中的函数
    create_results_datatable(analysis_results())
  })
}