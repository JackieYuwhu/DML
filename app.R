# app.R - 双重机器学习 (DML) 结果分析 Shiny 应用的主入口文件
# 此文件自动加载 ui.R 和 server.R

library(shiny)


 ui <- source("R/ui.R", local = TRUE)$value
 server <- source("R/server.R", local = TRUE)$value

shinyApp(ui = ui, server = server)