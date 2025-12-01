# R/helpers.R - 包含 DML 流程所需的配置生成、模型运行（真实 DML 逻辑）、结果绘图和 DT 表格渲染的辅助函数。

library(data.table)
library(ggplot2)
library(DT)
# 新增的库，用于支持用户提供的 DML 估计器
library(R6)
library(ranger)
library(glmnet)
library(stats)
library(xgboost)
library(nnet)  

# ===============================================================
# A. DML 核心逻辑类和辅助函数 (用户提供)
# ===============================================================

# ---------- DoubleMLData 类 (数据封装，支持权重) ----------
DoubleMLData <- R6Class("DoubleMLData",
                        public = list(
                          data = NULL,
                          y_col = NULL,
                          d_col = NULL,
                          x_cols = NULL,
                          weights_col = NULL,
                          
                          initialize = function(data, y_col, d_col, x_cols, weights_col = NULL) {
                            stopifnot(is.data.frame(data))
                            self$data <- data
                            self$y_col <- y_col
                            self$d_col <- d_col
                            self$x_cols <- x_cols
                            self$weights_col <- weights_col
                          },
                          
                          get_y = function() self$data[[self$y_col]],
                          get_d = function() self$data[[self$d_col]],
                          get_x = function() {
                            # 确保 X 是一个数据框，以便 ranger/glmnet 处理
                            return(self$data[, self$x_cols, with = FALSE])
                          },
                          get_w = function() {
                            if (!is.null(self$weights_col) && self$weights_col %in% names(self$data)) {
                              return(self$data[[self$weights_col]])
                            }
                            return(rep(1, nrow(self$data))) # 默认返回单位权重
                          }
                        )
)

# ---------- 辅助函数：交叉验证分组 ----------
cross_split <- function(n, K = 5, seed = 123) {
  set.seed(seed)
  folds <- sample(rep(1:K, length.out = n))
  lapply(1:K, function(k) list(train = which(folds != k), test = which(folds == k)))
}

# ---------- 辅助函数：机器学习拟合器 ----------
ml_fit <- function(x, y, w, method = "ranger") {
  if (method == "ranger") {
    # ranger 拟合 (随机森林)
    df <- data.frame(y = y, x)
    fit <- ranger::ranger(y ~ ., data = df, num.trees = 500)
    return(list(predict = function(newx) predict(fit, data = newx)$predictions))
  } else if (method == "lasso") {
    # 加权 LASSO 拟合 (glmnet)
    fit <- glmnet::cv.glmnet(as.matrix(x), y, alpha = 1, weights = w)
    return(list(predict = function(newx) as.numeric(predict(fit, as.matrix(newx), s = "lambda.min"))))
  } else if (method == "xgboost") {
    # XGBoost 拟合 (回归)
    dtrain <- xgboost::xgb.DMatrix(data = as.matrix(x), label = y, weight = w)
    params <- list(booster = "gbtree", objective = "reg:squarederror", eta = 0.1, max_depth = 4)
    fit <- xgboost::xgb.train(params = params, data = dtrain, nrounds = 100, verbose = 0)
    return(list(predict = function(newx) predict(fit, as.matrix(newx))))
  } else if (method == "nnet") {
    # 神经网络拟合 (nnet)
    # 注意: nnet 不直接支持权重，我们暂时忽略 w
    df <- data.frame(y = y, x)
    fit <- nnet::nnet(y ~ ., data = df, size = 5, linout = TRUE, trace = FALSE)
    return(list(predict = function(newx) as.numeric(predict(fit, newx))))
  } else {
    stop(paste("不支持的机器学习学习器:", method))
  }
}

# ---------- DoubleMLPLR 类 (修正后的加权 PLR 估计器) ----------
DoubleMLPLR <- R6Class("DoubleMLPLR",
                       public = list(
                         data = NULL,
                         ml_g = NULL,
                         ml_m = NULL,
                         n_folds = 5,
                         theta = NULL,
                         se = NULL,
                         ci = NULL,
                         
                         initialize = function(data, ml_g, ml_m, n_folds = 5) {
                           self$data <- data
                           self$ml_g <- ml_g
                           self$ml_m <- ml_m
                           self$n_folds <- n_folds
                         },
                         
                         fit = function() {
                           y <- self$data$get_y()
                           d <- self$data$get_d()
                           x <- self$data$get_x()
                           w <- self$data$get_w()
                           n <- length(y)
                           
                           folds <- cross_split(n, self$n_folds)
                           u_all <- numeric(n)
                           v_all <- numeric(n)
                           
                           # 1. 交叉拟合去噪
                           for (k in seq_along(folds)) {
                             idx_tr <- folds[[k]]$train
                             idx_te <- folds[[k]]$test
                             
                             # 将 data.table 子集转换为 data.frame 以适应 ml_fit 内部 ranger/glmnet 的处理
                             x_tr <- as.data.frame(x[idx_tr, , drop = FALSE])
                             x_te <- as.data.frame(x[idx_te, , drop = FALSE])
                             
                             g_fit <- ml_fit(x_tr, y[idx_tr], w[idx_tr], self$ml_g)
                             m_fit <- ml_fit(x_tr, d[idx_tr], w[idx_tr], self$ml_m)
                             
                             g_hat <- g_fit$predict(x_te)
                             m_hat <- m_fit$predict(x_te)
                             
                             u_all[idx_te] <- y[idx_te] - g_hat
                             v_all[idx_te] <- d[idx_te] - m_hat
                           }
                           
                           # 2. 修正后的加权 ATE 估计 (双重稳健估计)
                           sum_num <- sum(w * v_all * u_all)
                           sum_den <- sum(w * v_all^2)
                           
                           self$theta <- sum_num / sum_den
                           
                           # 3. 修正后的标准误 (SE) 和置信区间 (CI) 计算
                           
                           # 影响函数（已进行标准化）
                           den_ate <- sum_den
                           # psi_i = w_i * v_i * (u_i - v_i * theta) / [ Sum(w_j * v_j^2) / n ]
                           infl_all <- (w * v_all * (u_all - v_all * self$theta)) / (den_ate / n)
                           
                           # 标准误 (SE) 估计：使用影响函数的均方根
                           self$se <- sqrt(mean(infl_all^2) / n)
                           
                           self$ci <- c(self$theta - 1.96 * self$se, self$theta + 1.96 * self$se)
                         },
                         
                         summary = function() {
                           # 此函数仅用于调试，不直接在 Shiny 输出中使用
                           cat("========================================================\n")
                           cat("Double/Debiased Machine Learning (PLR) - Custom Weighted\n")
                           cat("========================================================\n")
                           cat(sprintf("Treatment Variable: %s\n", self$data$d_col))
                           cat(sprintf("Outcome Variable: %s\n", self$data$y_col))
                           cat(sprintf("ML Learners: g=%s, m=%s\n", self$ml_g, self$ml_m))
                           cat(sprintf("Weighted Estimate (theta): %.4f\n", self$theta))
                           cat(sprintf("Std. Error: %.4f\n", self$se))
                           cat(sprintf("95%% CI: [%.4f, %.4f]\n", self$ci[1], self$ci[2]))
                         }
                       )
)


# ===============================================================
# B. DML 运行主函数 (已替换为真实逻辑)
# ===============================================================

#' 运行单个 DML 模型并返回结果指标 (使用 DoubleMLPLR 真实估计)
run_dml_stages <- function(config, Q_type, G_type) {
  
  # --- 1. 输入检查与映射 ---
  # Map UI selection to internal ML function names
  map_ml <- function(type) {
    if (type == "rf") return("ranger")
    if (type == "glmnet") return("lasso")
    if (type == "xgb") return("xgboost") # 映射到新的 xgboost 实现
    if (type == "nn") return("nnet")     # 映射到新的 nnet 实现
    stop(paste("不支持的机器学习类型:", type))
  }
  
  ml_g_name <- map_ml(Q_type) # Learner for Y (Q model)
  ml_m_name <- map_ml(G_type) # Learner for D (G model)
  
  # --- 2. 准备 DoubleMLData 对象 ---
  data_dt <- config$Data
  weight_col <- config$Weight
  
  # 确保 weight_col 是 NULL 或有效的列名
  if (is.null(weight_col) || weight_col == "") {
    weight_col <- NULL
  }
  
  dml_data <- DoubleMLData$new(
    data = data_dt,
    y_col = config$Y,
    d_col = config$D,
    x_cols = config$C,
    weights_col = weight_col
  )
  
  # --- 3. 拟合 DoubleMLPLR 模型 ---
  dml_plr <- DoubleMLPLR$new(
    data = dml_data,
    ml_g = ml_g_name,
    ml_m = ml_m_name,
    n_folds = 5 # 使用默认 5 折交叉验证
  )
  
  dml_plr$fit()
  
  # --- 4. 提取和格式化结果 ---
  estimated_ate <- dml_plr$theta
  std_error <- dml_plr$se
  ci_lower <- dml_plr$ci[1]
  ci_upper <- dml_plr$ci[2]
  
  # 计算 P-Value: Z-统计量
  t_stat <- estimated_ate / std_error
  p_value <- 2 * pnorm(-abs(t_stat)) # 双边 P 值
  
  metrics_data <- data.table(
    Statistic = c("Estimate", "Std. Error", "P-Value", "CI (Lower)", "CI (Upper)"),
    Value = c(estimated_ate, std_error, p_value, ci_lower, ci_upper)
  )
  
  return(list(
    treatment = config$D,
    model_Q_type = Q_type,
    model_G_type = G_type,
    metrics = metrics_data
  ))
}

# ===============================================================
# C. 结果可视化辅助函数 (与之前一致)
# ===============================================================

#' 生成结果对比条形图
plot_comparison_bar <- function(analysis_results) {
  
  plot_data_list <- lapply(analysis_results, function(res) {
    if (is.null(res$metrics) || !any(res$metrics$Statistic == "Estimate")) return(NULL)
    
    metrics_dt <- res$metrics
    
    # 安全提取指标
    estimate <- metrics_dt[Statistic == "Estimate", Value][1]
    ci_lower <- metrics_dt[Statistic == "CI (Lower)", Value][1]
    ci_upper <- metrics_dt[Statistic == "CI (Upper)", Value][1]
    
    # 排除无效结果
    if (length(estimate) == 0 || is.na(estimate)) return(NULL)
    
    data.frame(
      Label = paste(res$treatment, res$model_Q_type, res$model_G_type, sep = " | "),
      Estimate = estimate,
      CI_Lower = ci_lower,
      CI_Upper = ci_upper
    )
  })
  
  plot_data <- do.call(rbind, plot_data_list)
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    return(ggplot() + labs(title = "无可展示的结果"))
  }
  
  ggplot(plot_data, aes(x = reorder(Label, Estimate), y = Estimate, fill = Estimate > 0)) +
    geom_col(color = "black", alpha = 0.7) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2, color = "darkgrey") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    scale_fill_manual(values = c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")) +
    coord_flip() +
    labs(
      title = "DML ATE/LATE 估计值 (95% CI)",
      x = "模型组合 (D | Q | G)",
      y = "估计系数 (Estimate)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title.y = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

# ===============================================================
# D. DT 表格渲染辅助函数 (已修复解析错误)
# ===============================================================

#' 生成 DML 结果的 DT 表格
create_results_datatable <- function(res_list) {
  
  rows <- lapply(res_list, function(res) {
    metrics <- res$metrics
    
    required_stats <- c("Estimate", "Std. Error", "P-Value", "CI (Lower)", "CI (Upper)")
    
    if (is.null(metrics) || !"Statistic" %in% names(metrics) || !"Value" %in% names(metrics)) {
      wide_values <- setNames(rep(NA_real_, length(required_stats)), required_stats)
    } else {
      metrics_wide <- data.table::dcast(metrics, . ~ Statistic, value.var = "Value")
      
      wide_values <- sapply(required_stats, function(stat) {
        if (stat %in% names(metrics_wide)) {
          return(metrics_wide[[stat]][1])
        } else {
          return(NA_real_)
        }
      }, simplify = TRUE)
    }
    
    data.table(
      Treatment = res$treatment,
      Model_Q = res$model_Q_type,
      Model_G = res$model_G_type,
      Estimate = wide_values["Estimate"],
      `Std. Error` = wide_values["Std. Error"],
      `P-Value` = wide_values["P-Value"],
      `CI (Lower)` = wide_values["CI (Lower)"],
      `CI (Upper)` = wide_values["CI (Upper)"]
    )
  })
  
  dt <- rbindlist(rows, fill = TRUE)
  dt <- dt[!is.na(Estimate)]
  
  DT::datatable(
    dt,
    caption = "DML ATE/LATE 估计结果",
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      scrollX = TRUE,
      # 修复解析错误的极度压缩列表，确保 'next' 参数有值
      language = list(
        search = "搜索:",
        info = "_START_ 到 _END_ 项，共 _TOTAL_ 项",
        paginate = list(
          previous = "上一页"
        )
      )
    ),
    rownames = FALSE
  ) |>
    formatRound(columns = c("Estimate", "Std. Error", "CI (Lower)", "CI (Upper)"), digits = 4) |>
    formatStyle(columns = "P-Value",
                backgroundColor = styleInterval(c(0.01, 0.05, 0.10), c("red", "gold", "lightcyan", "white")),
                color = styleInterval(c(0.05), c('darkred', 'black')))
}