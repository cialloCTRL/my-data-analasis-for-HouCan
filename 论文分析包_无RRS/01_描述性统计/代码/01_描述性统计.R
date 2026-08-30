# -*- coding: utf-8 -*-
# 01_描述性统计.R — 无 RRS 版描述性统计
#   样本量/年龄/性别；各量表 Cronbach's α；操纵检查（单样本 t 检验 vs 4 分中点）；
#   关键量表均分 M/SD/range。
# 输入: ../数据/数据_去RRS新版.xlsx（sheet=数据）
# 输出: ../表格.md（本目录下，随脚本生成）
suppressMessages({ library(openxlsx) })

SCRIPT_DIR <- { a <- commandArgs(trailingOnly = FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)]); if (length(f)) dirname(normalizePath(f)) else getwd() }
FOLDER <- dirname(SCRIPT_DIR)
PKG <- dirname(FOLDER)
DATA_PATH <- file.path(PKG, "数据", "数据_去RRS新版.xlsx")

md <- function(...) {
  cat(sprintf(...), "\n", file = file.path(FOLDER, "表格.md"), append = TRUE)
}

dat <- read.xlsx(DATA_PATH, sheet = "数据")
N <- nrow(dat)

# ---------- 样本描述 ----------
age <- as.numeric(dat$age)
male_n <- sum(dat$gender == 1, na.rm = TRUE)
female_n <- N - male_n

# ---------- 信度 ----------
items <- list(
  CAIDS_total     = paste0("CAIDS_", 1:7),
  CAIDS_anxiety   = paste0("CAIDS_", 1:4),
  CAIDS_avoidance = paste0("CAIDS_", 5:7),
  PHQ9            = paste0("PHQ9_", 1:9),
  GAD7            = paste0("GAD7_", 1:7)
)
alpha_df <- data.frame(
  scale = names(items), n_items = sapply(items, length),
  alpha = sapply(items, function(v) psych::alpha(dat[, v], check.keys = FALSE, warnings = FALSE)$total$raw_alpha))

# ---------- 操纵检查 ----------
manip <- data.frame(
  open_up = dat$open_up, felt_support = dat$felt_support,
  emotion_arousal = dat$emotion_arousal,
  total = dat$open_up + dat$felt_support + dat$emotion_arousal)
tt <- function(x, mu) { t <- (mean(x) - mu) / (sd(x) / sqrt(N)); c(M = mean(x), SD = sd(x), t = t, p = 2 * pt(-abs(t), N - 1), d = t / sqrt(N)) }
manip_res <- rbind(
  open_up = tt(manip$open_up, 4),
  felt_support = tt(manip$felt_support, 4),
  emotion_arousal = tt(manip$emotion_arousal, 4),
  total = tt(manip$total, 12))

# ---------- 量表均分 ----------
scales <- c("CAIDS_anxiety_avg", "CAIDS_avoidance_avg", "CAIDS_total_avg", "PHQ9_avg", "GAD7_avg")
desc <- data.frame(
  scale = scales,
  M  = sapply(scales, function(v) mean(dat[[v]], na.rm = TRUE)),
  SD = sapply(scales, function(v) sd(dat[[v]], na.rm = TRUE)),
  min = sapply(scales, function(v) min(dat[[v]], na.rm = TRUE)),
  max = sapply(scales, function(v) max(dat[[v]], na.rm = TRUE)))

# ================= 写表格.md =================
file.remove(file.path(FOLDER, "表格.md"))
md("# 描述性统计结果（无 RRS 版）")
md("")
md("> 说明：本包已按导师意见剔除反刍（RRS）全部量表（症状反刍/强迫思考/反省深思/总量表）及其参与的分析。本表为剔除后的描述性统计。")
md("")
md("## 1. 样本描述（N = %d）", N)
md("")
md("| 指标 | M | SD | 范围 |")
md("|---|---|---|---|")
md("| 年龄 | %.2f | %.2f | %d–%d |", mean(age), sd(age), min(age), max(age))
md("| 性别 | 男 %d 人（%.1f%%）| 女 %d 人（%.1f%%）| — |", male_n, 100 * male_n / N, female_n, 100 * female_n / N)
md("")
md("## 2. 量表信度（Cronbach's α，无 RRS）")
md("")
md("| 量表 | 条目数 | α |")
md("|---|---|---|")
for (i in seq_len(nrow(alpha_df))) md("| %s | %d | %.3f |", alpha_df$scale[i], alpha_df$n_items[i], alpha_df$alpha[i])
md("")
md("## 3. 操纵检查（单样本 t 检验，与 4 分中点比较，N = %d）", N)
md("")
md("| 操纵题 | M | SD | t | p | Cohen's d |")
md("|---|---|---|---|---|---|")
for (i in seq_len(nrow(manip_res))) {
  md("| %s | %.2f | %.3f | %.2f | %s | %.2f |",
     rownames(manip_res)[i], manip_res[i, "M"], manip_res[i, "SD"], manip_res[i, "t"],
     ifelse(manip_res[i, "p"] < 0.001, "< 0.001", sprintf("%.3f", manip_res[i, "p"])), manip_res[i, "d"])
}
md("")
md("## 4. 关键量表均分描述（无 RRS）")
md("")
md("| 量表 | M | SD | min | max |")
md("|---|---|---|---|---|")
for (i in seq_len(nrow(desc))) md("| %s | %.3f | %.3f | %.2f | %.2f |", desc$scale[i], desc$M[i], desc$SD[i], desc$min[i], desc$max[i])

cat("表格.md 已生成：", file.path(FOLDER, "表格.md"), "\n")
