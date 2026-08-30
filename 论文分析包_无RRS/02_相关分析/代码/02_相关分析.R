# -*- coding: utf-8 -*-
# 02_相关分析.R — 无 RRS 版相关分析
#   ① 量表间相关（5 个量表：依恋焦虑/依恋回避/总依恋/抑郁PHQ9/焦虑GAD7）+ 热力图
#   ② TextMind 137 变量 × 3 个依恋指标 的皮尔逊相关（零方差剔除），显著变量表 + 热力图
# 输入: ../数据/数据_去RRS新版.xlsx（sheet=数据）
# 输出: 表格.md、图片/scale_heatmap.png、图片/tm_sig_*.png
suppressMessages({ library(openxlsx); library(corrplot) })

SCRIPT_DIR <- { a <- commandArgs(trailingOnly = FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)]); if (length(f)) dirname(normalizePath(f)) else getwd() }
FOLDER <- dirname(SCRIPT_DIR)
PKG <- dirname(FOLDER)
DATA_PATH <- file.path(PKG, "数据", "数据_去RRS新版.xlsx")
IMG <- file.path(FOLDER, "图片")
dir.create(IMG, showWarnings = FALSE)

md <- function(...) cat(sprintf(...), "\n", file = file.path(FOLDER, "表格.md"), append = TRUE)

dat <- read.xlsx(DATA_PATH, sheet = "数据")
N <- nrow(dat)

# ---------- 定位列 ----------
non_tm <- c("pid","Q1","Q2","Q3","JSPSYCH_2","open_up","felt_support","emotion_arousal",
            "age","gender", paste0("CAIDS_",1:7), paste0("PHQ9_",1:9), paste0("GAD7_",1:7),
            "CAIDS_anxiety_avg","CAIDS_avoidance_avg","CAIDS_total_avg","PHQ9_avg","GAD7_avg")
llm_cols <- grep("^LLM_", names(dat), value = TRUE)
tm_names <- setdiff(names(dat), c(non_tm, llm_cols))
cat("TextMind 变量数:", length(tm_names), "\n")
TM <- dat[, tm_names]

DVS <- c(CAIDS_anxiety_avg="依恋焦虑", CAIDS_avoidance_avg="依恋回避", CAIDS_total_avg="总依恋(探索)")
SCALES5 <- c("CAIDS_anxiety_avg","CAIDS_avoidance_avg","CAIDS_total_avg","PHQ9_avg","GAD7_avg")
ZH5 <- c(CAIDS_anxiety_avg="依恋焦虑", CAIDS_avoidance_avg="依恋回避", CAIDS_total_avg="总依恋",
         PHQ9_avg="抑郁(PHQ9)", GAD7_avg="焦虑(GAD7)")

safe_cor <- function(x, y) {
  if (sd(x, na.rm=TRUE) == 0 || sd(y, na.rm=TRUE) == 0) return(c(r=NA_real_, p=NA_real_))
  ct <- cor.test(x, y); c(r=unname(ct$estimate), p=ct$p.value)
}

file.remove(file.path(FOLDER, "表格.md"))
md("# 相关分析结果（无 RRS 版）")
md("")
md("> 说明：已剔除反刍（RRS）全部量表。量表相关由 9×9 缩减为 5×5；TextMind×依恋相关不受 RRS 影响，本处照常报告。")

# ---------- ① 量表 5x5 相关 ----------
S <- dat[, SCALES5]
R <- cor(S, use="complete.obs")
P <- matrix(1, 5, 5, dimnames=list(SCALES5, SCALES5))
for (i in 1:5) for (j in 1:5) if (i != j) P[i,j] <- cor.test(S[[i]], S[[j]])$p.value
stars <- matrix("", 5, 5, dimnames=list(SCALES5, SCALES5))
stars[P < 0.05] <- "*"; stars[P < 0.01] <- "**"; stars[P < 0.001] <- "***"

png(file.path(IMG, "scale_heatmap.png"), width=1500, height=1400, res=180)
corrplot(R, method="color", col=colorRampPalette(c("#2166AC","white","#B2182B"))(200),
         type="upper", tl.col="black", tl.cex=0.8, addCoef.col="black", number.cex=0.8,
         number.digits=2, p.mat=P, sig.level=0.05, insig="label_sig", pch.col="red",
         pch.cex=1.2, mar=c(0,0,1,0), tl.pos="lt",
         title="量表相关（无 RRS）(* p<.05)")
dev.off()

md("")
md("## 1. 量表间相关（N = %d，无 RRS）", N)
md("")
md("| | 依恋焦虑 | 依恋回避 | 总依恋 | 抑郁 | 焦虑 |")
md("|---|---|---|---|---|---|")
for (i in 1:5) {
  cell <- sapply(1:5, function(j) if (i >= j) sprintf("%.3f%s", R[i,j], stars[i,j]) else "")
  md("| %s | %s | %s | %s | %s | %s |", ZH5[SCALES5[i]], cell[1], cell[2], cell[3], cell[4], cell[5])
}
md("")
md("注：下三角为 r，星号标显著（* p<.05，** p<.01，*** p<.001）。总依恋为补充指标。")

# ---------- ② TextMind x 依恋 ----------
md("")
md("## 2. TextMind 变量与依恋的相关（探索性描述，N = %d）", N)
md("")
for (dv in names(DVS)) {
  y <- dat[[dv]]
  tmp <- sapply(TM, safe_cor, y=y)
  rv <- tmp["r", ]; pv <- tmp["p", ]
  n_zv <- sum(is.na(rv))
  sig <- !is.na(rv) & pv < 0.05
  nsig <- sum(sig)
  r_sig <- rv[sig]
  md("### %s", DVS[[dv]])
  md("")
  if (nsig == 0) { md("无显著相关变量。"); next }
  md("显著相关变量 %d 个（|r| = %.2f–%.2f）：", nsig, min(abs(r_sig)), max(abs(r_sig)))
  md("")
  ord <- order(rv[sig], decreasing = TRUE)
  tab <- data.frame(变量 = names(rv)[sig][ord], r = round(rv[sig][ord], 3), p = round(pv[sig][ord], 4))
  md("| 变量 | r | p |")
  md("|---|---|---|")
  for (k in seq_len(nrow(tab))) md("| %s | %+.3f | %s |", tab$变量[k], tab$r[k],
                                    ifelse(tab$p[k] < 0.001, "< 0.001", sprintf("%.3f", tab$p[k])))
  md("")
  # 显著热力图（r 排序，对应论文附图一/二）
  ord2 <- order(rv[sig])
  mm <- matrix(rv[sig][ord2], ncol=1, dimnames=list(names(rv)[sig][ord2], DVS[[dv]]))
  pm <- matrix(pv[sig][ord2], ncol=1, dimnames=list(names(rv)[sig][ord2], DVS[[dv]]))
  png(file.path(IMG, sprintf("tm_sig_%s.png", dv)), width=560, height=max(300, 80+22*nsig), res=150)
  par(mar=c(3, 9, 3, 1))
  corrplot(mm, method="color", col=colorRampPalette(c("#2166AC","white","#B2182B"))(200),
           is.corr=TRUE, tl.col="black", tl.cex=0.7, cl.cex=0.7, tl.srt=0,
           p.mat=pm, sig.level=0.05, insig="label_sig", pch.col="red", pch.cex=0.8,
           title=sprintf("TextMind × %s 显著相关 (* p<.05)", DVS[[dv]]))
  dev.off()
  md("图片：`图片/tm_sig_%s.png`", dv)
  md("")
}
cat("完成：表格.md + 图片已输出到", FOLDER, "\n")
