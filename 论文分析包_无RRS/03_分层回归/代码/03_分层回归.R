# -*- coding: utf-8 -*-
# 03_分层回归.R — 无 RRS 版分层回归（理论驱动，块级 ΔR²）
#   L1 人口学（年龄、性别）→ L2 量表（抑郁 PHQ9、焦虑 GAD7，已剔除 RRS）→ L3 词汇类别块（9 块）
#   每个 DV（依恋焦虑/依恋回避/总依恋）× 每个类别块独立检验，报告 ΔR²/ΔF/Δp 与标准化 β。
# 输入: ../数据/数据_去RRS新版.xlsx（sheet=数据）
# 输出: 表格.md
suppressMessages({ library(openxlsx); library(car) })

SCRIPT_DIR <- { a <- commandArgs(trailingOnly = FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)]); if (length(f)) dirname(normalizePath(f)) else getwd() }
FOLDER <- dirname(SCRIPT_DIR)
PKG <- dirname(FOLDER)
DATA_PATH <- file.path(PKG, "数据", "数据_去RRS新版.xlsx")
md <- function(...) cat(sprintf(...), "\n", file = file.path(FOLDER, "表格.md"), append = TRUE)

dat <- read.xlsx(DATA_PATH, sheet = "数据")
N <- nrow(dat)
dat$age <- as.numeric(dat$age)

CTRL <- c("age", "gender")                  # 性别哑变量（1=男），避免与 TextMind male 变量冲突
SCALE <- c("PHQ9_avg", "GAD7_avg")          # 已剔除 RRS
DVS <- c(CAIDS_anxiety_avg="依恋焦虑", CAIDS_avoidance_avg="依恋回避", CAIDS_total_avg="总依恋(探索)")
SCALE_ZH <- c(PHQ9_avg="抑郁(PHQ9)", GAD7_avg="焦虑(GAD7)")

CATBLOCKS <- list(
  Social     = c("Social","socbehav","prosocial","polite","conflict","moral","comm",
                 "socrefs","family","friend","female","male","Humans"),
  ppron      = c("ppron","i","we","you","YouPL","shehe","they"),
  Affect     = c("affect","tone_pos","tone_neg","emotion","emo_pos","emo_neg",
                 "emo_anx","emo_anger","emo_sad","swear"),
  States     = c("need","want","acquire","lack","fulfill","fatigue"),
  Motives    = c("reward","risk","curiosity","allure"),
  Cognition  = c("Cognition","allnone","cogproc","insight","cause","discrep","tentat",
                 "certitude","Inhibition","Inclusive","Exclusive","differ","memory"),
  Perception = c("Perception","attention","motion","space","visual","auditory","feeling"),
  Time       = c("time","focuspast","focuspresent","focusfuture","TenseM","ProgM",
                 "tPast","tNow","tFuture"),
  Relative   = c("Relative"))
CATBLOCKS <- lapply(CATBLOCKS, function(vs) vs[sapply(dat[, vs, drop=FALSE], function(x) sd(x, na.rm=TRUE) > 0)])

file.remove(file.path(FOLDER, "表格.md"))
md("# 分层回归结果（无 RRS 版）")
md("")
md("> 说明：第二层量表已剔除反刍（RRS），仅保留抑郁（PHQ-9）与焦虑（GAD-7）。第三层为 9 个理论映射的 TextMind 词汇类别块，逐块独立检验块级 ΔR²。")
md("")

rows <- list(); scale_rows <- list(); block_rows <- list()
for (dv in names(DVS)) {
  f1 <- as.formula(sprintf("%s ~ %s", dv, paste(sprintf("`%s`", CTRL), collapse=" + ")))
  f2 <- as.formula(sprintf("%s ~ %s + %s", dv, paste(sprintf("`%s`", CTRL), collapse=" + "),
                           paste(sprintf("`%s`", SCALE), collapse=" + ")))
  m1 <- lm(f1, data=dat); m2 <- lm(f2, data=dat)
  s1 <- summary(m1); s2 <- summary(m2); a12 <- anova(m1, m2)

  # L2 标准化 β
  zdat <- dat
  for (cc in c(CTRL, SCALE, dv)) { s <- sd(dat[[cc]]); zdat[[cc]] <- if (s>0) as.numeric(scale(dat[[cc]])) else dat[[cc]] }
  mz2 <- lm(f2, data=zdat)
  cf2 <- s2$coefficients
  for (nm in SCALE) scale_rows[[length(scale_rows)+1]] <- data.frame(
    dv=DVS[[dv]], variable=nm, zh=SCALE_ZH[nm], beta=round(coef(mz2)[nm],3),
    B=round(cf2[nm,1],3), SE=round(cf2[nm,2],3), t=round(cf2[nm,3],3), p=cf2[nm,4])

  for (bn in names(CATBLOCKS)) {
    vars <- CATBLOCKS[[bn]]
    f3 <- as.formula(sprintf("%s ~ %s + %s + %s", dv, paste(sprintf("`%s`", CTRL), collapse=" + "),
                             paste(sprintf("`%s`", SCALE), collapse=" + "),
                             paste(sprintf("`%s`", vars), collapse=" + ")))
    m3 <- lm(f3, data=dat); s3 <- summary(m3); a23 <- anova(m2, m3)
    dR2 <- s3$r.squared - s2$r.squared
    cf <- s3$coefficients
    zdat2 <- dat
    for (cc in c(CTRL, SCALE, vars, dv)) { s <- sd(dat[[cc]]); zdat2[[cc]] <- if (s>0) as.numeric(scale(dat[[cc]])) else dat[[cc]] }
    mz3 <- lm(f3, data=zdat2)
    sig_in <- vars[cf[vars, 4] < 0.10]
    block_rows[[length(block_rows)+1]] <- data.frame(
      dv=DVS[[dv]], category=bn, n_vars=length(vars),
      base_R2=round(s2$r.squared,4), full_R2=round(s3$r.squared,4),
      dR2=round(dR2,4), dF=round(a23$F[2],3), dp=a23$`Pr(>F)`[2],
      sig_vars=paste(sig_in, collapse=","))
  }
  rows[[length(rows)+1]] <- data.frame(dv=DVS[[dv]], layer="L1_人口学", n_add=2,
    R2=round(s1$r.squared,4), dR2=NA, dF=NA, dp=NA)
  rows[[length(rows)+1]] <- data.frame(dv=DVS[[dv]], layer="L2_量表(抑郁+焦虑)", n_add=2,
    R2=round(s2$r.squared,4), dR2=round(s2$r.squared-s1$r.squared,4),
    dF=round(a12$F[2],3), dp=a12$`Pr(>F)`[2])
}

hier_df  <- do.call(rbind, rows)
scale_df <- do.call(rbind, scale_rows)
block_df <- do.call(rbind, block_rows)

md("## 1. 量表层增量（表七 对应，N = %d）", N)
md("")
md("| 因变量 | 第一层 R²（人口学） | 第二层 ΔR²（抑郁+焦虑） | ΔF | Δp |")
md("|---|---|---|---|---|")
for (i in seq_len(nrow(hier_df))) if (hier_df$layer[i] == "L1_人口学") {
  r2 <- hier_df[i, ]; r2b <- hier_df[i+1, ]
  md("| %s | %.3f | %.3f | %.2f | %s |", r2$dv, r2$R2, r2b$dR2, r2b$dF,
     ifelse(r2b$dp < 0.001, "< 0.001", sprintf("%.3f", r2b$dp)))
}
md("")
md("## 2. 第二层标准化回归系数 β（表八 对应）")
md("")
md("| 因变量 | 抑郁(PHQ9) β | p | 焦虑(GAD7) β | p |")
md("|---|---|---|---|---|")
for (dv in names(DVS)) {
  s <- scale_df[scale_df$dv == DVS[[dv]], ]
  md("| %s | %+.3f | %s | %+.3f | %s |", DVS[[dv]],
     s$beta[s$variable=="PHQ9_avg"], ifelse(s$p[s$variable=="PHQ9_avg"]<0.001, "< 0.001", sprintf("%.3f", s$p[s$variable=="PHQ9_avg"])),
     s$beta[s$variable=="GAD7_avg"], ifelse(s$p[s$variable=="GAD7_avg"]<0.001, "< 0.001", sprintf("%.3f", s$p[s$variable=="GAD7_avg"])))
}
md("")
md("## 3. 类别块增量（表九/表十 对应）")
md("")
for (dv in names(DVS)) {
  sub <- block_df[block_df$dv == DVS[[dv]], ]
  sub <- sub[order(sub$dR2, decreasing=TRUE), ]
  md("### %s", DVS[[dv]])
  md("")
  md("| 词汇类别块 | 变量数 | ΔR² | ΔF | Δp | 块内 p<.10 变量 |")
  md("|---|---|---|---|---|---|")
  for (i in seq_len(nrow(sub))) {
    md("| %s | %d | %.4f | %.2f | %s | %s |", sub$category[i], sub$n_vars[i], sub$dR2[i], sub$dF[i],
       ifelse(sub$dp[i] < 0.001, "< 0.001", sprintf("%.3f", sub$dp[i])), sub$sig_vars[i])
  }
  md("")
}

# 汇总显著性（供论文叙述）
md("## 4. 显著块汇总")
md("")
for (dv in names(DVS)) {
  sub <- block_df[block_df$dv == DVS[[dv]] & block_df$dp < 0.05, ]
  sub <- sub[order(sub$dR2, decreasing=TRUE), ]
  if (nrow(sub)) {
    md("**%s**：显著块 %d 个 — %s", DVS[[dv]], nrow(sub),
       paste(sprintf("%s(ΔR²=%.3f, p=%s)", sub$category, sub$dR2,
                     ifelse(sub$dp<0.001, "<0.001", sprintf("%.3f", sub$dp))), collapse="；"))
  } else md("**%s**：无显著块。", DVS[[dv]])
}
cat("表格.md 已生成：", file.path(FOLDER, "表格.md"), "\n")
