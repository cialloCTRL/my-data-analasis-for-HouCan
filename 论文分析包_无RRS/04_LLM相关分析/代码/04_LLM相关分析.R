# -*- coding: utf-8 -*-
# 04_LLM相关分析.R — 无 RRS 版大语言模型编码分析
#   融合评分 = (DeepSeek 5次均值 + qwen-plus 5次均值)/2（在脚本内重建，用于 ICC/描述/内部相关）
#   ① ICC（模型内 ICC(3,1)、模型间 ICC(2,1)、融合信度 ICC(2,k)）
#   ② 融合评分描述统计（10 维）
#   ③ 效标关联效度：LLM 10 维 × 依恋量表 相关（附表四）
#   ④ 增量效度：L1 人口学 → L2 量表（抑郁+焦虑，已剔 RRS）→ L3 各 LLM 表现块（表十二）
#   ⑤ 会聚效度：LLM 维度 × 对应 TextMind 类别块（表十三）
#   ⑥ LLM 8 语言维内部相关热力图（附图四）
# 输入: ../../forhoucan/llm_coding/results_*.csv、../数据/数据_去RRS新版.xlsx
# 输出: 表格.md、图片/*
suppressMessages({ library(openxlsx); library(corrplot) })

SCRIPT_DIR <- { a <- commandArgs(trailingOnly = FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)]); if (length(f)) dirname(normalizePath(f)) else getwd() }
FOLDER <- dirname(SCRIPT_DIR)
PKG <- dirname(FOLDER)
IMG <- file.path(FOLDER, "图片"); dir.create(IMG, showWarnings = FALSE)
FORH <- file.path(dirname(PKG), "forhoucan")
LLM_WORK <- file.path(FORH, "llm_coding")
DATA_PATH <- file.path(PKG, "数据", "数据_去RRS新版.xlsx")
md <- function(...) cat(sprintf(...), "\n", file = file.path(FOLDER, "表格.md"), append = TRUE)

# ---------- 融合评分（原始列名） ----------
read_avg <- function(block, model) {
  d <- read.csv(file.path(LLM_WORK, sprintf("results_%s_%s.csv", block, model)),
                fileEncoding = "UTF-8", stringsAsFactors = FALSE, check.names = FALSE)
  names(d) <- sub("^\ufeff", "", names(d))
  d$pid <- as.integer(sub("P", "", d$participant_id))
  dims <- setdiff(names(d), c("participant_id", "model", "run", "pid"))
  aggregate(d[dims], by = list(pid = d$pid), FUN = mean)
}
ds_att <- read_avg("attachment","deepseek-chat"); qw_att <- read_avg("attachment","qwen-plus")
ds_lang <- read_avg("language","deepseek-chat"); qw_lang <- read_avg("language","qwen-plus")
att <- merge(ds_att, qw_att, by="pid", suffixes=c("_ds","_qw"))
lang <- merge(ds_lang, qw_lang, by="pid", suffixes=c("_ds","_qw"))
llm <- data.frame(pid=att$pid)
for (col in setdiff(names(ds_att),"pid")) llm[[col]] <- (att[[paste0(col,"_ds")]] + att[[paste0(col,"_qw")]])/2
for (col in setdiff(names(ds_lang),"pid")) llm[[col]] <- (lang[[paste0(col,"_ds")]] + lang[[paste0(col,"_qw")]])/2

LLM_LANG <- c("relationship_focus","proximity_seeking","emotional_expression","abandonment_vigilance",
              "intellectualization","psychological_distance","emotional_dependence","self_disclosure")
PLAIN_ALL <- c("attachment_anxiety","attachment_avoidance", LLM_LANG)          # llm 对象列名
DAT_ALL   <- c("LLM_anxiety","LLM_avoidance", paste0("LLM_", LLM_LANG))        # 数据表列名
ZH <- c(LLM_anxiety="依恋焦虑(LLM)", LLM_avoidance="依恋回避(LLM)",
        LLM_relationship_focus="关系关注", LLM_proximity_seeking="寻求接近", LLM_emotional_expression="情绪表达",
        LLM_abandonment_vigilance="抛弃警觉", LLM_intellectualization="理智化", LLM_psychological_distance="心理距离",
        LLM_emotional_dependence="情感依赖", LLM_self_disclosure="自我披露")
names(PLAIN_ALL) <- DAT_ALL

dat <- read.xlsx(DATA_PATH, sheet = "数据")
dat$age <- as.numeric(dat$age)
N <- nrow(dat)

CTRL <- c("age","gender"); SCALE <- c("PHQ9_avg","GAD7_avg")   # 已剔除 RRS；gender 避免与 TextMind male 冲突
DVS <- c(CAIDS_anxiety_avg="依恋焦虑", CAIDS_avoidance_avg="依恋回避", CAIDS_total_avg="总依恋")

# ---------- ① ICC ----------
icc_3_1 <- function(arr) {
  n <- nrow(arr); k <- ncol(arr)
  grand <- mean(arr); sm <- rowMeans(arr); rm <- colMeans(arr)
  SS_t <- sum((arr-grand)^2); SS_s <- k*sum((sm-grand)^2); SS_r <- n*sum((rm-grand)^2)
  SS_e <- SS_t - SS_s - SS_r
  MSB <- SS_s/(n-1); MSE <- SS_e/((n-1)*(k-1))
  (MSB-MSE)/(MSB+(k-1)*MSE)
}
icc_2_1 <- function(arr) {
  n <- nrow(arr); k <- ncol(arr)
  grand <- mean(arr); sm <- rowMeans(arr); rm <- colMeans(arr)
  SS_t <- sum((arr-grand)^2); SS_s <- k*sum((sm-grand)^2); SS_r <- n*sum((rm-grand)^2)
  SS_e <- SS_t - SS_s - SS_r
  MSB <- SS_s/(n-1); MSJ <- SS_r/(k-1); MSE <- SS_e/((n-1)*(k-1))
  (MSB-MSE)/(MSB+(k-1)*MSE+k*(MSJ-MSE)/n)
}
icc_2_k <- function(arr) {
  n <- nrow(arr); k <- ncol(arr)
  grand <- mean(arr); sm <- rowMeans(arr); rm <- colMeans(arr)
  SS_t <- sum((arr-grand)^2); SS_s <- k*sum((sm-grand)^2); SS_r <- n*sum((rm-grand)^2)
  SS_e <- SS_t - SS_s - SS_r
  MSB <- SS_s/(n-1); MSJ <- SS_r/(k-1); MSE <- SS_e/((n-1)*(k-1))
  (MSB-MSE)/(MSB+(MSJ-MSE)/n)
}
read_runs <- function(block, model) {
  d <- read.csv(file.path(LLM_WORK, sprintf("results_%s_%s.csv", block, model)),
                fileEncoding="UTF-8", stringsAsFactors=FALSE, check.names=FALSE)
  names(d) <- sub("^\ufeff","",names(d)); d$pid <- as.integer(sub("P","",d$participant_id))
  d <- d[order(d$pid, d$run), ]   # 统一按 pid→run 排序（源 CSV 行序不一致）
  d
}
icc_rows <- list()
for (nm in DAT_ALL) {
  plain <- unname(PLAIN_ALL[nm])
  src <- if (plain %in% c("attachment_anxiety","attachment_avoidance")) "attachment" else "language"
  d_ds <- read_runs(src, "deepseek-chat"); d_qw <- read_runs(src, "qwen-plus")
  mat_ds <- matrix(d_ds[[plain]], nrow=N, byrow=TRUE); mat_qw <- matrix(d_qw[[plain]], nrow=N, byrow=TRUE)
  cb <- if (plain %in% c("attachment_anxiety","attachment_avoidance"))
    cbind(att[[paste0(plain,"_ds")]], att[[paste0(plain,"_qw")]])
  else cbind(lang[[paste0(plain,"_ds")]], lang[[paste0(plain,"_qw")]])
  icc_rows[[nm]] <- data.frame(维度=ZH[[nm]], DeepSeek_ICC3_1=round(icc_3_1(mat_ds),3),
                               qwen_ICC3_1=round(icc_3_1(mat_qw),3),
                               ICC2_1=round(icc_2_1(cb),3), 融合信度ICC2_k=round(icc_2_k(cb),3))
}
icc_df <- do.call(rbind, icc_rows)

# ---------- ② 描述统计 ----------
desc_rows <- data.frame(维度=ZH[DAT_ALL], M=round(sapply(DAT_ALL, function(v) mean(llm[[PLAIN_ALL[v]]])),3),
                        SD=round(sapply(DAT_ALL, function(v) sd(llm[[PLAIN_ALL[v]]])),3),
                        min=round(sapply(DAT_ALL, function(v) min(llm[[PLAIN_ALL[v]]])),2),
                        max=round(sapply(DAT_ALL, function(v) max(llm[[PLAIN_ALL[v]]])),2))

# ---------- ③ 效标关联效度 ----------
cor_rows <- list()
for (dv in names(DVS)) for (nm in DAT_ALL) {
  ct <- cor.test(dat[[nm]], dat[[dv]])
  cor_rows[[length(cor_rows)+1]] <- data.frame(量表DV=DVS[[dv]], 维度=ZH[[nm]], r=round(ct$estimate,3), p=ct$p.value)
}
cor_df <- do.call(rbind, cor_rows)

# ---------- ④ 增量效度（表十二） ----------
inc_rows <- list()
DIM_GRP <- c(LLM_relationship_focus="焦虑组", LLM_proximity_seeking="焦虑组", LLM_emotional_expression="焦虑组",
             LLM_abandonment_vigilance="焦虑组", LLM_intellectualization="回避组", LLM_psychological_distance="回避组",
             LLM_emotional_dependence="回避组", LLM_self_disclosure="回避组")
for (dv in names(DVS)) {
  f1 <- as.formula(sprintf("%s ~ %s", dv, paste(sprintf("`%s`",CTRL),collapse=" + ")))
  f2 <- as.formula(sprintf("%s ~ %s + %s", dv, paste(sprintf("`%s`",CTRL),collapse=" + "),
                           paste(sprintf("`%s`",SCALE),collapse=" + ")))
  m1 <- lm(f1, data=dat); m2 <- lm(f2, data=dat)
  for (nm in names(DIM_GRP)) {
    f3 <- as.formula(sprintf("%s ~ %s + %s + `%s`", dv, paste(sprintf("`%s`",CTRL),collapse=" + "),
                             paste(sprintf("`%s`",SCALE),collapse=" + "), nm))
    m3 <- lm(f3, data=dat); a <- anova(m2, m3)
    dR2 <- summary(m3)$r.squared - summary(m2)$r.squared
    inc_rows[[length(inc_rows)+1]] <- data.frame(dv=DVS[[dv]], 表现块=ZH[[nm]], 归属=DIM_GRP[[nm]],
      dR2=round(dR2,4), dF=round(a$F[2],3), dp=a$`Pr(>F)`[2])
  }
  for (grp in c("焦虑组","回避组")) {
    vs <- names(DIM_GRP)[DIM_GRP == grp]
    f3 <- as.formula(sprintf("%s ~ %s + %s + %s", dv, paste(sprintf("`%s`",CTRL),collapse=" + "),
                             paste(sprintf("`%s`",SCALE),collapse=" + "),
                             paste(sprintf("`%s`",vs),collapse=" + ")))
    m3 <- lm(f3, data=dat); a <- anova(m2, m3)
    dR2 <- summary(m3)$r.squared - summary(m2)$r.squared
    inc_rows[[length(inc_rows)+1]] <- data.frame(dv=DVS[[dv]], 表现块=sprintf("%s联合(4维)",grp), 归属=grp,
      dR2=round(dR2,4), dF=round(a$F[2],3), dp=a$`Pr(>F)`[2])
  }
}
inc_df <- do.call(rbind, inc_rows)

# ---------- ⑤ 会聚效度（表十三） ----------
CATBLOCKS <- list(
  Social=c("Social","socbehav","prosocial","polite","conflict","moral","comm","socrefs","family","friend","female","male","Humans"),
  ppron=c("ppron","i","we","you","YouPL","shehe","they"),
  Affect=c("affect","tone_pos","tone_neg","emotion","emo_pos","emo_neg","emo_anx","emo_anger","emo_sad","swear"),
  States=c("need","want","acquire","lack","fulfill","fatigue"),
  Motives=c("reward","risk","curiosity","allure"),
  Cognition=c("Cognition","allnone","cogproc","insight","cause","discrep","tentat","certitude","Inhibition","Inclusive","Exclusive","differ","memory"),
  Perception=c("Perception","attention","motion","space","visual","auditory","feeling"),
  Time=c("time","focuspast","focuspresent","focusfuture","TenseM","ProgM","tPast","tNow","tFuture"),
  Relative=c("Relative"))
CATBLOCKS <- lapply(CATBLOCKS, function(vs) vs[sapply(dat[, vs, drop=FALSE], function(x) sd(x, na.rm=TRUE)>0)])
BRIDGE <- list(
  LLM_relationship_focus=c("Social","ppron"), LLM_proximity_seeking=c("States","Motives"),
  LLM_emotional_expression=c("Affect"), LLM_abandonment_vigilance=c("Cognition","Affect"),
  LLM_intellectualization=c("Cognition"), LLM_psychological_distance=c("ppron","Perception","Time","Relative"),
  LLM_emotional_dependence=c("Affect","States","Social"), LLM_self_disclosure=c("ppron","Affect"))
conv_rows <- list()
for (lv in names(BRIDGE)) {
  tm_vars <- unique(unlist(CATBLOCKS[BRIDGE[[lv]]]))
  dat$tm_score <- rowMeans(dat[, tm_vars, drop=FALSE])
  ct <- cor.test(dat[[lv]], dat$tm_score)
  f <- as.formula(sprintf("%s ~ %s", lv, paste(sprintf("`%s`", tm_vars), collapse=" + ")))
  m <- lm(f, data=dat); fs <- summary(m)$fstatistic
  conv_rows[[length(conv_rows)+1]] <- data.frame(LLM维度=ZH[[lv]], 归属=DIM_GRP[[lv]],
    对应TextMind块=paste(BRIDGE[[lv]], collapse="+"), 相关_r=round(ct$estimate,3), 相关_p=ct$p.value,
    多元R2=round(summary(m)$r.squared,3), 回归F=round(fs[1],2), 回归p=pf(fs[1],fs[2],fs[3],lower.tail=FALSE))
}
conv_df <- do.call(rbind, conv_rows)

# ---------- ⑥ LLM 8 语言维内部相关 ----------
LLM_LANG_DAT <- paste0("LLM_", LLM_LANG)
R8 <- cor(llm[, LLM_LANG])
colnames(R8) <- rownames(R8) <- ZH[LLM_LANG_DAT]
png(file.path(IMG, "llm_8维相关热力图.png"), width=1200, height=1100, res=160)
corrplot(R8, method="color", col=colorRampPalette(c("#2166AC","white","#B2182B"))(200),
         type="upper", tl.col="black", tl.cex=0.7, addCoef.col="black", number.cex=0.65,
         number.digits=2, mar=c(0,0,1,0), title="LLM 8 个语言维度相关（融合评分）")
dev.off()

# ---------- 写表格.md ----------
file.remove(file.path(FOLDER, "表格.md"))
md("# LLM 编码分析结果（无 RRS 版）")
md("")
md("> 说明：融合评分 =（DeepSeek 5 次均值 + 通义千问 5 次均值）/2，N = %d。增量效度回归中的第二层量表已剔除 RRS，仅含抑郁（PHQ-9）与焦虑（GAD-7）。", N)
md("")
md("## 1. 评分一致性（ICC）")
md("")
md("| 维度 | DeepSeek ICC(3,1) | 通义千问 ICC(3,1) | 模型间 ICC(2,1) | 融合信度 ICC(2,k) |")
md("|---|---|---|---|---|")
for (i in seq_len(nrow(icc_df))) md("| %s | %.3f | %.3f | %.3f | %.3f |", icc_df$维度[i], icc_df$DeepSeek_ICC3_1[i], icc_df$qwen_ICC3_1[i], icc_df$ICC2_1[i], icc_df$融合信度ICC2_k[i])
md("")
md("## 2. 融合评分描述统计")
md("")
md("| 维度 | M | SD | min | max |")
md("|---|---|---|---|---|")
for (i in seq_len(nrow(desc_rows))) md("| %s | %.2f | %.2f | %.1f | %.1f |", desc_rows$维度[i], desc_rows$M[i], desc_rows$SD[i], desc_rows$min[i], desc_rows$max[i])
md("")
md("## 3. 效标关联效度（LLM 维度 × 依恋量表）")
md("")
md("| LLM 维度 | 依恋焦虑 | 依恋回避 | 总依恋 |")
md("|---|---|---|---|")
for (nm in DAT_ALL) {
  r <- cor_df$r[cor_df$维度==ZH[[nm]]]; p <- cor_df$p[cor_df$维度==ZH[[nm]]]
  cell <- function(i) sprintf("%+.3f%s", r[i], ifelse(p[i]<0.05, ifelse(p[i]<0.01, ifelse(p[i]<0.001,"***","**"),"*"), ""))
  md("| %s | %s | %s | %s |", ZH[[nm]], cell(1), cell(2), cell(3))
}
md("")
md("注：* p<.05，** p<.01，*** p<.001。")
md("")
md("## 4. 增量效度（表十二 对应；L3 为各 LLM 表现块，控制人口学与抑郁/焦虑后）")
md("")
md("| 表现块 | 归属 | 依恋焦虑 ΔR² | Δp | 依恋回避 ΔR² | Δp | 总依恋 ΔR² | Δp |")
md("|---|---|---|---|---|---|---|---|")
for (nm in names(DIM_GRP)) {
  rp <- sapply(names(DVS), function(dv) {
    s <- inc_df[inc_df$dv==DVS[[dv]] & inc_df$表现块==ZH[[nm]], ]
    c(d = sprintf("%.3f%s", s$dR2, ifelse(s$dp<0.05, ifelse(s$dp<0.01, ifelse(s$dp<0.001,"***","**"),"*"), "")),
      p = ifelse(s$dp<0.001, "<0.001", sprintf("%.3f", s$dp)))
  })
  md("| %s | %s | %s | %s | %s | %s | %s | %s |", ZH[[nm]], DIM_GRP[[nm]], rp["d",1], rp["p",1], rp["d",2], rp["p",2], rp["d",3], rp["p",3])
}
md("")
md("### 组联合块增量")
md("")
md("| 联合块 | 依恋焦虑 ΔR² (Δp) | 依恋回避 ΔR² (Δp) | 总依恋 ΔR² (Δp) |")
md("|---|---|---|---|")
for (grp in c("焦虑组","回避组")) {
  cell <- sapply(names(DVS), function(dv) {
    s <- inc_df[inc_df$dv==DVS[[dv]] & inc_df$表现块==sprintf("%s联合(4维)",grp), ]
    sprintf("%.3f (%s)", s$dR2, ifelse(s$dp<0.001,"<0.001",sprintf("%.3f",s$dp)))
  })
  md("| %s | %s | %s | %s |", grp, cell[1], cell[2], cell[3])
}
md("")
md("## 5. 会聚效度（表十三 对应；LLM 维度 × 对应 TextMind 类别块）")
md("")
md("| LLM 维度 | 对应 TextMind 块 | r | 多元 R² | p |")
md("|---|---|---|---|---|")
for (i in seq_len(nrow(conv_df))) md("| %s | %s | %+.3f%s | %.3f | %s |", conv_df$LLM维度[i], conv_df$对应TextMind块[i], conv_df$相关_r[i],
   ifelse(conv_df$相关_p[i]<0.05,"*",""), conv_df$多元R2[i],
   ifelse(conv_df$回归p[i]<0.001,"< 0.001",sprintf("%.3f", conv_df$回归p[i])))
md("")
md("## 6. LLM 8 语言维度内部相关")
md("")
md("图片：`图片/llm_8维相关热力图.png`（融合评分，上三角）")
cat("表格.md 已生成：", file.path(FOLDER, "表格.md"), "\n")
