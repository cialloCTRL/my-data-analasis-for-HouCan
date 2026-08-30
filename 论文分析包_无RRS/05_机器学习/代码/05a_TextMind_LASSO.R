# -*- coding: utf-8 -*-
# 05a_TextMind_LASSO.R — 无 RRS 版词典特征 LASSO（探索性机器学习）
#   强制变量（不惩罚）：年龄、性别、抑郁 PHQ9、焦虑 GAD7（已剔除 RRS）
#   候选惩罚变量：TextMind 137 个语言特征
#   ① 全量 199 探索（附表二 对应）
#   ② 训练/验证划分 169/30、149/50（附表三 对应；划分文件与 RRS 无关，直接复用）
# 输入: ../数据/数据_去RRS新版.xlsx、../../forhoucan/analysis_R/split_*.csv
# 输出: 表格.md
suppressMessages({ library(openxlsx); library(glmnet) })

SCRIPT_DIR <- { a <- commandArgs(trailingOnly = FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)]); if (length(f)) dirname(normalizePath(f)) else getwd() }
FOLDER <- dirname(SCRIPT_DIR)
PKG <- dirname(FOLDER)
DATA_PATH <- file.path(PKG, "数据", "数据_去RRS新版.xlsx")
SPLIT_DIR <- file.path(dirname(PKG), "forhoucan", "analysis_R")
md <- function(...) cat(sprintf(...), "\n", file = file.path(FOLDER, "表格.md"), append = TRUE)

dat <- read.xlsx(DATA_PATH, sheet = "数据")
N <- nrow(dat)
dat$age <- as.numeric(dat$age)

non_tm <- c("pid","Q1","Q2","Q3","JSPSYCH_2","open_up","felt_support","emotion_arousal",
            "age","gender", paste0("CAIDS_",1:7), paste0("PHQ9_",1:9), paste0("GAD7_",1:7),
            "CAIDS_anxiety_avg","CAIDS_avoidance_avg","CAIDS_total_avg","PHQ9_avg","GAD7_avg")
tm_names <- setdiff(names(dat), c(non_tm, grep("^LLM_", names(dat), value = TRUE)))
zvar <- sapply(dat[, tm_names], function(x) sd(x, na.rm=TRUE) == 0)
tm_keep <- tm_names[!zvar]
cat("TextMind 可用变量:", length(tm_keep), "（剔除零方差", sum(zvar), "）\n")

FORCE <- c("age","gender","PHQ9_avg","GAD7_avg")
DVS <- c(CAIDS_anxiety_avg="依恋焦虑", CAIDS_avoidance_avg="依恋回避", CAIDS_total_avg="总依恋")

ZH <- c(age="年龄", gender="性别(男=1)", PHQ9_avg="抑郁(PHQ9)", GAD7_avg="焦虑(GAD7)",
        "Linguistic"="语言特征", "function"="功能词", "pronoun"="代词", "ppron"="人称代词",
        "i"="我", "we"="我们", "you"="你", "shehe"="她/他", "they"="他们", "ipron"="非人称代词",
        "number"="数字", "prep"="介词", "auxverb"="助动词", "adverb"="副词", "conj"="连词",
        "negate"="否定词", "verb"="动词", "quantity"="数量词", "Drives"="驱动类",
        "affiliation"="亲和", "achieve"="成就", "power"="权力", "Cognition"="认知类",
        "allnone"="全无类词", "cogproc"="认知过程", "insight"="洞察", "cause"="因果词",
        "discrep"="差异", "tentat"="不确定词", "certitude"="确定词", "differ"="区分",
        "memory"="记忆", "affect"="情感过程", "tone_pos"="积极语气", "tone_neg"="消极语气",
        "emotion"="情绪词", "emo_pos"="积极情绪", "emo_neg"="消极情绪", "emo_anx"="焦虑情绪",
        "emo_anger"="愤怒情绪", "emo_sad"="悲伤情绪", "swear"="咒骂词", "Social"="社会过程",
        "socbehav"="社会行为", "prosocial"="亲社会", "polite"="礼貌词", "conflict"="冲突词",
        "moral"="道德词", "comm"="沟通词", "socrefs"="社会参照", "family"="家庭",
        "friend"="朋友", "female"="女性词", "male"="男性词", "Culture"="文化词", "politic"="政治",
        "ethnicity"="民族", "tech"="技术", "Lifestyle"="生活方式", "leisure"="休闲",
        "home"="家/家居", "work"="工作", "money"="金钱", "relig"="宗教", "Physical"="身体生理",
        "health"="健康", "illness"="疾病", "wellness"="养生", "mental"="心理/精神",
        "substances"="物质", "sexual"="性", "food"="食物", "death"="死亡",
        "need"="需要", "want"="想要", "acquire"="获得", "lack"="缺乏",
        "fulfill"="满足", "fatigue"="疲劳/倦怠", "reward"="奖励", "risk"="风险",
        "curiosity"="好奇", "allure"="吸引", "Perception"="感知觉", "attention"="注意",
        "motion"="运动/移动", "space"="空间", "visual"="视觉", "auditory"="听觉", "feeling"="感受",
        "time"="时间", "focuspast"="过去聚焦", "focuspresent"="现在聚焦", "focusfuture"="未来聚焦",
        "Conversation"="对话词", "netspeak"="网络用语", "assent"="同意词", "nonflu"="不流畅",
        "filler"="填充词", "Body"="身体词", "Compare"="比较词", "Interrog"="疑问词",
        "Relative"="相对词", "Humans"="人类词", "Inhibition"="抑制词", "Exclusive"="排他词",
        "Inclusive"="包容词", "ProgM"="进行时标记", "Particle"="语气助词", "General_pa"="一般过去时",
        "Modal_pa"="情态过去时", "PrepEnd"="介词结尾", "QuanUnit"="量词单位", "SpecArt"="特定量词",
        "TenseM"="时态标记", "YouPL"="你们", "Psychology"="心理词", "Love"="爱",
        "tPast"="过去时态", "tNow"="现在时态", "tFuture"="将来时态", "Period"="句号",
        "Comma"="逗号", "Colon"="冒号", "SemiC"="分号", "QMark"="问号", "Exclam"="感叹号",
        "Dash"="破折号", "Quote"="引号", "Apostrophe"="撇号", "Parenth"="括号", "OtherP"="其他标点",
        "WordCount"="词数", "WordPerSentence"="每句词数", "RateDicCover"="词典覆盖率",
        "RateNumeral"="数字词占比", "RateSixLtrWord"="长词占比", "RateFourCharWord"="四字词占比",
        "RateLatinWord"="外文词占比", "NumAtMention"="提及次数", "NumEmotion"="表情符号",
        "NumHashTag"="话题标签", "NumURLs"="链接数")
zh_of <- function(v) ifelse(v %in% names(ZH), ZH[v], v)

run_lasso <- function(tr, va, yname, tm_vars) {
  X_tr <- as.matrix(tr[, c(FORCE, tm_vars)]); y_tr <- tr[[yname]]
  pf <- c(rep(0, length(FORCE)), rep(1, length(tm_vars)))
  set.seed(20260822)
  cv <- cv.glmnet(X_tr, y_tr, alpha=1, penalty.factor=pf, nfolds=10)
  lam <- cv$lambda.1se
  fit <- glmnet(X_tr, y_tr, alpha=1, penalty.factor=pf)
  b <- as.numeric(coef(fit, s=lam)); names(b) <- c("(Intercept)", FORCE, tm_vars)
  nz <- setdiff(names(b)[b != 0], "(Intercept)")
  pred <- predict(fit, newx=as.matrix(va[, c(FORCE, tm_vars)]), s=lam)[,1]
  r2  <- 1 - sum((va[[yname]]-pred)^2)/sum((va[[yname]]-mean(va[[yname]]))^2)
  pred_tr <- predict(fit, newx=X_tr, s=lam)[,1]
  r2_tr <- 1 - sum((y_tr-pred_tr)^2)/sum((y_tr-mean(y_tr))^2)
  sd_x <- apply(X_tr, 2, sd); sd_y <- sd(y_tr)
  b_std <- b; b_std[-1] <- b[-1]*sd_x/sd_y
  list(lam=lam, nz=nz, b_std=b_std, r2=r2, r2_tr=r2_tr)
}

file.remove(file.path(FOLDER, "表格.md"))
md("# 机器学习结果（无 RRS 版）")
md("")
md("> 说明：本部分为探索性分析，不作结论性证据。LASSO 强制变量仅含年龄、性别、抑郁（PHQ-9）、焦虑（GAD-7），已剔除 RRS。划分文件（169/30、149/50）与 RRS 无关，直接复用原划分。")

# ---------- ① 全量 199 探索 ----------
md("")
md("## 1. 全量 199 探索性 LASSO（附表二 对应，候选 = %d 个 TextMind 变量）", length(tm_keep))
md("")
md("| 因变量 | 候选变量数 | LASSO 纳入的 TextMind 变量（标准化系数） |")
md("|---|---|---|")
for (dv in names(DVS)) {
  r <- run_lasso(dat, dat, dv, tm_keep)
  tm_nz <- intersect(r$nz, tm_keep)
  sel <- if (length(tm_nz)) paste(sprintf("%s(%.3f)", zh_of(tm_nz), r$b_std[tm_nz]), collapse="、") else "无文本变量纳入"
  md("| %s | %d | %s |", DVS[[dv]], length(tm_keep), sel)
}

# ---------- ② 169/30 与 149/50 ----------
for (sp in c("169_30", "149_50")) {
  spd <- read.csv(file.path(SPLIT_DIR, sprintf("split_%s.csv", sp)), fileEncoding="UTF-8")
  tr_idx <- which(spd$训练集); va_idx <- which(spd$验证集)
  tr <- dat[tr_idx, ]; va <- dat[va_idx, ]
  md("")
  md("## 2. 训练/验证划分 %s（训练 %d / 验证 %d，附表三 对应）", sp, length(tr_idx), length(va_idx))
  md("")
  md("| 因变量 | 纳入的 TextMind 变量 | 训练集 R² | 验证集 R² |")
  md("|---|---|---|---|")
  for (dv in names(DVS)) {
    r <- run_lasso(tr, va, dv, tm_keep)
    tm_nz <- intersect(r$nz, tm_keep)
    sel <- if (length(tm_nz)) paste(sprintf("%s(%.3f)", zh_of(tm_nz), r$b_std[tm_nz]), collapse="、") else "无文本变量纳入"
    md("| %s | %s | %.3f | %+.3f |", DVS[[dv]], sel, r$r2_tr, r$r2)
  }
}
md("")
md("注：LASSO 仅作探索性报告。去除 RRS 后，强制变量由 5 个降为 4 个，纳入变量与 R² 与旧版不同，属预期变化。")
cat("表格.md 已生成：", file.path(FOLDER, "表格.md"), "\n")
