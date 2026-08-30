# 04 LLM 相关分析

## 目的
无 RRS 版大语言模型（LLM）编码分析：
1. **评分一致性（ICC）**：模型内 ICC(3,1)（DeepSeek / 通义千问各 5 次）、模型间 ICC(2,1)、融合信度 ICC(2,k)
2. **描述统计**：10 维融合评分（2 依恋特质 + 8 语言维度）的 M/SD/范围
3. **效标关联效度**：LLM 10 维 × 依恋量表相关（对应论文附表四）
4. **增量效度**：控制人口学 + 抑郁/焦虑后，各 LLM 表现块的 ΔR²（对应论文表十二）
5. **会聚效度**：LLM 维度 × 对应 TextMind 类别块（对应论文表十三）
6. LLM 8 语言维内部相关热力图（对应论文附图四）

## 与旧版（含 RRS）的差异
- **增量效度回归（表十二）第二层控制变量由 PHQ9+GAD7+RRS三分量表 减为 PHQ9+GAD7**，所有 ΔR²/Δp 重算。
- ICC、描述统计、效标相关、会聚效度不涉及 RRS，数值不变（本脚本照常重算确认）。
- 融合评分在脚本内从 `llm_coding/results_*.csv` 重建（离线、确定性），无需调用任何 API。

## 输入
- `../../forhoucan/llm_coding/results_{attachment,language}_{deepseek-chat,qwen-plus}.csv`（融合评分原始数据）
- `../数据/数据_去RRS新版.xlsx`（量表与 TextMind 数据）

## 运行
```r
Rscript 代码/04_LLM相关分析.R
```

## 输出
- `表格.md`：ICC 表、描述统计表、效标相关表、增量效度表、会聚效度表
- `图片/llm_8维相关热力图.png`

## 依赖
R 包：`openxlsx`、`corrplot`
