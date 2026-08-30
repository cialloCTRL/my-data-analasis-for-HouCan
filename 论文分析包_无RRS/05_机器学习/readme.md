# 05 机器学习（探索性）

## 目的
无 RRS 版机器学习探索性分析（**仅作探索，不作结论性证据**）：
1. **05a 词典特征 LASSO**（R）：强制变量 = 年龄/性别/抑郁/焦虑（已剔 RRS），候选 = TextMind 137 变量。
   - 全量 199 探索（对应论文附表二）
   - 169/30、149/50 训练/验证（对应论文附表三）
2. **05b 样本外对比**（Python）：同一框架下对比三种方法层——
   - TextMind 类别块（引用 03 结果）
   - LLM 融合语言块（8 维）
   - 语义嵌入（text-embedding-v3 → PCA，K 由训练集内 5 折选择）
   - 固定划分 169/30、149/50 + 全体 10 折嵌套 CV

## 与旧版（含 RRS）的差异
- **LASSO 强制变量由 5 个（含 RRS_total）减为 4 个**（年龄/性别/PHQ9/GAD7），附表二/三的纳入变量与 R² 全部重算。
- 划分文件（169/30、149/50）按性别×依恋焦虑分层生成，**与 RRS 无关，直接复用**。
- LLM 语言块与 Embedding 的嵌套 CV 本就不含 RRS（`ml_predict.py`、`llm_lasso_cv.py`），此处统一重算确认。
- 语义嵌入使用已保存的 `../../forhoucan/embedding_ml/embeddings.npz`（离线），不重新调用 API。

## 输入
- `../数据/数据_去RRS新版.xlsx`
- `../../forhoucan/analysis_R/split_{169_30,149_50}.csv`
- `../../forhoucan/embedding_ml/embeddings.npz`
- `../../forhoucan/清理后数据_含对话.xlsx`
- `../../forhoucan/llm_coding/results_language_{deepseek-chat,qwen-plus}.csv`

## 运行（顺序：先 05a 生成表格.md，05b 追加第 3、4 节）
```r
Rscript 代码/05a_TextMind_LASSO.R
python 代码/05b_样本外对比.py
```

## 输出
- `表格.md`：全量/训练验证 LASSO 表 + 三方法样本外对比表 + 小结

## 依赖
R 包：`openxlsx`、`glmnet`；Python：`numpy`、`openpyxl`
