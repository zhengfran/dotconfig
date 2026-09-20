# 按任务选模型与推理强度：OpenAI、Anthropic、DeepSeek、xAI

> 调研日期：2026-09-19（2026-09-20 补充 Kiro 实测，见 A.4 与 E 节）
> 证据范围：只用官方模型文档、API 参考、官方价格页、官方发布文章、model card 和厂商自报的 benchmark。第三方数据都单独标为 **〔第三方〕**。
> **访问限制（重要）**：本机所在的公司代理拦截了 `openai.com`、`developers.openai.com`、`api-docs.deepseek.com`、`docs.x.ai`、`x.ai`（返回 303 跳到 `sia-transparent-auto.automotive-wan.com` 认证页）。Anthropic 文档、GitHub 和 Hugging Face 可以直接读。所以：
> - 标 **〔索引〕** 的内容出自官方域名页面，但本次只经 WebSearch 的索引摘要核对，**没有打开原页面逐字比对**。数字的可信度比直接读原页低一档。
> - 标 **〔本机〕** 的内容来自本机第一方工具的实时返回：`codex app-server` 的 `model/list`（codex-cli 0.154.0），以及 pi 的 `~/.pi/agent/models-store.json`（其中是 GitHub Copilot 和 ChatGPT-Codex 两个 provider 自报的模型元数据）。

## 结论

**一句话：先按任务长度和风险定 effort，再按厂商定模型。router 现有的 effort 默认值基本符合厂商指引。但真正的问题在执行层：Claude harness 传的 effort 对当前 Claude 模型不生效，Codex harness 用不到 `max`，Kiro harness 压根没传 effort。先修这三处，再调默认值，收益更大。**

> **实施状态（2026-09-20）**：A.1、A.2、B（large_refactor→xhigh）、新增 `quick`/`planning`、D（改为配置文件里的 `piModels`）都已落地。A.4 的前提被实测推翻——Kiro **支持** effort，只是我们没传；已补上 `--effort`，C.1 的 tier 调整随之回退。详见 E 节。

厂商指引有四点共识：

1. **中档是 agentic 编码的默认档，高档留给“诊断/规划/推理代码”，最高档要有 eval 证明值得再用。** OpenAI 的原话大意是：抽取、路由、分类、简单改写用 `low`；诊断问题、比较方案、写计划、推理代码用 `medium`/`high`；`xhigh`/`max` 只在代表性 eval 证明值得时使用〔索引〕 [OpenAI builder's guide to GPT‑5.6](https://openai.com/index/builders-guide-to-gpt-5-6/) [Reasoning models](https://developers.openai.com/api/docs/guides/reasoning)。DeepSeek 的划分是：简单任务 `low`，日常 agent 任务 `high`，复杂场景 `max`〔索引〕 [DeepSeek Change Log](https://api-docs.deepseek.com/updates/)。
2. **Anthropic 的默认档比 OpenAI 高一档。** Claude Opus 5、Sonnet 5 和 Fable 5.1 在 API 上都默认 `high`。`xhigh` 用于“超过 30 分钟、token 预算上百万”的长时 agentic/编码任务。`low` 的典型场景直接写的是 “subagents”。[Effort 文档](https://platform.claude.com/docs/en/build-with-claude/effort)
3. **新一代小/中模型在低 effort 下已经超过上一代的高 effort。** 例如，OpenAI 称 GPT‑5.6 Sol 在 `low` 下胜过 GPT‑5.5 的 `high`〔索引〕 [builder's guide](https://openai.com/index/builders-guide-to-gpt-5-6/)。Anthropic 称 Sonnet 5 的 `medium`≈Sonnet 4.6 的 `high` [Effort 文档](https://platform.claude.com/docs/en/build-with-claude/effort)。所以沿用旧代模型的 effort 习惯往往偏贵，Anthropic 明确要求为 Opus 5 重新做 effort sweep。
4. **不同厂商的 effort 刻度不能直接互换。** OpenAI 的 `medium`、Claude 的 `high`、DeepSeek 的 `high`、Grok 的 `high` 都是各自的“默认”。router 用同一个词跨 harness 传递时，意义并不相同。

## 推荐矩阵

每格写作“模型 · effort”。“最佳”指只看质量、不看价格时的首选；“够用最省”指在官方证据下质量仍可接受的最低成本选项。价格见下一节。

| 任务 | OpenAI | Anthropic | DeepSeek | xAI | 最佳 | 够用最省 |
|---|---|---|---|---|---|---|
| 代码库调研 / 问答 | `gpt-5.6-terra` · medium | `claude-sonnet-5` · medium | `deepseek-flash` · high | `grok-4.6` · medium | `claude-opus-5` · medium | `gpt-5.6-luna` · medium |
| 代码审查 | `gpt-5.6-sol` · high | `claude-opus-5` · high | `deepseek-flash` · high | `grok-4.6` · high | `claude-opus-5` · high | `claude-sonnet-5` · medium |
| 调试 / 根因定位 | `gpt-5.6-sol` · high | `claude-opus-5` · high | `deepseek-flash` · high | `grok-4.6` · high | `claude-opus-5` · high（长复现 → xhigh） | `gpt-5.6-terra` · high |
| 规划 / 架构 | `gpt-6-astra` · high | `claude-opus-5` · high→xhigh；最难的用 `claude-fable-5-1` | `deepseek-flash` · max | `grok-4.6` · high | `gpt-6-astra` · high 或 `claude-fable-5-1` · high | `claude-sonnet-5` · high |
| 大型重构 / 迁移 | `gpt-6-astra` · xhigh | `claude-opus-5` · xhigh | `deepseek-flash` · high | `grok-4.6` · xhigh | `claude-opus-5` · xhigh（数小时 → `claude-fable-5-1`） | `claude-sonnet-5` · high |
| 编写测试 | `gpt-5.6-sol` · medium | `claude-sonnet-5` · high | `deepseek-flash` · high | `grok-4.6` · medium | `claude-opus-5` · high | `gpt-5.6-terra` · medium |
| 边界清晰的实现 | `gpt-5.6-sol` · medium | `claude-sonnet-5` · high / `claude-opus-5` · medium | `deepseek-flash` · high | `grok-4.6` · medium / `grok-build-0.1` | `gpt-5.6-sol` · medium | `gpt-5.6-luna` · medium |
| 算法 / 硬推理 | `gpt-6-astra` · xhigh（`max` 需 eval 证明值得） | `claude-opus-5` · xhigh/max | `deepseek-flash` · max | `grok-4.6` · xhigh | `gpt-6-astra` · xhigh | `deepseek-flash` · max |
| 琐碎编辑 / 搜索 / 摘要 | `gpt-5.6-luna` · low | `claude-haiku-4-5`（无 effort）或 `claude-sonnet-5` · low | `deepseek-flash` · none/low | `grok-4.3` · none/low | `gpt-5.6-luna` · low | `deepseek-flash` · none |
| 长上下文（>272K） | `gpt-6-astra` / `gpt-5.6-sol`（仅 API/Copilot，超出部分加价） | `claude-opus-5` · high | `deepseek-flash` · high | `grok-4.3`（1M）；`grok-4.6` 上限 500K | `claude-fable-5-1` · high 或 `claude-opus-5` · high | `deepseek-flash` · high |
| 文档写作 | `gpt-5.6-terra` · low/medium | `claude-sonnet-5` · low/medium | `deepseek-flash` · low | `grok-4.6` · low | `claude-opus-5` · medium（用 prompt 控长度） | `gpt-5.6-luna` · low |
| 长时 agentic 工具使用 | `gpt-6-astra` · high/xhigh（Codex `ultra` 可并行分派） | `claude-fable-5-1` · high/xhigh | `deepseek-flash` · high | `grok-4.6` · xhigh | `claude-fable-5-1` · xhigh 或 `gpt-6-astra` · xhigh | `claude-opus-5` · medium |

各格的依据：

- **调研/问答用 medium**：Anthropic 称 Opus 5 的 `low`/`medium` “以一小部分 token 和延迟得到强质量”，并建议把低档当作主要成本杠杆 [What's new in Opus 5](https://platform.claude.com/docs/en/models/opus-5/whats-new-opus-5)。但 Fable 5.1 在 `low` 下会更少调用搜索/检索工具、更多凭记忆回答 [What's new in Fable 5.1](https://platform.claude.com/docs/en/models/fable-5-1/whats-new-fable-5-1)。需要读仓库的调研**不宜用 `low`**。
- **审查用 Opus 5**：Anthropic 把“代码审查和找 bug：每轮发现真实 bug 的比例高、误报少，并在较低 effort 下保持准确”列为 Opus 5 的专项提升 [What's new in Opus 5](https://platform.claude.com/docs/en/models/opus-5/whats-new-opus-5)。所以“够用最省”可以放心降到 Sonnet 5 medium，或 Opus 5 medium。
- **大型重构用 Opus 5 xhigh**：Anthropic 的选型矩阵把“大规模重构”写在 Opus 5 名下 [Choosing a model](https://platform.claude.com/docs/en/about-claude/models/choosing-a-model)。`xhigh` 对应 “>30 分钟长时 agentic/编码” [Effort](https://platform.claude.com/docs/en/build-with-claude/effort)。DeepSeek model card 的对比表显示，Opus 5 在 NL2Repo-Bench 为 75.3，GPT‑5.6 Sol 为 56.8；在 ProgramBench 为 37.0 对 23.0。这些是 DeepSeek 自测，对 Anthropic/OpenAI 而言属第三方 **〔第三方：DeepSeek 自测〕** [DeepSeek-V4.1-Flash model card](https://huggingface.co/deepseek-ai/DeepSeek-V4.1-Flash)。
- **边界实现用 Sol medium**：Codex 官方把 `medium` 称为“兼顾智能与速度的全能交互式编码档，最难的任务再用 `high`/`xhigh`”〔索引〕 [Codex Prompting Guide](https://developers.openai.com/cookbook/examples/gpt-5/codex_prompting_guide)。本机 `codex` 对 `gpt-5.6-sol` 的描述是 “Latest frontier agentic coding model”〔本机〕。
- **算法用 Astra**：OpenAI 称 GPT‑6 Astra 在 FrontierMath Tier 4 上达到 98%〔索引，厂商自报〕 [GPT‑6 Astra](https://openai.com/index/gpt-6-astra/)。按 OpenAI 的指引，`max` 仅在 eval 证明值得时使用；Anthropic 对 4.7 也说，`max` 在多数负载上“成本显著、收益较小，某些任务会过度思考” [Effort](https://platform.claude.com/docs/en/build-with-claude/effort)。
- **长时 agentic 用 Fable 5.1 或 Astra**：Anthropic 称 Fable 5.1 为“要求最高的推理与长时 agentic 工作”而生，只有 Opus 5 在 `xhigh`/`max` 下仍不够时才升级 [Models overview](https://platform.claude.com/docs/en/about-claude/models/overview)。OpenAI 称 Astra 在长任务中保持连贯性普遍优于 GPT‑5.6 Sol〔索引〕 [Model guidance](https://developers.openai.com/api/docs/guides/latest-model)。Terminal-Bench 4.0 上，Astra 为 57.9%，Fable 5.1 为 55.8%，GPT‑5.6 Sol 为 37.3%〔索引，OpenAI 自报〕 [GPT‑6 Astra for work](https://openai.com/index/gpt-6-astra-next-generation-work/)。
- **琐碎任务用 Luna**：OpenAI 对 Luna 的定位是“快速经济，适合聚焦或重复任务，如抽取信息、分类、短编辑”〔索引〕 [Model guidance](https://developers.openai.com/api/docs/guides/latest-model)。Anthropic 对 Haiku 4.5 的定位包括 “sub-agent tasks” [Choosing a model](https://platform.claude.com/docs/en/about-claude/models/choosing-a-model)。
- **文档写作**：Opus 5 的默认输出更长，而且“改 effort 并不能可靠地缩短回答，应在 prompt 里控制长度” [Effort](https://platform.claude.com/docs/en/build-with-claude/effort)。

## 各厂商详情

### OpenAI（GPT / Codex）

| 模型 ID | 定位 | 上下文 | 价格 $/1M 输入 / 输出 | effort 取值 |
|---|---|---|---|---|
| `gpt-6-astra` | 最强，面向最难的端到端工作；2026-09-03 发布 | 1,050,000 | 10 / 50（缓存读 1） | low, medium, high, xhigh, max；**不支持 `none`** |
| `gpt-5.6-sol` | 旗舰 agentic 编码 | 1,050,000（API） | 标价 5 / 30；2026-08-21 起限时降为 4 / 20，至少到 2026-11-21 | none, low, medium, high, xhigh, max |
| `gpt-5.6-terra` | 均衡，日常工作 | 1,050,000（API） | 2 / 12（2026-07-30 降价后） | 同上 |
| `gpt-5.6-luna` | 快速、最便宜 | 1,050,000（API） | 0.20 / 1.20（2026-07-30 降价后） | 同上 |
| `gpt-5.5`、`gpt-5.3-codex`、`gpt-5.2` | 上一代，仍可用 | — | 5.5：5 / 30 | 至 xhigh |

出处：

- Astra 的价格、effort 取值和上下文〔索引〕：[GPT‑6 Astra model page](https://developers.openai.com/api/docs/models/gpt-6-astra)、[Model guidance](https://developers.openai.com/api/docs/guides/latest-model)。
- GPT‑5.6 系列：定位见[GPT‑5.6](https://openai.com/index/gpt-5-6/)〔索引〕。2026-07-30 的 Terra/Luna 降价见 OpenAI 官方社区公告[Price drop for 5.6 Terra and Luna](https://community.openai.com/t/announcing-a-major-price-drop-for-5-6-terra-and-luna-and-fast-mode-for-5-6-sol/1388484)〔索引〕；Sol 的限时降价见[20% price reduction for GPT 5.6 Sol](https://community.openai.com/t/20-price-reduction-for-gpt-5-6-sol-api-codex-credits-and-chatgpt-work/1391726)〔索引〕。
- 注意：搜索索引里仍有 Terra 2.50/15、Luna 1/6 的旧价残留，本表采用降价后的数字，这与本机 models-store 的 Copilot 条目一致〔本机〕。

计费规则：输入超过 272K token 的请求，**整单**按 2 倍输入、1.5 倍输出计价；Batch/Flex 打五折；Fast mode 为 2 倍价格，最高快 2.5 倍〔索引〕 [GPT‑5.6 Sol model page](https://developers.openai.com/api/docs/models/gpt-5.6-sol) [Price drop 公告](https://community.openai.com/t/announcing-a-major-price-drop-for-5-6-terra-and-luna-and-fast-mode-for-5-6-sol/1388484)。

**推理控制**：参数是 `reasoning.effort`。按模型不同，取值可能包括 `none`/`minimal`/`low`/`medium`/`high`/`xhigh`/`max`；GPT‑5.6 系列 API 默认 `medium`。`max` 随 GPT‑5.6 引入〔索引〕 [Reasoning models](https://developers.openai.com/api/docs/guides/reasoning) [GPT‑5.6](https://openai.com/index/gpt-5-6/)。

Codex 客户端里的情况〔本机〕：

- GPT‑5.6 系列和 Astra 还多一个 `ultra` 档，说明为“最大推理并自动分派任务”。Codex 文档说明：`ultra` 用 subagent 并行处理复杂任务的不同部分，适合能拆成有意义子任务的工作〔索引〕 [Codex Models](https://developers.openai.com/codex/models)。
- Codex 的默认 effort：`gpt-6-astra` 和 `gpt-5.6-sol` 为 **low**，Terra/Luna/5.5 为 **medium**。Codex 产品的默认值比 API 更激进（更省）。

**厂商的选型指引**：

- effort 的用法已在“结论”第 1 点引述〔索引〕 [builder's guide](https://openai.com/index/builders-guide-to-gpt-5-6/)。从 `none`/`minimal` 迁到 Astra 时，先用 `low` 比较结果〔索引〕 [Model guidance](https://developers.openai.com/api/docs/guides/latest-model)。
- Codex 选型：多数推理负载从 `gpt-6-astra` 起步；要省钱用 `gpt-5.6-terra`；成本和延迟最低用 `gpt-5.6-luna`〔索引〕 [Codex Models](https://developers.openai.com/codex/models)。
- 厂商自报对比：Terra 略高于 Claude Fable 5；Luna 超过 Opus 4.8，而且各自只用约三分之一时间、约四分之一估算成本〔索引，厂商自报〕 [GPT‑5.6 frontier efficiency](https://openai.com/index/gpt-5-6-frontier-intelligence-efficiency/)。

### Anthropic（Claude）

| 模型 ID | 定位 | 上下文 / 最大输出 | 价格 $/1M 输入 / 输出 | 思考方式与默认 effort |
|---|---|---|---|---|
| `claude-fable-5-1` | 最强的普遍可用模型：要求最高的推理与长时 agentic | 1M / 128K | 10 / 50；缓存读 0.025× | 自适应思考（常开），默认 `high` |
| `claude-opus-5` | 复杂 agentic 编码与企业工作；**官方推荐的起点** | 1M / 128K | 5 / 25；Fast mode 10 / 50 | 自适应思考，默认 `high` |
| `claude-sonnet-5` | 速度与智能的最佳平衡 | 1M / 128K | 2 / 10 | 自适应思考，默认 `high` |
| `claude-haiku-4-5-20251001` | 最快、最便宜 | 200K / 64K | 1 / 5 | 旧式 extended thinking，**不支持 effort** |

出处：[Models overview](https://platform.claude.com/docs/en/about-claude/models/overview)、[What's new in Opus 5](https://platform.claude.com/docs/en/models/opus-5/whats-new-opus-5)、[What's new in Fable 5.1](https://platform.claude.com/docs/en/models/fable-5-1/whats-new-fable-5-1)。

仍可用的旧型号包括 Fable 5、Opus 4.8/4.7/4.6/4.5、Sonnet 4.6/4.5。Sonnet 5 和 Opus 4.7 以后使用新 tokenizer，同样文本约多 30% token。因此 Sonnet 5 的单价虽然比 Sonnet 4.6 低，同等请求的实际成本不会等比例下降 [What's new in Sonnet 5](https://platform.claude.com/docs/en/models/sonnet-5/whats-new-sonnet-5)。

**推理控制**：参数是 `output_config.effort`，取值 `low | medium | high | xhigh | max`，默认 `high`，而且 `high` 等同于不传。

- effort 影响**所有**输出 token，包括工具调用：低档会更少、更简洁地调用工具。
- 手动 extended thinking（`budget_tokens`）在 Opus 4.6/Sonnet 4.6 上已废弃，在更新的模型上“不被接受”，Sonnet 5 上直接返回 400 [Effort](https://platform.claude.com/docs/en/build-with-claude/effort) [What's new in Sonnet 5](https://platform.claude.com/docs/en/models/sonnet-5/whats-new-sonnet-5)。
- Opus 5 在 `xhigh`/`max` 下不能关闭思考 [What's new in Opus 5](https://platform.claude.com/docs/en/models/opus-5/whats-new-opus-5)。
- Opus 5 和 Fable 5.1 支持会话中逐条消息改 effort（beta），而且不破坏 prompt cache [Effort](https://platform.claude.com/docs/en/build-with-claude/effort)。

**厂商对各档的指引** [Effort](https://platform.claude.com/docs/en/build-with-claude/effort)：

| 档位 | 用途 |
|---|---|
| `max` | 最深推理，不限 token |
| `xhigh` | 长时 agentic 和编码任务（超过 30 分钟、token 预算上百万） |
| `high` | 复杂推理、困难编码、agentic 任务 |
| `medium` | 兼顾速度、成本和性能的 agentic 任务 |
| `low` | 需要最快、最省的简单任务，**例如 subagents** |

按模型的具体建议：

- **Opus 5**：从 `high` 起步。高要求编码/agentic 升到 `xhigh`。“在 eval 允许的地方大胆使用 `low` 和 `medium`”。从旧模型迁来时要重新做 effort sweep。
- **Opus 4.7/4.8**：编码和 agentic 从 `xhigh` 起步。旧 prompt 若照搬这个习惯，在 Opus 5 上会偏贵。
- **Sonnet 5**：`medium` 是省钱的降档，约等于 Sonnet 4.6 的 `high`；`xhigh` 用于最难的编码和 agentic 任务。

选型矩阵与“先 Haiku 再升级”或“先 Opus 5 再降 effort/降模型”两种起步策略见 [Choosing a model](https://platform.claude.com/docs/en/about-claude/models/choosing-a-model)。

**Claude Code / Agent SDK**：CLI 用 `--effort`、`CLAUDE_CODE_EFFORT_LEVEL` 或 `/effort` 设置 effort；subagent frontmatter 也可以写 `effort:`。Fable、Sonnet 5 和 Opus 4.7 以后的模型“始终使用自适应推理，固定思考预算模式不适用”，`MAX_THINKING_TOKENS` 只在固定预算模式下起作用 [Claude Code model config](https://code.claude.com/docs/en/model-config)。Agent SDK 的 `Options.effort` 取 `'low'|'medium'|'high'|'xhigh'|'max'`；`maxThinkingTokens` 标为 *Deprecated* [Agent SDK TypeScript](https://code.claude.com/docs/en/agent-sdk/typescript)。本仓库安装的 `@anthropic-ai/claude-agent-sdk@0.3.273` 的类型定义里已经有 `effort?: EffortLevel`〔本机：`tools/ai/pi/node_modules/@anthropic-ai/claude-agent-sdk/sdk.d.ts`〕。

### DeepSeek

| 模型 | API 名 | 上下文 | 价格 $/1M | 推理控制 |
|---|---|---|---|---|
| DeepSeek-V4.1-Flash（2026-09-10 上线；多模态 MoE，总参 552B，预填充激活 8B、解码激活 16B） | `deepseek-flash`。旧名 `deepseek-v4-flash` 仍接受，但由 V4.1-Flash 服务 | 1M | **未能核实新价**。9-10 起有新价，高峰/低谷分时，低谷为 5 折。V4-Flash 旧价为 0.14 / 0.28，缓存读 0.028 | `reasoning_effort`：`none`（关思考）、`low`、`high`（默认）、`max` |
| DeepSeek-V4-Pro（2026-08-13 GA） | `deepseek-v4-pro` | 1M | 旧价 1.74 / 3.48 | 同上 |
| `deepseek-chat` / `deepseek-reasoner` | 已于 2026-07-24 退役 | — | — | — |

出处：

- V4.1-Flash 的名称和上线〔索引〕：[V4.1-Flash 发布](https://api-docs.deepseek.com/news/news260910/)、[Models & Pricing](https://api-docs.deepseek.com/quick_start/pricing)。架构参数见 [HF model card](https://huggingface.co/deepseek-ai/DeepSeek-V4.1-Flash)（直接读取）。
- 旧模型退役与 V4-Pro 状态〔索引〕：[Change Log](https://api-docs.deepseek.com/updates/)。

`reasoning_effort` 的兼容映射：`minimal` 映射为 `low`，`medium`/`xhigh` 映射为 `high`〔索引〕 [Thinking Mode](https://api-docs.deepseek.com/guides/thinking_mode/)。所以 router 的 `medium` 发给 DeepSeek 实际就是 `high`。开源权重在本地推理时支持 1–100 的连续 effort（model card 的评测用 `reasoning_effort=100`） [HF model card](https://huggingface.co/deepseek-ai/DeepSeek-V4.1-Flash)。

**V4-Pro 状态有矛盾（未解决）**：同一组官方页面的索引摘要里，既有“应用户需求，9-14 之后继续提供 V4-Pro，计费不变”，又有“自 2026-09-14 04:00 UTC 起，`deepseek-v4-pro` 请求路由到 V4.1-Flash 并按 Flash 计价，直到 V4.1-Pro 上线”。后者更具体，而且时间更晚，所以本文按后者处理：**当前实际可用的 DeepSeek 旗舰就是 V4.1-Flash**。

**厂商指引**：简单任务用 `low`，日常 agent 任务用 `high`，更复杂的场景用 `max`〔索引〕 [Change Log](https://api-docs.deepseek.com/updates/)。

**厂商自报成绩**（均为 max effort，来自 [HF model card](https://huggingface.co/deepseek-ai/DeepSeek-V4.1-Flash)，**对比对象的分数也是 DeepSeek 测的**）：

| Benchmark | V4.1-Flash | Opus 5 | GPT‑5.6 Sol |
|---|---|---|---|
| DeepSWE v1.1 | 74.2 | 74.0 | 73.0 |
| Terminal-Bench 2.1 | 90.6 | 89.1 | 88.8 |
| Terminal-Bench 4.0 | 31.2 | 51.8 | 39.9 |
| NL2Repo | 64.0 | 75.3 | 56.8 |
| Codeforces 评分 | 3471 | — | — |

结论是：V4.1-Flash 的强项是中短任务和算法题，长程任务（Terminal-Bench 3.0/4.0、NL2Repo）明显落后于 Opus 5。

model card 还比较了不同 harness 下的 DeepSWE 成绩：mini-SWE 74.2、Claude Code 69.8、**Pi 66.2**、Codex 65.6。同一个模型，换 harness 会差几个点。

### xAI（Grok；官方品牌现为 SpaceXAI）

| 模型 ID | 定位 | 上下文 | 价格 $/1M 输入 / 输出（<200K；≥200K） | 推理控制 |
|---|---|---|---|---|
| `grok-4.6` | 前沿模型，面向编码、agentic 和知识工作；2026-08 发布 | 500K | 2 / 6（缓存读 0.5）；4 / 12 | `reasoning_effort`：low, medium, **high（默认）**, xhigh |
| `grok-build-0.1`（API 公测） | 最快的编码模型，100+ tok/s | 256K | 1 / 2；2 / 4 | 可配 effort，具体取值未核实 |
| `grok-4.3` | 1M 上下文、低价 | 1M | 1.25 / 2.50；2.50 / 5.00 | none, low, medium, high |
| `grok-4.5` | 上一代 | — | — | 至 high；传 `xhigh` 按 `high` 处理 |

出处〔索引〕：[Grok 4.6 docs](https://docs.x.ai/developers/grok-4-6)、[Models & Pricing](https://docs.x.ai/developers/models)、[Grok Build 0.1](https://x.ai/news/grok-build-0-1)、[Grok 4.3](https://docs.x.ai/developers/models/grok-4.3)、[Reasoning](https://docs.x.ai/developers/model-capabilities/text/reasoning)。

**厂商指引与成绩**：

- 官方写的是“包括代码在内的一切，用 Grok 4.6”；effort 越高，分析越充分，延迟也越高〔索引〕 [Reasoning](https://docs.x.ai/developers/model-capabilities/text/reasoning)。
- xAI 没有像 OpenAI/Anthropic 那样按任务类型给出 effort 建议（在能检索到的范围内）。
- model card 自报：DeepSWE v1.1 上 `high` 为 65.9%，`xhigh` 为 67.0%〔索引〕 [Grok 4.6 model card](https://media.x.ai/v1/website/card-4p6-4cd2dc57.pdf)。从 high 升到 xhigh 只多约 1 个点，说明 **Grok 4.6 用默认 `high` 通常就够**。另称在 Artificial Analysis Intelligence Index 上与 GPT‑5.6 Sol 持平〔索引，引用第三方〕。
- 比较：同一 DeepSWE v1.1，DeepSeek 表中 Opus 5 为 74.0，与 xAI 的自测不是同一次实验，不能严格横比。

## 成本与延迟

| 模型 | 输入 | 输出 | 长上下文加价 / 备注 |
|---|---:|---:|---|
| gpt-6-astra | 10 | 50 | >272K：整单 2× 输入、1.5× 输出 |
| claude-fable-5-1 | 10 | 50 | 缓存读仅 2.5%，长会话反复读前缀时更便宜 |
| claude-opus-5 | 5 | 25 | 1M 全窗同价；Fast mode 为 2 倍价格，约 2.5 倍速度 |
| gpt-5.6-sol | 4（标价 5） | 20（标价 30） | >272K 加价；限时价至少到 2026-11-21 |
| gpt-5.6-terra | 2 | 12 | >272K 加价 |
| claude-sonnet-5 | 2 | 10 | tokenizer 多约 30% token |
| grok-4.6 | 2 | 6 | >200K：2× |
| grok-build-0.1 | 1 | 2 | 100+ tok/s |
| claude-haiku-4-5 | 1 | 5 | 无 effort |
| gpt-5.6-luna | 0.20 | 1.20 | >272K 加价 |
| deepseek-flash（V4.1） | 未核实（旧 V4-Flash：0.14） | 未核实（旧：0.28） | 低谷时段 5 折 |

- **effort 主要影响输出 token，而输出单价通常是输入的 3–8 倍。** 把 `xhigh` 当默认值是最常见的成本失控来源。Anthropic 提醒：Opus 5 默认开启思考，思考 token 按输出计费 [What's new in Opus 5](https://platform.claude.com/docs/en/models/opus-5/whats-new-opus-5)。
- **按每个任务的总成本比较，结果可能与单价排序相反**（以下都是厂商自报）：
  - GPT‑5.6 Sol 在 max effort 下 AA Coding Agent Index 为 80，比 Fable 5 高 2.8 点，输出 token 不到一半，成本低约三分之一（OpenAI 引用 Artificial Analysis）〔索引〕 [GPT‑5.6](https://openai.com/index/gpt-5-6/)。
  - Astra 的单任务成本比 Sol 低约 9%，比 Fable 5.1 低约 63%〔索引〕 [GPT‑6 Astra for work](https://openai.com/index/gpt-6-astra-next-generation-work/)。
  - 反方向的数据：第三方测算 Astra 在 max effort 下单任务成本约比 Sol 高 75% **〔第三方：Artificial Analysis，经二手报道，未直接核实〕**。
- **Fast mode**：Opus 5 和 GPT‑5.6 Sol 都有，约 2 倍价格换 2.5 倍速度，适合交互式、对延迟敏感的场景，不适合后台 subagent。
- **Copilot 路径的价格**：本机 models-store 里 Copilot 条目的 `cost` 字段基本与各厂商 API 价格一致。但 Copilot 实际按订阅配额计费，这些字段只能作相对参考〔本机〕。

## 对 pi subagent router 的评估与建议

本节只提建议，没有改任何代码。

### 当前实现（已读代码）

- `extensions/subagents/src/routing.ts`：`TASK_KIND_REASONING_EFFORTS` 为 general/code_research=`medium`、code_review/large_refactor/test_authoring=`high`、isolated_implementation=`medium`、algorithmic=`xhigh`。`taskFitTiers`：
  - analysis/general：企业环境 `[[pi, claude, kiro]]`；个人环境再加一层 `[codex]`。
  - sustained_change：`[[claude, pi], …]`。
  - bounded_change：企业环境 `[[kiro, pi], [claude]]`；个人环境 `[[codex, pi], [claude, kiro]]`。
- `backends/pi.ts`：effort 直接作为 pi 的 `thinkingLevel`。模型不指定时继承父会话。`settings.json` 里的父会话默认是 `github-copilot` / `gpt-5.6-sol` / `high`。
- `backends/claude.ts`：把 effort 翻译成 **`maxThinkingTokens`**（off=0、low=4096、medium=10000、high=16000、xhigh=32000、max=63999）。
- `backends/codex.ts`：`off`/`minimal`→`minimal`，`xhigh`/`max`→**`xhigh`**，然后按 `model/list` 就近取值。
- `backends/kiro.ts`：不处理 effort，README 也写明 Kiro 目前忽略它（**这条前提是错的，见 E 节**）。
- 本机 `~/.pi/agent/subagent-routing.json` 的环境是 `corporate`，所以当前实际生效的是企业环境这组 tier。

### A. 执行层问题：比调默认值更重要

1. **Claude harness 的 effort 实际上不起作用。** 当前 Claude 模型（Opus 5、Sonnet 5、Fable 5/5.1、Opus 4.7/4.8）只用自适应思考，固定预算模式不适用 [Claude Code model config](https://code.claude.com/docs/en/model-config)；API 在这些模型上不接受 `budget_tokens` [Models overview](https://platform.claude.com/docs/en/about-claude/models/overview)；SDK 也把 `maxThinkingTokens` 标为 deprecated。所以 router 选的 `high`/`xhigh` 发到 Claude 时，大概率只剩“0 = 关、非 0 = 开”两种效果〔推断：SDK 注释说 Opus 4.6 上就是这样处理的〕，**Claude 会一直用它自己的默认值（`high`）**。
   → **建议**：改成传 SDK 的 `effort` 选项，映射为 `off/minimal→low`、`low→low`、`medium→medium`、`high→high`、`xhigh→xhigh`、`max→max`，并停用 `maxThinkingTokens`。已安装的 SDK 0.3.273 支持这个选项。
2. **Codex harness 永远到不了 `max`。** 本机 `model/list` 显示 `gpt-6-astra`、`gpt-5.6-sol/terra/luna` 都支持 `max`（前三个还支持 `ultra`）〔本机〕。现在的 `preferredCodexEffort` 把 `max` 压成了 `xhigh`。
   → **建议**：`max` 直接透传，因为后面 `supportedCodexEffort` 的就近逻辑已经会处理不支持的模型。这里用的是 `scale`，要在其中补上 `max`。`ultra` 带自动分派 subagent，与 router 自己做编排的目标冲突，**不建议映射**。
3. **`off`/`minimal` 在多数目标上会被静默抬高。** Astra 不支持 `none`；Copilot 把 `minimal` 映射为 `low`；DeepSeek 把 `minimal` 映射为 `low`〔本机/索引〕。影响不大，但 `/subagents route` 的输出最好显示“实际生效的 effort”，而不只是请求的值。
4. **Kiro harness 没有传 effort。** 原文据 README 推断 Kiro“忽略 effort”，并由此建议把 algorithmic 的 tier 单独排（C.1）。**2026-09-20 实测推翻了这个前提**：kiro-cli 自己支持 `--effort`，是我们的 backend 没传。见 E 节。
   → **建议**：在 `backends/kiro.ts` 的 `chat` 参数里加上 `--effort`，C.1 不必再改 tier。

### B. effort 默认值与厂商证据是否一致

| task_kind | 当前 | 证据 | 建议 |
|---|---|---|---|
| general | medium | OpenAI：诊断/推理用 medium–high；Anthropic：medium 适合 agentic 均衡 | 保持 |
| code_research | medium | Opus 5 的 low/medium 质量强；Fable 5.1 在 low 下少搜索 | 保持（不要降到 low） |
| code_review | high | Opus 5 在较低 effort 下审查仍准确；OpenAI 的“推理代码”对应 medium–high | 保持 high。如果以后按规模区分，小 diff 可降到 medium |
| large_refactor | high | Anthropic：>30 分钟的长时编码用 xhigh；选型矩阵把大规模重构列在 Opus 5 下 | **改为 `xhigh`**。这一行首选 Claude harness，前提是 A.1 已修，否则改了也不生效 |
| test_authoring | high | 通常是有界、可验证的任务；Sonnet 5 medium≈Sonnet 4.6 high | 保持 high 可以接受；如果主要花在配额上，可降到 medium（低优先级） |
| isolated_implementation | medium | Codex：medium 是全能编码档 | 保持 |
| algorithmic | xhigh | OpenAI/Anthropic：xhigh/max 要 eval 证明值得；DeepSeek 在复杂场景用 max | 保持 `xhigh`，不要默认 `max` |

**需要新增的 task_kind**（按优先级）：

- **`quick`（琐碎编辑/搜索/摘要）→ `low`。** 这是厂商共识里最明确的一档，OpenAI 和 Anthropic 都点名 subagent 和抽取/分类类工作用 low。现在没有这一类，这些任务会落到 `general=medium`，大约多花一档的输出 token。
- **`planning`（规划/架构）→ `high`。** OpenAI 把“写计划”归入 medium/high。归类放在 analysis 下，tier 与 code_review 相同。
- 调试可以沿用 `code_research`（只做诊断）或 `isolated_implementation`（诊断并修复）。如果单独拆出来，应设为 `high`。

### C. harness tier 顺序

1. ~~**`algorithmic` 应与 `isolated_implementation` 分开排 tier。**~~ **已作废（2026-09-20）**：这条建议的唯一理由是“Kiro 忽略 effort”，而该前提是错的（见 E 节）。Kiro 接上 `--effort` 后，algorithmic 继续留在 bounded_change 即可。原建议为：企业环境 `[[pi], [claude], [kiro]]`，个人环境 `[[codex, pi], [claude]]`。
2. **sustained_change 保持 Claude 优先是有依据的。** 一是 Anthropic 官方把大规模重构和多小时自主编码划给 Opus 5/Fable 5.1；二是 DeepSeek 的第三方对比中，Opus 5 在 NL2Repo、ProgramBench、Terminal-Bench 3.0/4.0 等长程任务上领先 Sol。但 OpenAI 自报 Astra 在 Terminal-Bench 4.0 上高于 Fable 5.1。所以**个人环境里 codex 放第二层合理**；如果 Codex 默认模型是 Astra，可以考虑与 pi 并列。这一点证据不足，暂不建议改。
3. **bounded_change 在个人环境里 codex 优先是合理的。** GPT‑5.6 Sol medium 正是官方的“全能编码”定位。
4. **长上下文任务要避开 Codex harness。** 本机 models-store 显示 `openai-codex` provider（ChatGPT 订阅后端）所有模型的 `contextWindow` 都是 272,000，而同样的模型经 Copilot/API 是 1,050,000〔本机〕。Claude 当前模型都是 1M。router 目前没有“长上下文”这个维度。如果以后加，应只选 claude 或 pi（Copilot 1M 模型）。

### D. 为 pi harness 按任务给默认模型

现在 pi harness 不指定模型，继承父会话的 `gpt-5.6-sol`。所以不管什么任务，pi 都在跑 Sol，与 effort 的精细化配置不匹配。建议增加一张 `TASK_KIND_PI_MODEL_HINTS` 表，只在调用方没给 `model` 时使用。下表都是本机 Copilot models-store 里已有的 ID〔本机〕：

| task_kind | pi 模型提示（Copilot） | effort |
|---|---|---|
| quick（新增） | `github-copilot/gpt-5.6-luna` | low |
| general / code_research | `github-copilot/gpt-5.6-terra` | medium |
| planning（新增） | `github-copilot/claude-opus-5` 或 `github-copilot/gpt-6-astra` | high |
| code_review | `github-copilot/claude-opus-5` | high |
| large_refactor | `github-copilot/claude-opus-5` | xhigh |
| test_authoring | `github-copilot/claude-sonnet-5` | high |
| isolated_implementation | `github-copilot/gpt-5.6-sol` | medium |
| algorithmic | `github-copilot/gpt-6-astra` | xhigh |

这张表里的具体选择是本文综合厂商证据**推断**出来的，不是任何厂商的原话。上线前应该用 5–10 个真实子任务做一次小规模对照。

### E. Kiro 实测补充（2026-09-20，〔本机〕）

A.4 和 C.1 原本建立在“Kiro 忽略 effort”这个来自 README 的推断上。用本机的 `kiro-cli` 2.21.2 实测后，这个前提不成立：

- **支持 effort。** `kiro-cli chat --help` 里有 `--effort <EFFORT>`，说明写的是 `Initial effort level (e.g. low, medium, high, xhigh, max)`，与共享档位几乎一一对应（没有 `off`/`minimal`）。实跑 `--effort xhigh` 正常完成。
- **不校验 effort 取值。** 传 `--effort not-a-level` 时 CLI 既不报错也不退出，照常回答。所以传值没有崩溃风险，但**也无法从外部证明 effort 真的被送到了模型**，这是目前唯一的存疑点。
- **只提供 Claude 模型。** 从 `~/.kiro/sessions/cli/*.json` 的历史会话统计，出现过的模型只有 `auto`（Kiro 自己的选择器）、`claude-opus-5`、`claude-opus-4.8`、`claude-opus-4.7`、`claude-sonnet-5`、`claude-sonnet-4.6`、`claude-haiku-4.5`。**Kiro 不提供任何非 Claude 模型**，所以 A 节里 Anthropic 的 effort 指引同样适用于 Kiro：默认就是 `high`，`low` 适合 subagent 式的轻任务。
- **agent 只有两个**：`kiro_default` 和 `kiro_planner`。agent 选择决定接哪些 MCP connector，与模型选择是两件事。`kiro_planner` 适合配给 `planning`。
- 本机 `kiro-cli agent list` 需要登录才能跑，上面的模型和 agent 名是从会话文件里统计的，不是 CLI 的权威列表。

## 未解决问题

1. **OpenAI、DeepSeek、xAI 的官方页面没有直接读到**，只经搜索索引核对（公司代理拦截）。上线前应在非公司网络下逐字核对标 〔索引〕 的价格和 effort 取值，特别是 Astra 在 API 上的默认 effort：Codex 客户端显示 `low`，但 API 文档的默认值没有搜到。
2. **DeepSeek V4.1-Flash 的新价格没有核实**；V4-Pro 在 9-14 之后的状态，官方页面之间自相矛盾。
3. ~~**Kiro 可用的模型和推理控制**~~：**已解决（2026-09-20）**，见 E 节。剩下的疑点是 Kiro 是否真的把 `--effort` 传给了模型——它对非法值不报错，所以无法从 CLI 外部证明。
4. **Grok 4.6 在 Terminal-Bench 上的成绩**，以及 `grok-build-0.1` 的 effort 取值，都没有拿到。
5. **所有 benchmark 都是各家自测**，而且用的 harness 不同。DeepSeek 自己的数据显示，同一模型换 harness 差 5–8 个点。本文的排序只能当先验，最终应以 pi 自己在真实子任务上的对照为准。
6. **A.1 的实际效果**：`maxThinkingTokens` 在 Opus 5 或 Sonnet 5 上到底是被完全忽略，还是被当作开/关，官方只对 Opus 4.6 做了说明。可以抓一次 `claude` 子进程的请求或 `/status` 输出来确认。

## 来源

**Anthropic（直接读取）**

- [Models overview](https://platform.claude.com/docs/en/about-claude/models/overview)
- [Choosing a model](https://platform.claude.com/docs/en/about-claude/models/choosing-a-model)
- [Effort](https://platform.claude.com/docs/en/build-with-claude/effort)
- [What's new in Claude Opus 5](https://platform.claude.com/docs/en/models/opus-5/whats-new-opus-5)
- [What's new in Claude Fable 5.1](https://platform.claude.com/docs/en/models/fable-5-1/whats-new-fable-5-1)
- [What's new in Claude Sonnet 5](https://platform.claude.com/docs/en/models/sonnet-5/whats-new-sonnet-5)
- [Introducing Claude Opus 5](https://www.anthropic.com/news/claude-opus-5)
- [Claude Code model config](https://code.claude.com/docs/en/model-config)
- [Agent SDK TypeScript reference](https://code.claude.com/docs/en/agent-sdk/typescript)

**OpenAI（经搜索索引核对）**

- [GPT‑6 Astra](https://openai.com/index/gpt-6-astra/)
- [GPT‑6 Astra for work](https://openai.com/index/gpt-6-astra-next-generation-work/)
- [GPT‑6 Astra model page](https://developers.openai.com/api/docs/models/gpt-6-astra)
- [Model guidance](https://developers.openai.com/api/docs/guides/latest-model)
- [Reasoning models](https://developers.openai.com/api/docs/guides/reasoning)
- [GPT‑5.6](https://openai.com/index/gpt-5-6/)
- [GPT‑5.6 frontier efficiency](https://openai.com/index/gpt-5-6-frontier-intelligence-efficiency/)
- [Builder's guide to GPT‑5.6](https://openai.com/index/builders-guide-to-gpt-5-6/)
- [GPT‑5.6 Sol](https://developers.openai.com/api/docs/models/gpt-5.6-sol)
- [GPT‑5.6 Luna](https://developers.openai.com/api/docs/models/gpt-5.6-luna)
- [Pricing](https://developers.openai.com/api/docs/pricing)
- [Codex Models](https://developers.openai.com/codex/models)
- [Codex Prompting Guide](https://developers.openai.com/cookbook/examples/gpt-5/codex_prompting_guide)
- [Terra/Luna 降价公告](https://community.openai.com/t/announcing-a-major-price-drop-for-5-6-terra-and-luna-and-fast-mode-for-5-6-sol/1388484)
- [Sol 降价公告](https://community.openai.com/t/20-price-reduction-for-gpt-5-6-sol-api-codex-credits-and-chatgpt-work/1391726)

**DeepSeek**

- [DeepSeek-V4.1-Flash model card](https://huggingface.co/deepseek-ai/DeepSeek-V4.1-Flash)（直接读取）
- 以下经搜索索引核对：
  - [V4.1-Flash 发布](https://api-docs.deepseek.com/news/news260910/)
  - [Models & Pricing](https://api-docs.deepseek.com/quick_start/pricing)
  - [Thinking Mode](https://api-docs.deepseek.com/guides/thinking_mode/)
  - [Change Log](https://api-docs.deepseek.com/updates/)
  - [V4-Pro GA](https://api-docs.deepseek.com/news/news260813/)

**xAI（经搜索索引核对）**

- [Grok 4.6 docs](https://docs.x.ai/developers/grok-4-6)
- [Models & Pricing](https://docs.x.ai/developers/models)
- [Reasoning](https://docs.x.ai/developers/model-capabilities/text/reasoning)
- [Grok 4.3](https://docs.x.ai/developers/models/grok-4.3)
- [Introducing Grok 4.6](https://x.ai/news/grok-4-6)
- [Grok 4.6 model card](https://media.x.ai/v1/website/card-4p6-4cd2dc57.pdf)
- [Grok Build 0.1](https://x.ai/news/grok-build-0-1)

**本机第一方数据**

- `codex app-server` 的 `model/list`（codex-cli 0.154.0）
- `~/.pi/agent/models-store.json`
- `tools/ai/pi/node_modules/@anthropic-ai/claude-agent-sdk/sdk.d.ts`

**本仓库**

- [`tools/ai/pi/README.md`](../../tools/ai/pi/README.md)
- [`routing.ts`](../../tools/ai/pi/extensions/subagents/src/routing.ts)
- [`backends/`](../../tools/ai/pi/extensions/subagents/src/backends/)
- [`settings.json`](../../tools/ai/pi/settings.json)
