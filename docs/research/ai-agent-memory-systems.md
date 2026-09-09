# AI Agent Memory System 选型：EverOS、Nowledge Mem 与 MemPalace

> 调研日期：2026-09-07  
> 证据范围：仅使用官方仓库、官方文档、项目论文和官方网站。所有 benchmark 都按其真实指标解读，不把 retrieval recall 和 end-to-end QA accuracy 混在一起。

## 结论

**如果只选一个长期主系统，并且你接受运维一个中心服务，最适合你的是 EverOS + EverMe。如果“开箱即用的跨设备 + Obsidian 检索”比开源与可审计更重要，则改选 Nowledge Mem。**

> 2026-09-07 补充：你的人工知识库已迁移到 Obsidian。这会提高 Nowledge 的适配度，但它的 Obsidian connector 目前是让 AI Now 在本机搜索、读取 vault，不是双向编辑或 Obsidian Sync 替代品。[官方 Obsidian 接入说明](https://mem.nowledge.co/docs/use-cases/notes-everywhere)

决定性理由是：

- 你的 Obsidian、dotfiles 和 Git 工作方式都倾向“文本是真相，索引可重建”。EverOS 正好用 Markdown 作为 source of truth，SQLite/LanceDB 只承担状态和索引，可直接编辑、`grep`、diff 和 Git 版本化。[官方 README](https://github.com/EverMind-AI/EverOS#why-everos)
- 你的 memory 可能混有公司/工程敏感上下文。EverOS 核心 Apache-2.0、可完整自托管，并能把 provider 指向你自己允许的本地或内网端点；这比将核心数据层交给专有引擎更容易审计和长期控制。
- EverOS 原本的集成短板已由官方 [EverMe](https://github.com/EverMind-AI/EverMe) CLI/plugin suite 部分补上：同一个客户端层面向 Claude Code、Codex、Cursor、Hermes、OpenClaw 等，也可通过 `EVERME_API_BASE` 指向自托管 EverOS。

Nowledge Mem 仍然非常值得做对照试用：你同时使用 Claude Code、Codex、OpenCode 和 Pi，它对这四个都有专门 connector，覆盖启动上下文、历史检索和 session/thread 自动捕获。[官方 connector 概览](https://mem.nowledge.co/docs/integrations/agent-plugins) 而且你的 Claude 配置已启用 `nowledge-mem@nowledge-community`，试用边际成本最低；见 [`tools/ai/claude/settings.json`](../../tools/ai/claude/settings.json)。不过当前 shell 的 `PATH` 中找不到 `nmem`，因此必须先验证 Nowledge 服务和 CLI，不能把“插件已启用”当成“memory 链路已可用”。

Nowledge 的硬边界是：**它是专有软件，不是开源 memory engine**。免费版只有 50 条本地 memory；无限本地 memory 需要 Mem Vault 一次性许可或 Plus 订阅。[官方价格页](https://mem.nowledge.co/pricing?billing=month) [官方使用条款](https://mem.nowledge.co/terms)

MemPalace 排第三。它最适合“先完整保存原文，之后再找”的证据库，也是三者中离线检索成本最低的选择；但它对你最在意的多工具“自动连续性”和知识演化不如 Nowledge 完整，数据也不像 EverOS 那样以人类可读 Markdown 作为 canonical source。

## 项目身份核对

| 你的写法 | 准确项目 | 说明 |
|---|---|---|
| `everos` | [EverMind-AI/EverOS](https://github.com/EverMind-AI/EverOS) | 官方当前名称是 **EverOS**；旧资料常称 **EverMemOS**。EverOS 1.0 是新架构，不应把旧版部署方式当成当前事实。[官方迁移说明](https://github.com/EverMind-AI/EverOS/blob/main/docs/migration-to-1.0.0.md) |
| `nowledgemem` | [Nowledge Mem](https://mem.nowledge.co/) | 官方品牌就是 **Nowledge**（没有 K）。其 [GitHub 仓库](https://github.com/nowledge-co/nowledge-mem) 主要是说明和交换格式引用，并非完整产品源码。 |
| `mempalace` | [MemPalace/mempalace](https://github.com/MemPalace/mempalace) | 官方特别警告存在仿冒网站和恶意软件；只信任该 GitHub 组织、[PyPI](https://pypi.org/project/mempalace/) 和 `mempalaceofficial.com`。[官方历史/更正](https://github.com/MemPalace/mempalace/blob/develop/docs/HISTORY.md) |

## 横向比较

| 维度 | EverOS | Nowledge Mem | MemPalace |
|---|---|---|---|
| 主要定位 | 可组装的开源 memory runtime，同时建模用户记忆和 agent 经验 | 跨工具个人 context/knowledge hub，强调自动捕获和成品 UI | 原文优先的本地 memory archive + retrieval layer |
| 记忆模型 | 用户 track：episode/profile；agent track：case/skill；另有 Knowledge Wiki 和离线 reflection | Thread、Memory（事实/偏好/决策/计划/程序等）、Working Memory、Library、Skills、Space、Crystal、EVOLVES 时间链和知识图谱 | 原文 drawer，按 wing/room/hall 分区；closet 索引、temporal KG、agent diary，可选本地 NLP/LLM 增强 |
| 召回 | BM25、vector、hybrid，以及更慢/更贵的 agentic search | 语义 + BM25 + label + entity/community/graph，再加时间、衰减、置信度和可选 LLM deep mode | 原文 embedding 召回，可加 keyword/temporal/preference boost 和可选 LLM rerank |
| Canonical 存储 | **Markdown 真实数据源** + SQLite 状态/队列 + LanceDB 索引 | KuzuDB graph + LanceDB vector/BM25 索引；可导出 portable text bundle，但日常 canonical store 不是普通文本 | 默认嵌入式 ChromaDB，另有 SQLite exact、Milvus、Qdrant、pgvector backend |
| 本地/隐私 | 本地存储，默认 loopback；记忆提取需 LLM，向量/重排可分别配置 | 核心存储和快速搜索可离线；远程 LLM 是用户选择，但高级提炼/后台智能取决于 provider 和 license | 默认全本地，基本检索无 API key；只有用户显式选择 reranker/远程 backend 时数据才外发 |
| Agent/协议 | HTTP API v2；官方 [EverMe](https://github.com/EverMind-AI/EverMe) CLI/plugin suite 覆盖 Claude Code、Cursor、Codex、Hermes、OpenClaw 等，可通过 `EVERME_API_BASE` 指向自托管 EverOS | 通用 streamable HTTP MCP + REST API + CLI；对你的 Claude Code/Codex/OpenCode/Pi 都有专用 connector | 45 个 MCP tools；Claude Code/Codex/Cursor 有 auto-save hooks，其他 MCP client 可通用接入 |
| 部署运维 | Python 3.12，`pip/uv` + 常驻 server；一个 OpenRouter key 可跑 Tier 1，高质量功能要再配 embedding/rerank | 桌面应用最省事；也有 Linux headless 和 Docker；跨设备需一台 always-on 主机或付费 Nowledge Link | Python 3.9+，推荐 `uv tool`；默认下载 80–300 MB embedding model；有 Docker 和安全的远程/team server |
| 许可 | Apache-2.0，核心开源 | **专有许可**；Free 50 memories，Mem Vault 一次买断或 Plus 订阅 | MIT，核心开源 |
| 锁定风险 | **低**；Markdown 可直接读取、Git 版本化，HTTP API 也比较直接 | **中高**；有文本导出缓解数据锁定，但引擎、自动提炼和 connector 行为受专有产品与许可控制 | **中低**；MIT + 可换 backend，但默认 canonical 数据在 ChromaDB 而不是可直接维护的 Markdown |

### 架构与记忆语义

#### EverOS

EverOS 的特色是把“记住用户”和“agent 自己从执行中学会”分成两条一等路径：用户有 episode/profile，agent 有 case/skill。它能将多个 case 反思、聚类为可复用 skill，这对编码 agent 长期学习很有价值。[官方 API 与记忆类型](https://github.com/EverMind-AI/EverOS/blob/main/docs/api.md)

其存储设计是三者中最适合你直接管理的：Markdown 保存事实，SQLite 保存队列/审计，LanceDB 保存 vector/BM25/scalar index；手动改 `.md` 后 watcher 会更新索引。[官方存储说明](https://github.com/EverMind-AI/EverOS/blob/main/docs/how-memory-works.md)

代价是配置面更宽。只配 LLM 可以提取记忆和 keyword search；vector/hybrid、reflection/skill extraction、agentic search 分别需要 embedding/rerank 能力。[官方快速开始](https://github.com/EverMind-AI/EverOS#quick-start)

#### Nowledge Mem

Nowledge 的优势是完整的记忆生命周期：先存原始 Thread，再提炼成 typed Memory；知识改变时用 EVOLVES 链保留历史，多个来源收敛时可形成 Crystal，最后用 Working Memory 在 agent 启动时提供当前焦点。[官方 Concepts](https://mem.nowledge.co/docs/concepts) [Crystals](https://mem.nowledge.co/docs/concepts/crystals)

快速搜索会并行运行语义、全文和 entity 匹配；deep mode 再做意图分类、HyDE、时间理解与可选 LLM rerank。排序还受约 30 天半衰期、访问频率、importance floor 和 confidence 影响。[官方搜索架构](https://mem.nowledge.co/docs/concepts/search-architecture) [官方相关性说明](https://mem.nowledge.co/docs/search-relevance)

这是三者中最强的产品层，但也是锁定最明显的一个。它可导出开放、文本化的数据包，却无法像 EverOS 那样自己修改或 fork 核心引擎。[官方数据可携带性说明](https://mem.nowledge.co/docs/)

#### MemPalace

MemPalace 的基本判断很鲜明：不让 LLM 在写入时删改原文，而是保存完整对话，用 wing/room/hall/drawer 分层与检索。这避免了提炼错误造成的不可逆信息丢失。[官方 README](https://github.com/MemPalace/mempalace#what-it-is)

它已经不只有向量库：当前还有 SQLite-backed temporal KG、closet 索引、BM25 hybrid、agent diary 和可插拔 backend；但它的主轴仍是“保真 + 召回”，不是 Nowledge 式的主动知识整理。[官方 Knowledge Graph/MCP 概览](https://github.com/MemPalace/mempalace#knowledge-graph)

### 部署、隐私与跨工具接入

#### 跨设备的准确含义

EverOS 可以让多台设备共用同一份 memory，但官方路径是“一个中心后端 + 多个客户端”，不是“每台设备各有数据库，离线后再合并”。使用 EverMe 托管服务时，多设备和多 agent 读写同一个用户 memory pool；自托管时，各设备的 EverMe 客户端通过 `EVERME_API_BASE` 指向同一个 EverOS endpoint。[官方 EverMe 架构](https://github.com/EverMind-AI/EverMe#architecture)

不建议用 Obsidian Sync、Syncthing 或 Dropbox 同步整个 `~/.everos/` 运行目录：虽然 Markdown 是真实数据源、索引可重建，但官方没有提供多主写入或文件冲突合并协议。如果自己做文件备份，应将 `.index/`、`.tmp/`和本机配置排除，并把它当备份而不是在线同步。这是根据官方存储和一致性设计做出的工程判断，不是官方承诺的同步功能。[官方存储说明](https://github.com/EverMind-AI/EverOS/blob/main/docs/how-memory-works.md)

另一个安全边界：EverOS 默认只绑定 `127.0.0.1`，且本体不内置身份验证。自托管跨设备时不能直接暴露 `:8000`，必须在前面放 VPN 或带 TLS/认证的 gateway。[官方 API 安全边界](https://github.com/EverMind-AI/EverOS/blob/main/docs/api.md#authentication)

#### EverOS

- `uv pip install everos`，Python 3.12+，默认在 `127.0.0.1:8000` 运行 HTTP API v2。[官方 README](https://github.com/EverMind-AI/EverOS#quick-start) [API 文档](https://github.com/EverMind-AI/EverOS/blob/main/docs/api.md)
- EverMe 是官方跨 agent 客户端层，支持托管服务或通过 `EVERME_API_BASE` 指向自己的 EverOS；这降低了手写每个 agent 集成的成本。[官方 EverMe 仓库](https://github.com/EverMind-AI/EverMe)
- 默认存储完全在本机，但如果用 OpenRouter/OpenAI-compatible 服务做提取、embedding 或 rerank，相关内容会依你的 provider 配置外发。

#### Nowledge Mem

- 桌面端覆盖 macOS，Windows/Linux 当前仍标为 Preview；也提供 amd64/arm64 Docker 和 Linux headless，适合你的 WSL/Linux 主环境。[官方下载页](https://mem.nowledge.co/download) [Linux server 文档](https://mem.nowledge.co/docs/server-deployment)
- 本地服务为 `127.0.0.1:14242`，同一套 REST API 被应用、CLI 和 MCP 使用。[官方 API 文档](https://mem.nowledge.co/docs/api)
- 数据默认存在本机，官方声明无遥测/无数据收集；启用远程 LLM 时数据只发给你选择的 provider。[官方隐私页](https://mem.nowledge.co/privacy)
- 跨设备的本质是“一台 always-on Mem server + 多个客户端”；可用自己的 Cloudflare/Tailscale 通道，也可买 Nowledge Link。[官方远程访问文档](https://mem.nowledge.co/docs/remote-access)

#### MemPalace

- Python 3.9+，官方建议 `uv tool install mempalace`；默认 ChromaDB 与本地 embedding model，首次需下载模型。[官方安装与要求](https://github.com/MemPalace/mempalace#install)
- MCP 层功能很宽，但对你的工具组合来说，只有 Claude Code/Codex/Cursor 被明确列为 auto-save hook 目标；OpenCode/Pi 需依赖通用 MCP 或自己编排捕获。[官方 hooks 说明](https://github.com/MemPalace/mempalace#auto-save-hooks)
- 如果从本机升级到 team server，可以用 HTTP MCP + Milvus/Qdrant/pgvector，但这会失去单机 local-first 的部署简单性。[官方 remote/team server 文档](https://github.com/MemPalace/mempalace/blob/develop/website/guide/remote-server.md)

## Benchmark 与证据强度

### EverOS：学术证据最完整，但要区分旧 EverMemOS 与当前 1.x

[EverMemOS 论文](https://arxiv.org/abs/2601.02163) 将记忆分为 MemCell（episode/atomic fact/foresight）、MemScene 语义巩固和 agentic reconstructive recollection，并报告 LoCoMo/LongMemEval 上的端到端结果。官方旧版项目页曾报告 LoCoMo 93.05%、LongMemEval 83.00%。

但当前 EverOS 1.x 是重写架构，上述数字不能直接当成你部署当前版本的 SLA。现仓库提供可复现 LoCoMo runner，样例报告展示约 93.3% majority-vote QA accuracy，但样例不等于一份独立第三方验证。[当前官方评测 runner](https://github.com/EverMind-AI/EverOS/blob/main/benchmarks/README.md)

### Nowledge Mem：搜索管线透明，但没有标准公开分数

官方文档详细公开了排序信号、衰减和 deep mode，但截至本次调研，**没有在官方来源中找到 LoCoMo、LongMemEval 或其他标准数据集的可复现结果**。因此它的优势应归因于产品和集成完整度，而不是已被公开 benchmark 证明的召回质量。

### MemPalace：可复现的 retrieval 很强，但不是端到端 QA

官方当前可辩护的数字是：

- LongMemEval-s raw semantic retrieval：96.6% R@5，500 题，无 LLM/API；
- hybrid v4：在 50 题开发集调整后，450 道 held-out 问题为 98.4% R@5，无 LLM；
- LoCoMo hybrid v5：88.9% R@10，无 rerank。

[官方 benchmark 说明与原始结果](https://github.com/MemPalace/mempalace/blob/develop/benchmarks/BENCHMARKS.md)

这些数字的含义是“正确 session 是否出现在 top-k”，不包含让 LLM 生成最终答案再被 judge 判定正确的步骤。官方现已主动更正早期将它们与其他系统 QA 分数并排的说法。所以可以认为 MemPalace 的原文召回很强，但不能由此宣称它的整体记忆问答胜过 EverOS 或 Nowledge。

## 成熟度与维护风险

三者都在 2026 年快速变化，都不应被当作“十年不动的基础设施”。

- **EverOS**：1.0 于 2026-06 发布，1.2.x 仍在快速调整 schema、provider capability 与 API；官方 release 记录中还出现过 1.2.0 重新引入已修补 path traversal 的情况，说明项目活跃但尚年轻。[官方 Releases](https://github.com/EverMind-AI/EverOS/releases)
- **Nowledge Mem**：截至 2026-09-05 为 0.10.78；0.10 是核心引擎的原生 Rust 重写，官方自己也明说新引擎可能有意外，且保留回滚到 0.9.29 的路径。最近更新频率极高，新功能与修复密集，有产品团队，也意味着当前变动风险最大。[官方 Changelog](https://mem.nowledge.co/changelog)
- **MemPalace**：2026-08 已到 3.9.0，有频繁发布、多后端、数据修复与大库性能工作，且公开记录了 benchmark 更正。工程透明度很好，但版本速度和历史上的 Chroma/HNSW 一致性修复也表明要做备份并 pin 版本。[官方 Changelog](https://github.com/MemPalace/mempalace/blob/develop/CHANGELOG.md) [官方 Releases](https://github.com/MemPalace/mempalace/releases)

## 针对你的排名

| 排名 | 系统 | 适合条件 | 不适合条件 |
|---:|---|---|---|
| 1A | **EverOS** | 你最看重 Markdown/Git 可审计性、低锁定、敏感数据自托管、agent case → skill 长期演化，并愿意运维一个受保护的中心 server | 你需要多端离线写入后自动合并，或不想管 provider/gateway/API 配置 |
| 1B | **Nowledge Mem** | 你把开箱即用的跨设备、Claude/Codex/OpenCode/Pi 闭环和 Obsidian 本地搜索放在第一位，并接受 $19.99 beta 买断或订阅 | 你要必须开源、完全可 fork，要求 Obsidian 双向写回，或不接受核心数据库格式受单一厂商控制 |
| 3 | **MemPalace** | 你主要想建一个离线、保留每个字、检索召回强的对话/项目证据库 | 你要的是多 agent 全生命周期自动捕获、主动知识演化与稳定的成品 UI |

## 建议的试用方式

1. **先用一个非敏感活跃项目做两周对照实验，不是永久承诺。** 主线部署 EverOS + EverMe；先用 `nmem status` 确认 Nowledge 后端可用，再让它处理同一组可公开/可清理的对话。不导入公司历史和密钥相关内容。
2. 连续记录 20–30 个真实查询，分别测：精确决策、旧方案被新方案覆盖、跨工具接力、中文记忆、项目边界、密钥/公司信息防泄漏。
3. 每周做一次 portable text export 并实际打开检查；如果无法在不依赖 Nowledge 的情况下读懂、搜索和还原关键决策，就不能把它当唯一副本。
4. 两周后只看四个门槛：自动捕获成功率 ≥95%；有来源的 top-5 召回成功率 ≥90%；错误过时记忆进入答案 <5%；每周手工维护 <30 分钟。
5. EverOS 若连续败在集成/维护门槛，再升 Nowledge 为主系统；如果两者都败在“提炼丢掉原话”，改用 MemPalace。

## 最终判断

Nowledge Mem 赢在**现成的跨工具闭环**，EverOS 赢在**可拥有、可审计的长期数据模型**，MemPalace 赢在**低成本保真召回**。

对你而言，最稳妥的路径是：**Obsidian 继续做人工知识的 canonical store；agent memory 另立一层。愿意运维中心服务就选 EverOS + EverMe，更重视成品化跨设备与 Obsidian 搜索就选 Nowledge Mem。** MemPalace 只在你明确需要原文证据库时选用。
