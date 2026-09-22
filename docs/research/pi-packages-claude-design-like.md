# Pi Packages Similar to Claude Design / Codex Design

Date: 2026-09-21

This corrects an earlier comparison that focused on Claude Code/Codex agent UX. Here, "Claude Design" means Anthropic's Claude Design product: a visual-design workspace for prototypes, mockups, slides, marketing collateral, design-system-aware generation/refinement, export, and handoff to code. "Codex Design" is interpreted as OpenAI/Codex design workflows around UI mocks and Figma/design-context iteration.

Primary reference: Anthropic's Claude Design announcement says it can create designs, prototypes, slides, one-pagers, marketing collateral; build team design systems from code/design files; import text/images/documents/codebase/web captures; refine via comments/direct edits/sliders; collaborate/share; export to Canva/PDF/PPTX/HTML/folders; and hand off bundles to Claude Code. Source: https://www.anthropic.com/news/claude-design-anthropic-labs

OpenAI/Codex design references include OpenAI's Figma/Codex workflow and skill docs for design-to-code and UI mocks. Sources: https://developers.openai.com/blog/building-frontend-uis-with-codex-and-figma and https://developers.openai.com/codex/use-cases/user-stories-to-ui-mocks

## High-level conclusion

No Pi package found is a full Claude Design equivalent. Pi packages cover pieces:

- local HTML/design-system workspace: `pi-design-mode`
- Figma/design input: `pi-figma`, `pi-figma-mcp`
- mockup-first frontend workflows: `@nklisch/pi-ux-ui-design`, `@blackbelt-technology/frontend-mockup-loop`, `pi-ui-design`
- PPTX/slides: `@tmustier/pi-clean-slides`, `pi-powerpoint`
- visual explanatory HTML/slides: `visual-explainer`
- poster/graphic artifacts: `@joemccann/pi-canvas-design`

For a Claude Design-like setup inside Pi, the best practical stack is:

```bash
pi install npm:pi-design-mode
pi install npm:pi-figma-mcp        # if you use Figma desktop MCP
pi install npm:@blackbelt-technology/frontend-mockup-loop
pi install npm:@tmustier/pi-clean-slides
pi install git:github.com/nicobailon/visual-explainer
```

## Comparison table

| Package | Closest Claude/Codex Design capability | Strengths | Gaps vs Claude Design | Install |
|---|---|---|---|---|
| `pi-design-mode` | Local design workspace with tokens/components, revisions, interactive HTML export | Most directly analogous to Claude Design's design workspace. Has `.pi-design/` design-system source of truth, browser editor, revisions, conflict detection, interactive HTML/CSS/JS export with receipt, `/design` commands | Limited sample components; not arbitrary element editing; no PPTX/PDF/Canva export; no org collaboration; macOS-focused verification | `pi install npm:pi-design-mode@0.3.4` |
| `pi-ui-design` | Figma-like free-form canvas for UI skeletons | Infinite canvas, primitives, components, theme controls, review notes, exports HTML/CSS, React JSX, Tailwind; model can read/modify via tools | More wireframe/blueprint than polished design product; package metadata looked less polished; gallery page fetch was unreliable | `pi install npm:pi-ui-design` per gallery/README |
| `@blackbelt-technology/frontend-mockup-loop` | Codex-like frontend design loop with screenshots and validation | Strongest structured frontend loop: ground → contract → mockup → test → fix → promote → learn; live mockup server; Playwright breakpoint screenshots; design contract; preset systems (shadcn, MUI, Material 3, Fluent 2, Apple HIG) | It is a workflow/gate, not a visual editor; requires project/frontend context; low downloads in gallery snapshot | `pi install npm:@blackbelt-technology/frontend-mockup-loop` |
| `@nklisch/pi-ux-ui-design` | Mockup-first UI/UX workflow shared across Claude Code, Codex, and Pi | Generates standalone `.mockups/` HTML options, flows, palettes, tokens; explicitly supports Pi, Claude Code, and Codex; very portable/no build step | Skill-only, no native visual canvas/server; explicitly not for production code or highly interactive prototypes | `pi install npm:@nklisch/pi-ux-ui-design` |
| `pi-figma` | Figma REST design inspection/import | Reads Figma files/components/styles/nodes/assets/comments via REST; exports assets; useful for design-to-code handoff | Read-oriented; no Figma editing; needs token; not a local design workspace | `pi install npm:pi-figma` |
| `pi-figma-mcp` | Codex/Figma-style live design context | Connects Pi to Figma desktop MCP at `127.0.0.1:3845/mcp`; exposes selection/frame/screenshot/metadata tools; good for implementing selected designs | Requires Figma desktop + MCP support; read/inspect oriented | `pi install npm:pi-figma-mcp` |
| `@tmustier/pi-clean-slides` | Presentation/deck generation | YAML-to-PPTX consulting-style slides; can use templates; inspect/edit/render; good for structured business/table slides | Deliberately not general-purpose visual deck builder; no freeform graphics/animation | `pi install npm:@tmustier/pi-clean-slides` |
| `pi-powerpoint` | General PowerPoint creation/editing | Skill wrapping a PowerPoint MCP server CLI; create slides, text, images, tables, charts, shapes, themes | More technical/CLI-like; dependencies Bun+uv; known upstream state issues | `pi install npm:pi-powerpoint` |
| `visual-explainer` | Styled HTML explainers/diagrams/slides | Generates self-contained HTML pages with Mermaid diagrams, tables, plan/diff reviews; optional PPTX utility; supports Pi/Claude/Codex skill paths | Explanation/diagram artifact tool, not design workspace; install via git for Pi | `pi install git:github.com/nicobailon/visual-explainer` |
| `@joemccann/pi-canvas-design` | Posters/visual art/PDF/PNG artifacts | Skill for museum-quality posters/art prints/books as PNG/PDF with bundled fonts | Not UI/product design; no iterative editor/collaboration | `pi install npm:@joemccann/pi-canvas-design` |

## Feature coverage vs Claude Design

| Claude Design feature | Pi package coverage |
|---|---|
| Prompt-to-polished visual artifact | Partial: `pi-design-mode`, `pi-ui-design`, `@nklisch/pi-ux-ui-design`, `visual-explainer`, `@joemccann/pi-canvas-design` |
| Interactive prototypes | Best: `pi-design-mode`; also `pi-ui-design`/mockup packages via HTML |
| Team design system from code/design files | Partial: `pi-design-mode` tokens/components; `frontend-mockup-loop` design contracts/presets; Figma packages read design context |
| Inline comments/direct edits | Partial: `pi-ui-design` review notes; `pi-design-mode` browser edits/revisions |
| Adjustment sliders/knobs | Partial: `pi-design-mode` token/component controls; not Claude-generated custom knobs |
| Import images/docs/PPTX/XLSX | Weak. Figma/code/design-system inputs are covered; document import is not comparable |
| Web capture | Weak. Could combine browser/screenshot tools, but no Claude Design-like capture workflow found |
| Collaboration/org sharing | Not found as a Pi package equivalent |
| Export HTML | Strong: `pi-design-mode`, `pi-ui-design`, `@nklisch/pi-ux-ui-design`, `visual-explainer` |
| Export PDF/PNG | `@joemccann/pi-canvas-design`; maybe browser print workflows for HTML outputs |
| Export PPTX | `@tmustier/pi-clean-slides`, `pi-powerpoint`, optional `visual-explainer-pptx` |
| Export Canva | Not found |
| Handoff to code agent | Partial: `pi-design-mode /design frontend`; `pi-ui-design read_ui_design`; `frontend-mockup-loop PROMOTE`; Figma packages for implementation context |

## Recommendations

1. **Closest single package to Claude Design:** `pi-design-mode`.
   - It has a real local browser design workspace, design tokens/components, revisions, and interactive HTML export.
   - Best for local prototypes and design-system experiments inside Pi.

2. **Closest Codex-style design-to-code workflow:** `pi-figma-mcp` + `@blackbelt-technology/frontend-mockup-loop`.
   - Use Figma MCP for live design context and screenshots.
   - Use the mockup loop for breakpoint screenshots, validation, design contracts, and promotion to implementation.

3. **Fast low-friction UI exploration:** `@nklisch/pi-ux-ui-design`.
   - Skill-only, portable, produces `.mockups/` HTML options/flows/palettes.
   - Good when you want the agent to generate several design directions before coding.

4. **Slides/decks:** `@tmustier/pi-clean-slides` if you want structured consulting-style PPTX; `pi-powerpoint` if you want more general PowerPoint manipulation.

5. **Not currently matched well in Pi:** Canva export, multi-user org collaboration, document import from DOCX/PPTX/XLSX into a design surface, and Claude Design's polished all-in-one refinement UI.

## Security note

All Pi packages/extensions run with the permissions of the Pi process. Design tools may start local HTTP servers, read project files, connect to Figma, write exports, or invoke browser/PowerPoint/Python tooling. Review package source and avoid exposing local servers beyond localhost unless you intend to.
