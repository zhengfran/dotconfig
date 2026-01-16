# Emacs Scripts

This directory contains utility scripts for Emacs integration with external tools.

## emacs-snippet-search.ps1

PowerShell script that launches Emacs snippet search frame via emacsclient.

### Purpose
Enables system-wide snippet access through Raycast launcher on Windows.

### Requirements
- Emacs with modular config running (server auto-starts)
- emacsclient.exe in PATH (installed via Scoop)
- Raycast for Windows installed

### Usage

**Via Raycast (Recommended):**
1. Press `Alt+Space` (or your Raycast hotkey)
2. Type `ss`
3. Press `Enter`
4. Emacs snippet search frame appears
5. Search for snippet with fuzzy matching
6. Press `Enter` to select snippet
7. Snippet is copied to clipboard
8. Frame closes automatically
9. Paste snippet anywhere with `Ctrl+V`

**Direct execution (Testing):**
```powershell
powershell -File "C:\Users\uie41442\dotconfig\scripts\emacs\emacs-snippet-search.ps1"
```

### Raycast Configuration

**Method 1: Simple Command (Quickest)**
1. Open Raycast settings
2. Add new Script Command
3. Configure:
   - **Name**: Emacs Snippet Search
   - **Keyword**: `ss`
   - **Command**: `powershell -ExecutionPolicy Bypass -File "C:\Users\uie41442\dotconfig\scripts\emacs\emacs-snippet-search.ps1"`
   - **Icon**: 📝 or 📋
   - **Description**: Search and insert Emacs yasnippets

**Method 2: Raycast Script (If supported on Windows)**
1. Copy script to Raycast scripts directory
2. Add Raycast metadata header
3. Restart Raycast
4. Script appears automatically

### How It Works

```
User types 'ss' in Raycast
    ↓
Raycast runs PowerShell script
    ↓
Script verifies:
  - emacsclient exists
  - Emacs server is running
    ↓
Script calls: emacsclient -f "server-file" -c -e "(my/global-snippet-search)"
    ↓
Emacs opens popup frame (120x40, centered on primary monitor)
    ↓
Consult/Vertico fuzzy search interface
    ↓
User searches and selects snippet
    ↓
Emacs:
  - Evaluates backtick expressions (e.g., `(format-time-string "%Y-%m-%d")`)
  - Strips yasnippet variables ($1, ${2:default})
  - Copies processed snippet to Windows clipboard
    ↓
Frame closes automatically
    ↓
User pastes snippet in any application
```

### Troubleshooting

**Error: "Emacs server is not running!"**
- **Cause**: Emacs not started or server didn't start
- **Solution**: 
  ```powershell
  emacs --init-directory "C:\Users\uie41442\dotconfig\basic\editor\emacs\modular"
  ```
- Verify server started: Check *Messages* buffer for "Emacs server started for snippet engine"

**Error: "emacsclient not found"**
- **Cause**: Emacs not installed or not in PATH
- **Solution**:
  ```powershell
  scoop install emacs
  ```

**Raycast doesn't find 'ss' command**
- **Cause**: Script command not configured
- **Solution**: Re-check Raycast settings, verify script path, restart Raycast

**Frame doesn't close after selection**
- **Cause**: Emacs config issue
- **Solution**: Test snippet search in Emacs: `SPC s g`
- Check *Messages* buffer for errors

### Files

- **Script**: `~/dotconfig/scripts/emacs/emacs-snippet-search.ps1`
- **Emacs Config**: `~/dotconfig/basic/editor/emacs/modular/modules/snippets.el`
- **Snippets**: `~/org/snippets/MODE-NAME/*.snippet`
- **Server Auth**: `~/.config/emacs/modular/var/server/auth/server`

### Migration Notes

This script replaces the previous AutoHotkey integration:
- **Old**: AutoHotkey script with `Ctrl+Shift+Space` hotkey
- **New**: Raycast launcher with `ss` keyword
- **Removed**: `~/dotconfig/basic/editor/emacs/emacs-snippet-hotkey.ahk`

### Benefits over AutoHotkey

- ✅ No dedicated background process
- ✅ Integrated with existing Raycast launcher
- ✅ Visual feedback (see "Snippet Search" before executing)
- ✅ Consistent UX with other Raycast commands
- ✅ More flexible (can add more Emacs commands easily)
- ✅ Modern launcher ecosystem

### Future Enhancements

- Add more Emacs commands (open file, search notes, etc.)
- Direct snippet list in Raycast UI (no Emacs frame)
- Snippet preview in Raycast results
- Tag-based filtering
- Frequently used snippets tracking

---

## emacs-snippet-capture.ps1

PowerShell script that launches Emacs snippet capture form via emacsclient.

### Purpose
Create new yasnippet snippets interactively via a popup form from anywhere in the system.

### Requirements
- Emacs with modular config running (server auto-starts)
- emacsclient.exe in PATH (installed via Scoop)
- Raycast for Windows installed

### Usage

**Via Raycast (Recommended):**
1. Press `Alt+Space` (or your Raycast hotkey)
2. Type `sc` (snippet capture)
3. Press `Enter`
4. Emacs snippet capture form appears
5. Fill in the form fields:
   - **Mode**: Select existing mode or create new (e.g., `python-mode`)
     - Press TAB to see available modes
     - Choose `+ Create new mode` to create a new mode directory
   - **Name**: Display name for snippet (e.g., `Leetcode Notes`)
   - **Key**: Optional trigger key (e.g., `!lc`, `<date`)
   - **Content**: Snippet body with placeholders
6. Press `C-c C-c` to save (or `C-c C-k` to cancel)
7. Frame closes automatically after save
8. Snippet is immediately available in Emacs

**Via Emacs:**
- Keybinding: `SPC s n`
- Opens same form in current Emacs session (no frame management)

**Direct execution (Testing):**
```powershell
powershell -File "C:\Users\uie41442\dotconfig\scripts\emacs\emacs-snippet-capture.ps1"
```

### Snippet Placeholders

Use yasnippet syntax in the content field:

| Syntax | Description | Example |
|--------|-------------|---------|
| `$0` | Final cursor position | `Hello $1, $0` |
| `$1`, `$2`, `$3` | Tab stops | `$1 + $2 = $3` |
| `${1:default}` | Tab stop with default text | `Hello ${1:World}` |
| `` `(elisp)` `` | Evaluated expression | `` `(format-time-string "%Y-%m-%d")` `` |

**Example snippet content:**
```
* Thought Process
$0
* My Answer
#+BEGIN_SRC python
${1:# code here}
#+END_SRC

Today: `(format-time-string "%Y-%m-%d")`
```

### Mode Selection

- **Existing modes**: Select from dropdown (uses history, last-used mode appears first)
- **New mode**: Select `+ Create new mode` and enter name (e.g., `python-mode`)
  - Script suggests appending `-mode` suffix if missing
- **Duplicate detection**: If snippet name already exists in mode, save is aborted with error

### File Location

Snippets saved to: `~/org/snippets/MODE/filename`
- Filename auto-generated from name: `"Leetcode Notes"` → `leetcode_notes`
- Mode directory created automatically if it doesn't exist
- Snippets automatically loaded after save (via `yas-reload-all`)

### Raycast Configuration

**Method 1: Simple Command (Quickest)**
1. Open Raycast settings
2. Add new Script Command
3. Configure:
   - **Name**: Emacs Snippet Capture
   - **Keyword**: `sc`
   - **Command**: `powershell -ExecutionPolicy Bypass -File "C:\Users\uie41442\dotconfig\scripts\emacs\emacs-snippet-capture.ps1"`
   - **Icon**: ✏️ or 📝
   - **Description**: Create new Emacs yasnippet

### How It Works

```
User types 'sc' in Raycast
    ↓
Raycast runs PowerShell script
    ↓
Script verifies:
  - emacsclient exists
  - Emacs server is running
    ↓
Script calls: emacsclient -f "server-file" -c -e "(my/global-snippet-capture)"
    ↓
Emacs opens popup frame (120x50, centered on primary monitor)
    ↓
Widget-based form with fields: Mode, Name, Key, Content
    ↓
User fills form and presses C-c C-c
    ↓
Emacs:
  - Validates: mode and name required, checks for duplicates
  - Generates filename from name
  - Creates mode directory if needed
  - Writes snippet file in yasnippet format
  - Reloads all snippets (yas-reload-all)
    ↓
Frame closes automatically
    ↓
Success message: "Snippet 'name' saved to ~/org/snippets/mode/filename"
    ↓
Snippet immediately available in Emacs
```

### Troubleshooting

**Error: "Emacs server is not running!"**
- **Cause**: Emacs not started or server didn't start
- **Solution**: 
  ```powershell
  emacs --init-directory "C:\Users\uie41442\dotconfig\basic\editor\emacs\modular"
  ```
- Verify server started: Check *Messages* buffer for "Emacs server started for snippet engine"

**Error: "emacsclient not found"**
- **Cause**: Emacs not installed or not in PATH
- **Solution**:
  ```powershell
  scoop install emacs
  ```

**Error: "Mode is required" or "Name is required"**
- **Cause**: Required fields left empty
- **Solution**: Fill in Mode and Name fields before pressing C-c C-c

**Error: "Snippet 'foo' already exists in mode"**
- **Cause**: Duplicate snippet name in same mode
- **Solution**: 
  - Choose a different name
  - Delete existing snippet first: `~/org/snippets/MODE/filename`
  - Snippets with same name in different modes are OK

**Raycast doesn't find 'sc' command**
- **Cause**: Script command not configured
- **Solution**: Re-check Raycast settings, verify script path, restart Raycast

**Frame doesn't appear**
- **Cause**: Emacs config issue or server not running
- **Solution**: Test snippet capture in Emacs: `SPC s n`
- Check *Messages* buffer for errors
- Verify server running: `Test-Path "~\.config\emacs\modular\var\server\auth\server"`

### Workflow Tips

**Creating Multiple Snippets:**
- Mode selection uses history - last-used mode appears first
- Quick workflow: Create all snippets for one mode, then switch to another

**Naming Conventions:**
- Use descriptive names: `"Python Class Template"` not `"class"`
- Names can have spaces and special chars (auto-converted to safe filenames)
- Keys should be short triggers: `!py`, `<date`, `cls`

**Content Best Practices:**
- Use `$0` for final cursor position
- Use `${1:default}` for placeholder hints
- Use backtick expressions for dynamic content (dates, times, calculations)
- Test expressions in Emacs before adding to snippets

### Files

- **Script**: `~/dotconfig/scripts/emacs/emacs-snippet-capture.ps1`
- **Emacs Config**: `~/dotconfig/basic/editor/emacs/modular/modules/snippets.el`
- **Snippets**: `~/org/snippets/MODE-NAME/filename`
- **Server Auth**: `~/.config/emacs/modular/var/server/auth/server`

### Integration with Snippet Search

After creating a snippet with `sc`, you can immediately:
1. Search for it: `Alt+Space` → `ss` → type snippet name
2. Insert it in Emacs: `SPC s i` → select snippet
3. Use trigger key: Type key and press TAB (e.g., `!lc` + TAB)
4. Complete in code: `SPC s y` → fuzzy completion

### Future Enhancements

- Snippet editing: Open snippet file from search
- Snippet deletion: Delete from search interface
- Template snippets: Pre-fill content from templates
- Snippet preview before save
- Export/import snippet collections

---

## emacs-bookmark-search.ps1

PowerShell script that launches Emacs bookmark search frame via emacsclient.

### Purpose
Enable system-wide access to Emacs URL bookmarks stored in `~/org/bookmarks.org`. Search with fuzzy matching and open bookmarks in default browser.

### Requirements
- Emacs with modular config running (server auto-starts)
- emacsclient.exe in PATH (installed via Scoop)
- Bookmark file: `~/org/bookmarks.org` (with HTTP/HTTPS URLs)
- Raycast for Windows installed

### Usage

**Via Raycast (Recommended):**
1. Open Raycast: `Alt+Space`
2. Type: `bs` (bookmark search)
3. Press `Enter`
4. Emacs bookmark search frame appears (100x30, centered)
5. Fuzzy search for bookmarks (type to filter)
6. Press `Enter` to select bookmark
7. Bookmark opens in default browser
8. Frame closes automatically
9. Focus returns to previous application

**Via Emacs:**
- Keybinding: `SPC b u`
- Opens same popup frame (no Raycast needed)

**Direct execution (Testing):**
```powershell
powershell -File "C:\Users\uie41442\dotconfig\scripts\emacs\emacs-bookmark-search.ps1"
```

### Bookmark Format

Bookmarks stored in `~/org/bookmarks.org` in org-mode format:

```org
#+TITLE: Linkmarks Bookmarks

* Development

** GitHub
[[https://github.com]]

** Stack Overflow
[[https://stackoverflow.com][My Favorite Q&A Site]]

* Documentation

** Emacs Manual :emacs:
[[https://www.gnu.org/software/emacs/manual/]]
```

**Description extraction rules:**
1. Explicit description: `[[URL][Description]]` → Uses "Description"
2. Nearest heading: `** GitHub` → Uses "GitHub"
3. Tags preserved: `** Emacs Manual :emacs:` → Uses "Emacs Manual :emacs:"
4. Fallback: Uses URL if no heading or description

**Supported URL types:**
- HTTP/HTTPS URLs only (opens in browser)
- Ignores `file:` links and `elisp:` links

### Raycast Configuration

1. Open Raycast settings
2. Add new Script Command
3. Configure:
   - **Name**: Bookmark Search
   - **Keyword**: `bs`
   - **Command**: `powershell -ExecutionPolicy Bypass -File "C:\Users\uie41442\dotconfig\scripts\emacs\emacs-bookmark-search.ps1"`
   - **Icon**: 🔖 or 🌐
   - **Description**: Search and open Emacs URL bookmarks

### How It Works

```
User types 'bs' in Raycast
    ↓
Raycast runs PowerShell script
    ↓
Script verifies:
  - emacsclient exists
  - Emacs server is running
    ↓
Script calls: emacsclient -e "(my/search-bookmarks-in-frame)"
    ↓
Emacs creates popup frame (100x30, centered on primary monitor)
    ↓
Parses ~/org/bookmarks.org:
  - Extracts HTTP/HTTPS links
  - Extracts descriptions (explicit, heading, or URL)
  - Preserves org tags
    ↓
Consult/Vertico fuzzy search interface
    ↓
User searches and selects bookmark
    ↓
Emacs calls w32-shell-execute to open URL in default browser
    ↓
Frame closes automatically
    ↓
Focus returns to previous application
```

### Troubleshooting

**Error: "Emacs server not running"**
- **Cause**: Emacs not started or server didn't start
- **Solution**: 
  ```powershell
  emacs --init-directory "C:\Users\uie41442\dotconfig\basic\editor\emacs\modular"
  ```
- Verify server: `Test-Path "~\.config\emacs\modular\var\server\auth\server"`

**Error: "No HTTP(S) bookmarks found"**
- **Cause**: `~/org/bookmarks.org` doesn't exist or has no HTTP/HTTPS links
- **Solution**: Create bookmarks file or add HTTP/HTTPS URLs
- Example: `** GitHub`\n`[[https://github.com]]`

**Error: "Bookmark file not found"**
- **Cause**: `~/org/bookmarks.org` doesn't exist
- **Solution**: Create file with org-mode format (see Bookmark Format above)
- Can start with empty file: `echo "#+TITLE: Bookmarks" > ~/org/bookmarks.org`

**Frame doesn't appear**
- **Cause**: Emacs not running or script path wrong
- **Solution**: Test in Emacs: `SPC b u`
- Verify script path in Raycast settings

**Browser doesn't open**
- **Cause**: `w32-shell-execute` requires Windows
- **Solution**: Should work automatically on Windows. Check if bookmark URL is valid HTTP/HTTPS.

### Files

- **Script**: `~/dotconfig/scripts/emacs/emacs-bookmark-search.ps1`
- **Emacs Config**: `~/dotconfig/basic/editor/emacs/modular/modules/bookmarks.el`
- **Bookmarks**: `~/org/bookmarks.org`
- **Server Auth**: `~/.config/emacs/modular/var/server/auth/server`
- **History**: Saved in `~/.config/emacs/modular/var/savehist.el` (`my/bookmark-history`)

### Workflow Tips

**Organizing Bookmarks:**
- Use org headings as categories: `* Development`, `* Documentation`, `* Personal`
- Add tags for filtering: `** Emacs Manual :emacs:docs:`
- Keep frequently-used bookmarks near top of file
- Descriptions preserved in search results

**Quick Access:**
- Type partial bookmark name for fuzzy match
- Type domain for all bookmarks from that site: "github"
- Type category heading: "development" shows all Development bookmarks
- Search history available with up/down arrows

---

## emacs-bookmark-capture.ps1

PowerShell script that launches Emacs bookmark capture frame via emacsclient.

### Purpose
Quickly capture URL bookmarks from anywhere in Windows with automatic clipboard detection, duplicate checking, and smart description suggestions.

### Requirements
- Emacs with modular config running (server auto-starts)
- emacsclient.exe in PATH (installed via Scoop)
- Bookmark file: `~/org/bookmarks.org` (created automatically if missing)
- Raycast for Windows installed

### Usage

**Via Raycast (Recommended):**
1. Copy URL to clipboard (optional but recommended)
2. Open Raycast: `Alt+Space`
3. Type: `bc` (bookmark capture)
4. Press `Enter`
5. Emacs bookmark capture frame appears (80x20, centered)
6. Workflow:
   - **If URL in clipboard**: Detected automatically, prompts for description only
   - **If no URL in clipboard**: Prompts for URL first, then description
7. Description prompt shows domain as default suggestion
8. Enter description and press `Enter`
9. Bookmark saved to `~/org/bookmarks.org` under `* Inbox` heading
10. Frame closes automatically
11. Success message: "✓ Captured bookmark: Description → URL"

**Via Emacs:**
- Keybinding: `SPC b c`
- Opens prompts in current Emacs window (no frame)

**Direct execution (Testing):**
```powershell
# Without URL in clipboard
powershell -File "C:\Users\uie41442\dotconfig\scripts\emacs\emacs-bookmark-capture.ps1"

# With URL in clipboard (recommended)
Set-Clipboard "https://github.com/user/repo"
powershell -File "C:\Users\uie41442\dotconfig\scripts\emacs\emacs-bookmark-capture.ps1"
```

### Features

**1. Clipboard Detection**
- Automatically detects HTTP/HTTPS URLs from clipboard
- Validates URL format (must start with `http://` or `https://`)
- If clipboard has URL: Only prompts for description
- If clipboard empty/invalid: Prompts for both URL and description

**2. Duplicate Checking**
- Case-sensitive URL comparison
- Searches entire bookmarks.org for existing URL
- If duplicate found: Shows error with line number and heading
- Example: "URL already exists at line 15 under heading: GitHub"
- Aborts capture (doesn't overwrite existing bookmarks)

**3. Smart Description Suggestions**
- Extracts domain from URL as default suggestion
- Example: `https://github.com/user/repo` → Suggests "github.com"
- User can accept default or type custom description
- Empty description not allowed (validation error)

**4. Automatic File Management**
- Creates `~/org/bookmarks.org` if doesn't exist
- Creates `* Inbox` heading if missing
- Appends bookmark to end of Inbox section
- Format: `** DESCRIPTION`\n`[[URL]]`

### Bookmark Storage

**File location:** `~/org/bookmarks.org`

**Structure:**
```org
#+TITLE: Linkmarks Bookmarks
#+DESCRIPTION: Emacs Linkmarks - Org-mode based bookmarks

* Inbox

** github.com
[[https://github.com]]

** My Favorite Blog
[[https://example.com/blog]]

* Development
(other bookmarks organized manually)
```

**Inbox workflow:**
1. All captured bookmarks start in `* Inbox`
2. Organize manually later: Move to appropriate categories
3. Edit descriptions, add tags, etc.
4. Delete duplicates or outdated bookmarks

### Raycast Configuration

1. Open Raycast settings
2. Add new Script Command
3. Configure:
   - **Name**: Bookmark Capture
   - **Keyword**: `bc`
   - **Command**: `powershell -ExecutionPolicy Bypass -File "C:\Users\uie41442\dotconfig\scripts\emacs\emacs-bookmark-capture.ps1"`
   - **Icon**: ➕ or 📌
   - **Description**: Capture URL to Emacs bookmarks

### How It Works

```
User copies URL and types 'bc' in Raycast
    ↓
Raycast runs PowerShell script
    ↓
Script verifies:
  - emacsclient exists
  - Emacs server is running
    ↓
Script calls: emacsclient -c -e "(my/global-bookmark-capture)"
    ↓
Emacs creates popup frame (80x20, centered on primary monitor)
    ↓
Emacs runs bookmark capture function:
  1. Check clipboard for URL (PowerShell via Emacs)
  2. If URL found: Skip URL prompt
  3. Validate URL format (must be HTTP/HTTPS)
  4. Search bookmarks.org for duplicate URL (case-sensitive)
  5. If duplicate: Show error and abort
  6. Extract domain from URL (e.g., "github.com")
  7. Prompt for description (with domain as default)
  8. Validate description not empty
  9. Append to Inbox heading: ** DESCRIPTION\n[[URL]]
  10. Save bookmarks.org
    ↓
Frame closes automatically
    ↓
Success message shows in minibuffer: "✓ Captured bookmark: ..."
    ↓
Focus returns to previous application
```

### Troubleshooting

**Error: "Emacs server not running"**
- **Cause**: Emacs not started
- **Solution**: Start Emacs (server auto-starts with modular config)

**Error: "Invalid URL format. Must start with http:// or https://"**
- **Cause**: Clipboard/input doesn't contain valid HTTP/HTTPS URL
- **Solution**: Copy valid URL to clipboard before running, or type full URL including protocol

**Error: "URL already exists at line X under heading: Y"**
- **Cause**: Bookmark with same URL already exists (duplicate detection)
- **Solution**: 
  - Open bookmarks.org and check existing entry
  - Edit existing bookmark instead of creating duplicate
  - If intentional duplicate needed, edit bookmarks.org manually

**Error: "Description cannot be empty"**
- **Cause**: Pressed Enter without typing description
- **Solution**: Type description or accept domain suggestion (press Enter)

**Frame doesn't appear**
- **Cause**: Emacs not running or script path wrong
- **Solution**: 
  - Verify Emacs running: `Get-Process emacs`
  - Test in Emacs: `SPC b c`
  - Check script path in Raycast settings

**Clipboard detection doesn't work**
- **Cause**: Clipboard empty or contains non-URL content
- **Solution**: Normal - will prompt for URL instead
- Tip: Copy URL before running `bc` for faster workflow

### Workflow Tips

**Fast Capture:**
1. Browse to interesting page
2. Copy URL: `Ctrl+L`, `Ctrl+C` (in browser)
3. `Alt+Space` → `bc`
4. Type description (or accept domain suggestion)
5. Done! Bookmark saved to Inbox

**Organizing Later:**
- Open bookmarks.org: `SPC f f ~/org/bookmarks.org`
- Move bookmarks from Inbox to appropriate categories
- Add tags: `** Description :tag1:tag2:`
- Edit descriptions for clarity
- Delete outdated bookmarks

**Best Practices:**
- Use descriptive names: "GitHub Actions Docs" not "github.com"
- Add context if URL is specific page: "Emacs Evil Mode Guide"
- Domain suggestions are good for homepages: "github.com", "reddit.com"
- Use tags for filtering later: `:work:`, `:personal:`, `:reference:`

### Files

- **Script**: `~/dotconfig/scripts/emacs/emacs-bookmark-capture.ps1`
- **Emacs Config**: `~/dotconfig/basic/editor/emacs/modular/modules/bookmarks.el`
- **Bookmarks**: `~/org/bookmarks.org` (created automatically)
- **Server Auth**: `~/.config/emacs/modular/var/server/auth/server`

### Integration with Bookmark Search

After capturing bookmarks with `bc`, you can immediately:
1. Search for them: `Alt+Space` → `bs` → type description
2. Open in browser from search results
3. Edit in org-mode: `SPC f f ~/org/bookmarks.org`
4. Organize into categories manually

**Workflow example:**
```
1. Browse web, find 5 interesting links
2. For each link:
   - Copy URL (Ctrl+L, Ctrl+C)
   - Alt+Space → bc
   - Enter description
3. Later: Open bookmarks.org and organize
   - Move from Inbox to Development/Documentation/etc.
   - Add tags, edit descriptions
4. Use: Alt+Space → bs → fuzzy search → open in browser
```

### Future Enhancements

- Category selection: Choose heading during capture (not always Inbox)
- Tag support: Add tags during capture
- Edit existing: Modify bookmark descriptions from search
- Delete from search: Remove bookmarks without opening org file
- Import from browser: Bulk import browser bookmarks
- Export: Share bookmark collections

---

## Future Scripts

Potential additional Emacs integrations:

- **emacs-org-capture.ps1** (`oc`) - Quick note capture
- **emacs-org-roam-find.ps1** (`or`) - Search org-roam notes
- **emacs-find-file.ps1** (`ef`) - Open file in Emacs
- **emacs-dired.ps1** (`ed`) - Open directory in dired
- **emacs-bookmark-edit.ps1** (`be`) - Edit selected bookmark
- **emacs-bookmark-delete.ps1** (`bd`) - Delete bookmark from search

---

Last updated: 2026-01-16


