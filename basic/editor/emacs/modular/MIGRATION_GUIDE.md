# org-roam → denote Migration Guide

## Overview

This guide will help you migrate from org-roam to denote. The migration has been prepared and is ready to execute.

**Status**: ✅ Branch created, code updated, backup complete
**Branch**: `denote`
**Backup**: `~/org/notes-backup-20260131/`

## What's Changed

### Code Changes (Ready)
- ✅ `denote-config.el` created (replaces org-roam-config.el)
- ✅ `init.el` updated to load denote-config
- ✅ All dependent modules updated
- ✅ Migration script created with dry-run mode

### Directory Structure (After Migration)
```
Before (org-roam):          After (denote):
~/org/notes/                ~/org/notes/
├── daily/        →         ├── journal/
├── projects/     →         ├── (project files in root with _project keyword)
├── ref/          →         ├── ref/
└── *.org                   └── (ID--title__keywords.org format)
```

## Migration Steps

### Step 1: Verify Backup

```bash
ls -lh ~/org/notes-backup-20260131/
```

You should see your backup directory with all files.

### Step 2: Temporarily Switch Back to Org-Roam

Since the migration script needs org-roam loaded, temporarily revert init.el:

```bash
cd /Users/zhichengzheng/dotconfig/basic/editor/emacs/modular
```

Edit `init.el` line 131, temporarily change:
```elisp
# Change this line temporarily:
(require 'denote-config)           ; 14. Denote notes system

# Back to:
(require 'org-roam-config)           ; 14. Org-roam notes system
```

### Step 3: Start Emacs and Run DRY-RUN

```bash
emacs --init-directory ~/dotconfig/basic/editor/emacs/modular/
```

In Emacs:

1. Load the migration script:
   ```
   M-x load-file RET
   ~/dotconfig/basic/editor/emacs/modular/modules/migration/org-roam-to-denote.el RET
   ```

2. Load denote:
   ```
   M-x straight-use-package RET denote RET
   ```

3. Configure denote:
   ```elisp
   M-: (setq denote-directory "~/org/notes/") RET
   M-: (setq denote-known-keywords '("project" "journal" "trade" "ref" "archived")) RET
   M-: (setq denote-file-type 'org) RET
   ```

4. Run DRY-RUN:
   ```
   M-x my/migration-run RET
   ```

   Review the output. It will show:
   - What files would be migrated
   - New filenames in denote format
   - Keywords that will be added
   - No actual changes are made

### Step 4: Review Dry-Run Output

Look for:
- ✅ All files listed for migration
- ✅ Sensible denote filenames generated
- ✅ Correct keywords assigned (journal, project, etc.)
- ❌ Any errors or warnings

Example dry-run output:
```
[DRY-RUN] Would migrate:
  From: daily/2024-01-15.org
  To:   journal/20240115T000000--2024-01-15-mon__journal.org
  Keywords: journal

[DRY-RUN] Would migrate:
  From: projects/20241006-my-project.org
  To:   20241006T000000--my-project__project.org
  Keywords: project
```

### Step 5: Run LIVE Migration

If dry-run looks good:

```elisp
M-: (setq my/migration-dry-run nil) RET
M-x my/migration-run RET
```

This will:
- Create denote-formatted files
- Convert `[[id:...]]` links to `[[denote:...]]`
- Add appropriate keywords
- Preserve original content

**Time**: ~2-5 minutes for 159 files

### Step 6: Verify Migration

Check the results:

```bash
# Check journal files
ls ~/org/notes/journal/*.org | head -5

# Check project files (now in root)
ls ~/org/notes/*--*__project.org | head -5

# Check all denote files
find ~/org/notes -name "*--*.org" | wc -l
```

You should see:
- Journal files in `journal/` with `__journal` keyword
- Project files in root with `__project` keyword
- All files following denote naming: `YYYYMMDDTHHMMSS--title__keywords.org`

### Step 7: Switch to Denote Config

Once migration is verified:

1. Edit `init.el` line 131 back to:
   ```elisp
   (require 'denote-config)           ; 14. Denote notes system
   ```

2. Restart Emacs:
   ```bash
   emacs --init-directory ~/dotconfig/basic/editor/emacs/modular/
   ```

3. Verify denote loads without errors

### Step 8: Refresh Agenda Files

```
M-x my/denote-refresh-agenda-list RET
```

This adds all project files to `org-agenda-files`.

### Step 9: Test Core Workflows

Test the following:

| Workflow | Command | Expected |
|----------|---------|----------|
| Open today's journal | `SPC n d` | Opens journal note |
| Create journal note | `SPC n j` | Creates with template |
| Find note | `SPC n f` | Fuzzy search all notes |
| Insert link | `SPC n i` | Link completion |
| View backlinks | `SPC n l` | Shows backlinks buffer |
| Find project | `SPC n p` | Lists projects |
| Create project | `SPC n P` | Creates with template |
| Capture task | `SPC n t` | Adds TODO to project |
| Capture trade | `SPC n r` | Strategy template |

### Step 10: Cleanup (Optional)

After confirming everything works:

```bash
# Remove org-roam database
rm -f ~/org/notes/.org-roam.db

# Archive old org-roam-config.el
mkdir -p ~/dotconfig/basic/editor/emacs/modular/modules/archived
mv ~/dotconfig/basic/editor/emacs/modular/modules/org-roam-config.el \
   ~/dotconfig/basic/editor/emacs/modular/modules/archived/

# Remove old daily/ directory (after confirming journal/ has everything)
# CAREFUL: Only do this after verifying all dailies migrated successfully
ls ~/org/notes/daily/*.org | wc -l  # Should match journal/ count
ls ~/org/notes/journal/*.org | wc -l
# If counts match:
rm -rf ~/org/notes/daily/

# Remove old projects/ directory (files now in root)
ls ~/org/notes/projects/*.org | wc -l  # Note the count
ls ~/org/notes/*__project.org | wc -l  # Should match
# If counts match:
rm -rf ~/org/notes/projects/
```

## Rollback Plan

If migration fails or you want to revert:

```bash
# 1. Stop Emacs

# 2. Restore backup
rm -rf ~/org/notes
mv ~/org/notes-backup-20260131 ~/org/notes

# 3. Revert init.el to org-roam
cd ~/dotconfig/basic/editor/emacs/modular
# Change init.el line 131 back to:
# (require 'org-roam-config)

# 4. Checkout main branch
cd ~/dotconfig
git checkout main
git branch -D denote

# 5. Restart Emacs
```

## Troubleshooting

### "org-roam-node-from-id: No such node"
- Org-roam database may be out of sync
- Run: `M-x org-roam-db-sync RET`
- Try migration again

### "denote-directory is not set"
- Make sure you configured denote in Step 3
- Verify: `M-: denote-directory RET`

### Links not converting
- Some `[[roam:...]]` links may need manual fixing
- After migration, search: `M-x consult-ripgrep RET [[roam: RET`
- Replace with denote-style links

### Missing files after migration
- Check the migration statistics for errors
- Look in original subdirectories
- Check backup: `~/org/notes-backup-20260131/`

## Post-Migration Notes

### What's Different

| org-roam | denote |
|----------|--------|
| Database-backed | File-based (no DB) |
| `[[id:...]]` links | `[[denote:...]]` links |
| Tags in database | Keywords in filename |
| `daily/` subdirectory | `journal/` subdirectory |
| `projects/` subdirectory | Root with `__project` keyword |
| org-roam-ui graph | No equivalent (simpler) |
| org-roam-db-sync | Not needed |

### New Workflows

- **Backlinks**: `SPC n l` still works (denote-backlinks)
- **Search**: `SPC n g` for grep, `SPC n e` for find
- **Rename**: `SPC n r` to rename with new keywords/title
- **Keywords**: `SPC n a` to add, `SPC n D` to remove

### Benefits

- ✅ No database to maintain
- ✅ Faster searches (grep-based)
- ✅ Simpler architecture
- ✅ More portable (just files)
- ✅ Git-friendly (no binary DB)

## Support

If you encounter issues:

1. Check `*Messages*` buffer in Emacs
2. Review migration statistics
3. Verify backup exists
4. Use rollback plan if needed

Your backup is safe at: `~/org/notes-backup-20260131/`
