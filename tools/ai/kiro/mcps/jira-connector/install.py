#!/usr/bin/env python3
"""
Jira MCP Server - GUI Installer for Kiro

Double-click this file or run: python install.py

This installer will:
  1. Let you choose where to install the server (or keep current location)
  2. Create a virtual environment and install dependencies
  3. Ask for your Jira base URL and Personal Access Token
  4. Configure Kiro's user-level MCP settings (~/.kiro/settings/mcp.json)
"""
import json
import os
import platform
import shutil
import subprocess
import sys
import threading
import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from pathlib import Path


# --- Constants ---
MIN_PYTHON = (3, 10)
KIRO_MCP_CONFIG_PATH = Path.home() / ".kiro" / "settings" / "mcp.json"
SOURCE_DIR = Path(__file__).parent.resolve()
DEFAULT_INSTALL_DIR = Path.home()
INSTALL_SUBDIR = "jira-mcp-server"
SERVER_FILES = ["src", "requirements.txt", "pyproject.toml", "README.md"]


class InstallerApp:
    def __init__(self, root: tk.Tk):
        self.root = root
        self.root.title("Jira MCP Server - Installer")
        self.root.resizable(False, False)

        # Try to set a reasonable window size and center it
        win_width, win_height = 560, 580
        screen_w = self.root.winfo_screenwidth()
        screen_h = self.root.winfo_screenheight()
        x = (screen_w - win_width) // 2
        y = (screen_h - win_height) // 2
        self.root.geometry(f"{win_width}x{win_height}+{x}+{y}")

        self._build_ui()
        self._use_current = False

    def _build_ui(self):
        # Main frame with padding
        main = ttk.Frame(self.root, padding=20)
        main.pack(fill="both", expand=True)

        # --- Title ---
        title = ttk.Label(main, text="Jira MCP Server Installer", font=("Segoe UI", 14, "bold"))
        title.pack(anchor="w")
        subtitle = ttk.Label(main, text="Set up the Jira MCP server for local use with Kiro.", font=("Segoe UI", 9))
        subtitle.pack(anchor="w", pady=(0, 15))

        # --- Install Location ---
        loc_frame = ttk.LabelFrame(main, text="Install Location", padding=10)
        loc_frame.pack(fill="x", pady=(0, 10))

        self.install_dir_var = tk.StringVar(value=str(DEFAULT_INSTALL_DIR))

        ttk.Label(loc_frame, text=f"Server files will be installed into a '{INSTALL_SUBDIR}' subfolder at this location.",
                  font=("Segoe UI", 8), foreground="gray").pack(anchor="w", pady=(0, 4))

        dir_row = ttk.Frame(loc_frame)
        dir_row.pack(fill="x")
        self.dir_entry = ttk.Entry(dir_row, textvariable=self.install_dir_var, width=50)
        self.dir_entry.pack(side="left", fill="x", expand=True, padx=(0, 5))
        browse_btn = ttk.Button(dir_row, text="Browse...", command=self._browse_dir)
        browse_btn.pack(side="right")

        use_current_btn = ttk.Button(loc_frame, text="Use current directory (where this script is)",
                                     command=self._use_current_dir)
        use_current_btn.pack(anchor="w", pady=(5, 0))

        # --- Jira Configuration ---
        jira_frame = ttk.LabelFrame(main, text="Jira Configuration", padding=10)
        jira_frame.pack(fill="x", pady=(0, 10))

        ttk.Label(jira_frame, text="Jira Base URL:").pack(anchor="w")
        self.url_var = tk.StringVar(value="https://ix.jira.automotive.cloud")
        url_entry = ttk.Entry(jira_frame, textvariable=self.url_var, width=60)
        url_entry.pack(fill="x", pady=(0, 8))

        pat_label_row = ttk.Frame(jira_frame)
        pat_label_row.pack(fill="x")
        ttk.Label(pat_label_row, text="Personal Access Token (PAT):").pack(side="left")
        create_pat_btn = ttk.Button(pat_label_row, text="Create PAT in Jira...", command=self._open_pat_page)
        create_pat_btn.pack(side="right")

        self.pat_var = tk.StringVar()
        pat_entry = ttk.Entry(jira_frame, textvariable=self.pat_var, show="*", width=60)
        pat_entry.pack(fill="x", pady=(0, 2))
        ttk.Label(jira_frame, text="Opens your Jira profile page where you can create a token",
                  font=("Segoe UI", 8), foreground="gray").pack(anchor="w")

        # --- Install Button (pack before log so it stays visible) ---
        btn_frame = ttk.Frame(main)
        btn_frame.pack(fill="x", side="bottom")
        self.install_btn = ttk.Button(btn_frame, text="Install", command=self._start_install)
        self.install_btn.pack(side="right")

        # --- Progress / Log ---
        self.log_text = tk.Text(main, height=6, state="disabled", font=("Consolas", 9), wrap="word",
                                background="#f5f5f5", relief="sunken", borderwidth=1)
        self.log_text.pack(fill="both", expand=True, pady=(5, 10), side="bottom")

    def _browse_dir(self):
        chosen = filedialog.askdirectory(title="Choose install location", initialdir=str(Path.home()))
        if chosen:
            self.install_dir_var.set(chosen)
            self._use_current = False

    def _use_current_dir(self):
        self.install_dir_var.set(str(SOURCE_DIR))
        self._use_current = True

    def _open_pat_page(self):
        base_url = self.url_var.get().strip().rstrip("/")
        if not base_url:
            messagebox.showwarning("Jira URL Required", "Please enter your Jira base URL first.")
            return
        import webbrowser
        pat_url = f"{base_url}/secure/ViewProfile.jspa?selectedTab=com.atlassian.pats.pats-plugin:jira-user-personal-access-tokens"
        webbrowser.open(pat_url)

    def _log(self, msg: str):
        self.log_text.config(state="normal")
        self.log_text.insert("end", msg + "\n")
        self.log_text.see("end")
        self.log_text.config(state="disabled")
        self.root.update_idletasks()

    def _start_install(self):
        # Validate inputs
        base_url = self.url_var.get().strip().rstrip("/")
        pat = self.pat_var.get().strip()
        install_dir = Path(self.install_dir_var.get().strip())

        # When not using current directory, create a subdirectory
        if not self._use_current:
            install_dir = install_dir / INSTALL_SUBDIR

        if not base_url:
            messagebox.showerror("Missing Field", "Please enter your Jira base URL.")
            return
        if not base_url.startswith("http"):
            messagebox.showerror("Invalid URL", "Jira URL should start with https://")
            return
        if not pat:
            messagebox.showerror("Missing Field", "Please enter your Personal Access Token.")
            return

        # Disable button during install
        self.install_btn.config(state="disabled")

        # Run install in a thread so the GUI stays responsive
        thread = threading.Thread(target=self._run_install, args=(install_dir, base_url, pat), daemon=True)
        thread.start()

    def _run_install(self, install_dir: Path, base_url: str, pat: str):
        try:
            self._do_install(install_dir, base_url, pat)
        except Exception as e:
            self.root.after(0, lambda: self._log(f"ERROR: {e}"))
            self.root.after(0, lambda: messagebox.showerror("Installation Failed", str(e)))
            self.root.after(0, lambda: self.install_btn.config(state="normal"))

    def _do_install(self, install_dir: Path, base_url: str, pat: str):
        # Step 1: Check Python version
        self.root.after(0, lambda: self._log("Checking Python version..."))
        v = sys.version_info
        if (v.major, v.minor) < MIN_PYTHON:
            raise RuntimeError(f"Python {MIN_PYTHON[0]}.{MIN_PYTHON[1]}+ required, found {v.major}.{v.minor}")
        self.root.after(0, lambda: self._log(f"  Python {v.major}.{v.minor}.{v.micro} — OK"))

        # Step 2: Copy files if install dir differs from source
        if install_dir.resolve() != SOURCE_DIR:
            self.root.after(0, lambda: self._log(f"Copying server files to {install_dir}..."))
            install_dir.mkdir(parents=True, exist_ok=True)
            for item_name in SERVER_FILES:
                src_path = SOURCE_DIR / item_name
                dst_path = install_dir / item_name
                if src_path.is_dir():
                    if dst_path.exists():
                        shutil.rmtree(dst_path)
                    shutil.copytree(src_path, dst_path)
                elif src_path.is_file():
                    shutil.copy2(src_path, dst_path)
            self.root.after(0, lambda: self._log("  Files copied."))
        else:
            self.root.after(0, lambda: self._log("Installing in current directory."))

        # Step 3: Create venv and install dependencies
        venv_dir = install_dir / "venv"
        if platform.system() == "Windows":
            venv_python = venv_dir / "Scripts" / "python.exe"
        else:
            venv_python = venv_dir / "bin" / "python"

        if not venv_python.exists():
            self.root.after(0, lambda: self._log("Creating virtual environment..."))
            subprocess.check_call(
                [sys.executable, "-m", "venv", str(venv_dir)],
                stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
            )
            self.root.after(0, lambda: self._log("  Virtual environment created."))
        else:
            self.root.after(0, lambda: self._log("Virtual environment already exists."))

        self.root.after(0, lambda: self._log("Installing dependencies (this may take a moment)..."))
        req_file = install_dir / "requirements.txt"
        subprocess.check_call(
            [str(venv_python), "-m", "pip", "install", "--quiet", "-r", str(req_file)],
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
        )
        self.root.after(0, lambda: self._log("  Dependencies installed."))

        # Step 4: Configure Kiro MCP
        server_script = install_dir / "src" / "server.py"
        self.root.after(0, lambda: self._log("Configuring Kiro MCP settings..."))

        new_entry = {
            "command": str(venv_python),
            "args": [str(server_script)],
            "env": {
                "JIRA_BASE_URL": base_url,
                "JIRA_PAT": pat
            },
            "disabled": False,
            "autoApprove": ["list_jira_configs", "get_active_jira"]
        }

        existing_config = {}
        if KIRO_MCP_CONFIG_PATH.exists():
            try:
                with open(KIRO_MCP_CONFIG_PATH, "r", encoding="utf-8") as f:
                    existing_config = json.load(f)
            except (json.JSONDecodeError, IOError):
                existing_config = {}

        if "mcpServers" not in existing_config:
            existing_config["mcpServers"] = {}

        existing_config["mcpServers"]["jira-connector"] = new_entry

        KIRO_MCP_CONFIG_PATH.parent.mkdir(parents=True, exist_ok=True)
        with open(KIRO_MCP_CONFIG_PATH, "w", encoding="utf-8") as f:
            json.dump(existing_config, f, indent=2)

        self.root.after(0, lambda: self._log(f"  Config written to {KIRO_MCP_CONFIG_PATH}"))

        # Done
        self.root.after(0, lambda: self._log("\nInstallation complete!"))
        self.root.after(0, lambda: messagebox.showinfo(
            "Installation Complete",
            "Jira MCP Server installed successfully!\n\n"
            "Restart Kiro to activate the jira-connector MCP server.\n\n"
            f"Config: {KIRO_MCP_CONFIG_PATH}\n"
            f"Server: {install_dir}"
        ))
        self.root.after(0, lambda: self.install_btn.config(state="normal"))


def check_python_early():
    """Fail fast with a message box if Python is too old for tkinter to even work properly."""
    v = sys.version_info
    if (v.major, v.minor) < MIN_PYTHON:
        try:
            root = tk.Tk()
            root.withdraw()
            messagebox.showerror(
                "Python Version Error",
                f"Python {MIN_PYTHON[0]}.{MIN_PYTHON[1]}+ is required.\n"
                f"You have Python {v.major}.{v.minor}.{v.micro}.\n\n"
                "Please install a newer version of Python."
            )
            root.destroy()
        except Exception:
            print(f"ERROR: Python {MIN_PYTHON[0]}.{MIN_PYTHON[1]}+ required, found {v.major}.{v.minor}")
        sys.exit(1)


def main():
    check_python_early()
    root = tk.Tk()
    InstallerApp(root)
    root.mainloop()


if __name__ == "__main__":
    main()
