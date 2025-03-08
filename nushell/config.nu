# config.nu
#
# Installed by:
# version = "0.102.0"
#
# This file is used to override default Nushell settings, define
# (or import) custom commands, or run any other startup tasks.
# See https://www.nushell.sh/book/configuration.html
#
# This file is loaded after env.nu and before login.nu
#
# You can open this file in your default editor using:
# config nu
#
# See `help config nu` for more options
#
# You can remove these comments if you want or leave
# them for future reference.

$env.config.keybindings = [
  {
    name: reload_config
    modifier: control
    keycode: char_l
    mode: [emacs vi_normal vi_insert]
    event: {
      send: executehostcommand
      cmd: $"source '($nu.env-path)'; source '($nu.config-path)'"
    }
  }
]
if $nu.os-info.name == "windows" {
  export-env {
    $env.KOMOREBI_CONFIG_HOME = $"($env.USERPROFILE)\\.config\\komorebi"
  }
} else {
}

open ~/.netrc | lines | each { |line|
    if $line =~ '^\s*(\S+)\s*=\s*(\S+)\s*$' {
        let key = $line | get 0 | split column '=' | str trim
        let value = $line | get 1 | split column '=' | str trim
        load-env {$key: $value}
    }
}

$env.config.edit_mode = "vi"
$env.config.shell_integration.osc133 = false
$env.config.buffer_editor = "nvim"
$env.config.history = {
  file_format: sqlite
  max_size: 1_000_000
  sync_on_enter: true
  isolation: true
}

alias cl = clear
alias ll = ls -l
alias lg = lazygit

def rk [] {
   komorebic stop --ahk
   komorebic start --ahk
}

def --env y [...args] {
	let tmp = (mktemp -t "yazi-cwd.XXXXXX")
	yazi ...$args --cwd-file $tmp
	let cwd = (open $tmp)
	if $cwd != "" and $cwd != $env.PWD {
		cd $cwd
	}
	rm -fp $tmp
}

