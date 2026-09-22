# Mobile access to remote desktop / terminal / dev sessions

> Evidence checked: **2026-09-20**.
> Note on the prompt: the request said "remote herdr sessions," which is not a known product name — treated here as **remote desktop / terminal / development sessions** accessed from an iOS or Android phone. If "herdr" referred to something specific (a tool, an internal name), flag it and this doc can be retargeted.

## Bottom line

- **Quick SSH/tmux access to a dev box (most common case for a developer):** use **Tailscale** to reach the box privately, then **Termius** (iOS/Android, free tier covers SSH/Mosh/SFTP) or **Blink Shell/Prompt 3** (iOS-only, one-time or subscription) as the terminal, with **tmux** on the server for session persistence and **Mosh** for roaming across cellular/Wi-Fi handoffs. This combination needs no port-forwarding and no relay trust beyond Tailscale/WireGuard itself.
- **Best phone UI specifically for Herdr / terminal AI-agent sessions:** **Collie** is more targeted than generic SSH: it is a mobile PWA for Herdr/tmux/zellij panes over Tailscale, with agent-waiting dashboard, prompt buttons, special keys, file attachments, and push notifications. Use it only on a private tailnet; it intentionally exposes terminal read/write access.
- **Full GUI remote desktop to a Linux/Windows workstation, self-hosted, free:** **RustDesk** (self-hosted `rustdesk-server`, open source, E2E-encrypted, iOS/Android apps) or **Apache Guacamole** (browser-only, no app to install, good for locked-down/BYOD phones) fronted by Tailscale/WireGuard for defense in depth.
- **Zero-config mesh networking as the transport layer for everything else:** **Tailscale** (WireGuard-based, free tier for personal use, official mobile apps for iOS/Android, Tailscale SSH built in).
- **Game-streaming-style low-latency GUI (heavy interactive apps, video/3D work) — self-hosted:** **Moonlight (client, iOS/Android) + Sunshine (self-hosted host)** — free, open source, GPU hardware encode. **Parsec** is the commercial alternative but currently **has no iOS client**, Android-only on mobile.
- **Turnkey commercial remote desktop / ad-hoc support-style access:** **AnyDesk** or **TeamViewer** are easy options, but expect free/personal tiers to have restrictions and paid upgrades for regular unattended access. **NoMachine** is also worth evaluating, but verify current server licensing before relying on it long-term.
- **Chrome Remote Desktop** is a fallback only, not a first choice: Google documents both iPhone/iPad and Android use, but it is Google-account/relay dependent and not self-hostable.

---

## Comparison table

| Solution | Platform support | Mobile apps (iOS/Android) | Security / auth model | Self-hosting | Pricing / licensing | Notable limitations |
|---|---|---|---|---|---|---|
| **RustDesk** | Win/macOS/Linux/iOS/Android/Web | Native apps on both [App Store](https://apps.apple.com/us/app/rustdesk-remote-desktop/id1581225015) and Play Store | P2P, end-to-end encryption based on NaCl; direct or relay connection; Pro server adds 2FA/LDAP/SSO [official docs](https://rustdesk.com/docs/en/) | Yes — free open-source `rustdesk-server`, or paid Professional Server (SSO, web console, admin controls) [docs](https://rustdesk.com/docs/en/) | Client + OSS server: free/open source (AGPL-3.0 core). Pro server is a paid add-on. | iOS app is **controller-only** (cannot be controlled/screen-shared from the iPhone itself) [RustDesk blog](https://rustdesk.com/blog/rustdesk-remote-control-android-ios/); brand is a known target for tech-support-scam impersonation — verify official install sources. |
| **Tailscale** (+ RDP/VNC/SSH/Mosh over it) | Win/macOS/Linux/iOS/Android + routers | Native apps, official [Tailscale SSH](https://tailscale.com/docs/features/tailscale-ssh) and Taildrop file-share via Share sheet on both OSes [docs](https://tailscale.com/docs/features/taildrop) | WireGuard-based mesh; per-device auth key + identity provider SSO; Tailscale SSH manages/authorizes SSH itself | Yes — self-hosted control plane (Headscale, community) or Tailscale's own coordination server (SaaS) | Free tier for personal/small use; paid tiers for orgs (see tailscale.com/pricing) | Tailscale itself is only the *network* layer — you still need RDP/VNC/SSH/Mosh client+server on top; Taildrop was still in alpha as of Jan 2026 [docs](https://tailscale.com/docs/features/taildrop.md). |
| **Collie** | Host: Linux/macOS supported, Windows experimental; backend multiplexer: Herdr, tmux, or zellij | No native app; mobile web/PWA opened from the phone over Tailscale | Tailscale Serve identity (`COLLIE_TRUSTED_USER`) plus optional device pairing/write token; docs warn that the URL can read panes and send arbitrary keystrokes as your user | Yes — runs locally, binds loopback, default ingress is `tailscale serve`; MIT-licensed source | Free/open source; install as Herdr plugin, install script, Nix/mise/package, or from source | Highly targeted at terminal AI-agent sessions, not a general remote desktop. Herdr is primary supported target; tmux/zellij support is experimental. Single-user only; never expose via public `tailscale funnel`. [README](https://github.com/AltanS/collie), [security docs](https://github.com/AltanS/collie/blob/main/docs/security.md) |
| **Apache Guacamole** | Any device with an HTML5 browser (server: Linux) | No native app needed — works via mobile browser on iOS/Android | Browser-based gateway; pluggable auth (DB, LDAP, OpenID, etc. via extensions); TLS terminates at the web app | Yes — fully self-hosted (guacd + web app), Apache 2.0 | Free, open source (Apache License 2.0); commercial support available from third parties | Clientless is a double-edged sword: no offline reconnect logic like Mosh; deploying/hardening guacd + reverse proxy is entirely on you; official docs are light on authentication specifics — read the extension docs for the auth backend you pick. [guacamole.apache.org](https://guacamole.apache.org/) |
| **Microsoft Windows App** (successor to Remote Desktop/RDP client) | Windows, macOS, iOS/iPadOS ≥16.0, Android ≥11, web | Official apps on [App Store](https://apps.apple.com/) and [Play Store](https://play.google.com/store/apps/details?id=com.microsoft.rdc.androidx) | RDP with Microsoft identity (Entra ID) auth; Relayed RDP Shortpath | No — designed for Azure Virtual Desktop / Windows 365 / Dev Box / RDS / remote PCs, not a self-hosted alternative in itself (still needs an RDP-capable Windows host) | Free client app; backend (AVD/W365/RDS) has its own Microsoft licensing costs | Only speaks RDP; best fit is Windows-centric shops already on Azure/W365, not a generic cross-platform tool. [Microsoft Learn](https://learn.microsoft.com/en-us/windows-app/get-started-connect-devices-desktops-apps) |
| **Chrome Remote Desktop** | Windows/macOS/Linux (host), Chrome browser or app/client (client) | Google documents use from both [iPhone/iPad](https://support.google.com/chrome/answer/1649523?co=GENIE.Platform%3DiOS&hl=en) and [Android](https://support.google.com/chrome/answer/1649523?co=GENIE.Platform%3DAndroid&hl=en); Android app is on [Play Store](https://play.google.com/store/apps/details?id=com.google.chromeremotedesktop&hl=en) | Google account auth; Google support states remote desktop sessions are encrypted | No — access is brokered through Google's service; not self-hostable | Free | Convenience fallback, but not a good fit if you require self-hosting, vendor independence, or stronger administrative controls. |
| **Parsec** | Windows/macOS/Linux (host), Android (client) | **No iOS client** despite being commonly listed as cross-platform; Android app exists | Account-based auth; low-latency proprietary streaming protocol | No (SaaS relay); "Parsec for Teams"/on-prem options exist commercially | Free tier for personal use; paid tier ~$9.99/mo for creative-pro features | Not usable from an iPhone/iPad at all — hard blocker for iOS users; best fit is Android-to-PC low-latency streaming (e.g., gaming, GPU work). |
| **Moonlight (client) + Sunshine (self-hosted host)** | Host: Win/macOS/Linux (Sunshine) or Nvidia GameStream; Client (Moonlight): Windows/macOS/Linux/Android/iOS/Apple TV/Raspberry Pi/smart TVs | Native, open-source apps on both iOS and Android [moonlight-stream.org](https://moonlight-stream.org/) | Pairing-PIN based pairing between client and host; encrypted video/input stream | Yes — Sunshine is a self-hosted, open-source GameStream-compatible host (GPU hardware encode: Nvidia/AMD/Intel) [LizardByte docs](https://docs.lizardbyte.dev/projects/sunshine/latest/) | Free, open source (GPLv3 for Moonlight clients) | Optimized for low-latency video/game streaming, not for typical "remote desktop" file/clipboard workflows; needs a capable GPU on the host and a solid LAN or well-tuned WAN link for best results. |
| **NoMachine** | Windows/macOS/Linux (host+client), iOS, Android | Official apps on [App Store](https://apps.apple.com/us/app/nomachine/id874286563) and [Play Store](https://play.google.com/store/apps/details?id=com.nomachine.nxplayer) | NX protocol; password or key-based auth | Yes, historically — but **NoMachine 10 discontinued the free server "Free Edition"**, replaced by a paid Personal Edition subscription; mobile *client* apps remain free | Client app free; server now requires Personal Edition (subscription) or Enterprise licensing for full features | Free-tier server functionality has shrunk over versions (e.g., headless Linux VM capped at 800×600 on some free tiers) — verify current licensing before relying on it long-term. |
| **AnyDesk** | Windows/macOS/Linux/iOS/Android/BSD/Raspberry Pi | Native apps both platforms | Proprietary TLS 1.2 + RSA key exchange; 2FA available on paid plans | No official self-hosted relay for consumers (On-Premises server exists at enterprise tier) | Free for strictly personal/non-commercial use; paid plans from ~€22.9/mo (Solo) [anydesk.com/pricing](https://anydesk.com/en/pricing) | Free plan: session time capped (~1 hr), address book limited to 5 entries, single TCP tunnel, **no unattended access**, no session recording — not viable for hands-off dev-box access without a paid plan. |
| **TeamViewer** | Windows/macOS/Linux/iOS/Android/ChromeOS | Native apps both platforms | Proprietary session encryption, 2FA available | Primarily SaaS; enterprise/on-prem options may be available depending on plan | Free personal use plus paid commercial plans | Strong turnkey support workflow, but not self-hosted and likely overkill/costly for personal always-on dev-box access. |
| **Termius** | Windows/macOS/Linux/iOS/Android/Web | Native apps both platforms, [App Store](https://apps.apple.com/us/app/termius-modern-ssh-client/id549039908) | SSH key/password auth, cloud-synced vault (E2E-encrypted per Termius's own claims) | N/A (client only; connects to your own SSH/Mosh servers) | Free "Starter" tier: SSH, Mosh, Telnet, port forwarding, SFTP. Pro ~$10/user/mo (~$119/yr) unlocks cross-device sync, snippets, more. | Pro paywall covers exactly the features (multi-device sync) most useful for "start on desktop, continue on phone" workflows. |
| **Prompt 3 (Panic)** | macOS, iPhone, iPad, Apple Vision Pro (iOS ecosystem only) | Native, one purchase covers all Apple platforms | SSH, Mosh, Eternal Terminal, Telnet; Panic Sync for cross-device server list | N/A (client only) | $19.99/yr subscription or one-time purchase (~$100 historically); 7-day free trial | **No Android version** — Apple-ecosystem only. |
| **ConnectBot** | Android only | Open source, free, on Play Store/F-Droid | SSH key/password auth; agent forwarding | N/A (client only) | Free, open source | No file transfer; no native Mosh support; more bare-bones than Termius. |
| **JuiceSSH** | Android only | SSH/Mosh/Telnet/local shell | Standard SSH auth | N/A (client only) | Free tier + paid upgrade historically | Treat as lower-confidence for new setups unless you verify current app-store availability and maintenance status; Termius/ConnectBot are easier to recommend today. |
| **Mosh (Mobile Shell)** | GNU/Linux, BSD, macOS, Solaris, Android, Chrome, iOS (via Blink Shell) | No dedicated Mosh app; used inside Termius/JuiceSSH (Android) and Blink Shell (iOS) | SSP protocol over UDP, session keyed via SSH-established secret; intelligent local echo | Yes — `mosh-server` is open source, install alongside SSH on your dev box | Free, open source | Requires UDP (typically ports 60000–61000) reachable client→server, which some corporate/mobile NATs and firewalls block; not a full SSH replacement (no port forwarding by itself). [mosh.org](https://mosh.org/), [GitHub](https://github.com/mobile-shell/mosh) |

---

## Recommendations by use case

### 1. Quick SSH/tmux access to a dev box from a phone
- **Transport:** Tailscale between phone and dev box (no port forwarding, no public exposure). [Tailscale SSH docs](https://tailscale.com/docs/features/tailscale-ssh)
- **Terminal app:** Termius (cross-platform, free tier covers SSH/Mosh/SFTP) if you're on Android or want one app for both platforms; Prompt 3 or Blink Shell if you're iOS-only and want native polish/Mosh + Eternal Terminal resilience.
- **Session persistence:** run `tmux` (or `screen`) on the server so a dropped connection never kills your shell; layer Mosh on top of SSH for automatic roaming across Wi-Fi/cellular handoffs. [mosh.org](https://mosh.org/)
- Prefer Termius/Prompt/Blink for polished mobile use. ConnectBot is good if you want a free/open-source Android SSH client, but it is more bare-bones and lacks Mosh/file transfer.

### 1b. Herdr / AI-agent sessions from a phone
- **Best fit:** Collie. It was built for exactly this: monitor terminal AI agents from a phone, see which pane needs input, answer prompts with mobile-friendly controls, and send special keys without fighting a raw terminal UI.
- **Recommended shape:** Tailscale app on phone + `tailscale serve` on host + `COLLIE_TRUSTED_USER` + Collie device pairing. Install as a Herdr plugin if Herdr manages your sessions.
- **Caveat:** Collie is deliberately powerful: it can read pane output and inject keystrokes into a live shell. Treat its URL like shell access; keep it private to your tailnet and never use `tailscale funnel`.

### 2. Full remote desktop (GUI) to a Linux or Windows workstation
- **Self-hosted, free, cross-platform:** RustDesk with your own `rustdesk-server`, or Guacamole if you want zero app installs (pure browser access, good for a locked-down or shared phone).
- **Windows-specific, already on Azure/Microsoft 365 stack:** Microsoft Windows App (RDP-based).
- **Turnkey commercial, occasional ad-hoc use:** AnyDesk (more usable free tier than TeamViewer, but still capped — expect to pay if it's daily-driver usage).
- Chrome Remote Desktop is a workable free fallback, but it is Google-account/relay dependent and not self-hostable — treat it as a convenience option, not the primary privacy/control pick.

### 3. Game-streaming-style low-latency GUI access (e.g., heavy interactive/creative apps, occasional gaming)
- **Self-hosted and free:** Moonlight (iOS/Android client) + Sunshine (self-hosted host) — works with AMD/Intel/Nvidia GPU hardware encoding.
- **Commercial, Android-only:** Parsec — but note it currently has **no iOS client**, so it's a non-starter if the phone is an iPhone.

### 4. Zero-config mesh networking as the base layer
- Tailscale is the clear default: official iOS/Android apps, WireGuard security model, built-in SSH, and a free tier suitable for personal use. Layer any of the above tools (RDP/VNC/SSH/Mosh/RustDesk/Collie) on top of the tailnet instead of exposing ports publicly.

### 5. Secure, fully self-hosted setup end to end
- Tailscale (or self-hosted Headscale) for the network layer, combined with either:
  - RustDesk self-hosted server for GUI remote desktop, or
  - Guacamole for browser-only GUI access, or
  - OpenSSH + Mosh + tmux for terminal/dev work, or
  - Collie for a phone-optimized Herdr/tmux/zellij AI-agent dashboard.
- This avoids dependency on any vendor relay (RustDesk public relay, TeamViewer/AnyDesk cloud, Chrome's Google relay) for the actual session traffic.

---

## Sources
- RustDesk docs: https://rustdesk.com/docs/en/
- RustDesk mobile control blog: https://rustdesk.com/blog/rustdesk-remote-control-android-ios/
- RustDesk iOS App Store listing: https://apps.apple.com/us/app/rustdesk-remote-desktop/id1581225015
- Tailscale SSH: https://tailscale.com/docs/features/tailscale-ssh
- Tailscale Taildrop: https://tailscale.com/docs/features/taildrop
- Apache Guacamole: https://guacamole.apache.org/
- Collie README: https://github.com/AltanS/collie
- Collie security docs: https://github.com/AltanS/collie/blob/main/docs/security.md
- Collie install docs: https://github.com/AltanS/collie/blob/main/docs/install.md
- Microsoft Windows App docs: https://learn.microsoft.com/en-us/windows-app/get-started-connect-devices-desktops-apps
- Windows App on Google Play: https://play.google.com/store/apps/details?id=com.microsoft.rdc.androidx
- Chrome Remote Desktop Google support: https://support.google.com/chrome/answer/1649523
- Chrome Remote Desktop on Google Play: https://play.google.com/store/apps/details?id=com.google.chromeremotedesktop&hl=en
- Parsec: https://parsec.app/, https://parsec.app/technology
- Moonlight: https://moonlight-stream.org/, https://github.com/moonlight-stream/moonlight-android
- Sunshine (LizardByte) docs: https://docs.lizardbyte.dev/projects/sunshine/latest/
- NoMachine iOS app: https://apps.apple.com/us/app/nomachine/id874286563
- NoMachine Android app: https://play.google.com/store/apps/details?id=com.nomachine.nxplayer&hl=en
- AnyDesk pricing: https://anydesk.com/en/pricing
- Termius iOS App Store listing: https://apps.apple.com/us/app/termius-modern-ssh-client/id549039908
- Prompt 3 (Panic): https://panic.com/prompt/
- Mosh official site: https://mosh.org/
- Mosh GitHub: https://github.com/mobile-shell/mosh
