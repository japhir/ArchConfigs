This is my config dot file repository.

I use:
- [arch linux](https://archlinux.org/) as operating system
- [emacs](https://www.gnu.org/software/emacs/) for writing, email, agenda, note-taking, and text-editor
  - [evil](https://github.com/emacs-evil/evil) to use vim keys in emacs
  - [org-mode](https://orgmode.org/) mode that I use for GTD, writing, literate programming
  - [evil-org-mode](https://github.com/Somelauw/evil-org-mode) to use vim keybindings in org-mode
  - [org-roam](https://www.orgroam.com/) connected note-taking following the zettelkasten method
  - [magit](https://magit.vc/) the best git porcelain
  - [ess](https://ess.r-project.org/) data analysis in R
  - [modus-themes](https://gitlab.com/protesilaos/modus-themes/) beautiful light and dark theme
  - [mu4e](https://www.djcbsoftware.nl/code/mu/) read my email from emacs
- [hyprland](https://hyprland.org/) window manager
  (I recently switched from [sway](https://swaywm.org/), config still available)
  - [wofi](https://hg.sr.ht/~scoopta/wofi) for launching programs that I don't have a keybinding for
  - [nwg-panel](https://github.com/nwg-piotr/nwg-panel) for workspaces, bar, and other nice widgets
  - [swaync](https://github.com/ErikReider/SwayNotificationCenter) for notifications
  - [wlsunset](https://sr.ht/~kennylevinsen/wlsunset/) to change display brightness/colour in the evening
- [foot](https://codeberg.org/dnkl/foot) terminal emulator
 - [zsh](https://grml.org/zsh/) bash alternative
 - [fzf](https://github.com/junegunn/fzf) fuzzy file finder, this makes terminal's <kbd>ctrl</kbd>+<kbd>r</kbd> so nice!
 - [zoxide](https://github.com/ajeetdsouza/zoxide) fast directory navigation
- [firefox](https://firefox.org/) browser
  - [org-capture](https://github.com/sprig/org-capture-extension) capture websites into emacs org-mode

Personal scripts are in /bin and user services in /services.

If you want to use (some) of my configs, I recommend that you proceed with caution and use only the bits that you like!

Simply git clone in where you want and symlink the files to their proper destination.
I'm slowly moving things around so that in the future I'll be able to just git clone this directory to my `~/.config` without any symlinking.

For individual entries type: `ln -s {target-filename} {symbolic filename}`, for example for i3: `ln -s i3config ~/.config/i3/config`

For all the executables, make a directory (e.g. `~/bin`), add it to your `PATH`, then symlink with `ln -s /path/to/ArchConfigs/bin/* ~/bin`.
Possibly, you'll have to `chmod +x filename` to make the files executable.

Kind regards,

Japhir
