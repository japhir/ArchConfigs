This is my config dot file repository.

I use:
- [arch linux](https://archlinux.org/) as operating system
- [emacs](https://www.gnu.org/software/emacs/) for writing, email, agenda, note-taking, and text-editor
  - [evil](https://github.com/emacs-evil/evil) to use vim keys in emacs
  - [org-mode](https://orgmode.org/) mode that I use for GTD, writing, literate programming
  - [evil-org-mode](https://github.com/Somelauw/evil-org-mode) to use vim keybindings in org-mode
  - [org-roam](https://www.orgroam.com/) connected note-taking following the zettelkasten method
  - [ess](https://ess.r-project.org/) data analysis in R
  - [modus-themes](https://gitlab.com/protesilaos/modus-themes/) beautiful light and dark theme
- [sway](https://swaywm.org/) window manager
  - [waybar](https://github.com/Alexays/Waybar/) for workspaces, bar, and other nice widgets
  - [wofi](https://hg.sr.ht/~scoopta/wofi) for launching programs that I don't have a keybinding for
  - [mako](https://github.com/emersion/mako) for notifications
- [alacritty](https://github.com/alacritty/alacritty) terminal emulator
 - [zsh](https://grml.org/zsh/) bash alternative
 - [fzf](https://github.com/junegunn/fzf) fuzzy file finder, this makes terminal's <kbd>ctrl</kbd>+<kbd>r</kbd> so nice!
- [firefox](https://firefox.org/) browser
  - [vim-vixen](https://github.com/ueokande/vim-vixen/releases/tag/0.30) vim keybindings in firefox
  - [org-capture](https://github.com/sprig/org-capture-extension) capture websites into emacs org-mode

Personal scripts are in /bin and user services in /services.

If you want to use (some) of my configs, I recommend that you proceed with caution and use only the bits that you like!

Simply git clone in where you want and symlink the files to their proper destination.
You can also just execute the `symlink` file to link everything in one go, but I'm not keeping it fully up to date.

For individual entries type: `ln -s {target-filename} {symbolic filename}`, for example for i3: `ln -s i3config ~/.config/i3/config`

For all the executables, make a directory (e.g. `~/bin`), add it to your `PATH`, then symlink with `ln -s /path/to/ArchConfigs/bin/* ~/bin`.
Possibly, you'll have to `chmod +x filename` to make the files executable.

Kind regards,

Japhir
