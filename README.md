This is my config file repository.

Simply git clone in where you want and symlink the files to their proper
destination. You can also just execute the `symlink` file to link everything in
one go, but I'm not keeping it fully up to date.

For individual entries type: `ln -s {target-filename} {symbolic filename}`, for
example for i3: `ln -s i3config ~/.config/i3/config`

For all the executables, make a directory (e.g. `~/bin`), add it to your
`PATH`, then symlink with `ln -s /path/to/ArchConfigs/bin/* ~/bin`. Possibly,
you'll have to `chmod +x filename` to make the files executable.

Kind regards,

Japhir
