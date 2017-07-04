# These are my main configuration files. 

I keep my configuration files on Dropbox and then use [stow](https://www.gnu.org/software/stow/manual/stow.html) to create symbollic links from these config files to the appropriate locations on my filesystem. This setup enables me to keep all of my dotfiles organized the way I like on my dropbox.

Notice that this directory mirrors a standard /home/<user> directory. This is **mandatory** in order to use [stow](https://www.gnu.org/software/stow/manual/stow.html). The exact stow commands I use to do this can be found in my [clinks](config/clinks) script.
