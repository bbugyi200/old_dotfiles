# These are my main configuration files. 

I keep my configuration files on Sync and then use [stow](https://www.gnu.org/software/stow/manual/stow.html) to create symbollic links from these config files to the appropriate locations on my filesystem. This setup enables me to keep all of my dotfiles organized and separate from my home directory.

In order to utilize many of these configuration files, you will need access to the [scripts](https://github.com/bbugyi200/scripts) that they call upon.

Notice that this repository mirrors a standard `/home/<user>` directory. This is **mandatory** in order to use [stow](https://www.gnu.org/software/stow/manual/stow.html). The exact stow commands I use to do this can be found in my [clinks](https://github.com/bbugyi200/scripts/blob/master/main/clinks) script.
