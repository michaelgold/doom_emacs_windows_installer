# Doom Emacs Windows Installer
![Doom Emacs Windows Installation](.github/doom-emacs-install.gif)

## Pre-reqs
Install Python 3 on Windows. We recommend [Miniconda](https://conda.io/miniconda.html)

## Installation
``` shell
pip install -r requirements.txt
python ./install_doom_emacs.py
```
**note** you will need to accept the prompt to run python.exe as administrator to install Emacs and the All the Icons fonts

## What this script does

-   Enters an elevated mode and installs emacs-27.2
-   Updates the site-start.el with the logged in user's home directory
-   Downloads the dependencies [ripgrep](https://github.com/BurntSushi/ripgrep) and [fd](https://github.com/sharkdp/fd) and copies their Windows binaries to the `WindowsApps` path under the user's home directory 
-   Clones the Doom Emacs repo from github into the install location
-   Installs Doom Emacs
-   Configures the Doom Environment
-   Runs Doom Sync
-   Downloads and installs [All the Icons Fonts](https://github.com/domtronn/all-the-icons.el/)