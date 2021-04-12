from elevate import elevate
from os.path import expanduser
from os.path import realpath
from os.path import dirname
from pathlib import Path
from pathlib import PurePosixPath
from pathlib import PureWindowsPath
import git
import json
import requests
import shutil
import subprocess
import sys


def add_home_path_to_site_start(site_start_source, home):
    home = Path(home).as_posix() + "/"
    src = '(set-home-dir "c:/users/goldm/")'
    target = '(set-home-dir "{}")'.format(home)

    lines = []
    with open(site_start_source) as infile:
        for line in infile:
            line = line.replace(src, target)
            lines.append(line)
    with open(site_start_source, 'w') as outfile:
        for line in lines:
            outfile.write(line)

elevate()

base_path = dirname(realpath(__file__))
home = expanduser("~")


with open(Path(base_path, 'config/config.json'), 'r') as f:
    config = json.load(f)


print(config)

site_start_path = config["site_start_path"]

print('Beginning file download with requests')

emacs_download_url = config["emacs_download_url"]
emacs_installer_file = home + '/Downloads/emacs-installer.exe'


request = requests.get(emacs_download_url)
with open(emacs_installer_file, 'wb') as f:
    f.write(request.content)

subprocess.run([emacs_installer_file])


print('Copying additional scripts')

site_start_source = config["site_start_source"]
site_start_destination = "C:/Program Files/Emacs/x86_64/share/emacs/site-lisp/site-start.el"

dot_spacemacs_source = config["dot_spacemacs_source"]
dot_spacemacs_destination = home + "/.spacemacs"

add_home_path_to_site_start(base_path + "/" + site_start_source, home)
shutil.copyfile(site_start_source, site_start_destination)
shutil.copyfile(dot_spacemacs_source, dot_spacemacs_destination)

print("Cloning spacemacs repo")
spacemacs_git_url = "https://github.com/syl20bnr/spacemacs"
spacemacs_install_path = home + "/.emacs.d"

cloned_repo = git.repo.base.Repo.clone_from(spacemacs_git_url, spacemacs_install_path)