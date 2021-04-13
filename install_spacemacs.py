from elevate import elevate
from font_helpers import install_font
from os.path import dirname
from os.path import expanduser
from os.path import realpath
from pathlib import Path
from zipfile import ZipFile
import git
import json
import requests
import shutil
import subprocess
import sys

def install_emacs():
    print('Downloading Emacs installer. Please click next through the prompts...')

    emacs_download_url = config["emacs_download_url"]
    emacs_installer_file = home + '/Downloads/emacs-installer.exe'

    request = requests.get(emacs_download_url)
    with open(emacs_installer_file, 'wb') as f:
        f.write(request.content)

    subprocess.run([emacs_installer_file])

def update_home_path_in_site_start_file(site_start_source, home):
    '''
    use the logged in user's home path instead of the one listed in the config file
    '''
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

def install_site_start():
    print('Installing site-start.el')

    site_start_source = config["site_start_source"]

    # emacs will look in this path to load this file on start
    site_start_destination = "C:/Program Files/Emacs/x86_64/share/emacs/site-lisp/site-start.el"

    update_home_path_in_site_start_file(base_path + "/" + site_start_source, home)

    shutil.copyfile(site_start_source, site_start_destination)

def install_dot_spacemacs():
    '''
    copy `./config/.spacemacs` to `~/.spacemacs` 
    '''
    print('Installing .spacemacs')
    dot_spacemacs_source = config["dot_spacemacs_source"]
    dot_spacemacs_destination = home + "/.spacemacs"
    
    shutil.copyfile(dot_spacemacs_source, dot_spacemacs_destination)

def copy_local_dotfile_to_repo():
    '''
    copy `~/.spacemacs` to `./config/.spacemacs`
    '''
    print('Copying ~/.spacemacs to ./congig/.spacemacs')
    dot_spacemacs_source = home + "/.spacemacs"
    dot_spacemacs_destination = config["dot_spacemacs_source"]

    shutil.copyfile(dot_spacemacs_source, dot_spacemacs_destination)

def clone_spacemacs_github_repo():
    print("Cloning spacemacs repo")
    spacemacs_git_url = "https://github.com/syl20bnr/spacemacs"
    spacemacs_install_path = home + "/.emacs.d"

    git.repo.base.Repo.clone_from(spacemacs_git_url, spacemacs_install_path)

def install_source_code_pro_fonts():
    source_code_pro_download_url = config["source_code_pro_download_url"]
    source_code_pro_download_file = home + '/Downloads/source_code_pro_fonts.zip'
    request = requests.get(source_code_pro_download_url)
    with open(source_code_pro_download_file, 'wb') as f:
        f.write(request.content)

    font_path = home + "/Downloads/source_code_pro/"

    with ZipFile(source_code_pro_download_file, "r") as f:
        for name in f.namelist():
            if ".ttf" in name:
                print("Installing {}".format(name))
                f.extract(name, path=font_path)
                install_font(font_path + name)

def main():
    # run in admin mode so we can install emacs and fonts
    elevate()

    install_emacs()
    install_site_start()
    install_dot_spacemacs()
    clone_spacemacs_github_repo()
    install_source_code_pro_fonts()

base_path = dirname(realpath(__file__))
home = expanduser("~")

with open(Path(base_path, 'config/config.json'), 'r') as f:
    config = json.load(f)

if __name__ == '__main__':
    main()