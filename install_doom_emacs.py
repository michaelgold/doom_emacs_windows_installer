from elevate import elevate
from font_helpers import install_font
from os import chmod
from os import symlink
from os.path import dirname
from os.path import expanduser
from os.path import realpath
from os.path import isdir
from pathlib import Path
from pathlib import PureWindowsPath
from zipfile import ZipFile
import git
import json
import requests
import shutil
import subprocess
import stat
import sys

def install_emacs():
    print('Downloading Emacs installer. Please click next through the prompts...')

    emacs_download_url = config["emacs_download_url"]
    emacs_installer_file = home + '/Downloads/emacs-installer.exe'

    request = requests.get(emacs_download_url)
    with open(emacs_installer_file, 'wb') as f:
        f.write(request.content)

    subprocess.run([emacs_installer_file])

def download_and_install_dependencies():
    print('Downloading and installing dependencies.')

    dependencies = config["dependencies"]
    installation_path = home + "/AppData/Local/Microsoft/WindowsApps/"

    for dependency in dependencies:
        binary = dependency["binary"]
        request = requests.get(dependency["download_url"])
        downloaded_zip = home + "/Downloads/" + dependency["dependency_name"] + ".zip"
        with open(downloaded_zip, 'wb') as f:
            f.write(request.content)

        extracted_path = home + "/Downloads/" + dependency["dependency_name"] + "/"

        with ZipFile(downloaded_zip, "r") as f:
            for name in f.namelist():
                if binary in name:
                    f.extract(name, path=extracted_path)
                    extracted_binary_file = PureWindowsPath(extracted_path + name)
                    file_destination = PureWindowsPath(installation_path + binary)
                    try:
                        shutil.copyfile(extracted_binary_file, file_destination)
                        print("Installing {}".format(binary))
                    except IOError as e:
                        print("Unable to copy file. {}".format(e))
                        input("Press enter to continue...")
                    
def symlink_binaries_to_appdata():
    windows_apps_path = home + "/AppData/Local/Microsoft/WindowsApps"
    binaries = [
        {
            "symlink": PureWindowsPath(windows_apps_path + "/doom.cmd"),
            "source": PureWindowsPath(home + "/.emacs.d/bin/doom.cmd")
        },
        {
            "symlink": PureWindowsPath(windows_apps_path + "/doom"),
            "source": PureWindowsPath(home + "/.emacs.d/bin/doom")
        }
    ]

    for binary in binaries:
        try:
            symlink(binary["source"], binary["symlink"])
        except IOError as e:
            print("Unable to symlink file. {}".format(e))
            input("Press enter to continue...")
        
def run_doom_env():
    myenv_path = PureWindowsPath(home + "/.doom.d/myenv")
    try:
        subprocess.run(["doom.cmd", "env", "-o", myenv_path], capture_output=True)
    except IOError as e:
        print("Unable to create the doom env. {}".format(e))
        input("Press enter to continue...")


def run_doom_install():
    try:
        subprocess.run(["doom.cmd", "install"])
    except IOError as e:
        print("Unable to run doom install. {}".format(e))
        input("Press enter to continue...")

def run_doom_sync():
    try:
        subprocess.run(["doom.cmd", "sync"])
    except IOError as e:
        print("Unable to run doom sync. {}".format(e))
        input("Press enter to continue...")

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

def remove_readonly(func, path, excinfo):
    chmod(path, stat.S_IWRITE)
    func(path)

def remove_dir_if_exists(dir):
    if isdir(dir):
        shutil.rmtree(dir, onerror=remove_readonly)

def copy_tree(source, destination):
    # copytree fails if directory exists, so we'll remove it first
    remove_dir_if_exists(destination)
    try:
        shutil.copytree(source, destination)
    except IOError as e:
        print("Unable to copy file. {}".format(e))
        input("Press enter to continue...")

def install_custom_doom_cmd():
    '''
    Copy `config/.emacs.d/bin/doom.cmd` to `~/.emacs.d/bin`
    '''
    doom_cmd_source = PureWindowsPath(config["doom_cmd_source"])
    doom_cmd_destination = PureWindowsPath(home + "/.emacs.d/bin/doom.cmd")

    print('Copying config/.emacs.d/bin/doom.cmd to ~/.emacs.d/bin')
    try:
        shutil.copyfile(doom_cmd_source, doom_cmd_destination)
    except IOError as e:
        print("Unable to copy file. {}".format(e))
        input("Press enter to continue...")

def install_config_files():
    '''
    copy `./config/.doom.d` to `~/.doom.d` 
    '''
    print('Copying .doom.d to ~/.doom.d')
    doom_config_source = PureWindowsPath(config["doom_config_source"])
    doom_config_destination = PureWindowsPath(home + "/.doom.d")
    
    copy_tree(doom_config_source, doom_config_destination)

def copy_local_config_files_to_repo():
    '''
    copy `~/.doom.d` to `./config/.doom.d`
    '''
    print('Copying ~/.doom.d to ./config/.doom.d')
    doom_config_source = PureWindowsPath(home + "/.doom.d/")
    doom_config_destination = config["doom_config_source"]

    # copytree fails if directory exists, so we'll remove it first
    copy_tree(doom_config_source, doom_config_destination)

def clone_doom_emacs_github_repo():
    print("Cloning doom emacs repo")
    emacs_d_github_url = config["emacs_d_github_url"]
    local_emacs_d_path = PureWindowsPath(home + "/.emacs.d")
    remove_dir_if_exists(local_emacs_d_path)
    try:
        git.repo.base.Repo.clone_from(emacs_d_github_url, local_emacs_d_path)
    except IOError as e:
        print("Unable to clone repo. {}".format(e))
        input("Press enter to continue...")

def install_fonts():
    font_download_url = config["all_the_icons_download_url"]
    font_download_file = home + '/Downloads/icon_fonts.zip'
    request = requests.get(font_download_url)
    with open(font_download_file, 'wb') as f:
        f.write(request.content)

    font_path = home + "/Downloads/icon_fonts/

    with ZipFile(font_download_file, "r") as f:
        for name in f.namelist():
            if ".ttf" in name:
                print("Installing {}".format(name))
                f.extract(name, path=font_path)
                install_font(font_path + name)

def main():
    # doom_binary = PureWindowsPath(home + "/.emacs.d/bin/doom.cmd")
    # run in admin mode so we can install emacs and symlinks to binaries
    elevate()
    # install_emacs()
    # install_site_start()
    # download_and_install_dependencies()
    # clone_doom_emacs_github_repo()
    # install_custom_doom_cmd()
    # symlink_binaries_to_appdata()
    # run_doom_env()
    # run_doom_install()
    # install_config_files()
    # run_doom_sync()

    install_fonts()

base_path = dirname(realpath(__file__))
home = expanduser("~")

with open(Path(base_path, 'config/config.json'), 'r') as f:
    config = json.load(f)

if __name__ == '__main__':
    main()