import os
import subprocess
from install_doom_emacs import install_config_files, install_custom_doom_cmd, remove_dir_if_exists, run_doom_env, run_doom_sync 
from pathlib import PureWindowsPath

home = os.path.expanduser("~")

local_install = PureWindowsPath(os.path.join(home, '.emacs.d/.local/'))

remove_dir_if_exists(local_install)
subprocess.call(['doom', 'upgrade', '-f'], shell=True)
install_custom_doom_cmd()
run_doom_env()
install_config_files()
run_doom_sync()