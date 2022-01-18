import os
import shutil
import stat
import subprocess
from install_doom_emacs import install_config_files, run_doom_env, run_doom_sync 
from pathlib import PureWindowsPath

home = os.path.expanduser("~")


dir_path = PureWindowsPath(os.path.join(home, '.emacs.d/.local/'))

print(dir_path)

try:
    os.chmod(dir_path, stat.S_IWUSR, stat.S_IWRITE) #take ownership
    shutil.rmtree(dir_path)
except OSError as e:
    print("Error: %s : %s" % (dir_path, e.strerror))


subprocess.call(['doom', '-d', 'upgrade'], shell=True)
run_doom_env()
install_config_files()
run_doom_sync()