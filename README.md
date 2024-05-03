<div align="center">

# Celeste Emacs

<img src="https://static.miraheze.org/celestewiki/a/ac/Strawberry_flap.gif" alt="celeste strawberry"/>

Simple, lightweight and bespoked Emacs configurations.

</div>

---

## Installation

Dependency list:

- C/C++ compiler suites, including a compiler, `make`, `cmake`
- `libenchant` for jinx, the spell checker
- `ripgrep` for consult and deadgrep
- `libvterm` for vterm
- `macism` for [sis](https://github.com/laishulu/emacs-smart-input-source)

### macOS

``` shell
# Basic dependencies.
brew install make cmake ripgrep
# Extra dependencies for various packages.
brew install enchant libvterm
# For emacs-smart-input-source.
brew tap laishulu/macism && brew install macism
```

### Python Environment

If you want to use other brilliant plugins like lsp-bridge and EAF that are
based on a Python environment, some Python packages are also necessary. I
recommend manage the dedicated Python environment with venv. You can also choose
your favorite way, such as with conda, or even barely. Here is my way:

``` shell
# Make sure you have venv package is installed.
# sudo apt install python3-venv -y

python3 -m venv .venv
source .venv/bin/activate
# Use `deactivate` to deactivate the venv
```

Run `which python3` to see where the Python3 executable is located. You may need
to change corresponding Emacs variables to make the virtual environment take
effects. For example:

``` emacs-lisp
(setq lsp-bridge-python-command "Your Python executable path")
```

#### lsp-bridge

``` shell
pip3 install epc orjson sexpdata six setuptools paramiko rapidfuzz
```

## Copyright Notice

The banner under the project name is a work from Celeste game asset, borrowed
from [Celeste Wiki](https://github.com/laishulu/emacs-smart-input-source), and
licensed under [CC BY-NC-SA 4.0
License](https://creativecommons.org/licenses/by-nc-sa/4.0/deed.en). You may not
use the material for commercial purposes.

I use this banner just because I'm a huge fan of Celeste game. If my actions
infringe on any copyrights, please contact me immediately. Thank a lot.
