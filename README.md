# Whonix-Starter

Advanced Whonix-Starter for platform-independent use

## Build Program

Whonix-Starter was written in Free Pascal using the Lazarus IDE and can be compiled under Windows and Linux (but after minor adjustments it can also be used under all operating systems that are also supported by Lazarus).

### GUI Build Method

1. Install Lazarus IDE: https://www.lazarus-ide.org/index.php
2. Open file Whonix.lpi with Lazarus IDE
3. Create executable file under menu item Run->Build.

### CLI Build Method

1. Install dependencies.

Using [`ppcross_install`](https://github.com/Whonix/misc/blob/main/ppcross_install).

```
./ppcross_install
```

2. Build on Linux for Windows.

```
./build_on_linux_for_win64
```
