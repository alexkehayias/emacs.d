# Alex's Emacs Config

![CI](https://github.com/alexkehayias/emacs.d/workflows/CI/badge.svg)

Uses `straight` and `use-package` to manage packages. This init file is fully bootstrapped so all you need to do is load the file and it will install all required packages.

## Usage

```
git clone git@github.com:alexkehayias/emacs.d.git
mv ./emacs.d/* ~/.emacs.d
open emacs
```

## Other Setup

To get rust language server working with `eglot`:

```
rustup component add rls rust-analysis rust-src
```

To use `vterm`, you will need to include modules support:

```
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-modules
```

To upgrade an existing install:

```
brew upgrade emacs-mac --with-modules
```
