# Alex's Emacs Config

![CI](https://github.com/alexkehayias/emacs.d/workflows/CI/badge.svg)

Uses `use-package` to manage packages for my personal emacs setup so it can be shared across machines easily.

## Usage

```
git clone git@github.com:alexkehayias/emacs.d.git
mv ./emacs.d/* ~/.emacs.d
open emacs
```

Next you need to install `use-package`:

```
;; Eval this to search available packages
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; Find and install use-package
M-x package-list-package
;; Choose use-package and install
```

Restart emacs and re-open it to install all required packages.

## Other Setup

To get rust language server working with `eglot`:

```
rustup component add rls rust-analysis rust-src
```

To use ~vterm~, you will need to include modules support:

```
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-modules
```

To upgrade an existing install:

```
brew upgrade emacs-mac --with-modules
```
