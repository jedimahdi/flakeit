# Flakeit

Make it easier to work with Nix flake templates.

## Usage

```sh
flakeit --help
flakeit add github:NixOS/templates
flakeit list
```

### fzf

```sh
flakeit list | fzf | xargs flakeit init -t
```

## Installation

### Install from source using cabal

```sh
git clone https://github.com/jedimahdi/flakeit.git
cd flakeit
cabal install exe:flakeit
```

### nix-env

```sh
nix-env -i -f https://github.com/jedimahdi/flakeit/archive/master.tar.gz
nix profile install github:jedimahdi/flakeit/latest
```
