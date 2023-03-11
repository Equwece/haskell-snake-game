# Haskell Snake game

Simple snake game written in Haskell. Based on the great [Monomer](https://github.com/fjvallarino/monomer) framework.

WARNING: This is my first Haskell project, a lot of bad code can be here.

## Getting Started

To build the project from a source code use these instructions. Basically, this is a short version of setup [tutorial](https://github.com/fjvallarino/monomer/blob/main/docs/tutorials/00-setup.md) from Monomer docs, read that tutorial to get additional info.

### Prerequisites

Install the [ghcup](https://www.haskell.org/ghcup). It is a Haskell toolchain installer which allows you to easyly obtain Glasgow Haskell Compiler, Cabal and Stack.

Install dependency libraries (SDL2, GLEW) from your distro repository, for Arch Linux:

```bash
sudo pacman -S sdl2 glew
```

To get instructions for other Linux distros read a correspond part of the Monomer [tutorial](https://github.com/fjvallarino/monomer/blob/main/docs/tutorials/00-setup.md#libraries-sdl2-and-glew).

### Installing

Clone the repository:
```bash
git clone https://github.com/Equwece/haskell-snake-game
cd haskell-snake-game
```

Build the project:
```bash
stack build
```

Run the project:
```bash
stack run haskell-snake-game-exe
```

## Links

- Project repository: https://github.com/Equwece/haskell-snake-game

## Licensing

The code in this project is licensed under GPLv3 license.
