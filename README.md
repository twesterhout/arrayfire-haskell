## <a href="http://arrayfire.com/"><img src="http://arrayfire.com/logos/arrayfire_logo_whitebkgnd.png" width="300"></a>
`ArrayFire` is a general-purpose library that simplifies the process of developing software that targets parallel and massively-parallel architectures including CPUs, GPUs, and other hardware acceleration devices.

`arrayfire-haskell` is a [Haskell](https://haskell.org) binding for [ArrayFire](https://arrayfire.com).

## Table of Contents
 - [Installation](#Installation)
 - [Documentation](#Documentation)
 - [Hacking](#Hacking)
 - [Example](#Example)


## Installation
Install `ArraryFire` via the download page.
  - https://arrayfire.com/download/

Also note, if you plan on using ArrayFire's visualization features, you must install `fontconfig` and `glfw` on OSX or Linux.

## Documentation
  - [Hackage](http://hackage.haskell.org/package/arrayfire)
  - [ArrayFire](http://arrayfire.org/docs/gettingstarted.htm)

## Hacking
To hack on this library locally, complete the installation step above. We recommend installing the [nix](https://nixos.org/nix/download.html) package manager to facilitate development.

After the above tools are installed, clone the source from Github.

``bash
git clone git@github.com:arrayfire/arrayfire-haskell.git
cd arrayfire-haskell
```

To build and run all tests in response to file changes.

```bash
nix-shell --run test-runner
```

To perform interactive development w/ `ghcid`.

```bash
nix-shell --run ghcid
```

## Example
```haskell
module Main where

import qualified ArrayFire as A

main :: IO ()
main = A.printArray action `catch` (\(e :: A.AFException) -> print e)
  where
    action = A.matrix @Double (3,3) [1.0 ..] `A.mul` A.matrix @Double (3,3) [1.0 ..]
```
