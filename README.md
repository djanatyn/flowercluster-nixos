# flowercluster-nixos

my digital garden

built with:
* [stack](https://docs.haskellstack.org/en/stable/README/) + [shake](https://shakebuild.com/)
* [terraform](https://www.terraform.io/)

to build:
``` sh
./Grow.hs
```

The Grow module runs build tooling, parses manifest artifacts, and bootstraps
itself when stack is installed and configured.
