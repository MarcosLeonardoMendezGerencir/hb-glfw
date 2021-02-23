# Getting started

* [Harbour programming language](https://en.wikipedia.org/wiki/Harbour_(programming_language))
* [Homepage](https://harbour.github.io)
* Select your Harbour:
   - [hb30](https://sourceforge.net/projects/harbour-project/files/)
   - [hb32](https://github.com/harbour/core)
   - [hb34](https://github.com/vszakats/harbour-core)

# Install GLFW library 3.3.2-1

### MSYS2

``` shell

$ pacman -S mingw-w64-x86_64-glfw

```

``` shell

$ export HB_WITH_GLFW="$usr/mingw64/include"
$ cd hb-glfw
$ hbmk2 hbglfw.hbp

```

### MinGW-w64

Download binary archive from this page and unpack or install:

 - [glfw-3.3.2.bin.WIN64](https://github.com/glfw/glfw/releases/download/3.3.2/glfw-3.3.2.bin.WIN64.zip)

``` shell

c:\glfw\bin\libglfw3.a               305 548  20.01.2020 01:16
c:\glfw\bin\libglfw3dll.a             83 380  20.01.2020 01:16
c:\glfw\bin\glfw3.dll                249 344  20.01.2020 01:16
c:\glfw\include\GLFW\glfw3.h         213 871  20.01.2020 01:13
c:\glfw\include\GLFW\glfw3native.h    16 420  20.01.2020 01:13

```

``` shell

c:\>set HB_WITH_GLFW=c:\glfw\include
c:\>set PATH=c:\glfw\bin;%PATH%

```

### Ubuntu

``` shell

$ sudo apt-get install libglfw3-dev

```

``` shell

$ export HB_WITH_GLFW="$usr/include"
$ cd hb-glfw
$ hbmk2 hbglfw.hbp

```

---
[Request create tutorial](https://github.com/rjopek/harbour-gl/issues/new)