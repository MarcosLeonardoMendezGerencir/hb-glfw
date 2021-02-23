# ![Logo](docs/img/harbour_glfw.svg) hb-glfw

hb-glfw is a Harbour module providing bindings for the [GLFW](https://github.com/glfw/glfw) library. It provides a simple API for creating windows, contexts and surfaces, receiving input and events.

### Getting Started

Installing hb-glfw requires GLFW including its headers. For more info see [getting started](docs/tutorial/README.md).

### Example code

```Harbour

#include "hbglfw.ch"

FUNCTION Main()

   LOCAL window

   IF ! glfwInit()
      RETURN - 1
   ENDIF

   window := glfwCreateWindow( 640, 480, "Hello World", NIL, NIL )

   IF window == NIL
      glfwTerminate()
      RETURN - 1
   ENDIF

   glfwMakeContextCurrent( window )

   DO WHILE ! glfwWindowShouldClose( window )

      glClear( GL_COLOR_BUFFER_BIT )

      glfwSwapBuffers( window )

      glfwPollEvents()

   ENDDO

   glfwTerminate()

   RETURN 0

```

## License

[![License](http://img.shields.io/:license-mit-blue.svg?style=flat-square)](LICENSE)
