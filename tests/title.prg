/*
The example was taken from the original file
https://github.com/glfw/glfw/blob/master/tests/title.c
It is a copyrighted work provided under license.
Modified by Rafał Jopek
*/

#include "hbglfw.ch"

FUNCTION Main()

   LOCAL window

   IF ( ! glfwInit() )
      OutStd( e"\nError" )
      RETURN -1
   ENDIF

   window := glfwCreateWindow( 400, 400, "English 日本語 русский язык 官話 polski", NIL, NIL )

   IF window == NIL
      OutStd( e"\nFailed to create GLFW window" )
      glfwTerminate()
      RETURN -1
   ENDIF

   glfwMakeContextCurrent( window )
   gladLoadGL_glfwGetProcAddress()
   glfwSwapInterval( 1 )

   DO WHILE ! glfwWindowShouldClose( window )

      glClear( GL_COLOR_BUFFER_BIT )
      glfwSwapBuffers( window )
      glfwWaitEvents()

   ENDDO

   glfwTerminate()

   RETURN 0