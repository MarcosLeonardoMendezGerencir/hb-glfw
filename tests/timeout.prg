/*
The example was taken from the original file
https://github.com/glfw/glfw/blob/master/tests/timeout.c
It is a copyrighted work provided under license.
Modified by Rafał Jopek
*/

#include "hbglfw.ch"

#define RAND_MAX 0x7fff

PROCEDURE Main()

   LOCAL pWindow
   LOCAL r, g, b
   LOCAL width, height

   glfwSetErrorCallback( @error_callback() )

   IF ! glfwInit()
      OutStd( e"\nFailed to initialize GLFW" )
      RETURN
   ENDIF

   pWindow := glfwCreateWindow( 640, 480, "Event Wait Timeout Test", NIL, NIL )

   IF pWindow == NIL
      glfwTerminate()
      OutStd( e"\nFailed to open GLFW Window" )
      RETURN
   ENDIF

   glfwMakeContextCurrent( pWindow )
   gladLoadGL_glfwGetProcAddress()
   glfwSwapInterval( 1 )

   glfwSetKeyCallback( pWindow, @key_callback() )

   glClearColor( 0.5, 0.5, 0.5, 0 )

   DO WHILE ! glfwWindowShouldClose( pWindow )

      r := hb_Random( 0.1, 2 )
      g := hb_Random( 0.1, 2 )
      b := hb_Random( 0.1, 2 )

      glfwGetFramebufferSize( pWindow, @width, @height )
      glViewport( 0, 0, width, height )

      glClear( GL_COLOR_BUFFER_BIT )
      glClearColor( r, g, b, 1 )

      glfwWaitEventsTimeout( 1.0 )

      glfwSwapBuffers( pWindow )

   ENDDO

   glfwTerminate()

   OutStd( e"\nFinishing... ;)" )

   RETURN

STATIC PROCEDURE error_callback( nError, cDescription )

   HB_SYMBOL_UNUSED( nError )
   OutStd( e"\nError: ", cDescription )

   RETURN

STATIC PROCEDURE key_callback( pWindow, key, scancode, action, mods )

   HB_SYMBOL_UNUSED( pWindow )
   HB_SYMBOL_UNUSED( scancode )
   HB_SYMBOL_UNUSED( mods )

   IF key == GLFW_KEY_ESCAPE .AND. action == GLFW_PRESS
      glfwSetWindowShouldClose( pWindow, GLFW_TRUE )
   ENDIF

   RETURN