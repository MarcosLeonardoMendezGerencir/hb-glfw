/*
The example was taken from the original file
https://github.com/glfw/glfw/blob/master/tests/windows.c
It is a copyrighted work provided under license.
Modified by Rafał Jopek
*/

#include "hbglfw.ch"

STATIC titles := { "Red", "Green", "Blue", "Yellow" }

STATIC colors := { { 0.95, 0.32, 0.11 }, { 0.50, 0.80, 0.16 }, { 0., 0.68, 0.94 }, { 0.98, 0.74, 0.04 } }


PROCEDURE Main( argc )

   LOCAL i, ch
   LOCAL decorated := GLFW_FALSE
   LOCAL focusOnShow := GLFW_TRUE
   LOCAL running = GLFW_TRUE
   LOCAL windows := {}
   LOCAL left, top, right, bottom

   IF argc != NIL
      usage()
      RETURN
   ENDIF

   glfwSetErrorCallback( @error_callback() )

   IF ! glfwInit()
      RETURN
   ENDIF

   glfwWindowHint( GLFW_DECORATED, decorated )
   glfwWindowHint( GLFW_VISIBLE, GLFW_FALSE )

   FOR i := 1 TO 4

      IF ( i )
         glfwWindowHint( GLFW_FOCUS_ON_SHOW, focusOnShow )
      ENDIF

      IF window == NIL
         glfwTerminate()
         RETURN
      ENDIF

      glfwSetKeyCallback( windows[ i ], @key_callback() )

      glfwMakeContextCurrent( windows[ i ] )
      gladLoadGL_glfwGetProcAddress()

      glClearColor( colors[ i ], colors[ i ], colors[ i ], 1 )

      glfwGetWindowFrameSize( windows[ i ], @left, @top, @right, @bottom )
      glfwSetWindowPos( windows[ i ], 100 + ( i & 1 ) * ( 200 + left + right ), 100 + ( i >> 1 ) * ( 200 + top + bottom ) )

   NEXT

   FOR i = 1 TO 4
      glfwShowWindow( windows[ i ] )
   NEXT

   DO WHILE ( running )

      FOR i := 1 TO 4

         glfwMakeContextCurrent( windows[ i ] )
         glClear( GL_COLOR_BUFFER_BIT )
         glfwSwapBuffers( windows[ i ] )

         IF ( glfwWindowShouldClose( windows[ i ] ) )
            running := GLFW_FALSE
         ENDIF

      NEXT

      glfwWaitEvents()

   ENDDO

   glfwTerminate()
   OutStd( e"\nFinishing... ;)" )

   RETURN

STATIC PROCEDURE usage()

   OutStd( e"\nUsage: windows [-h] [-b] [-f]" )
   OutStd( e"\nOptions:" )
   OutStd( e"\n  -b create decorated windows" )
   OutStd( e"\n  -f set focus on show off for all but first window" )
   OutStd( e"\n  -h show this help" )

   RETURN


STATIC PROCEDURE error_callback( nError, cDescription )

   HB_SYMBOL_UNUSED( nError )
   OutStd( e"\nError: ", cDescription )

   RETURN

STATIC PROCEDURE key_callback( window, key, scancode, action, mods )

   LOCAL xpos, ypos

   IF action != GLFW_PRESS
      RETURN
   ENDIF

   SWITCH KEY

   CASE GLFW_KEY_SPACE
      glfwGetWindowPos( window, @xpos, @ypos )
      glfwSetWindowPos( window, xpos, ypos )
      EXIT
   CASE GLFW_KEY_ESCAPE
      glfwSetWindowShouldClose( window, GLFW_TRUE )
      EXIT
   OTHERWISE
      ? key, scancode, action, mods
   ENDSWITCH

   RETURN