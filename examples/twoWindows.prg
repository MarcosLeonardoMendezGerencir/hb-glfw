
#include "hbglfw.ch"

PROCEDURE Main()

   LOCAL pWindows := Array( 2 )
   LOCAL nWidth, nHeight
   LOCAL nXpos, nYpos
   LOCAL nLeft, nTop, nRight, nBottom
   LOCAL i

   glfwSetErrorCallback( @error_callback() )

   IF ! glfwInit()
      OutStd( e"\nFailed to initialize GLFW" )
      RETURN
   ENDIF

   glfwWindowHint( GLFW_CONTEXT_VERSION_MAJOR, 2 )
   glfwWindowHint( GLFW_CONTEXT_VERSION_MINOR, 0 )

   pWindows[ 1 ] := glfwCreateWindow( 400, 400, "First", NIL, NIL )
   IF pWindows[ 1 ] == NIL
      glfwTerminate()
      OutStd( e"\nFailed to open GLFW Window" )
      RETURN
   ENDIF

   glfwMakeContextCurrent( pWindows[ 1 ] )
   glfwSetKeyCallback( pWindows[ 1 ], @key_callback() )

   glfwSwapInterval( 1 )

   pWindows[ 2 ] := glfwCreateWindow( 400, 400, "Second", NIL, pWindows[ 1 ] )
   IF pWindows[ 2 ] == NIL
      glfwTerminate()
      OutStd( e"\nFailed to open GLFW Window" )
      RETURN
   ENDIF

   glfwGetWindowSize( pWindows[ 1 ], @nWidth, @nHeight )
   glfwGetWindowFrameSize( pWindows[ 1 ], @nLeft, @nTop, @nRight, @nBottom )
   HB_SYMBOL_UNUSED( nTop )
   HB_SYMBOL_UNUSED( nBottom )

   glfwGetWindowPos( pWindows[ 1 ], @nXpos, @nYpos )
   glfwSetWindowPos( pWindows[ 2 ], nXpos + nWidth + nLeft + nRight, nYpos )

   glfwMakeContextCurrent( pWindows[ 2 ] )
   gladLoadGL_glfwGetProcAddress()
   glfwSetKeyCallback( pWindows[ 2 ], @key_callback() )

   DO WHILE ! glfwWindowShouldClose( pWindows[ 1 ] ) .AND. ! glfwWindowShouldClose( pWindows[ 2 ] )

      FOR i := 1 TO 2

         glfwGetFramebufferSize( pWindows[ i ], @nWidth, @nHeight )
         glfwMakeContextCurrent( pWindows[ i ] )

         glViewport( 0, 0, nWidth, nHeight )

         glfwSwapBuffers( pWindows[ i ] )

      NEXT

      glfwWaitEvents()

   ENDDO

   glfwTerminate()

   OutStd( e"\nFinishing..." )

   RETURN

STATIC PROCEDURE error_callback( nError, cDescription )

   HB_SYMBOL_UNUSED( nError )
   OutStd( e"\nError: ", cDescription )

   RETURN

STATIC PROCEDURE key_callback( pWindows, key, scancode, action, mods )

   HB_SYMBOL_UNUSED( scancode )
   HB_SYMBOL_UNUSED( mods )

   IF key == GLFW_KEY_ESCAPE .AND. action == GLFW_PRESS
      glfwSetWindowShouldClose( pWindows, GLFW_TRUE )
   ENDIF

   RETURN