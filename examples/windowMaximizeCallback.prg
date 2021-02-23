/*
   Test window maximize callback.
   This function sets the maximization callback of the specified window, which is called when the window is maximized or restored.
   glfwSetWindowMaximizeCallback( pWindow, @window_maximize_callback() )
*/

#include "hbglfw.ch"

PROCEDURE Main()

   LOCAL pWindow
   LOCAL nWidth, nHeight

   glfwSetErrorCallback( @error_callback() )

   IF ! glfwInit()
      OutStd( e"\nFailed to initialize GLFW" )
      RETURN
   ENDIF

   pWindow := glfwCreateWindow( 640, 480, "Prosty przykład", NIL, NIL )

   IF pWindow == NIL
      glfwTerminate()
      OutStd( e"\nFailed to open GLFW Window" )
      RETURN
   ENDIF

   glfwMakeContextCurrent( pWindow )
   gladLoadGL_glfwGetProcAddress()

   glfwSetWindowMaximizeCallback( pWindow, @window_maximize_callback() )

   DO WHILE ! glfwWindowShouldClose( pWindow )

      glfwGetFramebufferSize( pWindow, @nWidth, @nHeight )

      glViewport( 0, 0, nWidth, nHeight )
      glClear( GL_COLOR_BUFFER_BIT )

      glfwSwapBuffers( pWindow )
      glfwPollEvents()

   ENDDO

   glfwDestroyWindow( pWindow )
   glfwTerminate()

   OutStd( e"\nFinishing..." )

   RETURN

STATIC PROCEDURE error_callback( nError, cDescription )

   HB_SYMBOL_UNUSED( nError )
   OutStd( e"\nError: ", cDescription )

   RETURN

STATIC PROCEDURE window_maximize_callback( pWindow, maximized )

   LOCAL slot := glfwGetWindowUserPointer( pWindow )

   IF maximized == 1
      OutStd( e"maximized", slot, e"\n" )
   ELSE
      OutStd( e"unmaximized", slot, e"\n" )
   ENDIF

   RETURN