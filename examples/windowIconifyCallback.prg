/*
   Test window iconify callback.
   This function sets the iconification callback of the specified window, which is called when the window is iconified or restored.
   glfwSetWindowIconifyCallback( pWindow, @window_iconify_callback() )
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

   glfwSetWindowIconifyCallback( pWindow, @window_iconify_callback() )

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

STATIC PROCEDURE window_iconify_callback( pWindow, iconified )

   LOCAL slot := glfwGetWindowUserPointer( pWindow )

   IF iconified == 1
      OutStd( e"iconified", slot, e"\n" )
   ELSE
      OutStd( e"uniconified", slot, e"\n" )
   ENDIF

   RETURN