/*
   Test mouse button callback.
   This function sets the mouse button callback of the specified window, which is called when a mouse button is pressed or released.
   glfwSetMouseButtonCallback( pWindow, @mouse_button_callback() )
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

   glfwSetMouseButtonCallback( pWindow, @mouse_button_callback() )

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

STATIC PROCEDURE mouse_button_callback( pWindow, button, action, mods )

   LOCAL slot := glfwGetWindowUserPointer( pWindow )

   HB_SYMBOL_UNUSED( mods )

   OutStd( get_button_name( button ), e"\n" )
   OutStd( get_action_name( action ), e"\n" )
   OutStd( slot, e"\n" )

   RETURN

STATIC FUNCTION get_button_name( button )

   SWITCH ( button )

   CASE GLFW_MOUSE_BUTTON_LEFT
      RETURN "left"
   CASE GLFW_MOUSE_BUTTON_RIGHT
      RETURN "right"
   CASE GLFW_MOUSE_BUTTON_MIDDLE
      RETURN "middle"
   ENDSWITCH

   RETURN NIL

STATIC FUNCTION get_action_name( action )

   SWITCH ( action )

   CASE GLFW_PRESS
      RETURN "pressed"
   CASE GLFW_RELEASE
      RETURN "released"
   CASE GLFW_REPEAT
      RETURN "repeated"
   ENDSWITCH

   RETURN "caused unknown action"