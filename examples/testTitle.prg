/*
   Test title.
*/

#include "hbglfw.ch"

PROCEDURE Main()

   LOCAL pWindow

   glfwSetErrorCallback( @error_callback() )

   IF ! glfwInit()
      OutStd( e"\nFailed to initialize GLFW" )
      RETURN
   ENDIF

   pWindow := glfwCreateWindow( 600, 400, "Title test press key 1 - 5", NIL, NIL )

   IF pWindow == NIL
      glfwTerminate()
      OutStd( e"\nFailed to open GLFW Window" )
      RETURN
   ENDIF

   glfwMakeContextCurrent( pWindow )
   gladLoadGL_glfwGetProcAddress()
   glfwSwapInterval( 1 )

   glfwSetKeyCallback( pWindow, @key_callback() )

   glClearColor( 0.1, 0.1, 0.1, 0 )

   DO WHILE !glfwWindowShouldClose( pWindow )
      glClear( GL_COLOR_BUFFER_BIT )

      glfwSwapBuffers( pWindow )
      glfwWaitEvents()

   ENDDO

   glfwTerminate()

   OutStd( e"\nFinishing... ;)" )

   RETURN

STATIC PROCEDURE error_callback( nError, cDescription )

   HB_SYMBOL_UNUSED( nError )
   OutStd( e"\nError: ", cDescription )

   RETURN

STATIC PROCEDURE key_callback( pWindow, key, scancode, action, mods )

   IF action != GLFW_PRESS
      RETURN
   ENDIF

   SWITCH ( key )
   CASE GLFW_KEY_1
      glfwSetWindowTitle( pWindow, "Zażółć gęślą jaźń" )
      EXIT

   CASE GLFW_KEY_2
      glfwSetWindowTitle( pWindow, "這是一個中文文本" )
      EXIT

   CASE GLFW_KEY_3
      glfwSetWindowTitle( pWindow, "هذا نص باللغة العربية" )
      EXIT

   CASE GLFW_KEY_4
      glfwSetWindowTitle( pWindow, "Me gusta el español" )
      EXIT

   CASE GLFW_KEY_5
      glfwSetWindowTitle( pWindow, "Man träffas och säger - Tjena."  )
      EXIT

   OTHERWISE
      glfwSetWindowTitle( pWindow, "Title test press key 1 - 5" )
      Scroll()
      SetPos( 0, 0 )
      OutStd( "key      = ", hb_ntos( key ),      e"\n" )
      OutStd( "scancode = ", hb_ntos( scancode ), e"\n" )
      OutStd( "action   = ", hb_ntos( action ),   e"\n" )
      OutStd( "mods     = ", hb_ntos( mods ),     e"\n" )
   ENDSWITCH

   RETURN