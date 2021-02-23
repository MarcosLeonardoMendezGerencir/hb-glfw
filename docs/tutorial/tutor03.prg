
#include "hbglfw.ch"

PROCEDURE Main()

   LOCAL pWindow
   LOCAL nRatio
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
   glfwSetKeyCallback( pWindow, @key_callback() )

   DO WHILE ! glfwWindowShouldClose( pWindow )

      glfwGetFramebufferSize( pWindow, @nWidth, @nHeight )
      nRatio := nWidth / nHeight

      glViewport( 0, 0, nWidth, nHeight )
      glClear( GL_COLOR_BUFFER_BIT )

      glMatrixMode( GL_PROJECTION )
      glLoadIdentity()
      glOrtho( - nRatio, nRatio, - 1, 1, 1, - 1 )
      glMatrixMode( GL_MODELVIEW )

      glLoadIdentity()
      glRotatef( glfwGetTime() * 50, 0, 0, 1 )

      glBegin( GL_TRIANGLES )
      glColor3f( 1, 0, 0 )
      glVertex3f( - 0.6, - 0.4, 0 )
      glColor3f( 0, 1, 0 )
      glVertex3f( 0.6, - 0.4, 0 )
      glColor3f( 0, 0, 1 )
      glVertex3f( 0, 0.6, 0 )
      glEnd()

      glfwSwapBuffers( pWindow )
      glfwPollEvents()

   ENDDO

   glfwDestroyWindow( pWindow )
   glfwTerminate()

   OutStd( e"\nFinishing... ;)" )

   RETURN

STATIC PROCEDURE error_callback( nError, cDescription )

   HB_SYMBOL_UNUSED( nError )
   OutStd( e"\nError: ", cDescription )

   RETURN

STATIC PROCEDURE key_callback( pWindow, KEY, scancode, action, mods )

   HB_SYMBOL_UNUSED( scancode )
   HB_SYMBOL_UNUSED( mods )

   IF KEY == GLFW_KEY_ESCAPE .AND. action == GLFW_PRESS
      glfwSetWindowShouldClose( pWindow, GLFW_TRUE )
   ENDIF

   RETURN