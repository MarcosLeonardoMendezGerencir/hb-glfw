
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
   glfwSwapInterval( 1 )

   glfwSetKeyCallback( pWindow, @key_callback() )

   DO WHILE ! glfwWindowShouldClose( pWindow )

      glfwGetFramebufferSize( pWindow, @nWidth, @nHeight )

      glViewport( 0, 0, nWidth, nHeight )
      glClear( GL_COLOR_BUFFER_BIT )

      glMatrixMode( GL_PROJECTION )
      glLoadIdentity()

      glOrtho( - 1000, 1000, - 1000 * ( nWidth / nHeight ), 1000 * ( nWidth / nHeight ), - 1000, 1000 )
      glMatrixMode( GL_MODELVIEW )

      glBegin( GL_QUAD_STRIP )
      /* Pierwszy czworokąt */
      glColor3f (   1,    0,    0 )
      glVertex3f( - 200, 500, - 600 )
      glVertex3f( - 200, 700, - 600 )
      glVertex3f(    0, 400, - 900 )
      glVertex3f(    0, 800, - 900 )
      /* Drugi czworokąt */
      glColor3f (   0,   1,    0 )
      glVertex3f( 600,   0, - 300 )
      glVertex3f( 600, 300, - 300 )
      /* Trzeci czworokąt */
      glColor3f (   0,   1,    0 )
      glVertex3f( 200,   0, - 200 )
      glVertex3f( 200, 200, - 200 )
      /* Czwarty czworokąt */
      glColor3f (    0,    0,   1 )
      glVertex3f( - 100, - 100, 200 )
      glVertex3f( - 100,  300, 200 )
      /* Piąty czworokąt */
      glColor3f (    1,   0,   0 )
      glVertex3f( - 600,   0,   0 )
      glVertex3f( - 600, 400,   0 )
      glEnd()
      glFlush()

      glfwSwapBuffers( pWindow )
      glfwPollEvents()

   ENDDO

   glfwDestroyWindow( pWindow )
   glfwTerminate()

   OutStd( e"\nFinishing... " )

   RETURN

STATIC PROCEDURE error_callback( nError, cDescription )

   HB_SYMBOL_UNUSED( nError )
   OutStd( e"\nError: ", cDescription )

   RETURN

STATIC PROCEDURE key_callback( pWindow, KEY, scancode, action, mods )

   HB_SYMBOL_UNUSED( scancode )
   HB_SYMBOL_UNUSED( mods )

   DO CASE
   CASE KEY == GLFW_KEY_ESCAPE .AND. action == GLFW_PRESS
      glfwSetWindowShouldClose( pWindow, GLFW_TRUE )

   CASE KEY == GLFW_KEY_1
      glPolygonMode( GL_FRONT_AND_BACK, GL_POINT )

   CASE KEY == GLFW_KEY_2
      glPolygonMode( GL_FRONT_AND_BACK, GL_LINE )

   CASE KEY == GLFW_KEY_3
      glPolygonMode( GL_FRONT_AND_BACK, GL_FILL )

   ENDCASE

   RETURN