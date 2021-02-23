
#include "hbglfw.ch"

PROCEDURE Main()

   LOCAL pWindow
   LOCAL nWidth, nHeight
   LOCAL nMaxWidth := 0, nMaxHeight := 0

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

      IF nMaxWidth != nWidth .OR. nMaxHeight != nHeight

         glViewport( 0, 0, nWidth, nHeight )
         glClear( GL_COLOR_BUFFER_BIT )

         glMatrixMode( GL_PROJECTION )
         glLoadIdentity()
         glOrtho( 0, nWidth, nHeight, 0, - 1, 1 )
         glMatrixMode( GL_MODELVIEW )
         glPointSize( 1 )

         nMaxWidth := nWidth
         nMaxHeight := nHeight
         OutStd( nWidth, nHeight, e"\n" )

      ENDIF

      glBegin( GL_POINTS )
      glColor3f( hb_randNum( 0.0, 1 ), hb_randNum( 0.0, 1 ), hb_randNum( 0.0, 1 ) )
      glVertex2i( hb_randInt( nWidth ), hb_randInt( nHeight ) )
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

   IF KEY == GLFW_KEY_ESCAPE .AND. action == GLFW_PRESS
      glfwSetWindowShouldClose( pWindow, GLFW_TRUE )
   ENDIF

   RETURN