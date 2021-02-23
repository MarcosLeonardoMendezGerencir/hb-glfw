
#include "hbglfw.ch"

PROCEDURE Main()

   LOCAL pWindow
   LOCAL nWidth, nHeight
   LOCAL nMaxWidth := 0, nMaxHeight := 0
   LOCAL x, y
   LOCAL cDirection := SubStr( "udlr", hb_randInt( 1, 4 ), 1 )
   LOCAL nTime := hb_MilliSeconds()
   LOCAL red := 1
   LOCAL green := 1
   LOCAL blue := 1

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
         glOrtho( 0, nWidth, nHeight, 0, -1, 1 )
         glMatrixMode( GL_MODELVIEW )
         glPointSize( 1 )

         x := nWidth / 2
         y := nHeight / 2
         nMaxWidth  := nWidth
         nMaxHeight := nHeight
         OutStd( x, y, e"\n" )

      ENDIF

      glBegin( GL_POINTS )
      glColor3f( red, green, blue )
      glVertex2i( x, y )
      glEnd()

      glFlush()

      SWITCH cDirection
      CASE "u"
         y := iif( y < 0, nHeight, y -1 )
         EXIT
      CASE "d"
         y := iif( y > nHeight, -1, y + 1 )
         EXIT
      CASE "l"
         x := iif( x < 0, nWidth, x - 1 )
         EXIT
      CASE "r"
         x := iif( x > nWidth, -1, x + 1 )
         EXIT
      ENDSWITCH

      IF hb_MilliSeconds() - nTime > hb_randInt( 1000, 10000 )
         SWITCH cDirection
         CASE "u"
            cDirection := SubStr( "udlr", hb_randInt( 3, 4 ), 1 )
            EXIT
         CASE "d"
            cDirection := SubStr( "udlr", hb_randInt( 3, 4 ), 1 )
            EXIT
         CASE "l"
            cDirection := SubStr( "udlr", hb_randInt( 1, 2 ), 1 )
            EXIT
         CASE "r"
            cDirection := SubStr( "udlr", hb_randInt( 1, 2 ), 1 )
            EXIT
         ENDSWITCH

         nTime := hb_MilliSeconds()

      ENDIF

      IF ( y < 0 .OR. y > nHeight .OR. x < 0 .OR. x > nWidth )
         red   := hb_randNum( 0.1, 1 )
         green := hb_randNum( 0.1, 1 )
         blue  := hb_randNum( 0.1, 1 )
      ENDIF

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

STATIC PROCEDURE key_callback( pWindows, key, scancode, action, mods )

   HB_SYMBOL_UNUSED( scancode )
   HB_SYMBOL_UNUSED( mods )

   IF key == GLFW_KEY_ESCAPE .AND. action == GLFW_PRESS
      glfwSetWindowShouldClose( pWindows, GLFW_TRUE )
   ENDIF

   RETURN