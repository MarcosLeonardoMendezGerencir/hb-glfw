
#include "hbglfw.ch"

STATIC aStarField := {}

PROCEDURE Main()

   LOCAL pWindow
   LOCAL nWidth, nHeight
   LOCAL nMaxWidth := 0, nMaxHeight := 0
   LOCAL i

   // LOCAL aStarField := {}
   LOCAL nMilliSeconds := hb_MilliSeconds()

   glfwSetErrorCallback( @error_callback() )

   IF ! glfwInit()
      OutStd( e"\nFailed to initialize GLFW" )
      RETURN
   ENDIF

   pWindow := glfwCreateWindow( 640, 480, "Prosty przykład - star field", NIL, NIL )

   IF pWindow == NIL
      glfwTerminate()
      OutStd( e"\nFailed to open GLFW Window" )
      RETURN
   ENDIF

   glfwMakeContextCurrent( pWindow )
   gladLoadGL_glfwGetProcAddress()
   glfwSwapInterval( 1 )

   glfwSetKeyCallback( pWindow, @key_callback() )

   glfwMaximizeWindow( pWindow )

   DO WHILE ! glfwWindowShouldClose( pWindow )

      glfwGetFramebufferSize( pWindow, @nWidth, @nHeight )
      glClear( GL_COLOR_BUFFER_BIT )

      IF nMaxWidth != nWidth .OR. nMaxHeight != nHeight

         FOR i := 1 TO 200
            AAdd( aStarField, { hb_randInt( 0, nWidth ), hb_randInt( 0, nHeight ), hb_randInt( 1, 9 ) } )
         NEXT

         glViewport( 0, 0, nWidth, nHeight )
         glMatrixMode( GL_PROJECTION )
         glLoadIdentity()
         glOrtho( 0, nWidth, nHeight, 0, -1, 1 )
         glMatrixMode( GL_MODELVIEW )
         glPointSize( 1 )

         nMaxWidth := nWidth
         nMaxHeight := nHeight

      ENDIF

      glBegin( GL_POINTS )
      glColor3f( 1, 1, 1 )

      FOR i := 1 TO Len( aStarField )

         glVertex2i( aStarField[ i ][ 1 ], aStarField[ i ][ 2 ] )

         DO CASE
         CASE aStarField[ i ][ 3 ] == 1 .OR.  aStarField[ i ][ 3 ] == 2 .OR. ;
               aStarField[ i ][ 3 ] == 3 .OR. aStarField[ i ][ 3 ] == 4

            aStarField[ i ][ 1 ] += 0.5

         CASE aStarField[ i ][ 3 ] == 6
            aStarField[ i ][ 1 ] += 1.5

         OTHERWISE
            aStarField[ i ][ 1 ]++
            
         ENDCASE

         IF aStarField[ i ][ 2 ] > nMaxWidth
            hb_ADel( aStarField, i, .T. )
         ENDIF

      NEXT

      IF hb_MilliSeconds() - nMilliSeconds > 70
         hb_AIns( aStarField, 1, { 0, hb_randInt( 0, nMaxHeight ), hb_randInt( 1, 9 ) }, .T. )
         nMilliSeconds := hb_MilliSeconds()
      ENDIF

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

STATIC PROCEDURE key_callback( pWindow, key, scancode, action, mods )

   HB_SYMBOL_UNUSED( scancode )
   HB_SYMBOL_UNUSED( mods )

   IF key == GLFW_KEY_ESCAPE .AND. action == GLFW_PRESS
      glfwSetWindowShouldClose( pWindow, GLFW_TRUE )
   ENDIF

   IF key == GLFW_KEY_SPACE .AND. action == GLFW_PRESS
      glfwSetWindowTitle( pWindow, Str( Len( aStarField ) ) )
   ENDIF

   RETURN