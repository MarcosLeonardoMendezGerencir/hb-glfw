
#include "hbglfw.ch"

PROCEDURE Main()

   LOCAL pWindow
   LOCAL nWidth, nHeight
   LOCAL nMaxWidth := 0, nMaxHeight := 0
   LOCAL x, y, z

   glfwSetErrorCallback( @error_callback() )

   IF ! glfwInit()
      OutStd( e"\nFailed to initialize GLFW" )
      RETURN
   ENDIF

   pWindow := glfwCreateWindow( 640, 480, "Prosty przykład ", NIL, NIL )

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

         glEnable( GL_CULL_FACE )
         glCullFace( GL_BACK )

         glMatrixMode( GL_PROJECTION )
         glLoadIdentity()
         glFrustum( - 2, 2, - 1.5, 1.5, 1, 40 )

         glMatrixMode( GL_MODELVIEW )
         glLoadIdentity()
         glTranslatef( 0, 0, - 3 )
         glRotatef( 50, 1, 0, 0 )
         glRotatef( 70, 0, 1, 0 )

         nMaxWidth := nWidth
         nMaxHeight := nHeight
         OutStd( nWidth, nHeight, e"\n" )

      ENDIF

      glBegin( GL_POINTS )

      FOR x := -1 TO 1 STEP 0.25
         FOR y := -1 TO 1 STEP 0.25
            FOR z := -1 TO 1 STEP 0.25

               glColor3f( ( x + 1 ) / 2, ( y + 1 ) / 2, 1 - ( y + 1 ) / 2 )
               glVertex3f( x, y, z )

            NEXT
         NEXT
      NEXT

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