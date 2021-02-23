#include "hbglfw.ch"

FUNCTION Main()

   LOCAL pWindow
   LOCAL lContinue := .T.

   IF ! glfwInit()
      OutStd( e"\nError" )
      RETURN -1
   ENDIF

   pWindow := glfwCreateWindow( 800, 600, "Learn OpenGL, GLFW and Harbour", NIL, NIL )

   IF pWindow == NIL
      OutStd( e"\nFailed to create GLFW Window" )
      glfwTerminate()
      RETURN -1
   ENDIF

   glfwMakeContextCurrent( pWindow )
   gladLoadGL_glfwGetProcAddress()
   glfwSwapInterval( 1 )

   glViewport( 0, 0, 800, 600 )

   DO WHILE ! lContinue

      IF glfwWindowShouldClose( pWindow )
         lContinue = .F.
      ENDIF

      glfwSwapBuffers( pWindow )
      glfwPollEvents()

   ENDDO

   glfwTerminate()

   RETURN 0