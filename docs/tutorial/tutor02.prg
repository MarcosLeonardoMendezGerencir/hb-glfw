#include "hbglfw.ch"

FUNCTION Main()

   LOCAL pWindow

   IF ! glfwInit()
      OutStd( e"\nError" )
      RETURN -1
   ENDIF

   glfwWindowHint( GLFW_CONTEXT_VERSION_MAJOR, 3 )
   glfwWindowHint( GLFW_CONTEXT_VERSION_MINOR, 3 )
   glfwWindowHint( GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE )

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

   DO WHILE ! glfwWindowShouldClose( pWindow )

      IF ( glfwGetKey( pWindow, GLFW_KEY_ESCAPE ) == GLFW_PRESS )
         glfwSetWindowShouldClose( pWindow, GLFW_TRUE )
      ENDIF

      glClearColor( 0.3, 0.3, 0.3, 1.0 )
      glClear( GL_COLOR_BUFFER_BIT )

      glfwSwapBuffers( pWindow )
      glfwPollEvents()

   ENDDO

   glfwTerminate()

   RETURN 0