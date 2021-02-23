#include "hbglfw.ch"

FUNCTION Main()

   LOCAL window

   /* Initialize the library */
   IF ! glfwInit()
      RETURN -1
   ENDIF

   /* Create a windowed mode window and its OpenGL context */
   window := glfwCreateWindow( 640, 480, "Hello World, Harbour is power", NIL, NIL )

   IF window == NIL
      glfwTerminate()
      RETURN -1
   ENDIF

   /* Make the window's context current */
   glfwMakeContextCurrent( window )
   gladLoadGL_glfwGetProcAddress()

   /* Loop until the user closes the window */
   DO WHILE ! glfwWindowShouldClose( window )

      /* Render here */
      glClear( GL_COLOR_BUFFER_BIT )

      /* Swap front and back buffers */
      glfwSwapBuffers( window )

      /* Poll for and process events */
      glfwPollEvents()

   ENDDO

   glfwTerminate()

   RETURN 0