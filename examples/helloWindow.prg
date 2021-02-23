#include "hbglfw.ch"

STATIC nRed := 0.1, nGreen := 0.1, nBlue := 0.1, nAlpha := 0.0

PROCEDURE Main()

   LOCAL pWindow

   // glfw: initialize and configure
   // ------------------------------
   IF ! glfwInit()
      OutStd( e"\nFailed to initialize GLFW" )
      RETURN
   ENDIF

   // glfw window creation
   // --------------------
   pWindow := glfwCreateWindow( 800, 600, "LearnOpenGL", NIL, NIL )

   IF pWindow == NIL
      OutStd( "Failed to create GLFW window" )
      glfwTerminate()
      RETURN
   ENDIF

   glfwMakeContextCurrent( pWindow )

   // glad: load all OpenGL function pointers
   // ---------------------------------------
   gladLoadGL_glfwGetProcAddress()

   glfwSetKeyCallback( pWindow, @key_callback() )
   glfwSetFramebufferSizeCallback( pWindow, @framebuffer_size_callback() )

   // render loop
   // -----------
   DO WHILE ! glfwWindowShouldClose( pWindow )

      // render
      // ------
      glClearColor( nRed, nGreen, nBlue, nAlpha )
      glClear( GL_COLOR_BUFFER_BIT )


      // glfw: swap buffers and poll IO events (keys pressed/released, mouse moved etc.)
      // -------------------------------------------------------------------------------
      glfwSwapBuffers( pWindow )
      glfwPollEvents()

   ENDDO

   // glfw: terminate, clearing all previously allocated GLFW resources.
   // ------------------------------------------------------------------
   glfwTerminate()

   RETURN

// This is the function signature for keyboard key callback functions.
// process all input: query GLFW whether relevant keys are pressed/released this frame and react accordingly
// ---------------------------------------------------------------------------------------------------------
STATIC PROCEDURE key_callback( pWindow, nKey, scancode, action, mods )

   IF action != GLFW_PRESS
      RETURN
   ENDIF

   SWITCH ( nKey )
   CASE GLFW_KEY_ESCAPE
      glfwSetWindowShouldClose( pWindow, GLFW_TRUE )
      EXIT
   OTHERWISE
      OutStd( nKey, scancode, action, mods )
   ENDSWITCH

   RETURN

// This is the function signature for framebuffer resize callback functions.
// glfw: whenever the window size changed (by OS or user resize) this callback function executes
// ---------------------------------------------------------------------------------------------
STATIC PROCEDURE framebuffer_size_callback( pWindow, width, height )

   HB_SYMBOL_UNUSED( pWindow )

   // make sure the viewport matches the new window dimensions; note that width and
   // height will be significantly larger than specified on retina displays.
   glViewport( 0, 0, width, height )

   RETURN