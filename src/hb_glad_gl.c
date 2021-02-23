/*
 * GLFW library: hb glad gl
 *
 * Copyright 2019-2020 Rafał Jopek ( rafaljopek at hotmail com )
 *
 */

#include "hbglfw.h"

HB_FUNC(GLADLOADGL_GLFWGETPROCADDRESS)
{
   hb_retni(gladLoadGL(glfwGetProcAddress));
}