/*
 * GLFW library: monitor
 *
 * Copyright 2019-2020 Rafał Jopek ( rafaljopek at hotmail com )
 *
 */

#include "hbglfw.h"

/* GLFWAPI GLFWmonitor** glfwGetMonitors(int* count) */
HB_FUNC(GLFWGETMONITORS)
{
}

/* GLFWAPI GLFWmonitor* glfwGetPrimaryMonitor(void) */
HB_FUNC(GLFWGETPRIMARYMONITOR)
{
}

/* GLFWAPI void glfwGetMonitorPos(GLFWmonitor* handle, int* xpos, int* ypos) */
HB_FUNC(GLFWGETMONITORPOS)
{
}

/* GLFWAPI void glfwGetMonitorWorkarea(GLFWmonitor* handle, int* xpos, int* ypos, int* width, int* height) */
HB_FUNC(GLFWGETMONITORWORKAREA)
{
}

/* GLFWAPI void glfwGetMonitorPhysicalSize(GLFWmonitor* handle, int* widthMM, int* heightMM) */
HB_FUNC(GLFWGETMONITORPHYSICALSIZE)
{
}

/* GLFWAPI void glfwGetMonitorContentScale(GLFWmonitor* handle, float* xscale, float* yscale) */
HB_FUNC(GLFWGETMONITORCONTENTSCALE)
{
}

/* GLFWAPI const char* glfwGetMonitorName(GLFWmonitor* handle) */
HB_FUNC(GLFWGETMONITORNAME)
{
}

/* GLFWAPI void glfwSetMonitorUserPointer(GLFWmonitor* handle, void* pointer) */
HB_FUNC(GLFWSETMONITORUSERPOINTER)
{
}

/* GLFWAPI void* glfwGetMonitorUserPointer(GLFWmonitor* handle) */
HB_FUNC(GLFWGETMONITORUSERPOINTER)
{
}

/* GLFWAPI GLFWmonitorfun glfwSetMonitorCallback(GLFWmonitorfun cbfun) */
HB_FUNC(GLFWSETMONITORCALLBACK)
{
}

/* GLFWAPI const GLFWvidmode* glfwGetVideoModes(GLFWmonitor* handle, int* count) */
HB_FUNC(GLFWGETVIDEOMODES)
{
}

/* GLFWAPI const GLFWvidmode* glfwGetVideoMode(GLFWmonitor* handle) */
HB_FUNC(GLFWGETVIDEOMODE)
{
}

/* GLFWAPI void glfwSetGamma(GLFWmonitor* handle, float gamma) */
HB_FUNC(GLFWSETGAMMA)
{
}

/* GLFWAPI const GLFWgammaramp* glfwGetGammaRamp(GLFWmonitor* handle) */
HB_FUNC(GLFWGETGAMMARAMP)
{
}

/* GLFWAPI void glfwSetGammaRamp(GLFWmonitor* handle, const GLFWgammaramp* ramp) */
HB_FUNC(GLFWSETGAMMARAMP)
{
}