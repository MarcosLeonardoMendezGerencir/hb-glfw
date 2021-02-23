/*
 * GLFW library: init
 *
 * Copyright 2019-2020 Rafał Jopek ( rafaljopek at hotmail com )
 *
 */

#include "hbglfw.h"

/* GLFWAPI int glfwInit(void) */
HB_FUNC(GLFWINIT)
{
   hb_retl(glfwInit());
}

/* GLFWAPI void glfwTerminate(void) */
HB_FUNC(GLFWTERMINATE)
{
   glfwTerminate();
}

/* GLFWAPI void glfwInitHint(int hint, int value) */
HB_FUNC(GLFWINITHINT)
{
   if (hb_param(1, HB_IT_INTEGER) != NULL && hb_param(2, HB_IT_INTEGER) != NULL)
   {
      int hint = hb_parni(1);
      int value = hb_parni(2);
      glfwInitHint(hint, value);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLFWAPI void glfwGetVersion(int* major, int* minor, int* rev) */
HB_FUNC(GLFWGETVERSION)
{
   if (HB_ISBYREF(1) && HB_ISBYREF(2) && HB_ISBYREF(3))
   {
      int major, minor, rev;
      glfwGetVersion(&major, &minor, &rev);
      hb_storni(major, 1);
      hb_storni(minor, 2);
      hb_storni(rev, 3);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLFWAPI const char* glfwGetVersionString(void) */
HB_FUNC(GLFWGETVERSIONSTRING)
{
   const char *string = glfwGetVersionString();
   hb_retc(string);
}

/* GLFWAPI int glfwGetError(const char** description) */
HB_FUNC(GLFWGETERROR)
{
}

/* GLFWAPI GLFWerrorfun glfwSetErrorCallback(GLFWerrorfun cbfun) */
HB_FUNC(GLFWSETERRORCALLBACK)
{
}