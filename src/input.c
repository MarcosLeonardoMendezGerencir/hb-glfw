/*
 * GLFW library: input
 *
 * Copyright 2019-2020 Rafał Jopek ( rafaljopek at hotmail com )
 *
 */

#include "hbglfw.h"

/* GLFWAPI int glfwGetInputMode(GLFWwindow* handle, int mode) */
HB_FUNC(GLFWGETINPUTMODE)
{
   PHB_GLFW phb = hbglfw_param(1, HBGLFW_TYPE_WINDOW);

   if (phb && hb_param(2, HB_IT_INTEGER) != NULL)
   {
      int mode = hb_parni(2);
      int result = glfwGetInputMode(phb->glfw_window->window, mode);
      hb_retni(result);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLFWAPI void glfwSetInputMode(GLFWwindow* handle, int mode, int value) */
HB_FUNC(GLFWSETINPUTMODE)
{
   PHB_GLFW phb = hbglfw_param(1, HBGLFW_TYPE_WINDOW);

   if (phb && hb_param(2, HB_IT_INTEGER) != NULL && hb_param(3, HB_IT_INTEGER) != NULL)
   {
      int mode = hb_parni(2);
      int value = hb_parni(3);
      glfwSetInputMode(phb->glfw_window->window, mode, value);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLFWAPI int glfwRawMouseMotionSupported(void) */
HB_FUNC(GLFWRAWMOUSEMOTIONSUPPORTED)
{
   int result = glfwRawMouseMotionSupported();
   hb_retl(result);
}

/* GLFWAPI const char* glfwGetKeyName(int key, int scancode) */
HB_FUNC(GLFWGETKEYNAME)
{
   if (hb_param(1, HB_IT_INTEGER) != NULL && hb_param(2, HB_IT_INTEGER) != NULL)
   {
      int key = hb_parni(1);
      int scancode = hb_parni(2);
      const char *string = glfwGetKeyName(key, scancode);
      hb_retc(string);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLFWAPI int glfwGetKeyScancode(int key) */
HB_FUNC(GLFWGETKEYSCANCODE)
{
   if (hb_param(1, HB_IT_INTEGER) != NULL)
   {
      int key = hb_parni(1);
      int result = glfwGetKeyScancode(key);
      hb_retni(result);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLFWAPI int glfwGetKey(GLFWwindow* handle, int key) */
HB_FUNC(GLFWGETKEY)
{
   PHB_GLFW phb = hbglfw_param(1, HBGLFW_TYPE_WINDOW);

   if (phb && hb_param(2, HB_IT_INTEGER) != NULL)
   {
      int key = hb_parni(2);
      int result = glfwGetKey(phb->glfw_window->window, key);
      hb_retni(result);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLFWAPI int glfwGetMouseButton(GLFWwindow* handle, int button) */
HB_FUNC(GLFWGETMOUSEBUTTON)
{
   PHB_GLFW phb = hbglfw_param(1, HBGLFW_TYPE_WINDOW);

   if (phb && hb_param(2, HB_IT_INTEGER) != NULL)
   {
      int button = hb_parni(2);
      int result = glfwGetMouseButton(phb->glfw_window->window, button);
      hb_retni(result);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLFWAPI void glfwGetCursorPos(GLFWwindow* handle, double* xpos, double* ypos) */
HB_FUNC(GLFWGETCURSORPOS)
{
   PHB_GLFW phb = hbglfw_param(1, HBGLFW_TYPE_WINDOW);

   if (phb && hb_param(2, HB_IT_DOUBLE) != NULL && hb_param(3, HB_IT_DOUBLE) != NULL)
   {
      double xpos, ypos;
      glfwGetCursorPos(phb->glfw_window->window, &xpos, &ypos);
      hb_stornd(xpos, 2);
      hb_stornd(ypos, 3);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLFWAPI void glfwSetCursorPos(GLFWwindow* handle, double xpos, double ypos) */
HB_FUNC(GLFWSETCURSORPOS)
{
   PHB_GLFW phb = hbglfw_param(1, HBGLFW_TYPE_WINDOW);

   if (phb && hb_param(2, HB_IT_DOUBLE) != NULL && hb_param(3, HB_IT_DOUBLE) != NULL)
   {
      double xpos = hb_parnd(2);
      double ypos = hb_parnd(3);
      glfwSetCursorPos(phb->glfw_window->window, xpos, ypos);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLFWAPI GLFWcursor* glfwCreateCursor(const GLFWimage* image, int xhot, int yhot) */
HB_FUNC(GLFWCREATECURSOR)
{
}

/* GLFWAPI GLFWcursor* glfwCreateStandardCursor(int shape) */
HB_FUNC(GLFWCREATESTANDARDCURSOR)
{
}

/* GLFWAPI void glfwDestroyCursor(GLFWcursor* handle) */
HB_FUNC(GLFWDESTROYCURSOR)
{
}

/* GLFWAPI void glfwSetCursor(GLFWwindow* windowHandle, GLFWcursor* cursorHandle) */
HB_FUNC(GLFWSETCURSOR)
{
}

/* GLFWAPI GLFWkeyfun glfwSetKeyCallback(GLFWwindow* handle, GLFWkeyfun cbfun) */
HB_FUNC(GLFWSETKEYCALLBACK)
{
   hb_setWindowCallback(HBGLFW_CALLBACK_KEY);
}

/* GLFWAPI GLFWcharfun glfwSetCharCallback(GLFWwindow* handle, GLFWcharfun cbfun) */
HB_FUNC(GLFWSETCHARCALLBACK)
{
   hb_setWindowCallback(HBGLFW_CALLBACK_CHAR);
}

/* GLFWAPI GLFWcharmodsfun glfwSetCharModsCallback(GLFWwindow* handle, GLFWcharmodsfun cbfun) */
HB_FUNC(GLFWSETCHARMODSCALLBACK)
{
   hb_setWindowCallback(HBGLFW_CALLBACK_CHAR_MODS);
}

/* GLFWAPI GLFWmousebuttonfun glfwSetMouseButtonCallback(GLFWwindow* handle, GLFWmousebuttonfun cbfun) */
HB_FUNC(GLFWSETMOUSEBUTTONCALLBACK)
{
   hb_setWindowCallback(HBGLFW_CALLBACK_MOUSE_BUTTON);
}

/* GLFWAPI GLFWcursorposfun glfwSetCursorPosCallback(GLFWwindow* handle, GLFWcursorposfun cbfun) */
HB_FUNC(GLFWSETCURSORPOSCALLBACK)
{
   hb_setWindowCallback(HBGLFW_CALLBACK_CURSOR_POSITION);
}

/* GLFWAPI GLFWcursorenterfun glfwSetCursorEnterCallback(GLFWwindow* handle, GLFWcursorenterfun cbfun) */
HB_FUNC(GLFWSETCURSORENTERCALLBACK)
{
   hb_setWindowCallback(HBGLFW_CALLBACK_CURSOR_ENTER);
}

/* GLFWAPI GLFWscrollfun glfwSetScrollCallback(GLFWwindow* handle, GLFWscrollfun cbfun) */
HB_FUNC(GLFWSETSCROLLCALLBACK)
{
   hb_setWindowCallback(HBGLFW_CALLBACK_SCROLL);
}

/* GLFWAPI GLFWdropfun glfwSetDropCallback(GLFWwindow* handle, GLFWdropfun cbfun) */
HB_FUNC(GLFWSETDROPCALLBACK)
{
   hb_setWindowCallback(HBGLFW_CALLBACK_DROP);
}

/* GLFWAPI int glfwJoystickPresent(int jid) */
HB_FUNC(GLFWJOYSTICKPRESENT)
{
   if (hb_param(1, HB_IT_INTEGER) != NULL)
   {
      int jid = hb_parni(1);
      int result = glfwJoystickPresent(jid);
      hb_retl(result);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLFWAPI const float* glfwGetJoystickAxes(int jid, int* count) */
HB_FUNC(GLFWGETJOYSTICKAXES)
{
}

/* GLFWAPI const unsigned char* glfwGetJoystickButtons(int jid, int* count) */
HB_FUNC(GLFWGETJOYSTICKBUTTONS)
{
}

/* GLFWAPI const unsigned char* glfwGetJoystickHats(int jid, int* count) */
HB_FUNC(GLFWGETJOYSTICKHATS)
{
}

/* GLFWAPI const char* glfwGetJoystickName(int jid) */
HB_FUNC(GLFWGETJOYSTICKNAME)
{
   if (hb_param(1, HB_IT_INTEGER) != NULL)
   {
      int jid = hb_parni(1);
      const char *string = glfwGetJoystickName(jid);
      hb_retc(string);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLFWAPI const char* glfwGetJoystickGUID(int jid) */
HB_FUNC(GLFWGETJOYSTICKGUID)
{
   if (hb_param(1, HB_IT_INTEGER) != NULL)
   {
      int jid = hb_parni(1);
      const char *string = glfwGetJoystickGUID(jid);
      hb_retc(string);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLFWAPI void glfwSetJoystickUserPointer(int jid, void* pointer) */
HB_FUNC(GLFWSETJOYSTICKUSERPOINTER)
{
}

/* GLFWAPI void* glfwGetJoystickUserPointer(int jid) */
HB_FUNC(GLFWGETJOYSTICKUSERPOINTER)
{
}

/* GLFWAPI GLFWjoystickfun glfwSetJoystickCallback(GLFWjoystickfun cbfun) */
HB_FUNC(GLFWSETJOYSTICKCALLBACK)
{
}

/* GLFWAPI int glfwUpdateGamepadMappings(const char* string) */
HB_FUNC(GLFWUPDATEGAMEPADMAPPINGS)
{
   if (hb_param(1, HB_IT_STRING) != NULL)
   {
      const char *string = hb_parc(1);
      int result = glfwUpdateGamepadMappings(string);
      hb_retl(result);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLFWAPI int glfwJoystickIsGamepad(int jid) */
HB_FUNC(GLFWJOYSTICKISGAMEPAD)
{
   if (hb_param(1, HB_IT_INTEGER) != NULL)
   {
      int jid = hb_parni(1);
      int result = glfwJoystickIsGamepad(jid);
      hb_retl(result);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLFWAPI const char* glfwGetGamepadName(int jid) */
HB_FUNC(GLFWGETGAMEPADNAME)
{
   if (hb_param(1, HB_IT_INTEGER) != NULL)
   {
      int jid = hb_parni(1);
      const char *string = glfwGetGamepadName(jid);
      hb_retc(string);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLFWAPI int glfwGetGamepadState(int jid, GLFWgamepadstate* state) */
HB_FUNC(GLFWGETGAMEPADSTATE)
{
}

/* GLFWAPI void glfwSetClipboardString(GLFWwindow* handle, const char* string) */
HB_FUNC(GLFWSETCLIPBOARDSTRING)
{
   PHB_GLFW phb = hbglfw_param(1, HBGLFW_TYPE_WINDOW);

   if (hb_param(2, HB_IT_STRING))
   {
      GLFWwindow *window = phb ? phb->glfw_window->window : NULL;
      const char *string = hb_parc(2);
      glfwSetClipboardString(window, string);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLFWAPI const char* glfwGetClipboardString(GLFWwindow* handle) */
HB_FUNC(GLFWGETCLIPBOARDSTRING)
{
   PHB_GLFW phb = hbglfw_param(1, HBGLFW_TYPE_WINDOW);

   GLFWwindow *window = phb ? phb->glfw_window->window : NULL;
   const char *string = glfwGetClipboardString(window);
   hb_retc(string);
}

/* GLFWAPI double glfwGetTime(void) */
HB_FUNC(GLFWGETTIME)
{
   hb_retnd(glfwGetTime());
}

/* GLFWAPI void glfwSetTime(double time) */
HB_FUNC(GLFWSETTIME)
{
   if (hb_param(1, HB_IT_INTEGER) != NULL)
   {
      double time = hb_parnd(1);
      glfwSetTime(time);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLFWAPI uint64_t glfwGetTimerValue(void) */
HB_FUNC(GLFWGETTIMERVALUE)
{
   HB_LONGLONG result = glfwGetTimerValue();
   hb_retnll(result);
}

/* GLFWAPI uint64_t glfwGetTimerFrequency(void) */
HB_FUNC(GLFWGETTIMERFREQUENCY)
{
   HB_LONGLONG result = glfwGetTimerFrequency();
   hb_retnll(result);
}