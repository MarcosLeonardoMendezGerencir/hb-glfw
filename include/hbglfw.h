#ifndef HBGLFW_H_
#define HBGLFW_H_

#include <stdbool.h>

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbvm.h"

#include <glad/gl.h>
#include <GLFW/glfw3.h>

typedef enum
{
   HBGLFW_TYPE_WINDOW,
   HBGLFW_TYPE_MONITOR,
   HBGLFW_TYPE_DISPLAY
} HBGLFW_TYPE;

typedef enum
{
   HBGLFW_CALLBACK_KEY,
   HBGLFW_CALLBACK_CHAR,
   HBGLFW_CALLBACK_CHAR_MODS,
   HBGLFW_CALLBACK_MOUSE_BUTTON,
   HBGLFW_CALLBACK_CURSOR_POSITION,
   HBGLFW_CALLBACK_CURSOR_ENTER,
   HBGLFW_CALLBACK_SCROLL,
   HBGLFW_CALLBACK_DROP,
   HBGLFW_CALLBACK_WINDOW_POS,
   HBGLFW_CALLBACK_WINDOW_SIZE,
   HBGLFW_CALLBACK_WINDOW_CLOSE,
   HBGLFW_CALLBACK_WINDOW_REFRESH,
   HBGLFW_CALLBACK_WINDOW_FOCUS,
   HBGLFW_CALLBACK_WINDOW_ICONIFY,
   HBGLFW_CALLBACK_WINDOW_MAXIMIZE,
   HBGLFW_CALLBACK_FRAMEBUFFER_SIZE,
   HBGLFW_CALLBACK_WINDOW_CONTENT_SCALE,
   SIZE_OF_CALLBACK
} HBGLFW_CALLBACK;

typedef struct _HB_GLFW_WINDOW
{
   GLFWwindow *window;
   PHB_ITEM pCallback[ SIZE_OF_CALLBACK ];
} HBGLFW_WINDOW, *PHBGLFW_WINDOW;

typedef struct _HB_GLFW
{
   HBGLFW_TYPE type;
   PHBGLFW_WINDOW glfw_window;
   GLFWmonitor *monitor;
} HB_GLFW, *PHB_GLFW;

PHB_GLFW hbglfw_dataContainer( HBGLFW_TYPE type, void *p );
PHB_GLFW hbglfw_param( int iParam, HBGLFW_TYPE type );

void hb_setWindowCallback( HBGLFW_CALLBACK callback_type );

#endif /* HBGLFW_H_ */