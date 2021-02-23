/*
 * GLFW library: monitor
 *
 * Copyright 2019-2020 Rafał Jopek ( rafaljopek at hotmail com )
 *
 * Native access
 *
 * The available window API macros are:
 *
 * GLFW_EXPOSE_NATIVE_WIN32
 * GLFW_EXPOSE_NATIVE_COCOA
 * GLFW_EXPOSE_NATIVE_X11
 * GLFW_EXPOSE_NATIVE_WAYLAND
 *
 * The available context API macros are:
 *
 * GLFW_EXPOSE_NATIVE_WGL
 * GLFW_EXPOSE_NATIVE_NSGL
 * GLFW_EXPOSE_NATIVE_GLX
 * GLFW_EXPOSE_NATIVE_EGL
 * GLFW_EXPOSE_NATIVE_OSMESA
 *
 */

#include "hbglfw.h"

// GLFWAPI const char* glfwGetWin32Adapter( GLFWmonitor* monitor );
HB_FUNC( GLFWGETWIN32ADAPTER )
{
#if defined( GLFW_EXPOSE_NATIVE_WIN32 )
#endif
}

// GLFWAPI const char* glfwGetWin32Monitor( GLFWmonitor* monitor );
HB_FUNC( GLFWGETWIN32MONITOR )
{
#if defined( GLFW_EXPOSE_NATIVE_WIN32 )
#endif
}

// GLFWAPI HWND glfwGetWin32Window( GLFWwindow* window );
HB_FUNC( GLFWGETWIN32WINDOW )
{
#if defined( GLFW_EXPOSE_NATIVE_WIN32 )
#endif
}

// GLFWAPI HGLRC glfwGetWGLContext( GLFWwindow* window );
HB_FUNC( GLFWGETWGLCONTEXT )
{
#if defined( GLFW_EXPOSE_NATIVE_WGL )
#endif
}

// GLFWAPI CGDirectDisplayID glfwGetCocoaMonitor( GLFWmonitor* monitor );
HB_FUNC( GLFWGETCOCOAMONITOR )
{
#if defined( GLFW_EXPOSE_NATIVE_COCOA )
#endif
}

// GLFWAPI id glfwGetCocoaWindow( GLFWwindow* window );
HB_FUNC( GLFWGETCOCOAWINDOW )
{
#if defined( GLFW_EXPOSE_NATIVE_COCOA )
#endif
}

// GLFWAPI id glfwGetNSGLContext( GLFWwindow* window );
HB_FUNC( GLFWGETNSGLCONTEXT )
{
#if defined( GLFW_EXPOSE_NATIVE_NSGL )
#endif
}

// GLFWAPI Display* glfwGetX11Display( void );
HB_FUNC( GLFWGETX11DISPLAY )
{
#if defined( GLFW_EXPOSE_NATIVE_X11 )
#endif
}

// GLFWAPI RRCrtc glfwGetX11Adapter( GLFWmonitor* monitor );
HB_FUNC( GLFWGETX11ADAPTER )
{
#if defined( GLFW_EXPOSE_NATIVE_X11 )
#endif
}

// GLFWAPI RROutput glfwGetX11Monitor( GLFWmonitor* monitor );
HB_FUNC( GLFWGETX11MONITOR )
{
#if defined( GLFW_EXPOSE_NATIVE_X11 )
#endif
}
// GLFWAPI Window glfwGetX11Window( GLFWwindow* window );
HB_FUNC( GLFWGETX11WINDOW )
{
#if defined( GLFW_EXPOSE_NATIVE_X11 )
#endif
}
// GLFWAPI void glfwSetX11SelectionString( const char* string );
HB_FUNC( GLFWSETX11SELECTIONSTRING )
{
#if defined( GLFW_EXPOSE_NATIVE_X11 )
#endif
}
// GLFWAPI const char* glfwGetX11SelectionString( void );
HB_FUNC( GLFWGETX11SELECTIONSTRING )
{
#if defined( GLFW_EXPOSE_NATIVE_X11 )
#endif
}
// GLFWAPI GLXContext glfwGetGLXContext( GLFWwindow* window );
HB_FUNC( GLFWGETGLXCONTEXT )
{
#if defined( GLFW_EXPOSE_NATIVE_GLX )
#endif
}
// GLFWAPI GLXWindow glfwGetGLXWindow( GLFWwindow* window );
HB_FUNC( GLFWGETGLXWINDOW )
{
#if defined( GLFW_EXPOSE_NATIVE_GLX )
#endif
}
// GLFWAPI struct wl_display* glfwGetWaylandDisplay( void );
HB_FUNC( GLFWGETWAYLANDDISPLAY )
{
#if defined( GLFW_EXPOSE_NATIVE_WAYLAND )
#endif
}
// GLFWAPI struct wl_output* glfwGetWaylandMonitor( GLFWmonitor* monitor );
HB_FUNC( GLFWGETWAYLANDMONITOR )
{
#if defined( GLFW_EXPOSE_NATIVE_WAYLAND )
#endif
}
// GLFWAPI struct wl_surface* glfwGetWaylandWindow( GLFWwindow* window );
HB_FUNC( GLFWGETWAYLANDWINDOW )
{
#if defined( GLFW_EXPOSE_NATIVE_WAYLAND )
#endif
}
// GLFWAPI EGLDisplay glfwGetEGLDisplay( void );
HB_FUNC( GLFWGETEGLDISPLAY )
{
#if defined( GLFW_EXPOSE_NATIVE_EGL )
#endif
}
// GLFWAPI EGLContext glfwGetEGLContext( GLFWwindow* window );
HB_FUNC( GLFWGETEGLCONTEXT )
{
#if defined( GLFW_EXPOSE_NATIVE_EGL )
#endif
}
// GLFWAPI EGLSurface glfwGetEGLSurface( GLFWwindow* window );
HB_FUNC( GLFWGETEGLSURFACE )
{
#if defined( GLFW_EXPOSE_NATIVE_EGL )
#endif
}
// GLFWAPI int glfwGetOSMesaColorBuffer( GLFWwindow* window, int* width, int* height, int* format, void** buffer );
HB_FUNC( GLFWGETOSMESACOLORBUFFER )
{
#if defined( GLFW_EXPOSE_NATIVE_OSMESA )
#endif
}
// GLFWAPI int glfwGetOSMesaDepthBuffer( GLFWwindow* window, int* width, int* height, int* bytesPerValue, void** buffer );
HB_FUNC( GLFWGETOSMESADEPTHBUFFER )
{
#if defined( GLFW_EXPOSE_NATIVE_OSMESA )
#endif
}
// GLFWAPI OSMesaContext glfwGetOSMesaContext( GLFWwindow* window );
HB_FUNC( GLFWGETOSMESACONTEXT )
{
#if defined( GLFW_EXPOSE_NATIVE_OSMESA )
#endif
}