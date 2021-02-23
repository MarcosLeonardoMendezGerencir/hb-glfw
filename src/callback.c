/*
 * GLFW library: callback
 *
 * Copyright 2019-2020 Rafał Jopek ( rafaljopek at hotmail com )
 *
 */

#include "hbglfw.h"

/* key callback */
static void key_callback( GLFWwindow *window, int key, int scancode, int action, int mods )
{
   PHB_GLFW pItem = glfwGetWindowUserPointer( window );

   if( pItem != NULL && pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_KEY ] )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPush( pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_KEY ] );
         hb_vmPushNil();

         hb_vmPushPointerGC( pItem );
         hb_vmPushInteger( key );
         hb_vmPushInteger( scancode );
         hb_vmPushInteger( action );
         hb_vmPushInteger( mods );

         hb_vmProc( 5 );
      }
   }
}

/* char callback */
static void char_callback( GLFWwindow *window, unsigned int codepoint )
{
   PHB_GLFW pItem = glfwGetWindowUserPointer( window );

   if( pItem != NULL && pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_CHAR ] )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPush( pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_CHAR ] );
         hb_vmPushNil();

         hb_vmPushPointerGC( pItem );
         hb_vmPushInteger( codepoint );

         hb_vmProc( 2 );
      }
   }
}

/* char mods callback */
static void char_mods_callback( GLFWwindow *window, unsigned int codepoint, int mods )
{
   PHB_GLFW pItem = glfwGetWindowUserPointer( window );

   if( pItem != NULL && pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_CHAR_MODS ] )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPush( pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_CHAR_MODS ]) ;
         hb_vmPushNil();

         hb_vmPushPointerGC( pItem );
         hb_vmPushInteger( codepoint );
         hb_vmPushInteger( mods );

         hb_vmProc( 3 );
      }
   }
}

/* mouse button callback */
static void mouse_button_callback( GLFWwindow *window, int button, int action, int mods )
{
   PHB_GLFW pItem = glfwGetWindowUserPointer( window );

   if( pItem != NULL && pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_MOUSE_BUTTON ] )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPush( pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_MOUSE_BUTTON ] );
         hb_vmPushNil();

         hb_vmPushPointerGC( pItem );
         hb_vmPushInteger( button );
         hb_vmPushInteger( action );
         hb_vmPushInteger( mods );

         hb_vmProc( 4 );
      }
   }
}

/* cursor position callback */
static void cursor_position_callback( GLFWwindow *window, double x, double y )
{
   PHB_GLFW pItem = glfwGetWindowUserPointer( window );

   if( pItem != NULL && pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_CURSOR_POSITION ] )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPush( pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_CURSOR_POSITION ] );
         hb_vmPushNil();

         hb_vmPushPointerGC( pItem );
         hb_vmPushDouble( x, HB_DEFAULT_DECIMALS );
         hb_vmPushDouble( y, HB_DEFAULT_DECIMALS );

         hb_vmProc( 3 );
      }
   }
}

/* cursor enter callback */
static void cursor_enter_callback( GLFWwindow *window, int entered )
{
   PHB_GLFW pItem = glfwGetWindowUserPointer( window );

   if( pItem != NULL && pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_CURSOR_ENTER ] )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPush( pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_CURSOR_ENTER ] );
         hb_vmPushNil();

         hb_vmPushPointerGC( pItem) ;
         hb_vmPushInteger( entered );

         hb_vmProc( 2 );
      }
   }
}

/* scroll callback */
static void scroll_callback( GLFWwindow *window, double x, double y )
{
   PHB_GLFW pItem = glfwGetWindowUserPointer( window );

   if( pItem != NULL && pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_SCROLL ] )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPush( pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_SCROLL ] );
         hb_vmPushNil();

         hb_vmPushPointerGC( pItem );
         hb_vmPushDouble( x, HB_DEFAULT_DECIMALS );
         hb_vmPushDouble( y, HB_DEFAULT_DECIMALS );

         hb_vmProc( 3 );
      }
   }
}

/* drop callback */
static void drop_callback( GLFWwindow *window, int count, const char **paths )
{
   PHB_GLFW pGLFWitem = glfwGetWindowUserPointer( window );

   if( pGLFWitem != NULL && pGLFWitem->glfw_window->pCallback[ HBGLFW_CALLBACK_DROP ] )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPush( pGLFWitem->glfw_window->pCallback[ HBGLFW_CALLBACK_DROP ] );
         hb_vmPushNil();

         PHB_ITEM pArray = hb_itemNew( NULL );
         PHB_ITEM pItem = hb_itemNew( NULL );

         if( paths )
         {
            hb_arrayNew( pArray, 0 );

            for( int i = 0; i < count; i++ )
            {
               hb_itemPutC( pItem, paths[ i ] );
               hb_arrayAdd( pArray, pItem );
            }
         }

         hb_vmPushPointerGC( pGLFWitem );
         hb_vmPushInteger( count );
         hb_vmPush( pArray );

         hb_vmProc( 3 );

         hb_itemRelease( pItem );
         hb_itemRelease( pArray );
      }
   }
}

/* window pos callback */
static void window_pos_callback( GLFWwindow *window, int x, int y )
{
   PHB_GLFW pItem = glfwGetWindowUserPointer( window );

   if( pItem != NULL && pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_WINDOW_POS ] )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPush( pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_WINDOW_POS ] );
         hb_vmPushNil();

         hb_vmPushPointerGC( pItem );
         hb_vmPushInteger( x );
         hb_vmPushInteger( y );

         hb_vmProc( 3 );
      }
   }
}

/* window size callback */
static void window_size_callback( GLFWwindow *window, int width, int height )
{
   PHB_GLFW pItem = glfwGetWindowUserPointer( window );

   if( pItem != NULL && pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_WINDOW_SIZE ] )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPush( pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_WINDOW_SIZE ] );
         hb_vmPushNil();

         hb_vmPushPointerGC( pItem );
         hb_vmPushInteger( width );
         hb_vmPushInteger( height );

         hb_vmProc( 3 );
      }
   }
}

/* window close callback */
static void window_close_callback( GLFWwindow *window )
{
   PHB_GLFW pItem = glfwGetWindowUserPointer( window );

   if( pItem != NULL && pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_WINDOW_CLOSE ] )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPush( pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_WINDOW_CLOSE ] );
         hb_vmPushNil();

         hb_vmPushPointerGC( pItem );

         hb_vmProc( 1 );
      }
   }
}

/* window refresh callback */
static void window_refresh_callback( GLFWwindow *window )
{
   PHB_GLFW pItem = glfwGetWindowUserPointer( window );

   if( pItem != NULL && pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_WINDOW_REFRESH ] )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPush( pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_WINDOW_REFRESH ] );
         hb_vmPushNil();

         hb_vmPushPointerGC( pItem );

         hb_vmProc( 1 );
      }
   }
}

/* window focus callback */
static void window_focus_callback( GLFWwindow *window, int focused )
{
   PHB_GLFW pItem = glfwGetWindowUserPointer( window );

   if( pItem != NULL && pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_WINDOW_FOCUS ] )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPush( pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_WINDOW_FOCUS ] );
         hb_vmPushNil();

         hb_vmPushPointerGC( pItem );
         hb_vmPushInteger( focused );

         hb_vmProc( 2 );
      }
   }
}

/* window iconify callback */
static void window_iconify_callback( GLFWwindow *window, int iconified )
{
   PHB_GLFW pItem = glfwGetWindowUserPointer( window );

   if( pItem != NULL && pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_WINDOW_ICONIFY ] )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPush( pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_WINDOW_ICONIFY ] );
         hb_vmPushNil();

         hb_vmPushPointerGC( pItem );
         hb_vmPushInteger( iconified );

         hb_vmProc( 2 );
      }
   }
}

/* window maximize callback */
static void window_maximize_callback( GLFWwindow *window, int maximized )
{
   PHB_GLFW pItem = glfwGetWindowUserPointer( window );

   if( pItem != NULL && pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_WINDOW_MAXIMIZE ])
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPush( pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_WINDOW_MAXIMIZE ] );
         hb_vmPushNil();

         hb_vmPushPointerGC( pItem );
         hb_vmPushInteger( maximized );

         hb_vmProc( 2 );
      }
   }
}

/* framebuffer size callback */
static void framebuffer_size_callback( GLFWwindow *window, int width, int height )
{
   PHB_GLFW pItem = glfwGetWindowUserPointer( window );

   if( pItem != NULL && pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_FRAMEBUFFER_SIZE ] )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPush( pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_FRAMEBUFFER_SIZE ] );
         hb_vmPushNil();

         hb_vmPushPointerGC( pItem );
         hb_vmPushInteger( width );
         hb_vmPushInteger( height );

         hb_vmProc( 3 );
      }
   }
}

/* window content scale callback */
static void window_content_scale_callback( GLFWwindow *window, float xscale, float yscale )
{
   PHB_GLFW pItem = glfwGetWindowUserPointer( window );

   if( pItem != NULL && pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_WINDOW_CONTENT_SCALE ] )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPush( pItem->glfw_window->pCallback[ HBGLFW_CALLBACK_WINDOW_CONTENT_SCALE ] );
         hb_vmPushNil();

         hb_vmPushPointerGC( pItem );
         hb_vmPushDouble( xscale, HB_DEFAULT_DECIMALS );
         hb_vmPushDouble( yscale, HB_DEFAULT_DECIMALS );

         hb_vmProc( 3 );
      }
   }
}

void hb_setWindowCallback( HBGLFW_CALLBACK callback_type )
{
   PHB_GLFW phb = hbglfw_param( 1, HBGLFW_TYPE_WINDOW );
   PHB_ITEM pCallback = hb_param( 2, HB_IT_SYMBOL );
   PHB_SYMB pSymbol = NULL;

   if( pCallback )
   {
      pSymbol = hb_itemGetSymbol( pCallback );
      if( !pSymbol->value.pFunPtr )
      {
         pSymbol = NULL;
      }
   }

   if ( phb && pSymbol )
   {

      switch( callback_type )
      {
      case HBGLFW_CALLBACK_KEY:
         glfwSetKeyCallback( phb->glfw_window->window, key_callback );
         break;
      case HBGLFW_CALLBACK_CHAR:
         glfwSetCharCallback( phb->glfw_window->window, char_callback );
         break;
      case HBGLFW_CALLBACK_CHAR_MODS:
         glfwSetCharModsCallback( phb->glfw_window->window, char_mods_callback );
         break;
      case HBGLFW_CALLBACK_MOUSE_BUTTON:
         glfwSetMouseButtonCallback( phb->glfw_window->window, mouse_button_callback );
         break;
      case HBGLFW_CALLBACK_CURSOR_POSITION:
         glfwSetCursorPosCallback( phb->glfw_window->window, cursor_position_callback );
         break;
      case HBGLFW_CALLBACK_CURSOR_ENTER:
         glfwSetCursorEnterCallback( phb->glfw_window->window, cursor_enter_callback );
         break;
      case HBGLFW_CALLBACK_SCROLL:
         glfwSetScrollCallback( phb->glfw_window->window, scroll_callback );
         break;
      case HBGLFW_CALLBACK_DROP:
         glfwSetDropCallback( phb->glfw_window->window, drop_callback );
         break;
      case HBGLFW_CALLBACK_WINDOW_CLOSE:
         glfwSetWindowCloseCallback( phb->glfw_window->window, window_close_callback );
         break;
      case HBGLFW_CALLBACK_WINDOW_POS:
         glfwSetWindowPosCallback( phb->glfw_window->window, window_pos_callback );
         break;
      case HBGLFW_CALLBACK_WINDOW_SIZE:
         glfwSetWindowSizeCallback( phb->glfw_window->window, window_size_callback );
         break;
      case HBGLFW_CALLBACK_WINDOW_REFRESH:
         glfwSetWindowRefreshCallback( phb->glfw_window->window, window_refresh_callback );
         break;
      case HBGLFW_CALLBACK_WINDOW_FOCUS:
         glfwSetWindowFocusCallback( phb->glfw_window->window, window_focus_callback );
         break;
      case HBGLFW_CALLBACK_WINDOW_ICONIFY:
         glfwSetWindowIconifyCallback( phb->glfw_window->window, window_iconify_callback );
         break;
      case HBGLFW_CALLBACK_WINDOW_MAXIMIZE:
         glfwSetWindowMaximizeCallback( phb->glfw_window->window, window_maximize_callback );
         break;
      case HBGLFW_CALLBACK_FRAMEBUFFER_SIZE:
         glfwSetFramebufferSizeCallback( phb->glfw_window->window, framebuffer_size_callback );
         break;
      case HBGLFW_CALLBACK_WINDOW_CONTENT_SCALE:
         glfwSetWindowContentScaleCallback( phb->glfw_window->window, window_content_scale_callback );
         break;
      default:
         return;
      }

      if( phb->glfw_window->pCallback[ callback_type ] )
      {
         hb_itemRelease(phb->glfw_window->pCallback[ callback_type ] );
      }

      phb->glfw_window->pCallback[ callback_type ] = hb_itemNew( pCallback );

      if( !glfwGetWindowUserPointer( phb->glfw_window->window ) )
      {
         glfwSetWindowUserPointer( phb->glfw_window->window, phb );
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}