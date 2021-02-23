/*
 * GLFW library: core
 *
 * Copyright 2019-2020 Rafał Jopek ( rafaljopek at hotmail com )
 *
 */

#include "hbglfw.h"

/* Object destructor, it's executed automatically */
static HB_GARBAGE_FUNC(hb_glfw_destructor)
{
   /* Retrieve object pointer holder */
   PHB_GLFW phb = Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if (phb)
   {
      /* Destroy the object */
      switch (phb->type)
      {
      case HBGLFW_TYPE_WINDOW:
         if (phb->glfw_window)
         {
            if (phb->glfw_window->window)
            {
               glfwDestroyWindow(phb->glfw_window->window);
            }
            for (size_t i = 0; i < SIZE_OF_CALLBACK; i++)
            {
               if (phb->glfw_window->pCallback[i])
               {
                  hb_itemRelease(phb->glfw_window->pCallback[i]);
               }
            }
            free(phb->glfw_window);
            phb->glfw_window = NULL;
         }
         break;
      case HBGLFW_TYPE_MONITOR:
         if (phb->monitor)
         {
            phb->monitor = NULL;
         }
         break;
      default:
         break;
      }
   }
}

static const HB_GC_FUNCS s_gc_glfw_funcs = {
    hb_glfw_destructor,
    hb_gcDummyMark};

PHB_GLFW hbglfw_dataContainer(HBGLFW_TYPE type, void *p)
{
   if (p)
   {
      PHB_GLFW phb = hb_gcAllocate(sizeof(HB_GLFW), &s_gc_glfw_funcs);
      phb->type = type;

      switch (type)
      {
      case HBGLFW_TYPE_WINDOW:
         phb->glfw_window = malloc(sizeof(HBGLFW_WINDOW));
         phb->glfw_window->window = p;
         for (size_t i = 0; i < SIZE_OF_CALLBACK; i++)
         {
            phb->glfw_window->pCallback[i] = NULL;
         }
         break;
      case HBGLFW_TYPE_MONITOR:
         phb->monitor = p;
         break;
      default:
         break;
      }
      return phb;
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
   return NULL;
}

PHB_GLFW hbglfw_param(int iParam, HBGLFW_TYPE type)
{
   PHB_GLFW phb = hb_parptrGC(&s_gc_glfw_funcs, iParam);

   if (phb && phb->type == type)
   {
      switch (type)
      {
      case HBGLFW_TYPE_WINDOW:
         if (phb->glfw_window->window)
         {
            return phb;
         }
         break;
      case HBGLFW_TYPE_MONITOR:
         if (phb->monitor)
         {
            return phb;
         }
      default:
         break;
      }
   }
   return NULL;
}