/*
 * GLFW library: GLAPI
 *
 * Copyright 2019-2020 Rafał Jopek ( rafaljopek at hotmail com )
 *
 */

/*
   GLAPI
   https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/index.php

*/

#include "hbglfw.h"

/* void glActiveTexture(GLenum texture) */
HB_FUNC(GLACTIVETEXTURE)
{
   if (HB_ISNUM(1))
   {
      GLenum texture = hb_parni(1);
      glActiveTexture(texture);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glAttachShader(GLuint program, GLuint shader) */
HB_FUNC(GLATTACHSHADER)
{
   if (HB_ISNUM(1) && HB_ISNUM(2))
   {
      GLuint program = hb_parni(1);
      GLuint shader = hb_parni(2);
      glAttachShader(program, shader);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glBegin(GLenum mode) */
HB_FUNC(GLBEGIN)
{
   if (HB_ISNUM(1))
   {
      GLenum mode = hb_parni(1);
      glBegin(mode);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glBindBuffer(GLenum target, GLuint buffer) */
HB_FUNC(GLBINDBUFFER)
{
   if (HB_ISNUM(1) && HB_ISNUM(2))
   {
      GLenum target = hb_parni(1);
      GLuint buffer = hb_parni(2);
      glBindBuffer(target, buffer);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glBindTexture(GLenum target, GLuint texture) */
HB_FUNC(GLBINDTEXTURE)
{
   if (HB_ISNUM(1) && HB_ISNUM(2))
   {
      GLenum target = hb_parni(1);
      GLuint texture = hb_parni(2);
      glBindTexture(target, texture);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glBindVertexArray(GLuint array) */
HB_FUNC(GLBINDVERTEXARRAY)
{
   if (HB_ISNUM(1))
   {
      GLuint array = hb_parni(1);
      glBindVertexArray(array);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glClear(GLbitfield mask) */
HB_FUNC(GLCLEAR)
{
   if (HB_ISNUM(1))
   {
      GLbitfield mask = hb_parni(1);
      glClear(mask);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glClearColor(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha) */
HB_FUNC(GLCLEARCOLOR)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4))
   {
      glClearColor((float)hb_parnd(1), (float)hb_parnd(2), (float)hb_parnd(3), (float)hb_parnd(4));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glClearDepth(GLclampd depth) */
HB_FUNC(GLCLEARDEPTH)
{
   if (HB_ISNUM(1))
   {
      GLclampd depth = hb_parnd(1);
      glClearDepth(depth);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glColor3f(GLfloat red, GLfloat green, GLfloat blue) */
HB_FUNC(GLCOLOR3F)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3))
   {
      glColor3f((float)hb_parnd(1), (float)hb_parnd(2), (float)hb_parnd(3));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLuint glCreateProgram(void) */
HB_FUNC(GLCREATEPROGRAM)
{
   hb_retni(glCreateProgram());
}

/* GLuint glCreateShader(GLenum type) */
HB_FUNC(GLCREATESHADER)
{
   if (HB_ISNUM(1))
   {
      GLenum type = hb_parni(1);
      hb_retni(glCreateShader(type));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glCompileShader(GLuint shader) */
HB_FUNC(GLCOMPILESHADER)
{
   if (HB_ISNUM(1))
   {
      GLuint shader = hb_parni(1);
      glCompileShader(shader);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glCullFace(GLenum mode) */
HB_FUNC(GLCULLFACE)
{
   if (HB_ISNUM(1))
   {
      GLenum mode = hb_parni(1);
      glCullFace(mode);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}
/* void glDeleteShader(GLuint shader) */
HB_FUNC(GLDELETESHADER)
{
   if (HB_ISNUM(1))
   {
      GLuint shader = hb_parni(1);
      glDeleteShader(shader);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glDepthFunc(GLenum func) */
HB_FUNC(GLDEPTHFUNC)
{
   if (HB_ISNUM(1))
   {
      GLenum func = hb_parni(1);
      glDepthFunc(func);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glDisable(GLenum cap) */
HB_FUNC(GLDISABLE)
{
   if (HB_ISNUM(1))
   {
      GLenum cap = hb_parni(1);
      glDisable(cap);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glDisableClientState(GLenum cap) */
HB_FUNC(GLDISABLECLIENTSTATE)
{
   if (HB_ISNUM(1))
   {
      GLenum cap = hb_parni(1);
      glDisableClientState(cap);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glDrawArrays(GLenum mode, GLint first, GLsizei count) */
HB_FUNC(GLDRAWARRAYS)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3))
   {
      GLenum mode = hb_parni(1);
      GLint first = hb_parni(2);
      GLsizei count = hb_parni(3);
      glDrawArrays(mode, first, count);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glEnable(GLenum cap) */
HB_FUNC(GLENABLE)
{
   if (HB_ISNUM(1))
   {
      GLenum cap = hb_parni(1);
      glEnable(cap);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glEnableClientState(GLenum cap) */
HB_FUNC(GLENABLECLIENTSTATE)
{
   if (HB_ISNUM(1))
   {
      GLenum cap = hb_parni(1);
      glEnableClientState(cap);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glEnableVertexAttribArray(GLuint index) */
HB_FUNC(GLENABLEVERTEXATTRIBARRAY)
{
   if (HB_ISNUM(1))
   {
      GLuint index = hb_parni(1);
      glEnableVertexAttribArray(index);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glEnd(void) */
HB_FUNC(GLEND)
{
   glEnd();
}

/* void glFlush(void) */
HB_FUNC(GLFLUSH)
{
   glFlush();
}

/* void glFogi(GLenum pname, GLint param ) */
HB_FUNC(GLFOGI)
{
   if (HB_ISNUM(1) && HB_ISNUM(2))
   {
      GLenum pname = hb_parni(1);
      GLint param = hb_parni(2);
      glFogi(pname, param);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glFogf(GLenum pname, GLfloat param) */
HB_FUNC(GLFOGF)
{
   if (HB_ISNUM(1) && HB_ISNUM(2))
   {
      GLenum pname = hb_parni(1);
      glFogf(pname, (float)hb_parnd(2));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glFrontFace( GLenum mode ) */
HB_FUNC(GLFRONTFACE)
{
   if (HB_ISNUM(1))
   {
      GLenum mode = hb_parni(1);
      glFrontFace(mode);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glFrustum( GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble nearVal, GLdouble farVal ) */
HB_FUNC(GLFRUSTUM)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5) && HB_ISNUM(6))
   {
      glFrustum((float)hb_parnd(1), (float)hb_parnd(2), (float)hb_parnd(3), (float)hb_parnd(4), (float)hb_parnd(5), (float)hb_parnd(6));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glGenBuffers(GLsizei n, GLuint *buffers) */
HB_FUNC(GLGENBUFFERS)
{
   if (HB_ISNUM(1) && HB_ISBYREF(2))
   {
      GLuint buffers;
      glGenBuffers(hb_parni(1), &buffers);
      hb_storni(buffers, 2);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glGetFloatv(GLenum pname, GLfloat *data) */
HB_FUNC(GLGETFLOATV)
{
   if (HB_ISNUM(1) && HB_ISBYREF(2))
   {
      GLenum pname = hb_parni(1);
      GLfloat data;
      glGetFloatv(pname, &data);
      hb_stornd(data, 2);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glGenTextures(GLsizei n, GLuint *textures) */
HB_FUNC(GLGENTEXTURES)
{
   if (HB_ISNUM(1) && HB_ISBYREF(2))
   {
      GLsizei n = hb_parni(1);
      GLuint textures;
      glGenTextures(n, &textures);
      hb_storni(textures, 2);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glGenVertexArrays(GLsizei n, GLuint *arrays) */
HB_FUNC(GLGENVERTEXARRAYS)
{
   if (HB_ISNUM(1) && HB_ISARRAY(2))
   {
      GLuint i;
      glGenVertexArrays(hb_parni(1), &i);
      hb_storni(i, 2);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLint glGetAttribLocation(GLuint program, const GLchar *name) */
HB_FUNC(GLGETATTRIBLOCATION)
{
   if (HB_ISNUM(1) && HB_ISCHAR(2))
   {
      GLuint program = hb_parni(1);
      const GLchar *name = hb_parc(2);
      hb_retni(glGetAttribLocation(program, name));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* GLint glGetUniformLocation(GLuint program, const GLchar *name) */
HB_FUNC(GLGETUNIFORMLOCATION)
{
   if (HB_ISNUM(1) && HB_ISCHAR(2))
   {
      GLuint program = hb_parni(1);
      const GLchar *name = hb_parc(2);
      hb_retni(glGetUniformLocation(program, name));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void APIENTRY glHint(GLenum target,GLenum mode) */
HB_FUNC(GLHINT)
{
   if (HB_ISNUM(1) && HB_ISNUM(2))
   {
      GLenum target = hb_parni(1);
      GLenum mode = hb_parni(2);
      glHint(target, mode);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glLinkProgram(GLuint program) */
HB_FUNC(GLLINKPROGRAM)
{
   if (HB_ISNUM(1))
   {
      GLuint program = hb_parni(1);
      glLinkProgram(program);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glLoadIdentity(void) */
HB_FUNC(GLLOADIDENTITY)
{
   glLoadIdentity();
}

/* void glMatrixMode(GLenum mode) */
HB_FUNC(GLMATRIXMODE)
{
   if (HB_ISNUM(1))
   {
      GLenum mode = hb_parni(1);
      glMatrixMode(mode);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glOrtho(GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar) */
HB_FUNC(GLORTHO)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4) && HB_ISNUM(5) && HB_ISNUM(6))
   {

      GLdouble left = hb_parnd(1);
      GLdouble right = hb_parnd(2);
      GLdouble bottom = hb_parnd(3);
      GLdouble top = hb_parnd(4);
      GLdouble zNear = hb_parnd(5);
      GLdouble zFar = hb_parnd(6);
      glOrtho(left, right, bottom, top, zNear, zFar);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glPointSize(GLfloat size) */
HB_FUNC(GLPOINTSIZE)
{
   if (HB_ISNUM(1))
   {
      glPointSize((float)hb_parnd(1));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glPolygonMode(GLenum face, GLenum mode) */
HB_FUNC(GLPOLYGONMODE)
{
   if (HB_ISNUM(1) && HB_ISNUM(2))
   {
      GLenum face = hb_parni(1);
      GLenum mode = hb_parni(2);
      glPolygonMode(face, mode);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glPushMatrix(void) */
HB_FUNC(GLPUSHMATRIX)
{
   glPushMatrix();
}

/* void glRectd( GLdouble x1, GLdouble y1, GLdouble x2, GLdouble y2 ) */
HB_FUNC(GLRECTD)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4))
   {
      GLdouble x1 = hb_parnd(1);
      GLdouble y1 = hb_parnd(2);
      GLdouble x2 = hb_parnd(3);
      GLdouble y2 = hb_parnd(4);
      glRectd(x1, y1, x2, y2);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glRectf( GLfloat x1, GLfloat y1, GLfloat x2, GLfloat y2 ) */
HB_FUNC(GLRECTF)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4))
   {
      glRectf((float)hb_parnd(1), (float)hb_parnd(2), (float)hb_parnd(3), (float)hb_parnd(4));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glRecti( GLint x1, GLint y1, GLint x2, GLint y2 ) */
HB_FUNC(GLRECTI)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4))
   {
      GLint x1 = hb_parni(1);
      GLint y1 = hb_parni(2);
      GLint x2 = hb_parni(3);
      GLint y2 = hb_parni(4);
      glRecti(x1, y1, x2, y2);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glRects( GLshort x1, GLshort y1, GLshort x2, GLshort y2 ) */

/* void glRotatef(GLfloat angle, GLfloat x, GLfloat y, GLfloat z) */
HB_FUNC(GLROTATEF)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4))
   {
      glRotatef((float)hb_parnd(1), (float)hb_parnd(2), (float)hb_parnd(3), (float)hb_parnd(4));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glShadeModel(GLenum mode) */
HB_FUNC(GLSHADEMODEL)
{
   if (HB_ISNUM(1))
   {
      GLenum mode = hb_parni(1);
      glShadeModel(mode);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glShaderSource(GLuint shader, GLsizei count, const GLchar *const*string, const GLint *length) */
HB_FUNC(GLSHADERSOURCE)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISCHAR(3))
   {
      GLuint shader = hb_parni(1);
      GLsizei count = hb_parni(2);
      const char *string = hb_parc(3);
      glShaderSource(shader, count, &string, NULL);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glTexParameteri(GLenum target, GLenum pname, GLint param) */
HB_FUNC(GLTEXPARAMETERI)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3))
   {
      GLenum target = hb_parni(1);
      GLenum pname = hb_parni(2);
      GLint param = hb_parni(3);
      glTexParameteri(target, pname, param);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glTranslatef(GLfloat x, GLfloat y, GLfloat z) */
HB_FUNC(GLTRANSLATEF)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3))
   {
      glTranslatef((float)hb_parnd(1), (float)hb_parnd(2), (float)hb_parnd(3));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glUseProgram(GLuint program) */
HB_FUNC(GLUSEPROGRAM)
{
   if (HB_ISNUM(1))
   {
      GLuint program = hb_parni(1);
      glUseProgram(program);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glVertex2s(GLshort x, GLshort y) */
HB_FUNC(GLVERTEX2S)
{
   if (HB_ISNUM(1) && HB_ISNUM(2))
   {
      glVertex2s((short)hb_parni(1), (short)hb_parni(2));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glVertex2i(GLint x, GLint y) */
HB_FUNC(GLVERTEX2I)
{
   if (HB_ISNUM(1) && HB_ISNUM(2))
   {
      glVertex2i(hb_parni(1), hb_parni(2));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glVertex2f(GLfloat x, GLfloat y) */
HB_FUNC(GLVERTEX2F)
{
   if (HB_ISNUM(1) && HB_ISNUM(2))
   {
      glVertex2f((float)hb_parnd(1), (float)hb_parnd(2));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glVertex2d(GLdouble x, GLdouble y) */
HB_FUNC(GLVERTEX2D)
{
   if (HB_ISNUM(1) && HB_ISNUM(2))
   {
      glVertex2d(hb_parnd(1), hb_parnd(2));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glVertex3s(GLshort x, GLshort y, GLshort z) */
HB_FUNC(GLVERTEX3S)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3))
   {
      glVertex3s((short)hb_parni(1), (short)hb_parni(2), (short)hb_parni(3));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glVertex3i(GLint x, GLint y, GLint z) */
HB_FUNC(GLVERTEX3I)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3))
   {
      glVertex3i(hb_parni(1), hb_parni(2), hb_parni(3));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glVertex3f(GLfloat x, GLfloat y, GLfloat z) */
HB_FUNC(GLVERTEX3F)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3))
   {
      glVertex3f((float)hb_parnd(1), (float)hb_parnd(2), (float)hb_parnd(3));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glVertex3d(GLdouble x, GLdouble y, GLdouble z) */
HB_FUNC(GLVERTEX3D)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3))
   {
      glVertex3d(hb_parnd(1), hb_parnd(2), hb_parnd(3));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glVertex4s(GLshort x, GLshort y, GLshort z, GLshort w) */
HB_FUNC(GLVERTEX4S)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4))
   {
      glVertex4s((short)hb_parni(1), (short)hb_parni(2), (short)hb_parni(3), (short)hb_parni(4));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glVertex4i(GLint x, GLint y, GLint z, GLint w) */
HB_FUNC(GLVERTEX4I)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4))
   {
      glVertex4i(hb_parni(1), hb_parni(2), hb_parni(3), hb_parni(4));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glVertex4f(GLfloat x, GLfloat y, GLfloat z, GLfloat w) */
HB_FUNC(GLVERTEX4F)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4))
   {
      glVertex4f((float)hb_parnd(1), (float)hb_parnd(2), (float)hb_parnd(3), (float)hb_parnd(4));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glVertex4d(GLdouble x, GLdouble y, GLdouble z, GLdouble w) */
HB_FUNC(GLVERTEX4d)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4))
   {
      glVertex4d(hb_parnd(1), hb_parnd(2), hb_parnd(3), hb_parnd(4));
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}

/* void glViewport(GLint x, GLint y, GLsizei width, GLsizei height) */
HB_FUNC(GLVIEWPORT)
{
   if (HB_ISNUM(1) && HB_ISNUM(2) && HB_ISNUM(3) && HB_ISNUM(4))
   {
      GLint x = hb_parni(1);
      GLint y = hb_parni(2);
      GLsizei width = hb_parni(3);
      GLsizei height = hb_parni(4);
      glViewport(x, y, width, height);
   }
   else
   {
      hb_errRT_BASE_SubstR(EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS);
   }
}