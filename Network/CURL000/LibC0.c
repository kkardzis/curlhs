/* ------------------------------------------------------------------------- */
/* |                                                                         */
/* Module      :  Network.CURL000.LibC0                                      */
/* Copyright   :  Copyright © 2012-2013 Krzysztof Kardzis                    */
/* License     :  ISC License (MIT/BSD-style, see LICENSE file for details)  */
/*                                                                           */
/* Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>                     */
/* Stability   :  experimental                                               */
/* Portability :  non-portable                                               */
/*                                                                           */
/* ------------------------------------------------------------------------- */

#include <string.h>
#include <stdio.h>

#include "RTLD.h"

#include "curl-7.30.0/curl.h"


/* ------------------------------------------------------------------------- */
/* global symbol table (for run-time linking)                                */
/* ------------------------------------------------------------------------- */
#define SYMTABLEN (sizeof (curlSYMTAB) / sizeof (*curlSYMTAB))

enum { CURL720, CURL721, CURL722, CURL723, CURL724
     , CURL725, CURL726, CURL727, CURL728, CURL729
     , CURL730
     };

#define CURLXXX CURL730

SYMTABENTRY curlSYMTAB[] =
  { {CURL720, CURLXXX, "curl_easy_cleanup"}
  , {CURL720, CURLXXX, "curl_easy_duphandle"}
  , {CURL720, CURLXXX, "curl_easy_escape"}
  , {CURL720, CURLXXX, "curl_easy_getinfo"}
  , {CURL720, CURLXXX, "curl_easy_init"}
  , {CURL720, CURLXXX, "curl_easy_pause"}
  , {CURL720, CURLXXX, "curl_easy_perform"}
  , {CURL720, CURLXXX, "curl_easy_recv"}
  , {CURL720, CURLXXX, "curl_easy_reset"}
  , {CURL720, CURLXXX, "curl_easy_send"}
  , {CURL720, CURLXXX, "curl_easy_setopt"}
  , {CURL720, CURLXXX, "curl_easy_strerror"}
  , {CURL720, CURLXXX, "curl_easy_unescape"}
  , {CURL720, CURLXXX, "curl_escape"}
  , {CURL720, CURLXXX, "curl_formadd"}
  , {CURL720, CURLXXX, "curl_formfree"}
  , {CURL720, CURLXXX, "curl_formget"}
  , {CURL720, CURLXXX, "curl_free"}
  , {CURL720, CURLXXX, "curl_getdate"}
  , {CURL720, CURLXXX, "curl_getenv"}
  , {CURL720, CURLXXX, "curl_global_cleanup"}
  , {CURL720, CURLXXX, "curl_global_init"}
  , {CURL720, CURLXXX, "curl_global_init_mem"}
  , {CURL720, CURLXXX, "curl_maprintf"}
  , {CURL720, CURLXXX, "curl_mfprintf"}
  , {CURL720, CURLXXX, "curl_mprintf"}
  , {CURL720, CURLXXX, "curl_msnprintf"}
  , {CURL720, CURLXXX, "curl_msprintf"}
  , {CURL720, CURLXXX, "curl_multi_add_handle"}
  , {CURL720, CURLXXX, "curl_multi_assign"}
  , {CURL720, CURLXXX, "curl_multi_cleanup"}
  , {CURL720, CURLXXX, "curl_multi_fdset"}
  , {CURL720, CURLXXX, "curl_multi_info_read"}
  , {CURL720, CURLXXX, "curl_multi_init"}
  , {CURL720, CURLXXX, "curl_multi_perform"}
  , {CURL720, CURLXXX, "curl_multi_remove_handle"}
  , {CURL720, CURLXXX, "curl_multi_setopt"}
  , {CURL720, CURLXXX, "curl_multi_socket"}
  , {CURL720, CURLXXX, "curl_multi_socket_action"}
  , {CURL720, CURLXXX, "curl_multi_socket_all"}
  , {CURL720, CURLXXX, "curl_multi_strerror"}
  , {CURL720, CURLXXX, "curl_multi_timeout"}
  , {CURL728, CURLXXX, "curl_multi_wait"}
  , {CURL720, CURLXXX, "curl_mvaprintf"}
  , {CURL720, CURLXXX, "curl_mvfprintf"}
  , {CURL720, CURLXXX, "curl_mvprintf"}
  , {CURL720, CURLXXX, "curl_mvsnprintf"}
  , {CURL720, CURLXXX, "curl_mvsprintf"}
  , {CURL720, CURLXXX, "curl_share_cleanup"}
  , {CURL720, CURLXXX, "curl_share_init"}
  , {CURL720, CURLXXX, "curl_share_setopt"}
  , {CURL720, CURLXXX, "curl_share_strerror"}
  , {CURL720, CURLXXX, "curl_slist_append"}
  , {CURL720, CURLXXX, "curl_slist_free_all"}
  , {CURL720, CURLXXX, "curl_strequal"}
  , {CURL720, CURLXXX, "curl_strnequal"}
  , {CURL720, CURLXXX, "curl_unescape"}
  , {CURL720, CURLXXX, "curl_version"}
  , {CURL720, CURLXXX, "curl_version_info"}
  };

void* curlADRTAB[SYMTABLEN] = {NULL};


/* ------------------------------------------------------------------------- */
/* wrappers for variadic functions                                           */
/* ------------------------------------------------------------------------- */
typedef int (*setoptFP)(void *, int, ...);

int curlOptLong(setoptFP setopt, void *handle, int opt, long val) {
  return setopt(handle, opt, val);
};

int curlOptCOff(setoptFP setopt, void *handle, int opt, curl_off_t val) {
  return setopt(handle, opt, val);
};

int curlOptDPtr(setoptFP setopt, void *handle, int opt, void *val) {
  return setopt(handle, opt, val);
};

int curlOptFPtr(setoptFP setopt, void *handle, int opt, void (*val)()) {
  return setopt(handle, opt, val);
};


/* ------------------------------------------------------------------------- */
/* function import macros (for hsc2hs)                                       */
/* ------------------------------------------------------------------------- */

#ifdef STDCALLCONV
#  define CALLCONV "stdcall"
#else
#  define CALLCONV "ccall"
#endif

#define hsc_FPID(fn)                              \
  { int i; for (i = 0; i < SYMTABLEN; i++) {      \
      if (strcmp(#fn, curlSYMTAB[i].name) == 0) { \
        printf("("); hsc_const(i); printf(")");   \
        break;                                    \
      };                                          \
    };                                            \
  }

#define hsc_SAFECALL(fn,ft...) CALL(fn, TYPE(ft),   safe, ARGS(ft));
#define hsc_FASTCALL(fn,ft...) CALL(fn, TYPE(ft), unsafe, ARGS(ft));

#define CALL(fn, ft, safety, args)                                 \
  printf("\n");                                                    \
  printf("{-# NOINLINE " #fn " #-}\n");                            \
  printf(#fn " :: " str(ft) "\n");                                 \
  printf(#fn " " str(args) " = peekFP "); hsc_FPID(fn);            \
  printf(" >>= \\fp -> " #fn "FC fp " str(args) "\n");             \
  printf("\n");                                                    \
  printf("type FT" #fn " = " str(ft) "\n");                        \
  printf("foreign import " CALLCONV " " #safety " \"dynamic\"\n"); \
  printf("  " #fn "FC :: FunPtr FT" #fn " -> FT" #fn "\n");

#define TYPE(...) ARGSCASE(__VA_ARGS__ \
  , T16(__VA_ARGS__) , T15(__VA_ARGS__) , T14(__VA_ARGS__) , T13(__VA_ARGS__) \
  , T12(__VA_ARGS__) , T11(__VA_ARGS__) , T10(__VA_ARGS__) , T09(__VA_ARGS__) \
  , T08(__VA_ARGS__) , T07(__VA_ARGS__) , T06(__VA_ARGS__) , T05(__VA_ARGS__) \
  , T04(__VA_ARGS__) , T03(__VA_ARGS__) , T02(__VA_ARGS__) , T01(__VA_ARGS__) \
  )

#define ARGSCASE(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,X,...) X

#define T16(a, ...) a -> T15(__VA_ARGS__)
#define T15(a, ...) a -> T14(__VA_ARGS__)
#define T14(a, ...) a -> T13(__VA_ARGS__)
#define T13(a, ...) a -> T12(__VA_ARGS__)
#define T12(a, ...) a -> T11(__VA_ARGS__)
#define T11(a, ...) a -> T10(__VA_ARGS__)
#define T10(a, ...) a -> T09(__VA_ARGS__)
#define T09(a, ...) a -> T08(__VA_ARGS__)
#define T08(a, ...) a -> T07(__VA_ARGS__)
#define T07(a, ...) a -> T06(__VA_ARGS__)
#define T06(a, ...) a -> T05(__VA_ARGS__)
#define T05(a, ...) a -> T04(__VA_ARGS__)
#define T04(a, ...) a -> T03(__VA_ARGS__)
#define T03(a, ...) a -> T02(__VA_ARGS__)
#define T02(a, ...) a -> T01(__VA_ARGS__)
#define T01(a     ) a

#define ARGS(...) ARGSCASE(__VA_ARGS__ \
  , a b c d e f g h i j k l m n o \
  , a b c d e f g h i j k l m n \
  , a b c d e f g h i j k l m \
  , a b c d e f g h i j k l \
  , a b c d e f g h i j k \
  , a b c d e f g h i j \
  , a b c d e f g h i \
  , a b c d e f g h \
  , a b c d e f g \
  , a b c d e f \
  , a b c d e \
  , a b c d \
  , a b c \
  , a b \
  , a \
  , \
  )

#define str(s) #s


/* ------------------------------------------------------------------------- */
/* callback import macros (for hsc2hs)                                       */
/* ------------------------------------------------------------------------- */

#ifdef STDBACKCONV
#  define BACKCONV "stdcall"
#else
#  define BACKCONV "ccall"
#endif

#define hsc_WRAP(fn, ft)                                       \
  printf("\n");                                                \
  printf("type " #fn " = " #ft "\n");                          \
  printf("\n");                                                \
  printf("foreign import " BACKCONV " \"wrapper\"\n");         \
  printf("  wrap" #fn " :: " #fn " -> IO (FunPtr " #fn ")\n");


/* ------------------------------------------------------------------------- */
/* constant import macros (for hsc2hs)                                       */
/* ------------------------------------------------------------------------- */

#define hsc_CURLENUM(type, ...)                               \
  printf("data " #type "\n");                                 \
  { char *x, xs[] = #__VA_ARGS__;                             \
    printf("  = %s\n", strtok(xs,","));                       \
    while ((x=strtok(NULL,",")) != NULL) {                    \
      printf("  |%s\n", x);                                   \
    };                                                        \
  };                                                          \
  printf("\ninstance CURLENUM " #type " where\n");            \
  printf("  enumlist = [ " #__VA_ARGS__ " ]\n");              \
  printf("  toCEnum x = case x of\n");                        \
  { char *x, xs[] = #__VA_ARGS__;                             \
    int i=1, vs[] = {__VA_ARGS__};                            \
    printf("    %s -> %d\n", strtok(xs,","), vs[0]);          \
    while ((x=strtok(NULL,",")) != NULL) {                    \
      printf("   %s -> %d\n", x, vs[i++]);                    \
    };                                                        \
  };                                                          \

