/* ------------------------------------------------------------------------- */
/* |                                                                         */
/* Module      :  Network.CURL000.LibC0                                      */
/* Copyright   :  Copyright (c) 2012-2015 Krzysztof Kardzis                  */
/* License     :  ISC License (MIT/BSD-style, see LICENSE file for details)  */
/*                                                                           */
/* Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>                     */
/* Stability   :  experimental                                               */
/* Portability :  non-portable                                               */
/*                                                                           */
/* ------------------------------------------------------------------------- */

#include "curl-7.30.0/curl.h"

#include "RTLD.h"


/* ------------------------------------------------------------------------- */
/* global symbol table (for run-time linking)                                */
/* ------------------------------------------------------------------------- */
enum { CURL720, CURL730 };

#define CURLXXX CURL730
#define SYMTAB curlSYMTAB

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
/*, {CURL728, CURLXXX, "curl_multi_wait"} */
  , {CURL730, CURLXXX, "curl_multi_wait"}
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

void* curlADRTAB[TABLEN] = {NULL};


/* ------------------------------------------------------------------------- */
/* wrappers for variadic functions                                           */
/* ------------------------------------------------------------------------- */
typedef int (*setoptFP)(void *, int, ...);

int curlOptLong(setoptFP setopt, void *handle, int opt, long val) {
  return setopt(handle, opt, val);
}

int curlOptCOff(setoptFP setopt, void *handle, int opt, curl_off_t val) {
  return setopt(handle, opt, val);
}

int curlOptDPtr(setoptFP setopt, void *handle, int opt, void *val) {
  return setopt(handle, opt, val);
}

int curlOptFPtr(setoptFP setopt, void *handle, int opt, void (*val)()) {
  return setopt(handle, opt, val);
}


/* ------------------------------------------------------------------------- */
#undef hsc_ENUM
#define hsc_ENUM(type, ...)                           \
  printf("data " #type "\n");                         \
  { char *x, xs[] = #__VA_ARGS__;                     \
    printf("  = %s\n", strtok(xs,","));               \
    while ((x=strtok(NULL,",")) != NULL) {            \
      printf("  |%s\n", x);                           \
    };                                                \
  };                                                  \
  printf("\n");                                       \
  printf("instance ENUM " #type " where\n");          \
  printf("  enumlist = [ " #__VA_ARGS__ " ]\n");      \
  printf("  toENUM x = case x of\n");                 \
  { char  *x, xs[] = #__VA_ARGS__;                    \
    unsigned long vs[] = {__VA_ARGS__}; int i=1;      \
    printf("    %s -> %lu\n", strtok(xs,","), vs[0]); \
    while ((x=strtok(NULL,",")) != NULL) {            \
      printf("   %s -> %lu\n", x, vs[i++]);           \
    };                                                \
  };                                                  \

