/*
 * Iconv binding support
 * Copyright (C) 2012-2014, AdaCore
 */

#include <iconv.h>
#include <errno.h>
#include <locale.h>

const int gnatcoll_errno_einval = EINVAL;
const int gnatcoll_errno_e2big  = E2BIG;
const int gnatcoll_errno_eilseq = EILSEQ;

void gnatcoll_iconv_set_locale(){
  setlocale (LC_ALL, "");
}

void *gnatcoll_iconv_open(char *tocode, char *fromcode){
  iconv_t res = iconv_open(tocode, fromcode);
  return (res == (iconv_t) -1) ? NULL : res;
}

int gnatcoll_iconv_close(iconv_t cd) {
   // iconv_close might be a macro
   return iconv_close (cd);
}

#if _LIBICONV_VERSION >= 0x010D
size_t gnatcoll_iconv
   (iconv_t cd,  const char** inbuf, size_t *inbytesleft, char** outbuf,
    size_t *outbytesleft)
#else
size_t gnatcoll_iconv
   (iconv_t cd,  char** inbuf, size_t *inbytesleft, char** outbuf,
    size_t *outbytesleft)
#endif
{
   // iconv might be a macro
   return iconv(cd, inbuf, inbytesleft, outbuf, outbytesleft);
}
