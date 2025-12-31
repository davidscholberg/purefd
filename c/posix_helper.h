#ifndef _POSIX_HELPER_H
#define _POSIX_HELPER_H

#include <dirent.h>

int purefd_is_dir(const char* path);

char* purefd_get_dir_entry(DIR* dirstream, int* entry_is_dir);

#endif //_POSIX_HELPER_H
