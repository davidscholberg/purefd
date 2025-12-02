#ifndef _POSIX_HELPER_H
#define _POSIX_HELPER_H

#include <errno.h>
#include <dirent.h>
#include <string.h>
#include <sys/stat.h>

static inline int ripfd_is_dir(const char* path) {
    struct stat statbuf;

    int ret = stat(path, &statbuf);
    if (ret == -1) {
        if (errno == ENOENT)
            return 0;

        return ret;
    }

    return (statbuf.st_mode & S_IFMT) == S_IFDIR;
}

static inline char* ripfd_get_dir_entry(DIR* dirstream, int* entry_is_dir) {
    while (1) {
        errno = 0;
        struct dirent* entry = readdir(dirstream);

        if (!entry)
            return 0;

        if (strcmp(entry->d_name, ".") && strcmp(entry->d_name, "..")) {
#if defined(_DIRENT_HAVE_D_TYPE) && defined(_DEFAULT_SOURCE)
            if (entry->d_type != DT_UNKNOWN)
                *entry_is_dir = entry->d_type == DT_DIR;
            else
                *entry_is_dir = -1;
#else
            *entry_is_dir = -1;
#endif

            return entry->d_name;
        }
    }
}

#endif //_POSIX_HELPER_H
