#ifndef _POSIX_HELPER_H
#define _POSIX_HELPER_H

#include <errno.h>
#include <dirent.h>
#include <string.h>
#include <sys/stat.h>

static inline int fh_is_dir(const char* path) {
    struct stat statbuf;

    int ret = stat(path, &statbuf);
    if (ret == -1) {
        if (errno == ENOENT)
            return 0;

        return ret;
    }

    return (statbuf.st_mode & S_IFMT) == S_IFDIR;
}

static inline char* fh_get_dir_entry(DIR* dirstream) {
    while (1) {
        errno = 0;
        struct dirent* entry = readdir(dirstream);

        if (!entry)
            return 0;

        if (strcmp(entry->d_name, ".") && strcmp(entry->d_name, ".."))
            return entry->d_name;
    }
}

#endif //_POSIX_HELPER_H
