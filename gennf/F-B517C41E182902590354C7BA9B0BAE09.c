#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <dirent.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#ifdef __CYGWIN__
#include <process.h>
#endif

/*
 * Null pointer test
 */

int mcvs_null_pointer_p(void *ptr)
{
  return ptr == 0;
}

/*
 * <errno.h> stuff
 */

int mcvs_get_errno(void)
{
  return errno;
}

int mcvs_set_errno(int value)
{
  return errno = value;
}

/*
 * <dirent.h> stuff
 */

typedef struct {
  unsigned long d_ino;
  char d_name[1024];
} mcvs_dirent;

mcvs_dirent *mcvs_readdir(DIR *dir)
{ 
  static mcvs_dirent dw;
  struct dirent *de = readdir(dir);
  if (de != 0) {
    strncpy(dw.d_name, de->d_name, sizeof dw.d_name - 1);
    dw.d_ino = de->d_ino;
    return &dw;
  }
  return 0;
}

/*
 * <unistd.h> stuff
 */

char *mcvs_readlink(const char *path)
{
  size_t size = 256;
  char *str = malloc(size);
  char *temp;

  if (str == 0)
    goto bail;

  for (;;) {
    int result = readlink(path, str, size);
    if (result == -1)
      goto bail;
    if (result < size) {
      str[result] = 0;
      break;
    }
    if (size * 2 < size)
      goto bail;
    size *= 2;
    if ((temp = realloc(str, size)) == 0)
      goto bail;
    str = temp;
  }

  /* No need to realloc to actual size, since CLISP will free this anyway */ 
  return str;

bail:
  free(str);
  return 0;
}

/*
 * <sys/stat.h> stuff
 */

struct mcvs_stat {
  unsigned long dev;
  unsigned long ino;
  unsigned long mode;
  unsigned int nlink;
  unsigned int uid;
  unsigned int gid;
  unsigned long rdev;
  unsigned long blksize;
  unsigned long blocks;
  unsigned long atime;
  unsigned long mtime;
  unsigned long ctime;
};

static void stat_to_wrap(struct mcvs_stat *out, const struct stat *in)
{
  out->dev = in->st_dev;
  out->ino = in->st_ino;
  out->mode = in->st_mode;
  out->nlink = in->st_nlink;
  out->uid = in->st_uid;
  out->gid = in->st_gid;
  out->rdev = in->st_rdev;
  out->blksize = in->st_blksize;
  out->blocks = in->st_blocks;
  out->atime = in->st_atime;
  out->mtime = in->st_mtime;
  out->ctime = in->st_ctime;
}

#define IMPL_STAT(FUNC, ARGTYPE) \
int mcvs_ ## FUNC(ARGTYPE arg, struct mcvs_stat *buf) \
{ \
  struct stat sbuf; \
  int retval = FUNC(arg, &sbuf); \
  if (retval == 0) \
    stat_to_wrap(buf, &sbuf); \
  return retval; \
}

IMPL_STAT(stat, const char *)
IMPL_STAT(lstat, const char *)
IMPL_STAT(fstat, int)

/*
 * <unistd.h> -- getcwd
 */

const char *mcvs_getcwd(void)
{
  size_t size = 256;
  char *str = malloc(size);
  char *temp;

  if (str == 0)
    goto bail;

  while (getcwd(str, size) == 0) {
    if (errno != ERANGE)
      goto bail;
    if (size * 2 < size)
      goto bail;
    size *= 2;
    if ((temp = realloc(str, size)) == 0)
      goto bail;
    str = temp;
  }

  /* No need to realloc to actual size, since CLISP will free this anyway */ 
  return str;

bail:
  free(str);
  return 0;
}

/* 
 * We need this because CLISP sets it to SIG_IGN, which is
 * inherited by the exec'd image, and causes the wait()
 * functions to have behavior that programs don't expect.
 */

void mcvs_default_sigchld()
{
    signal(SIGCHLD, SIG_DFL);
}

/*
 * <unistd.h> -- fork, wait*, exec*
 */

#ifdef __CYGWIN__
/*
 * On Cygwin, we have a straightforward alternative to fork exec 
 * and wait, so let's use it.
 */

int mcvs_spawn(const char *name, const char *const *argv)
{
  return spawnvp(_P_WAIT, name, argv);
}
#else
int mcvs_spawn(const char *name, char *const *argv)
{
  int result = -1;
  int status = 0;
  pid_t child;

  mcvs_default_sigchld();

  child = fork();

  if (child == -1)
    goto out;

  if (child == 0) {
    execvp(name, argv);
    _exit(EXIT_FAILURE);
  }

  do {
    result = waitpid(child, &status, 0);
  } while (result == -1 && errno == EINTR);

  if (result == -1)
    goto out;

  if (WIFEXITED(status))
    result = WEXITSTATUS(status);

out:
  free((void *) argv);
  return result;
}
#endif

/*
 * Terminal handling
 */

char *mcvs_ctermid(void)
{
  char *buf = malloc(L_ctermid + 1);
  if (buf == 0)
    return 0;
  return ctermid(buf);
}
