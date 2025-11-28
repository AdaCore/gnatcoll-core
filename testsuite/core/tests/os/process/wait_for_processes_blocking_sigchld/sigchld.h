#ifndef SIGCHLD_H
#define SIGCHLD_H

#include <signal.h>

/*
 * Block SIGCHLD and save the old mask internally.
 * Returns 0 on success, -1 on error.
 */
int block_sigchld(void);

/*
 * Restore the signal mask saved by block_sigchld().
 * Returns 0 on success, -1 on error.
 */
int restore_sigmask(void);

#endif // SIGCHLD_H