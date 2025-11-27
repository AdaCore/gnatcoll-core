#include "sigchld.h"
#include <stdio.h>

/* Global variable to hold the previous mask */
static sigset_t saved_mask;
static int mask_saved = 0;

int block_sigchld(void)
{
    sigset_t block_set;

    sigemptyset(&block_set);
    sigaddset(&block_set, SIGCHLD);

    /* Block SIGCHLD and save the current mask */
    if (sigprocmask(SIG_BLOCK, &block_set, &saved_mask) == -1) {
        perror("sigprocmask - block");
        return -1;
    }

    mask_saved = 1;
    return 0;
}

int restore_sigmask(void)
{
    if (!mask_saved) {
        fprintf(stderr, "restore_sigmask: mask not saved\n");
        return -1;
    }

    /* Restore the saved mask */
    if (sigprocmask(SIG_SETMASK, &saved_mask, NULL) == -1) {
        perror("sigprocmask - restore");
        return -1;
    }

    mask_saved = 0;
    return 0;
}